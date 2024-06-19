{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Task.Task where

import Data.Foldable hiding (all, any, elem, forM_, mapM_, null)
import Database.Persist.Sql (
  fromSqlKey,
  toSqlKey,
 )
import Import

type DocumentId = Int64


nextTask :: Task -> DB (Maybe Task)
nextTask PurchaseInvoiceProcessingTaskVerify = return $ Just PurchaseInvoiceProcessingTaskApproveOrReject
nextTask PurchaseInvoiceProcessingTaskApproveOrReject = return Nothing

nextTask _ = sendResponseStatus status500 ("Illegal task" :: Text)

setTasks :: (ToBackendKey SqlBackend a) => Entity a -> Task -> DB ()
setTasks entity tasks' = do
  let documentId = fromSqlKey (entityKey entity)
  let tasks = [tasks']
  forM_ tasks $ \task -> do
    groupId <- insert $ TaskGroup{taskGroupName = "generic", taskGroupComplete = False, taskGroupDocumentId = documentId, taskGroupTask = task}
    accessRightEntity <- selectFirst [AccessRightRight ==. task] []
    case accessRightEntity of
      Just accessRightEntity -> do
        roles <- selectList' [AccessRightRoleAccessRightId ==. entityKey accessRightEntity] []
        if null roles
          then sendResponseStatus status400 ("A role with " ++ show task ++ " right is missing. Please add.")
          else do
            let roleIds = map (accessRightRoleRoleId . entityVal) roles
            users <- selectList' [] []
            let userIds = map (userCompanyUserId . entityVal) users
            usersroles <- selectList [UserRoleUserId <-. userIds, UserRoleRoleId <-. roleIds] []
            if null usersroles
              then sendResponseStatus status400 ("A user with a role with" ++ show task ++ " right is missing. Please add.")
              else do
                let userIds = map (userRoleUserId . entityVal) usersroles
                keys <-
                  mapM
                    ( \userId ->
                        do
                          -- Let's check if the user already has the same task pending
                          taskGroup <-
                            selectFirst
                              [ TaskGroupDocumentId ==. documentId
                              , TaskGroupTask ==. task
                              --TaskGroupStatus ==. not pending!
                              ]
                              []

                          workQueueEntity <- case taskGroup of
                            Just group -> do
                              selectFirst
                                [ WorkQueueTaskGroupId ==. entityKey group
                                , WorkQueueUserId ==. userId
                                , WorkQueueTaskcomplete ==. False
                                ]
                                []
                            Nothing -> sendResponseStatus status400 ("The task is not set for the document" :: Text)
                          case workQueueEntity of
                            Nothing -> do
                              -- Add task
                              key <- insert (WorkQueue groupId userId False Nothing False)
                              return ()
                            _ -> sendResponseStatus status400 ("The user already has this task set" :: Text)
                    )
                    userIds

                return ()
      Nothing -> sendResponseStatus status400 ("AccessRight table is missing " ++ show task ++ " right. Please add.")

completeTaskOrFail :: DocumentId -> Task -> TaskResult -> DB ()
completeTaskOrFail documentId task result = do
  workQueuEntity <- findTaskEntityOrFail documentId task

  if workQueueTaskcomplete (entityVal workQueuEntity)
    then sendResponseStatus status400 ("The user has already handled this task" :: Text)
    else do
      -- Update the task by completing it
      update
        (entityKey workQueuEntity)
        [ WorkQueueTaskcomplete =. True
        , WorkQueueTaskresult =. Just result
        ]

findTaskEntityOrFail :: DocumentId -> Task -> DB (Entity WorkQueue)
findTaskEntityOrFail documentId task = do
  user <- liftHandler $ getAuthenticatedUser
  taskGroupEntity <-
    selectFirst
      [ TaskGroupDocumentId ==. documentId
      , TaskGroupTask ==. task
      , TaskGroupComplete ==. False
      ]
      []

  workQueueEntity <- case taskGroupEntity of
    Just x -> do
      selectFirst
        [ WorkQueueUserId ==. entityKey user
        , WorkQueueTaskGroupId ==. (entityKey x)
        ]
        []
    Nothing -> liftHandler $ sendResponseStatus status404 ("TaskGroup not found" :: Text)

  case workQueueEntity of
    Just task -> return task
    Nothing -> liftHandler $ sendResponseStatus status404 ("This document has not been set " ++ show task ++ " task")

removeTask :: DocumentId -> Task -> DB ()
removeTask documentId task = do
  taskEntity <- findTaskEntityOrFail documentId task
  if workQueueTaskcomplete (entityVal taskEntity)
    then update (entityKey taskEntity) [WorkQueueRemoved =. True]
    else sendResponseStatus status400 ("Task must be completed before it can be removed" :: Text)

processTasks :: GenericGADT _ -> Task -> TaskResult -> DocumentStatus -> DB (Maybe (GenericGADT _))
processTasks (MkDocument entity@(Entity key _)) task result newStatus = do
      completeTaskOrFail (fromSqlKey key) task result

      taskGroup <-
        selectList
          [ TaskGroupTask ==. task
          , TaskGroupDocumentId ==. fromSqlKey key
          , TaskGroupComplete ==. False
          ]
          []
      case taskGroup of
        [group] -> do
          taskList <- selectList [WorkQueueTaskGroupId ==. entityKey group,WorkQueueRemoved==. False] []
          result <- case taskList of
            -- If many users are involved in the process we do not accept contradictory results
            (x : xs : xss) -> do
              -- get the first element of the list
              let first = workQueueTaskresult (entityVal $ unsafeHead taskList)
              -- check if all results are the same as the first element
              let same = all ((== first) . (workQueueTaskresult . entityVal)) taskList
              let allComplete = all ((== True) . (workQueueTaskcomplete . entityVal)) taskList
         
              -- TODO: For tasks that can be completed by just 1 user, we should check the "task type" (single, multiple)
              -- and act accordingly here! Currently "all approvers" must approve then invoice before the invoice
              -- can be set Open
              if same && allComplete
                then do
                  return True
                else return False
            -- If just one user is involved in the process
            [x] -> do
              if (workQueueTaskcomplete . entityVal) x
                then do
                  print "-----------------------------"
                  return True
                else return False
            _ -> return False
          if result then do
              -- Update document status
              update key [#document_status =. newStatus]
              -- Mark task group as complete
              update (entityKey group) [TaskGroupComplete =. True]
              newValue <- get404 key
              -- set next task if any 
              next <- nextTask task
              case next of
                Just next -> setTasks entity next
                Nothing -> return ()
 
              -- return the document with new status
              return $ Just $ MkDocument{entityDocument = Entity key newValue}
          else return Nothing
        _ -> return Nothing

removeCompletedAction :: DocumentId -> Task -> DB ()
removeCompletedAction documentId task = do
  removeTask documentId task
