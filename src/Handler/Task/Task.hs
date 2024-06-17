{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}


module Handler.Task.Task where

import Data.Foldable hiding (all, forM_, mapM_, any, elem, null)
import Database.Persist.Sql (
  toSqlKey, fromSqlKey,
 )
import Import

type DocumentId = Int64

setTasks :: (ToBackendKey SqlBackend a) => CompanyId -> Entity a -> [Task] -> DB ()
setTasks companyId entity tasks = do
  let documentId = fromSqlKey (entityKey entity)
  forM_ tasks $ \task -> do
    groupId <- insert $ TaskGroup {taskGroupName="generic",taskGroupComplete=False,taskGroupDocumentId=documentId, taskGroupTask=task}
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
                keys <- mapM (\userId ->
                  do
                    -- Let's check if the user already has the same task pending
                    taskGroup <- selectFirst [TaskGroupDocumentId ==. documentId,
                                              TaskGroupTask ==. task
                                              --TaskGroupStatus ==. not pending!
                                              ] []

                    taskEntity <- case taskGroup of
                      Just group -> do
                        selectFirst [WorkQueueTaskGroupId ==. entityKey group,
                                               WorkQueueUserId ==. userId,
                                               WorkQueueTaskcomplete ==. False
                                              ] []
                      Nothing -> undefined  
                    case taskEntity of
                      Nothing -> do
                        -- Add task
                        key <- insert (WorkQueue groupId userId False Nothing False)
                        return ()
                      _ -> return ()
                      ) userIds

                return ()
      Nothing -> sendResponseStatus status400 ("AccessRight table is missing " ++ show task ++ " right. Please add.")

--dependentTasks :: [[Task]]
--dependentTasks = [[PurchaseInvoiceProcessingTaskApprove,PurchaseInvoiceProcessingTaskReject]]

{- getDependentTasks :: Task ->  [Task]
getDependentTasks task = do
        let allTasks = foldl (\acc item -> if task `elem` item then item else acc
                                                  ) [] dependentTasks
        -- remove task from the allTasks list
        filter (/= task) allTasks
 -}
-- dependent tasks!
completeTaskOrFail :: DocumentId -> Task -> TaskResult -> DB ()
completeTaskOrFail documentId task result = do
  taskEntity <- findTaskEntityOrFail documentId task
 -- let taskType = taskGroupTask (entityVal taskEntity)
 -- dependentEntities <- findDependentEntities documentId taskType

  if workQueueTaskcomplete (entityVal taskEntity)
    then sendResponseStatus status400 ("The user has already handled this task" :: Text)
    else do
      -- Update the task by compliting it
      update
        (entityKey taskEntity)
        [ WorkQueueTaskcomplete =. True
        , WorkQueueTaskresult =. Just result
        ]

      -- Update dependent tasks by completing them 
   --   mapM_ (\entity-> do
   --     update (entityKey entity) [WorkQueueTaskcomplete =. True, WorkQueueTaskresult =. Nothing] ) dependentEntities

findTaskEntityOrFail :: DocumentId -> Task -> DB (Entity WorkQueue)
findTaskEntityOrFail documentId task = do
  user <- liftHandler $ getAuthenticatedUser
  taskGroupEntity <-
    selectFirst
      [ TaskGroupDocumentId ==.  documentId
      , TaskGroupTask ==. task,
        TaskGroupComplete ==. False
      ]
      []

  taskEntity <- case taskGroupEntity of
    Just x -> do
        selectFirst
          [ WorkQueueUserId ==. entityKey user
          , WorkQueueTaskGroupId ==. (entityKey x)
          ]
          [] 
    Nothing -> liftHandler $ sendResponseStatus status404 ("TaskGroup not found" :: Text)
 
  case taskEntity of
    Just task -> return task
    Nothing -> liftHandler $ sendResponseStatus status404 ("This document has not been set " ++ show task ++ " task")

{- findDependentEntities :: DocumentId -> Task -> DB [Entity WorkQueue]
findDependentEntities documentId task = do
  user <- liftHandler $ getAuthenticatedUser
  taskEntities <-
    selectList
      [ WorkQueueDocumentId ==. documentId
      , WorkQueueUserId ==. entityKey user
      ]
      []
  return $ filter (\item->workQueueTask (entityVal item) `elem` getDependentTasks task) taskEntities
 -}
removeTask :: DocumentId -> Task -> DB ()
removeTask documentId task = do
  taskEntity <- findTaskEntityOrFail documentId task
  if workQueueTaskcomplete (entityVal taskEntity)
    then update (entityKey taskEntity) [WorkQueueRemoved =. True]
    else sendResponseStatus status400 ("Task must be completed before it can be removed" :: Text)

processTasks :: GenericGADT _ -> Task -> TaskResult -> DocumentStatus -> DB (Maybe (GenericGADT _))
processTasks gadtEntity task result newStatus = do

   case gadtEntity of
      MkDocument (Entity key _) -> do
        completeTaskOrFail  (fromSqlKey key) task result

        taskGroup <- selectList
          [ TaskGroupTask ==. task
          , TaskGroupDocumentId ==. fromSqlKey key
          , TaskGroupComplete ==. False
          ]
          []
        case taskGroup of
          [group] -> do
            taskList <- selectList[WorkQueueTaskGroupId ==. entityKey group][]
            case taskList of
              
              -- If many users are involved in the process we do not accept contradictory results
              (x:xs:xss) -> do
                -- get the first element of the list
                let first = workQueueTaskresult (entityVal $ unsafeHead taskList)
                -- check if all results are the same as the first element
                let same = all ((==first) . (workQueueTaskresult . entityVal)) taskList
                let allComplete = all ((==True) . (workQueueTaskcomplete . entityVal)) taskList

                -- TODO: For tasks that can be completed by just 1 user, we should check the "task type" (single, multiple)
                -- and act accordingly here! Currently "all approvers" must approve then invoice before the invoice
                -- can be set Open
                if same && allComplete
                  then do
                    update key [#document_status =. newStatus]
                    update (entityKey group) [TaskGroupComplete =. True]

                  -- TODO: This is a bit clumsy and slow, should be able to update the record instead, not fetch it from the
                  -- database again
                    newValue <- get404 key
                    return $ Just $ MkDocument {entityDocument=Entity key newValue}
                  else return $ Nothing
              -- If just one user is involved in the process
              [x] -> do
                  if (workQueueTaskcomplete . entityVal) x
                  then do
                    update key [#document_status =. newStatus]
                    newValue <- get404 key
                    return $ Just $ MkDocument {entityDocument=Entity key newValue}       
                  else do
                    return $ Nothing
              _ -> return $ Nothing
          _ -> return $ Nothing
      


removeCompletedAction :: DocumentId -> Task -> DB ()
removeCompletedAction documentId task = do
  removeTask documentId task

