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

import Database.Persist.Sql (
  fromSqlKey,
 )
import Import

type DocumentId = Int64

nextTask :: TaskType -> DB (Maybe TaskType)
nextTask PurchaseInvoiceProcessingTaskVerify = return $ Just PurchaseInvoiceProcessingTaskApproveOrReject
nextTask PurchaseInvoiceProcessingTaskApproveOrReject = return Nothing
nextTask _ = sendResponseStatus status500 ("Illegal task" :: Text)

setNewTasks :: (ToBackendKey SqlBackend a) => Entity a -> TaskType -> DB ()
setNewTasks entity tasks' = do
  let documentId = fromSqlKey (entityKey entity)
  let tasks = [tasks']
  companyId <- liftHandler getCompanyId
  forM_ tasks $ \task -> do
    groupId <- insert $ TaskGroup{taskGroupCompanyId = companyId, taskGroupName = "generic", taskGroupComplete = False, taskGroupDocumentId = documentId, taskGroupTask = task}
    userIds <- getUserIds task
    keys <-
      mapM
        ( \userId ->
            do
              key <- insert (Task companyId groupId userId False Nothing False)
              return ()
        )
        userIds
    return ()

removeTasks :: (ToBackendKey SqlBackend a, SymbolToField "document_status" a DocumentStatus) => TaskId -> Key a -> DB ()
removeTasks taskId documentId = do
  task <- getEntity404' taskId
  if taskComplete (entityVal task)
    then
      ( do
          task <- getEntity404' taskId
          taskGroup <- getEntity404' $ taskTaskGroupId (entityVal task)
          let lastTaskType = taskGroupTask (entityVal taskGroup)
          let previousStatus = getPreviousStatusByTask lastTaskType
          let documentId' = taskGroupDocumentId (entityVal taskGroup)

          taskGroupKeys <- selectKeysList [TaskGroupDocumentId ==. documentId', TaskGroupId >. taskTaskGroupId (entityVal task)] []
          -- Delete all tasks that are following this task, they will be reset
          deleteWhere [TaskTaskGroupId <-. taskGroupKeys]

          -- Delete all taskgroups that are following this taskgroup for this document, they will be reset
          deleteWhere [TaskGroupId <-. taskGroupKeys]

          -- Mark task as removed
          update (entityKey task) [TaskRemoved =. True]

          -- Mark task group as not completed
          update (entityKey taskGroup) [TaskGroupComplete =. False]

          -- Update document status
          update documentId [#document_status =. PurchaseInvoiceStatus previousStatus]

          companyId <- liftHandler getCompanyId

          userIds <- getUserIds lastTaskType
          keys <-
            mapM
              ( \userId ->
                  do
                    -- If there is a removed task (task marked as removed) for the user, we need to add a new one
                    key <- insert (Task companyId (taskTaskGroupId (entityVal task)) userId False Nothing False)
                    return ()
              )
              userIds
          return ()
      )
    else sendResponseStatus status400 ("Task is not completed and cannot therefore be removed. Complete it first!" :: Text)

getUserIds :: TaskType -> DB [Key User]
getUserIds task = do
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
            else return $ map (userRoleUserId . entityVal) usersroles
    Nothing -> sendResponseStatus status400 ("AccessRight table is missing " ++ show task ++ " right. Please add.")

getPreviousStatusByTask :: TaskType -> PurchaseInvoiceStatus
getPreviousStatusByTask task = case task of
  PurchaseInvoiceProcessingTaskVerify -> PurchaseInvoiceStatusInvoiceCreated
  PurchaseInvoiceProcessingTaskApproveOrReject -> PurchaseInvoiceStatusInvoiceVerified

completeTaskOrFail :: DocumentId -> TaskType -> TaskResult -> DB ()
completeTaskOrFail documentId task result = do
  taskEntity <- findTaskEntityOrFail documentId task

  if taskComplete (entityVal taskEntity {- && not (taskRemoved (entityVal taskEntity)) -})
    then sendResponseStatus status400 ("The user has already handled this task" :: Text)
    else do
      -- Update the task by completing it
      update
        (entityKey taskEntity)
        [ TaskComplete =. True
        , TaskResult =. Just result
        ]

findTaskEntityOrFail :: DocumentId -> TaskType -> DB (Entity Task)
findTaskEntityOrFail documentId taskType = do
  user <- liftHandler getAuthenticatedUser
  taskGroupEntity <-
    selectFirst
      [ TaskGroupDocumentId ==. documentId
      , TaskGroupTask ==. taskType
      , TaskGroupComplete ==. False
      ]
      []

  taskEntity <- case taskGroupEntity of
    Just x -> do
      selectFirst
        [ TaskUserId ==. entityKey user
        , TaskRemoved ==. False -- removed tasks do not count
        , TaskTaskGroupId ==. entityKey x
        ]
        []
    Nothing -> liftHandler $ sendResponseStatus status404 ("TaskGroup not found" :: Text)

  case taskEntity of
    Just task -> return task
    Nothing -> liftHandler $ sendResponseStatus status404 ("This document has not been set " ++ show taskType ++ " task")

processTasks :: GenericGADT _ -> TaskType -> TaskResult -> DocumentStatus -> DB (Maybe (GenericGADT _))
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
      taskList <- selectList [TaskTaskGroupId ==. entityKey group, TaskRemoved ==. False] []
      result <- case taskList of
        -- If many users are involved in the process we do not accept contradictory results
        (x : xs : xss) -> do
          -- get the first element of the list
          let first = taskResult (entityVal $ unsafeHead taskList)
          -- check if all results are the same as the first element
          let same = all ((== first) . (taskResult . entityVal)) taskList
          let allComplete = all ((== True) . (taskComplete . entityVal)) taskList

          -- TODO: For tasks that can be completed by just 1 user, we should check the "task type" (single, multiple)
          -- and act accordingly here! Currently all "approvers" etc. must "approve" the document before we can progress

          if same && allComplete then return True else return False
        -- If just one user is involved in the process
        [x] -> do
          if (taskComplete . entityVal) x then return True else return False
        _ -> return False
      if result
        then do
          -- Update document status
          update key [#document_status =. newStatus]
          -- Mark task group as complete
          update (entityKey group) [TaskGroupComplete =. True]
          newValue <- get404 key
          -- set next task if any
          next <- nextTask task
          forM_ next (setNewTasks entity)

          -- return the document with new status
          return $ Just $ MkDocument{entityDocument = Entity key newValue}
        else return Nothing
    _ -> return Nothing
