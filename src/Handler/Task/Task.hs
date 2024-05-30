{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.Task.Task where

import Data.Foldable hiding (forM_, mapM_, any, elem, null)
import Database.Persist.Sql (
  toSqlKey, fromSqlKey,
 )
import Import

type DocumentId = Int64

setTasks :: (ToBackendKey SqlBackend a) => CompanyId -> Entity a -> [Task] -> DB ()
setTasks companyId entity tasks = do
  let documentId = fromSqlKey (entityKey entity)
  forM_ tasks $ \task -> do
    accessRightEntity <- selectFirst [AccessRightRight ==. task] []
    case accessRightEntity of
      Just accessRightEntity -> do
        roles <- selectList [AccessRightRoleAccessRightId ==. (entityKey accessRightEntity), AccessRightRoleCompanyId ==. companyId] []
        if null roles
          then sendResponseStatus status400 ("A role with " ++ show task ++ " right is missing. Please add.")
          else do
            let roleIds = map accessRightRoleRoleId (map entityVal roles)
            users <- selectList [UserCompanyCompanyId ==. companyId] []
            let userIds = map (userCompanyUserId . entityVal) users
            usersroles <- selectList [UserRoleUserId <-. userIds, UserRoleRoleId <-. roleIds] []
            if null usersroles
              then sendResponseStatus status400 ("A user with a role with" ++ show task ++ " right is missing. Please add.")
              else do
                let userIds = map userRoleUserId (map entityVal usersroles)
                keys <- mapM (\userId ->
                  do
                    -- Let's check if the user  already has the same task pending
                    taskEntity <- selectFirst [WorkQueueDocumentId ==. documentId,
                                              WorkQueueUserId ==. userId,
                                              WorkQueueTask ==. task,
                                              WorkQueueTaskcomplete ==. False] []
                    case taskEntity of
                      Nothing -> do
                        -- Add task
                        key <- insert (WorkQueue documentId userId task False Nothing False)
                        return ()
                      _ -> return ()
                      ) userIds

                return ()
      Nothing -> sendResponseStatus status400 ("AccessRight table is missing " ++ show task ++ " right. Please add.")
dependentTasks :: [[Task]]
dependentTasks = [[PurchaseInvoiceProcessingTaskApproved,PurchaseInvoiceProcessingTaskRejected]]

getDependentTasks :: Task ->  [Task]
getDependentTasks task = do
        let allTasks = foldl (\acc item -> if task `elem` item then item else acc
                                                  ) [] dependentTasks
        -- remove task from the allTasks list
        filter (/= task) allTasks

-- dependent tasks!
completeTaskOrFail :: DocumentId -> Task -> DB ()
completeTaskOrFail documentId task = do
  taskEntity <- findTaskEntityOrFail documentId task
  let taskType = workQueueTask (entityVal taskEntity)
  dependentEntities <- findDependentEntities documentId taskType

  if workQueueTaskcomplete (entityVal taskEntity)
    then sendResponseStatus status400 ("The user has already handled this task" :: Text)
    else do
      -- Update the task by compliting it
      update
        (entityKey taskEntity)
        [ WorkQueueTaskcomplete =. True
        , WorkQueueTaskresult =. Just task
        ]

      -- Update dependent tasks by completing them 
      mapM_ (\entity-> do
        update (entityKey entity) [WorkQueueTaskcomplete =. True, WorkQueueTaskresult =. Nothing] ) dependentEntities


findTaskEntityOrFail :: DocumentId -> Task -> DB (Entity WorkQueue)
findTaskEntityOrFail documentId task = do
  user <- liftHandler $ getAuthenticatedUser
  taskEntity <-
    selectFirst
      [ WorkQueueDocumentId ==.  documentId
      , WorkQueueTask ==. task
      , WorkQueueUserId ==. entityKey user
      ]
      []
  case taskEntity of
    Just task -> return task
    Nothing -> liftHandler $ sendResponseStatus status404 ("This document has not been set " ++ show task ++ " task")


findDependentEntities :: DocumentId -> Task -> DB [Entity WorkQueue]
findDependentEntities documentId task = do
  user <- liftHandler $ getAuthenticatedUser
  taskEntities <-
    selectList
      [ WorkQueueDocumentId ==. documentId
      , WorkQueueUserId ==. entityKey user
      ]
      []
  let delendentTasks' = getDependentTasks task
  return $ filter (\item->elem (workQueueTask (entityVal item)) delendentTasks') taskEntities

removeTask :: DocumentId -> Task -> DB ()
removeTask documentId task = do
  taskEntity <- findTaskEntityOrFail documentId task
  if workQueueTaskcomplete (entityVal taskEntity)
    then update (entityKey taskEntity) [WorkQueueRemoved =. True]
    else sendResponseStatus status400 ("Task must be completed before it can be removed" :: Text)

processTasks :: (ToBackendKey SqlBackend a) => Entity a -> Task -> PurchaseInvoiceProcessingStatus -> DB ()
processTasks entity task status = do
  let documentId = fromSqlKey $ entityKey entity
  completeTaskOrFail documentId task
  taskList <-
    selectList
      [ WorkQueueTask ==. task
      , WorkQueueDocumentId ==. documentId
      , WorkQueueTaskcomplete ==. False
      ]
      []
  if null taskList
    then do
      update (toSqlKey documentId) [PurchaseInvoiceProcessingStatus =. status]
      return ()
    else return ()

removeCompletedAction :: DocumentId -> Task -> DB ()
removeCompletedAction documentId task = do
  removeTask documentId task
