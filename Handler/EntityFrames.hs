{-# LANGUAGE OverloadedStrings #-}
module Handler.EntityFrames where

import Import hiding (Entity, entityID)
import EntityDB
import Frames

getEntityFramesR :: Int -> Handler Html
getEntityFramesR nr = do
    entities <- liftIO $ fetchEntityDataByID [nr]
    let name = entityName entities
    frames <- liftIO $ fetchFrames [nr] []
    defaultLayout $(widgetFile "entityframes")

entityName :: [Entity] -> String
entityName ((Entity _ _ (name:_)):_) = name
entityName _ = ""

describeElements :: [Element] -> [Html]
describeElements =
 map (\(_,role,entityID, entity) -> preEscapedToMarkup $ "<br/>" ++ role ++ ": " ++ entity ++ "(" ++ show entityID ++ ")")