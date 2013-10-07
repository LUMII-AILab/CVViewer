{-# LANGUAGE OverloadedStrings #-}
module Handler.EntityFrames where

import Import hiding (Entity, entityID)
import EntityDB
import Frames
import Data.List ( (\\) )

getEntityFramesR :: Int -> Handler Html
getEntityFramesR nr = do
    entities <- liftIO $ fetchEntityDataByID [nr]
    let name = entityName entities
    frames2 <- liftIO $ fetchFrames [nr] []
    let frames = filter (\(Frame x _ _ _ _ _ _) -> notElem x [26164,63883, 26163, 26175, 26254, 45336]) frames2 --FIXME- jālabo šo freimu attēlošana un jāliek atpakaļ
    let bio = filterFrames ["Dzimšana","Miršana", "Attiecības"] frames
    let edu = filterFrames ["Izglītība"] frames
    let win = filterFrames ["Sasniegums"] frames
    let work = filterFrames ["Amats", "Darba sākums", "Darba beigas"] frames
    let others = frames \\ (bio ++ edu ++ win ++ work)
    --liftIO $ putStrLn $ show $ (\(Frame _ _ _ _ _ x) -> x) $ bio !! 0
    --liftIO $ mapM_ (\(Frame _ _ _ _ _ x) -> putStrLn $ concat $ map (\(_,role,entityID, entity) -> "<br/>" ++ role ++ ": " ++ entity ++ "(" ++ show entityID ++ ")") x) bio
    defaultLayout $(widgetFile "entityframes")

entityName :: [Entity] -> String
entityName ((Entity _ _ (name:_)):_) = name
entityName _ = ""

describeElements :: [Element] -> [Html]
describeElements =
 map (\(_,role,entityID, entity) -> preEscapedToMarkup $ "<br/>" ++ role ++ ": " ++ entity ++ "(" ++ show entityID ++ ")")

filterFrames :: [String] -> [Frame] -> [Frame]
filterFrames frameTypes = filter (\(Frame _ _ x _ _ _ _) -> elem x frameTypes)