{-# LANGUAGE OverloadedStrings #-}
module Handler.EntityFrames where

import Import hiding (Entity, entityID)
import EntityDB
import Frames
import Data.List ( (\\), sortBy )
import Data.Ord

getEntityFramesR :: Int -> Handler Html
getEntityFramesR = _getEntityFramesR True

getEntityRawFramesR :: Int -> Handler Html
getEntityRawFramesR = _getEntityFramesR False

_getEntityFramesR :: Bool -> Int -> Handler Html
_getEntityFramesR summary nr = do
    entities <- liftIO $ fetchEntityDataByID [nr]
    let name = entityName entities
    frames2 <- liftIO $ if summary then fetchSummaryFrames [nr] []
                                   else fetchFrames [nr] []
    let frames = sortBy (comparing $ \(Frame _ _ _ _ _ _ _ x _) -> -x) frames2 
    let bio = filterFrames ["Dzimšana","Miršana", "Attiecības"] frames
    let edu = filterFrames ["Izglītība"] frames
    let win = filterFrames ["Sasniegums"] frames
    let work = filterFrames ["Amats", "Darba sākums", "Darba beigas"] frames
    let others = frames \\ (bio ++ edu ++ win ++ work)
    let sections = [(bio, "Personas dati, ģimenes stāvoklis"), (edu, "Izglītība"), (win, "Sasniegumi"), (work, "Karjera"), (others, "Cits")] :: [([Frame], String)]
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
filterFrames frameTypes = filter (\(Frame _ _ x _ _ _ _ _ _) -> elem x frameTypes)