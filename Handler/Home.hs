{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import hiding (Entity)
import EntityDB

getHomeR :: Handler Html
getHomeR = do
    results <- liftIO $ fetchEntityDataByID [131426..131475]
    let entities = map (\(Entity nr _ names) -> (mainName names, nr)) results
    defaultLayout $(widgetFile "entityname")

-- non-partial replacement for classic head function
mainName :: [String] -> String
mainName (x:_) = x
mainName _ = ""