{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import hiding (Entity)
import EntityDB

getHomeR :: Handler Html
getHomeR = do
    results <- liftIO $ fetchEntityDataByID $ [1248014..1248021] ++ [131426..131475] ++ [1002911,1002884] ++ [1003759..1003786]
    let entities = map (\(Entity nr _ names) -> (mainName names, nr)) results
    defaultLayout $(widgetFile "entityname")

-- non-partial replacement for classic head function
mainName :: [String] -> String
mainName (x:_) = x
mainName _ = ""
