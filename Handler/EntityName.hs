module Handler.EntityName where

import Import hiding (Entity)
import EntityDB
import Data.Text(unpack)

getEntityNameR :: Handler Html
getEntityNameR = do
    result <- lookupGetParam "entityName"
    let name = case result of
            Just x -> x
            Nothing -> "Imants Ziedonis"
    defaultLayout $ do
            entities <- liftIO $ formatEntityResults name
            setTitle $ toHtml name
            $(widgetFile "entityname")

formatEntityResults :: Text -> IO [(String,Int)]
formatEntityResults name = do
    ids <- if (name == "Imants Ziedonis") then return [250423, 274418] -- FIXME - temporary hack
        else fetchEntityIDsByName [unpack name]
    entities <- fetchEntityDataByID ids
    return $ map (\(Entity nr _ names) -> (mainName names, nr)) entities

-- non-partial replacement for classic head function
mainName :: [String] -> String
mainName (x:_) = x
mainName _ = ""