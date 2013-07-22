module Handler.EntityName where

import Import
import EntityDB
import Data.Text(unpack)

getEntityNameR :: Handler String
getEntityNameR = do
    result <- lookupGetParam "entityName"
    case result of
        Just name -> liftIO $ formatEntityResults name
        Nothing -> invalidArgs ["Field 'entityName' expected"]

formatEntityResults :: Text -> IO String
formatEntityResults name = do
    ids <- fetchEntityIDsByName [unpack name]
    entities <- fetchEntityDataByID ids
    return $ show entities