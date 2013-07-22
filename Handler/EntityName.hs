module Handler.EntityName where

import Import

getEntityNameR :: Handler Text
getEntityNameR = do
    result <- lookupGetParam "entityName"
    case result of
        Just name -> return name
        Nothing -> invalidArgs ["Field 'entityName' expected"]
