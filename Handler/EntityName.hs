module Handler.EntityName where

import Import hiding (Entity)
import EntityDB
import Data.Text(unpack, length)
import Data.Maybe(fromJust)

getEntityNameR :: Handler Html
getEntityNameR = do
    query <- lookupGetParam "entityName"
    results <- case query of
        Nothing -> return Nothing -- Nav kverija, nav cepumiņu
        Just name ->
            if Data.Text.length name < 2 
                then return Nothing -- Tukšais kverijs ir ļauns jo API iedos visas entītes, kas ir par daudz
                else do
                    entities <- liftIO $ formatEntityResults name
                    return $ if null entities
                        then Nothing -- ja neatradām; FIXME - te vajag uzsetot paziņojumu ka neatradām
                        else Just entities
    case results of
        Just entities -> -- Ja ir pieprasīts konkrēts vārds
            defaultLayout $ do
                setTitle $ toHtml $ fromJust query
                $(widgetFile "entityname")
        Nothing -> do -- Ja nav, tad rādam formu
            (formWidget, formEnctype) <- generateFormGet $ renderDivs $ areq textField "entityName" Nothing
            let submission = Nothing :: Maybe (FileInfo, Text)
                handlerName = "getHomeR" :: Text
            defaultLayout $ do
                aDomId <- newIdent
                $(widgetFile "entitynameform")    

formatEntityResults :: Text -> IO [(String,Int)]
formatEntityResults name = do
    ids <- fetchEntityIDsByName [unpack name]
    entities <- fetchEntityDataByID ids
    return $ map (\(Entity nr _ names) -> (mainName names, nr)) entities

-- non-partial replacement for classic head function
mainName :: [String] -> String
mainName (x:_) = x
mainName _ = ""