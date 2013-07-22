{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module EntityDB (fetchEntityIDsByName, fetchEntityDataByID, fetchFrames) where

import Import hiding (Entity, entityID)
import Network.HTTP.Conduit
import Data.Conduit
import Data.ByteString.Lazy.UTF8 (toString, fromString)
import Data.List
import Text.JSON
import Control.Monad
import Data.Either

data Entity = Entity Int Int [String] -- Entity id category names
	deriving Show

serviceURL :: String
serviceURL = "http://sps.inside.lv:789/Rest/Semantic/"
user :: String
user = "PeterisP"

-- Fetches all frames matching the supplied lists of entity IDs and frame type IDs
fetchFrames :: [Int] -> [Int] -> IO String
fetchFrames entities frametypes = 
	postRequest (serviceURL ++ "GetFrame/" ++ user) 
		("{\"parameterList\":{\"QueryParameters\":[{\"EntityIdList\":[" ++ (formatNumList entities) ++ 
		 "],\"FrameTypes\": [" ++ (formatNumList frametypes) ++ "]}]}}")

fetchEntityDataByID :: [Int] -> IO [Entity]
fetchEntityDataByID ids = do
	json <- postRequest (serviceURL ++ "GetEntityDataById/" ++ user) 
		("{\"entityIdList\":{\"DataSet\":[\"AllUsers\"],\"SearchType\":\"AllData\",\"EntityIdList\":["
		++ (formatNumList ids) ++ "]}}")
	return $ decodeEntities json

-- fetch a list of entity IDs matching the supplied names
fetchEntityIDsByName :: [String] -> IO [Int]
fetchEntityIDsByName names = do
	json <- postRequest (serviceURL ++ "GetEntityIdByName/" ++ user) 
		("{\"entityNameList\":{\"EntityNameList\":[" ++ (formatList names) ++ "]}}")
	return $ decodeIDs json

-- temporary (assumes no errors) function to decode entity ID JSON
decodeIDs :: String -> [Int]
decodeIDs json =
	case (decodeAnswers "EntityIdList" json) of
		Ok answers -> head answers -- FIXME - assumes only one answer
		Error _ -> undefined -- FIXME - no error checking

-- decodes the JSON answer with a list of entities
decodeEntities :: String -> [Entity]
decodeEntities json = 
	case (decodeAnswers "Entity" >=> mapM decodeEntity $ json) of
		Ok answers -> answers
		Error message -> error message -- FIXME - no error checking

-- decodes an entity list
decodeEntity :: JSObject JSValue -> Result Entity
decodeEntity json = do
	name <- valFromObj "Name" json
	entityID <- valFromObj "EntityId" json
	category <- valFromObj "Category" json
	names <- valFromObj "OtherName" >=> readJSONs $ json -- reading a JSON array of strings here
	return $ Entity entityID category (name:names) -- we put the 'official' name as simply the first in list

-- decodes JSON answers to a list of separate answers. 
-- NB! If there are any answers with errorcodes, they will be simply skipped..
decodeAnswers :: (JSON a) => String -> String -> Result [a]
decodeAnswers resultFieldName json = 
	case (decode json) of
		Ok answers -> (liftM rights) ( (valFromObj "Answers") >=> readJSONs >=> mapM (decodeAnswer resultFieldName) $ answers)
		Error message -> Error message

decodeAnswer :: (JSON a) => String -> JSObject JSValue -> Result (Either String a)
decodeAnswer resultFieldName answer = do
	answerCode <- valFromObj "Answer" answer :: Result Int
	answerString <- valFromObj "AnswerTypeString" answer
	result <- valFromObj resultFieldName answer
	return $ if (answerCode == 0) 
		then Right result
		else Left answerString -- valid JSON, but answer contains an error message

formatNumList :: (Show a) => [a] -> String
formatNumList list = concat $ intersperse "," $ map show list

formatList :: [String] -> String
formatList list = concat $ intersperse "," $ map (\x -> "\"" ++ x ++ "\"") list

postRequest :: String -> String -> IO String
postRequest url query = runResourceT $ do
        manager <- liftIO $ newManager def
        initReq <- liftIO $ parseUrl url
        let req = initReq {method="POST", requestHeaders=[("Content-Type","application/json")], requestBody = RequestBodyLBS $ fromString query}
        res <- httpLbs req manager
        liftIO $ putStrLn $ query
        liftIO $ putStrLn $ toString $ responseBody res
        return $ toString $ responseBody res
	