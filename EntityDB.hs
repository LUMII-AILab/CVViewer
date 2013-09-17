{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module EntityDB (fetchEntityIDsByName, fetchEntityDataByID, fetchFrames, Entity(Entity)) where

import Import hiding (Entity, entityID)
import Network.HTTP.Conduit
import Data.Conduit
import Data.ByteString.Lazy.UTF8 (toString, fromString)
import Data.List
import Text.JSON
import Control.Monad
import Data.Either
import Frames
import System.IO
import qualified Data.Map

data Entity = Entity Int Int [String] -- Entity id category names
	deriving Show

serviceURL :: String
serviceURL = "http://sps.inside.lv:789/Rest/Semantic/"
user :: String
user = "PeterisP"

-- Fetches all frames matching the supplied lists of entity IDs and frame type IDs
fetchFrames :: [Int] -> [Int] -> IO [Frame]
fetchFrames entityIDs frametypes = do
	json <- postRequest (serviceURL ++ "GetFrame/" ++ user) 
		("{\"parameterList\":{\"QueryParameters\":[{\"EntityIdList\":[" ++ (formatNumList entityIDs) ++ 
		 "],\"FrameTypes\": [" ++ (formatNumList frametypes) ++ "]}]}}")
	let frames = decodeFrames json
	entities <- fetchEntityDataByID $ mentionedEntities frames
	return $ map (describeFrame $ entityLookup entities) frames

entityLookup :: [Entity] -> Int -> String
entityLookup entities entityID = 
	let table = Data.Map.fromList $ map (\(Entity xentityID _ names) -> (xentityID, head names)) entities
	in Data.Map.findWithDefault "" entityID table

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
	names <- valFromObj "OtherName" >=> readJSONsSafe  $ json -- reading a JSON array of strings here
	return $ Entity entityID category (name:names) -- we put the 'official' name as simply the first in list

-- decodes a list of Frames as given by the webservice answers
decodeFrames :: String -> [RawFrame]
decodeFrames json = 
	case (decodeAnswers "FrameData" >=> mapM (readJSONsSafe >=> mapM decodeFrame) $ json) of
		Ok answers -> concat answers -- FIXME - this concat loses structure of which frames are for which queried item
		Error message -> error message -- FIXME - no error checking	

decodeFrame :: JSObject JSValue -> Result RawFrame
decodeFrame json = do
	frameID <- valFromObj "FrameId" json
	frameType <- valFromObj "FrameType" json
	sentenceID <- valFromObj "SentenceId" json
	source <- valFromObjDefault "Source" json ""
	elements <- valFromObj "FrameData" >=> readJSONsSafe >=> mapM decodeFrameElement $ json
	return $ RawFrame frameID frameType sentenceID source elements

decodeFrameElement :: JSObject JSValue -> Result (Int, Int)
decodeFrameElement json = do
	role <- valFromObj "Key" json
	entity <- valFromObj "Value" >=> valFromObj "Entity" $ json -- ignoring PlaceInSentence field here
	return (role, entity)

-- decodes JSON answers to a list of separate answers. 
-- NB! If there are any answers with errorcodes, they will be simply skipped..
decodeAnswers :: (JSON a) => String -> String -> Result [a]
decodeAnswers resultFieldName json = 
	case (decode json) of
		Ok answers -> (liftM rights) ( (valFromObj "Answers") >=> readJSONsSafe >=> mapM (decodeAnswer resultFieldName) $ answers)
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
        liftIO $ putStrLn $ query
        liftIO $ hFlush stdout
        manager <- liftIO $ newManager def
        initReq <- liftIO $ parseUrl url
        let req = initReq {method="POST", responseTimeout = Just 60000000, requestHeaders=[("Content-Type","application/json")], requestBody = RequestBodyLBS $ fromString query}
        res <- httpLbs req manager        
        liftIO $ putStrLn $ toString $ responseBody res
        liftIO $ hFlush stdout
        return $ toString $ responseBody res

-- If element is not found, return the default value silently.
valFromObjDefault :: JSON a => String -> JSObject JSValue -> a -> Result a
valFromObjDefault key json defVal = 
	let v = valFromObj key json in
	case v of
		Ok _ -> v
		Error _ -> Ok defVal

-- readJSONs fails with critical error if the element doesn't contain a list. Needs a pull request to Text.JSON?
readJSONsSafe :: JSON a => JSValue -> Result [a]
readJSONsSafe (JSArray as) = mapM readJSON as
readJSONsSafe _ = Ok [] -- Fixme - no error message "Unable to read list"..
