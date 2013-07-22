{-# LANGUAGE ScopedTypeVariables #-}
module EntityDB (fetchEntityIDsByName, fetchEntityDataByID) where

import Network.Curl
import Network.Curl.Post
import Data.List
import Text.JSON
import Control.Monad
import Data.Either
import qualified Data.Map as Map

data Entity = Entity Int Int [String] -- Entity id category names
	deriving Show

serviceURL = "http://sps.inside.lv:789/Rest/Semantic/"
user = "PeterisP"

insertURL = serviceURL ++ "InsertEntity/" ++ user
getURL = serviceURL ++ "GetEntityDataById/" ++ user

--sampleJSON = "{EntityList : [{EntityId : 0,Name : \"Testeris\",OtherName : \"\",OuterId : \"\",Catregory : 0}]}"
--sampleJSON = { "entityNameList": { "EntityNameList": ["Saatčiane I. individuāli praktizējoša zvērināta advokāte"] } }

main = do
	allEntityIds <- fetchEntityIDsByName [""]
	allEntityData <- fetchEntityDataByID allEntityIds
	let names = Map.fromListWith (++) $ concat $ map (\(Entity _ cat names) -> map (\x -> (cat,[x])) names) allEntityData
	mapM_ writeNames $ Map.toList names
	putStrLn "Done!"

	--sampleJSON <- readFile "sampleJSON.txt"
	--putStrLn sampleJSON
	--answer <- curlPostString getURL sampleJSON
	--putStrLn answer

writeNames :: (Int, [String]) -> IO ()
writeNames (category, names) = 
	writeFile (show category ++ ".txt") $ unlines names

testemall = do
	testEntityData <- fetchEntityDataByID [1184, 1191, 4351, 5000]
	putStrLn $ show testEntityData
	testEntityIds <- fetchEntityIDsByName ["Bondars"]
	putStrLn $ show testEntityIds
	testFrames <- fetchFrames [] [0]
	putStrLn testFrames

-- Fetches all frames matching the supplied lists of entity IDs and frame type IDs
fetchFrames :: [Int] -> [Int] -> IO String
fetchFrames entities frametypes = 
	curlPostString (serviceURL ++ "GetFrame/" ++ user) 
		("{\"parameterList\":{\"QueryParameters\":[{\"EntityIdList\":[" ++ (formatNumList entities) ++ 
		 "],\"FrameTypes\": [" ++ (formatNumList frametypes) ++ "]}]}}")

fetchEntityDataByID :: [Int] -> IO [Entity]
fetchEntityDataByID ids = do
	json <- curlPostString (serviceURL ++ "GetEntityDataById/" ++ user) 
		("{\"entityIdList\":{\"DataSet\":[\"AllUsers\"],\"SearchType\":\"AllData\",\"EntityIdList\":["
		++ (formatNumList ids) ++ "]}}")
	return $ decodeEntities json

-- fetch a list of entity IDs matching the supplied names
fetchEntityIDsByName :: [String] -> IO [Int]
fetchEntityIDsByName names = do
	json <- curlPostString (serviceURL ++ "GetEntityIdByName/" ++ user) 
		("{\"entityNameList\":{\"EntityNameList\":[" ++ (formatList names) ++ "]}}")
	return $ decodeIDs json

-- temporary (assumes no errors) function to decode entity ID JSON
decodeIDs :: String -> [Int]
decodeIDs json =
	case (decodeAnswers "EntityIdList" json) of
		Ok answers -> head answers -- FIXME - assumes only one answer
		Error message -> undefined -- FIXME - no error checking

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

-- covering the gap in Haskell curl library - we need a POST statement that gets back the results and sends JSON paramaters
curlPostString :: String -> String -> IO String
curlPostString url query = withCurlDo $ do
	curl <- initialize
	let options = CurlPostFields [ query ] : CurlHttpHeaders [ "Content-Type: application/json; charset=UTF-8" ] : method_POST 
	r <- do_curl_ curl url options :: IO CurlResponse
	return $ respBody r