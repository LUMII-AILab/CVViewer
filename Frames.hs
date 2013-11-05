{-# LANGUAGE OverloadedStrings #-}
module Frames (RawFrame(RawFrame), Frame(Frame), Element, describeFrame, mentionedEntities) where

import Import hiding (entityID)
import Data.List (nub, intersperse, isPrefixOf)
import Data.Maybe

data RawFrame = RawFrame Int Int String String String String Int [(Int, Int)] -- id frametype sentenceID source document frametext framecount [(role, entity)]
data Frame = Frame Int String String String String String String Int [Element] -- id description frametype sentenceID source document frametext framecount elements
	deriving Eq
type Element = (Int, String, Int, String) -- [roleID, role, entityID, entity]

-- describes a frame
-- FIXME - aprakstam jābūt atkarīgam no 'fokusa' entītijas; kā arī tad vajag iekļaut ne visus freimus varbūt..
describeFrame :: (Int -> String) -> RawFrame -> Frame
describeFrame entityLookup (RawFrame frameID frameTypeID sentenceID source document frametext framecount rawelements) =
	let 		
		frameType = if frameTypeID < length frameTypes
			then frameTypes !! frameTypeID
			else error $ "Bad frame type ID " ++ show frameTypeID -- TODO - graceful fail
		elements = map (\(role, entityID) -> (role, fetchRole frameTypeID role, entityID, entityLookup entityID)) rawelements
		description = (describeDefault frameTypeID) elements
	in Frame frameID description frameType sentenceID source document (if frametext=="" then "[x]: "++description else frametext) framecount elements

-- fetch the role name from IDs, including boundary checks
fetchRole :: Int -> Int -> String
fetchRole frame role =
	if (frame >= length frameTypes) then "Bad frame type ID " ++ show frame
	else let roles = frameRoles !! frame
		in if (role > length roles) then "Bad role ID " ++ show role ++ " in frame " ++ show frame
			else roles !! (role-1) -- FIXME - te ir -1 vai 0 atkarībā no kā tad Mārtiņš beigās uztaisa lomu indeksus, 0based vai 1based

-- tries to find entity for the requested role
fetchElement :: [Element] -> Int -> Maybe String
fetchElement [] _ = Nothing
fetchElement ((role, _, _, entity):xs) neededRole = 
	if (role == neededRole) then Just entity
	else fetchElement xs neededRole

-- the same, but with a default value of ""
fetchElement_ :: [Element] -> Int -> String
fetchElement_ elements role = replaceAll (fromMaybe "" $ fetchElement elements role) replacements

replacements :: [(String, String)]  -- FIXME - nav aktuāls, jo tagad DB ir gan locījumi, gan renderētie teksti
replacements = [
	("trešdiena vakars", "trešdienas vakarā")]

type FrameTypeDescriber = [Element] -> String -- TODO - uz html? lai entītijas ir kā korekti urļi
-- gets the decoding function for each frame type
getDescriber :: Int -> FrameTypeDescriber
getDescriber frameTypeID = case frameTypeID of
			0 -> describeBirth -- dzimšana
			2 -> describeDeath -- miršana
			3 -> describeRelations -- attiecības
			6 -> describeEducation -- izglītība
			7 -> describeVocation -- nodarbošanās
			9 -> describeEmployment -- amats
			10 -> describeEmploymentStart -- darba sākums
			11 -> describeEmploymentEnd -- darba beigas
			13 -> describeElections -- vēlēšanas
			22 -> describeAchievement -- sasniegums
			x -> describeDefault x -- fallthrough

describeBirth :: FrameTypeDescriber
describeBirth elements = let
	laiks = fetchElement_ elements 2
	vieta = fetchElement_ elements 3
	in "dzimis " ++ laiks ++ " " ++ vieta

describeDeath :: FrameTypeDescriber
describeDeath elements = let
	laiks = fetchElement_ elements 2
	vieta = fetchElement_ elements 3
	in "miris " ++ laiks ++ " " ++ vieta	

describeRelations :: FrameTypeDescriber
describeRelations elements = let
	partneris = fetchElement_ elements 2
	attiecības = fetchElement_ elements 4
	in attiecības ++ ": " ++ partneris

describeEducation :: FrameTypeDescriber
describeEducation elements = let
	iestāde = fetchElement_ elements 2
	grāds = fetchElement_ elements 4
	laiks = fetchElement_ elements 5
	in if not (null grāds)
		then laiks ++ " ieguvis " ++ grāds ++ " " ++ iestāde
		else laiks ++ " pabeidzis " ++ iestāde

describeVocation :: FrameTypeDescriber
describeVocation elements = let
	nodarbe = fetchElement_ elements 2
	in "darbojies par " ++ nodarbe

describeEmployment :: FrameTypeDescriber
describeEmployment elements = let
	darbavieta = fetchElement_ elements 2
	amats = fetchElement_ elements 3
	laiks = fetchElement_ elements 6	
	statuss = fetchElement_ elements 7
	sākums = fetchElement_ elements 8
	in if not (null statuss) -- FIXME - uz guardiem
		then if not (null darbavieta)
			then statuss ++ " " ++ darbavieta ++ " " ++ amats
			else statuss ++ " " ++ amats
		else if not (null sākums) 
			then "No " ++ sākums ++ " bijis " ++ amats ++ " " ++ darbavieta
			else laiks ++ " strādājis " ++ darbavieta ++ " par " ++ amats

describeEmploymentStart :: FrameTypeDescriber
describeEmploymentStart elements = let
	darbavieta = fetchElement_ elements 2
	amats = fetchElement_ elements 3
	laiks = fetchElement_ elements 7
	in if not (null laiks) 
		then "No " ++ laiks ++ " strādājis " ++ darbavieta ++ " par " ++ amats
		else "Strādājis " ++ darbavieta ++ " par " ++ amats

describeEmploymentEnd :: FrameTypeDescriber
describeEmploymentEnd elements = let
	darbavieta = fetchElement_ elements 2
	amats = fetchElement_ elements 3
	laiks = fetchElement_ elements 7
	in laiks ++ " strādājis " ++ darbavieta ++ " par " ++ amats

describeElections :: FrameTypeDescriber
describeElections elements = let
	vēlēšanas = fetchElement_ elements 2
	rezultāts = fetchElement_ elements 5
	laiks = fetchElement_ elements 6
	in laiks ++ " " ++ rezultāts ++ " piedalījies " ++ vēlēšanas

describeAchievement :: FrameTypeDescriber
describeAchievement elements = let
	balva = fetchElement_ elements 2 -- FIXME - atkarīgs no sasnieguma veida
	laiks = fetchElement_ elements 6
	in laiks ++ " saņēmis " ++ balva

describeDefault :: Int -> FrameTypeDescriber
describeDefault frameTypeID elements = (frameTypes !! frameTypeID) ++ " - " ++ (describeElements elements)

describeElements :: [Element] -> String
describeElements =
 concat . map (\(_,role,_, entity) -> role ++ ": " ++ (replaceOne "<" "" entity) ++ "; ")

-- Lists all entity IDs mentioned in this list of frames
mentionedEntities :: [RawFrame] -> [Int]
mentionedEntities frames =
	nub $ concat $ map (\(RawFrame _ _ _ _ _ _ _ elements) -> map snd elements) frames

-- TODO - lasīt no faila, Template Haskell ?
-- pagaidām no frames.xls ar formulām
frameRoles :: [[String]] -- lomu indeksi no webservisa ir 1-based nevis 0-based!!
frameRoles = [
	["Bērns", "Laiks", "Vieta", "Radinieki"],
	["Persona", "Vecums"],
	["Mirušais", "Laiks", "Vieta", "Veids", "Cēlonis"],
	["Partneris_1", "Partneris_2", "Partneri", "Attiecības", "Laiks"],
	["Vārds", "Entītija", "Tips"],
	["Rezidents", "Vieta", "Biežums", "Laiks"],
	["Students", "Iestāde", "Nozare", "Grāds", "Laiks", "Vieta"],
	["Persona", "Nodarbošanās", "Laiks", "Statuss"],
	["Izcelsme", "Persona", "Tautība"],
	["Darbinieks", "Darbavieta", "Amats", "Alga", "Vieta", "Laiks", "Statuss", "Sākums", "Beigas"],
	["Darbinieks", "Darbavieta", "Amats", "Darba devējs", "Veids", "Vieta", "Laiks", "Iepriekšējais_darbinieks"],
	["Darbinieks", "Darbavieta", "Amats", "Darba devējs", "Veids", "Vieta", "Laiks", "Nākamais_darbinieks"],
	["Biedrs", "Organizācija", "Laiks", "Statuss"],
	["Dalībnieks", "Vēlēšanas", "Amats", "Uzvarētājs", "Rezultāts", "Laiks", "Vieta"],
	["Atbalstītājs", "Saņēmējs", "Tēma", "Laiks"],
	["Organizācija", "Dibinātājs", "Veids", "Nozare", "Laiks", "Vieta"],
	["Dalībnieks", "Notikums", "Laiks", "Vieta", "Veids", "Organizētājs"],
	["Organizācija", "Ienākumi", "Avots", "Peļņa", "Laiks", "Vienības", "Pieaugums"],
	["Īpašnieks", "Īpašums", "Laiks", "Daļa"],
	["Parādnieks", "Aizdevējs", "Aizdevums", "Ķīla", "Laiks", "Vienības"],
	["Apsūdzētais", "Apsūdzība", "Tiesa", "Prasītājs", "Advokāts", "Tiesnesis", "Vieta", "Laiks"],
	["Cietušais", "Uzbrucējs", "Sekas", "Apstākļi", "Notikuma_apraksts", "Iemesls", "Ierocis", "Veids", "Vieta", "Laiks"],
	["Dalībnieks", "Sasniegums", "Sacensības", "Rezultāts", "Rangs", "Laiks", "Vieta", "Organizētājs", "Pretinieks"],
	["Avots", "Autors", "Ziņa", "Laiks"],
	["Iepircējs", "Tēma", "Paredzētā_Summa", "Pretendenti", "Uzvarētājs", "Rezultāts", "Laiks"],
	["Zīmols", "Organizācija", "Produkts"],
	["Entītija","Īpašība"]]

frameTypes :: [String]
frameTypes = [
	"Dzimšana",
	"Vecums",
	"Miršana",
	"Attiecības",
	"Vārds",
	"Dzīvesvieta",
	"Izglītība",
	"Nodarbošanās",
	"Izcelsme",
	"Amats",
	"Darba sākums",
	"Darba beigas",
	"Dalība",
	"Vēlēšanas",
	"Atbalsts",
	"Dibināšana",
	"Piedalīšanās",
	"Finanses",
	"Īpašums",
	"Parāds",
	"Tiesvedība",
	"Uzbrukums",
	"Sasniegums",
	"Ziņošana",
	"Publisks iepirkums",
	"Zīmols",
	"Nestrukturēts"]

-- String replace - call as:
-- replace somestring [("a","b")]
replaceAll :: Eq a => [a] -> [([a],[a])] -> [a]
replaceAll = foldr (uncurry replaceOne)

-- FIXME - copypasta from MissingH
replaceOne :: Eq a => [a] -> [a] -> [a] -> [a]
replaceOne old new l = join new . split old $ l

split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (startswith delim) str
        in 
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else split delim 
                                                 (drop (length delim) x)

join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

startswith :: Eq a => [a] -> [a] -> Bool
startswith = isPrefixOf

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])

spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs