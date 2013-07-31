{-# LANGUAGE OverloadedStrings #-}
module Frames (RawFrame(RawFrame), Frame(Frame), Element, describeFrame, mentionedEntities) where

import Import hiding (entityID)
import Data.List (nub)
import Data.Maybe

data RawFrame = RawFrame Int Int String String [(Int, Int)] -- id frametype sentenceID source [(role, entity)]
data Frame = Frame Int String String String String [Element] -- id description frametype sentenceID source elements
type Element = (Int, String, Int, String) -- [roleID, role, entityID, entity]

-- describes a frame
-- FIXME - aprakstam jābūt atkarīgam no 'fokusa' entītijas; kā arī tad vajag iekļaut ne visus freimus varbūt..
describeFrame :: (Int -> String) -> RawFrame -> Frame
describeFrame entityLookup (RawFrame frameID frameTypeID sentenceID source rawelements) =
	let 
		frameType = frameTypes !! frameTypeID -- TODO - validācija
		elements = map (\(role, entityID) -> (role, fetchRole frameTypeID role, entityID, entityLookup entityID)) rawelements
		description = (getDescriber frameTypeID) elements
	in Frame frameID description frameType sentenceID source elements

-- fetch the role name from IDs, including boundary checks
fetchRole :: Int -> Int -> String
fetchRole frame role =
	if (frame >= length frameTypes) then "Bad frame type ID " ++ show frame
	else let roles = frameRoles !! frame
		in if (role > length roles) then "Bad role ID " ++ show role ++ " in frame " ++ show frame
			else roles !! (role-1)

-- tries to find entity for the requested role
fetchElement :: [Element] -> Int -> Maybe String
fetchElement [] _ = Nothing
fetchElement ((role, _, _, entity):xs) neededRole = 
	if (role == neededRole) then Just entity
	else fetchElement xs neededRole

-- the same, but with a default value of ""
fetchElement_ :: [Element] -> Int -> String
fetchElement_ elements role = fromMaybe "" $ fetchElement elements role

type FrameTypeDescriber = [Element] -> String -- TODO - uz html? lai entītijas ir kā korekti urļi
-- gets the decoding function for each frame type
getDescriber :: Int -> FrameTypeDescriber
getDescriber frameTypeID = case frameTypeID of
			0 -> describeBirth -- dzimšana
			3 -> describeRelations -- attiecības
			6 -> describeEducation -- izglītība
			7 -> describeVocation -- nodarbošanās
			9 -> describeEmployment -- amats
			10 -> describeEmploymentStart -- darba sākums
			11 -> describeEmploymentEnd -- darba beigas
			13 -> describeElections -- vēlēšanas
			x -> describeDefault x -- fallthrough

describeBirth :: FrameTypeDescriber
describeBirth elements = let
	laiks = fetchElement_ elements 2
	vieta = fetchElement_ elements 3
	in "dzimis " ++ laiks ++ " " ++ vieta

describeRelations :: FrameTypeDescriber
describeRelations elements = let
	partneris = fetchElement_ elements 2
	attiecības = fetchElement_ elements 4
	in "ir " ++ partneris ++ " " ++ attiecības

describeEducation :: FrameTypeDescriber
describeEducation elements = let
	iestāde = fetchElement_ elements 2
	in "studējis " ++ iestāde

describeVocation :: FrameTypeDescriber
describeVocation elements = let
	nodarbe = fetchElement_ elements 2
	in "darbojies par " ++ nodarbe

describeEmployment :: FrameTypeDescriber
describeEmployment elements = let
	darbavieta = fetchElement_ elements 2
	amats = fetchElement_ elements 3
	laiks = fetchElement_ elements 6	
	in laiks ++ " strādājis " ++ darbavieta ++ " par " ++ amats

describeEmploymentStart :: FrameTypeDescriber
describeEmploymentStart elements = let
	darbavieta = fetchElement_ elements 2
	amats = fetchElement_ elements 3
	laiks = fetchElement_ elements 7
	in laiks ++ " strādājis " ++ darbavieta ++ " par " ++ amats

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


describeDefault :: Int -> FrameTypeDescriber
describeDefault frameTypeID elements = (frameTypes !! frameTypeID) ++ " - " ++ (describeElements elements)

describeElements :: [Element] -> String
describeElements =
 concat . map (\(_,role,entityID, entity) -> role ++ ": " ++ entity ++ "; ")

-- Lists all entity IDs mentioned in this list of frames
mentionedEntities :: [RawFrame] -> [Int]
mentionedEntities frames =
	nub $ concat $ map (\(RawFrame _ _ _ _ elements) -> map snd elements) frames

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
	["Zīmols", "Organizācija", "Produkts"]]

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
	"Zīmols"]

