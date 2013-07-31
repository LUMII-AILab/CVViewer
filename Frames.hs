{-# LANGUAGE OverloadedStrings #-}
module Frames (RawFrame(RawFrame), Frame(Frame), Element, describeFrame, mentionedEntities) where

import Import hiding (entityID)
import Data.List (nub)
import Data.Maybe

data RawFrame = RawFrame Int Int String String [(Int, Int)] -- id frametype sentenceID source [(role, entity)]
data Frame = Frame Int String String String String [Element] -- id description frametype sentenceID source elements
type Element = (Int, String, Int, String) -- [roleID, role, entityID, entity]

describeFrame :: (Int -> String) -> RawFrame -> Frame
describeFrame entityLookup (RawFrame frameID frameTypeID sentenceID source rawelements) =
	let 
		frameType = frameTypes !! frameTypeID -- TODO - validācija
		elements = map (\(role, entityID) -> (role, fetchRole frameTypeID role, entityID, entityLookup entityID)) rawelements
		description = (getDescriber frameTypeID) elements
	in Frame frameID description frameType sentenceID source elements

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
			x -> describeDefault x -- fallthrough

describeBirth :: FrameTypeDescriber
describeBirth elements = let
	laiks = fetchElement_ elements 2
	vieta = fetchElement_ elements 3
	in "dzimis " ++ laiks ++ " " ++ vieta

describeDefault :: Int -> FrameTypeDescriber
describeDefault frameTypeID elements = (frameTypes !! frameTypeID) ++ ": " ++ (show elements)

-- Lists all entity IDs mentioned in this list of frames
mentionedEntities :: [RawFrame] -> [Int]
mentionedEntities frames =
	nub $ concat $ map (\(RawFrame _ _ _ _ elements) -> map snd elements) frames

-- TODO - lasīt no faila, Template Haskell ?
-- pagaidām no frames.xls ar formulām
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
