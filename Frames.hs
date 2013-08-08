{-# LANGUAGE OverloadedStrings #-}
module Frames (RawFrame(RawFrame), Frame(Frame), Element, describeFrame, mentionedEntities) where

import Import hiding (entityID)
import Data.List (nub, intersperse, isPrefixOf)
import Data.Maybe

data RawFrame = RawFrame Int Int String String [(Int, Int)] -- id frametype sentenceID source [(role, entity)]
data Frame = Frame Int String String String String [Element] -- id description frametype sentenceID source elements
	deriving Eq
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
fetchElement_ elements role = replaceAll (fromMaybe "" $ fetchElement elements role) replacements

replacements = [
	("trešdiena vakars", "trešdienas vakarā"),
	("Tukums 1. vidusskola", "Tukuma 1. vidusskolu"),
	("maijs", "maijā"),
	("decembris", "decembrī"),
	("augusts", "augusta"),
	("gads", "gadā"),
	(">", ""),
	("augsts literārs kurss", "augstākos literāros kursus"),
	("un Goda diplomu", "Goda diplomu"),
	("Latvija vēsture un filoloģija fakultāte", "Latvijas vēstures un filoloģijas fakultāti"),
	("tauta dzejnieks gods nosaukums", "tautas dzejnieka goda nosaukumu"),
	("arī 1991. gadā barikāde dalībnieks piemiņa zīme", "1991. gada barikāžu dalībnieka piemiņas zīmi"),
	("2. šķira trīs zvaigzne ordenis", "2. šķiras trīs zvaigžņu ordeni"),
	("ministrs kabinets balva par mūžs ieguldījums latvietis kultūra un izcils veikums latvietis literatūra", "MK balvu par mūža ieguldījumu latviešu kultūrā"),
	("starptautiska atzinība Hanss Kristians Andersens vārds nosaukt pasaka meistars diploms", "Hansa Kristiana Andersena vārdā nosaukto pasaku meistara diplomu"),
	("nopelns bagāts kultūra darbinieks nosaukums", "nopelniem bagātā kultūras darbinieka nosaukumu"),
	("jūrmala 1. vidusskola", "Jūrmalas 1. vidusskolā"),
	("Latvija kultūra fonds", "Latvijas kultūras fondā"),
	("skolotājs", "skolotāju"),
	("sekretārs", "sekretāru"),
	("priekšsēdētājs", "priekšsēdētāju"),
	("padomnieks", "padomnieku"),
	("Latvija krājbanka", "Latvijas Krājbankā"),
	("būt AP tauta izglītība", "bijis tautas izglītības AP"),
	("būt", "bijis par"),
	("Latvija tēls veidošana institūts", "Latvijas tēla veidošanas institūtā"),
	("Hārvards universitāte", "Hārvardas universitāti"),
	("valsts prezidente kanceleja vadītāju", "Valsts prezidentes kancelejas vadītājs"),
	("Rīga apriņķis sloka pagasts Ragaciems", "Rīgas apriņķa Slokas pagasta Ragaciemā")]

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
 concat . map (\(_,role,entityID, entity) -> role ++ ": " ++ (replaceOne "<" "" entity) ++ "; ")

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

-- String replace - call as:
-- replace somestring [("a","b")]
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