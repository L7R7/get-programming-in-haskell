import Data.List

ifEvenInc n =
  if even n
    then n + 1
    else n

ifEvenDouble n =
  if even n
    then n * 2
    else n

ifEvenSquare n =
  if even n
    then n ^ 2
    else n

ifEven f n =
  if even n
    then f n
    else n

inc n = n + 1

double n = n * 2

square n = n ^ 2

ifEvenIn2 = ifEven inc

ifEvenDoubl2 = ifEven double n

ifEvenSquar2 = ifEven square n

ifEvenCube = ifEven (^ 3)

ifEvenNegate = ifEven (\x -> -x)

names =
  [ ("Ian", "Curtis")
  , ("Bernard", "Summer")
  , ("Peter", "Hook")
  , ("Stephen", "Morris")
  ]

sortedByFirstName = sort names

-- compareLastNames name1 name2 =
--   if lastName1 > lastName2
--     then GT
--     else if lastName1 < lastName2
--            then LT
--            else EQ
--   where
--     lastName1 = snd name1
--     lastName2 = snd name2
compareLastNames name1 name2
  | lastName1 > lastName2 = GT
  | lastName1 < lastName2 = LT
  | otherwise = EQ
  where
    lastName1 = snd name1
    lastName2 = snd name2

compareLastName2 name1 name2 = compare (snd name1) (snd name2)

sortedByLastName = sortBy compareLastNames names

-- compareNamess name1 name2 =
--   if lastName1 > lastName2
--     then GT
--     else if lastName1 < lastName2
--            then LT
--            else if firstName1 > firstName2
--                   then GT
--                   else if firstName1 < firstName2
--                          then LT
--                          else EQ
--   where
--     firstName1 = fst name1
--     lastName1 = snd name1
--     firstName2 = fst name2
--     lastName2 = snd name2
compareNames name1 name2
  | lastName1 > lastName2 = GT
  | lastName1 < lastName2 = LT
  | firstName1 > firstName2 = GT
  | firstName1 < firstName2 = LT
  | otherwise = EQ
  where
    firstName1 = fst name1
    lastName1 = snd name1
    firstName2 = fst name2
    lastName2 = snd name2

compareName2 name1 name2 =
  if lastNameComparison == EQ
    then compare firstName1 firstName2
    else lastNameComparison
  where
    firstName1 = fst name1
    lastName1 = snd name1
    firstName2 = fst name2
    lastName2 = snd name2
    lastNameComparison = compare firstName1 firstName2

addressLetter name location = nameText ++ " - " ++ location
    -- nameText = (fst name) ++ " " ++ (snd name)
  where
    nameText = fst name ++ " " ++ snd name

bobsAddress =
  addressLetter ("Bob", "Smith") "PO Box 1234 - San Francisco, CA, 94111"

sfOffice name =
  if lastName < "L"
    then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
    else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where
    lastName = snd name
    nameText = fst name ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where
    nameText = fst name ++ " " ++ snd name

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where
    nameText = snd name

dcOffice name = nameText ++ " Esq"
  where
    nameText = fst name ++ " " ++ snd name

getLocationFunction location =
  case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "dc" -> dcOffice
    _ -> \name -> fst name ++ " " ++ snd name

addressLette2 name location = locationFunction name
  where
    locationFunction = getLocationFunction location
