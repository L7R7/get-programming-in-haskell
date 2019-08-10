-- patientInfo :: String -> String -> Int -> Int -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
  where
    name = lname ++ ", " ++ fname
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

type FirstName = String

type LastName = String

type Age = Int

type Height = Int

patientInfo :: FirstName -> LastName -> Age -> Height -> String
type PatientName = (FirstName, LastName)

firstName :: PatientName -> FirstName
firstName = fst

lastName :: PatientName -> LastName
lastName = snd

patientInfo2 :: PatientName -> Age -> Height -> String
patientInfo2 patientName age height = name ++ " " ++ ageHeight
  where
    name = fst patientName ++ ", " ++ snd patientName
    ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

-- patientInfo2 (fname, lname) age heigt = ...
data Sex
  = Male
  | Female

sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F' -- no error if this line is missing!?

data RhType
  = Pos
  | Neg

data ABOType
  = A
  | B
  | AB
  | O

data BloodType =
  BloodType ABOType RhType -- Data constructor's name may or may not match the type constructor's name?!

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"

name2 = NameWithMiddle "Jerome" "David" "Salinger"

type Weight = Int

data Patient =
  Patient Name Sex Age Height Weight BloodType

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeSmith :: Patient
janeSmith =
  Patient
    (NameWithMiddle "Jane" "Elizabeth" "Smith")
    Female
    24
    60
    140
    (BloodType AB Pos)

data Patient2 =
  Patient2
    { name :: Name
    , sex :: Sex
    , age :: Age
    , height :: Height
    , weight :: Weight
    , bloodType :: BloodType
    }

jackieSmith :: Patient2
jackieSmith =
  Patient2
    { name = Name "Jackie" "Smith"
    , age = 43
    , sex = Female
    , height = 62
    , weight = 115
    , bloodType = BloodType O Neg
    }

jHeight = height jackieSmith

jackieSmithUpdated = jackieSmith {age = 44}

canDonateTo2 :: Patient2 -> Patient2 -> Bool
canDonateTo2 p1 p2 = canDonateTo (bloodType p1) (bloodType p2)

patientSummary :: Patient2 -> String
patientSummary patient =
  "**************\n" ++
  name ++
  "\n" ++
  sex ++
  "\n" ++
  age ++
  "\n" ++ height ++ "\n" ++ weight ++ "\n" ++ bloodType ++ "\n**************"
  where
    name = showName2 patient
    sex = showSex patient
    age = showAge patient
    height = showHeight patient
    weight = showWeight patient
    bloodType = showBloodType2 patient

showName2 :: Patient2 -> String
showName2 p = "Patient Name: " ++ showName (name p)

showAge :: Patient2 -> String
showAge p = "Age: " ++ show (age p)

showSex :: Patient2 -> String
showSex p = "Sex: " ++ showSex2 (sex p)

showSex2 :: Sex -> String
showSex2 Male = "Male"
showSex2 Female = "Female"

showHeight :: Patient2 -> String
showHeight p = "Height: " ++ show (height p) ++ " in."

showWeight :: Patient2 -> String
showWeight p = "Weight: " ++ show (weight p) ++ " lbs."

showBloodType2 :: Patient2 -> String
showBloodType2 p = "BloodType: " ++ showBloodType (bloodType p)
