import Data.Char

addAnA [] = []
addAnA (x:xs) = ("a " ++ x) : addAnA xs

myMap f [] = []
myMap f (x:xs) = (f x) : myMap f xs

myFilter test [] = []
myFilter test (x:xs) =
  if test x
    then x : myFilter test xs
    else myFilter test xs

remove test [] = []
remove test (x:xs) =
  if test x
    then remove test xs
    else x : remove test xs

myProduct xs = foldl (*) 1 xs

myFoldl f init [] = []
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x

myFoldr f init [] = []
myFoldr f init (x:xs) = f x rightResult
  where rightResult = myFoldr f init xs

myElem a [] = False
myElem a (x:xs) = if a == x
                   then True
                   else myElem a xs

myElem2 val myList = (length filteredList) /= 0
  where filteredList = filter (== val) myList

isPalindrome text = filtered == reverse filtered
  where noSpaces = filter (/= ' ') text
        filtered = map toLower noSpaces

harmonic n = sum (take n seriesValues)
  where seriesPairs = zip (cycle [1.0]) [1.0,2.0 .. ]
        seriesValues = map (\pair -> (fst pair)/(snd pair)) seriesPairs
