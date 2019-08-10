cup flOz message = message flOz

coffeeCup = cup 12

getOz aCup = aCup (\flOz -> flOz)

drink aCup ozDrank = cup (flOz - ozDrank)
  where
    flOz = getOz aCup

drink2 aCup ozDrank =
  if ozDiff >= 0
    then cup ozDiff
    else cup 0
  where
    flOz = getOz aCup
    ozDiff = flOz - ozDrank

isEmpty aCup = getOz aCup == 0

afterManySips = foldl drink coffeeCup [1, 2, 3, 4]
