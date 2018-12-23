simple x = x

dollarPerSquareInch inches price = price / (pi * (inches / 2))

x = 2

calcChange owed given = if given - owed > 0
                        then given - owed
                        else 0

calcChang2 owed given = if change > 0
                        then change
                        else 0
  where change = given - owed

doublePlusTwo x = doubleX + 2
  where doubleX = x * 2

inc x = x + 1

double x = x * 2

square x = x * x

q23 n = if even n
        then n - 2
        else 3 * n + 1

main = do
  print "done"
