messyMain :: IO()
messyMain = do
  print "Who is the email for?"
  recipient <- getLine
  print "What's the title?"
  title <- getLine
  print "Who's the Author?"
  author <- getLine
  print ("Dear" ++ recipient ++ ",\n" ++ "Thanks for buying " ++ title ++ "\nthanks,\n" ++ author)

toPart recipient = "Dear " ++ recipient ++ ",\n"

bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"

fromPart author = "Thanks, \n" ++ author

createEmail recipient bookTitle author = toPart recipient ++
                                         bodyPart bookTitle ++
                                         fromPart author

main = do
  print "Who's the email for?"
  recipient <- getLine
  print "What is the Title?"
  title <- getLine
  print "Who's the Author?"
  author <- getLine
  print (createEmail recipient title author)
