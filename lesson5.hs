ifEven f n = if even n
                then f n
                else n
inc n = n + 1

genIfEven f = (\x -> ifEven f x)

ifEvenInc = genIfEven inc

genIfXEven x = (\f -> ifEven f x)

getRequestUrl host apiKey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

sampleUrl = getRequestUrl "http://example.com" "1337hAsk311" "book" "1234"

genHostRequestBuilder host = (\apiKey resource id -> getRequestUrl host apiKey resource id)

exampleUrlBuilder = genHostRequestBuilder "http://example.com"

sampleUrl2 = exampleUrlBuilder "1337hAsk311" "book" "1234"

genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)

exampleUrlBuilder2 = genApiRequestBuilder exampleUrlBuilder "1337hAsk311"

sampleUrl3 = exampleUrlBuilder2 "book" "1234"

genResourceRequestBuilder hostBuilder apiKey resource = (\id -> hostBuilder apiKey resource id)

exampleUrlBuilder3 = getRequestUrl "http://example.com"
myExampleUrlBuilder = exampleUrlBuilder "1337hAsk311"

exampleUrlBuilder4 = getRequestUrl "http://example.com" "1337hAsk311" "book"

flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x)

subtract2 = flip (-) 2

ifEvenInc = ifEven (\x -> x + 1)
ifEvenDouble = ifEven (\x -> x * 2)
ifEvenSquare = ifEven (\x -> x ^ 2)

-- binaryPartialApplication binF arg = flip binF arg
binaryPartialApplication binF arg = (\x -> binF arg x)

