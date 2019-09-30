halve :: Int -> Double
halve n = fromIntegral n / 2.0

halveMaybe :: Maybe Int -> Maybe Double
halveMaybe (Just n) = Just (halve n)
halveMaybe Nothing = Nothing
