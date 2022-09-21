module Chapter7.Composition where 

-- . composes two functions in a single one
-- f . g = \x -> f (g x) => it applies g to x, then f to the result

odd :: Integer -> Bool
odd = not . even 

twice :: (a -> a) -> a -> a
twice f = f . f 

sumsqreven :: [Integer] -> Integer
sumsqreven = sum . map (^2) . filter even 