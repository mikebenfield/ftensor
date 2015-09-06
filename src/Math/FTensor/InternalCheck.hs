module Math.FTensor.InternalCheck (
    check,
    noCheck,
) where

check :: Show b => String -> Int -> String -> Bool -> String -> b -> a -> a
check checkType lineNumber fileName False functionName otherData _ =
    error $ fileName ++ " (" ++ (show lineNumber) ++ ") (" ++
        functionName ++ "): Failed " ++ checkType ++ "check: " ++
            (show otherData)
check _ _ _ _ _ _ x = x

{-# INLINE noCheck #-}
noCheck :: a -> b -> c -> d -> d
noCheck _ _ _ x = x
