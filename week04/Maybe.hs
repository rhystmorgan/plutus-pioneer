module Week04.Maybe where 

import Text.Read (readMaybe)
import Week04.Monad

foo :: String -> String -> String -> Maybe Int  -- Here we are trying to parse all Strings as Int
foo x y z = case readMaybe x of                 -- If one of the Strings fail
    Nothing -> Nothing                          -- It will return Nothing
    Just k  -> case readMaybe y of              -- It does each element in series
        Nothing -> Nothing                      -- So if the first one fails it will fail straight away
        Just l  -> case readMaybe z of          -- Or it will move on to the next one
            Nothing -> Nothing
            Just m  -> Just (k + l + m)

-- foo "1" "2" "3"
-- Just 6

-- The problem with this is that we do the same computational actions 3 times, they are basically the same every time
-- eg we take something, if it is recognised it takes the element and adds it to the next, otherwise it returns nothing
-- so we want to simplify this
 
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b   -- bindMaybe is a simplification of the above
bindMaybe Nothing  _ = Nothing                      -- given Maybe a, it will take a and do Maybe b etc.
bindMaybe (Just x) f = f x                          -- If it is Nothing it doesnt matter what the nothing is, it will jjust retrn nothing
                                                    -- but if it is a Just, it will take x and apply f
foo' :: String -> String -> String -> Maybe Int
foo' x y z = readMaybe x `bindMaybe` \k ->          -- Now we take these Strings
             readMaybe y `bindMaybe` \l ->          -- if bindMaybe succeeds we take thr resut and call it K
             readMaybe z `bindMaybe` \m ->          -- Then we repeat with y and z
             Just (k + l + m)                       -- if all succeed then we return the sum of k l m

foo'' :: String -> String -> String -> Maybe Int
foo'' x y z = threeInts (readMaybe x) (readMaybe y) (readMaybe z)

-- this last version is an even more compact and simplified version of above
