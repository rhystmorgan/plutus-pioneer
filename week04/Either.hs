module Week04.Either where

import Text.Read (readMaybe)
import Week04.Monad

readEither :: Read a => String -> Either String a   -- This basically shows us the same
readEither s = case readMaybe s of                  -- But instead of Nothing we return a String
    Nothing -> Left $ "can't parse: " ++ s          -- We go through the same process of reducing the pattern
    Just a  -> Right a                              -- But too reduce to show a string as an error

foo :: String -> String -> String -> Either String Int
foo x y z = case readEither x of
    Left err -> Left err
    Right k  -> case readEither y of
        Left err -> Left err
        Right l  -> case readEither z of
            Left err -> Left err
            Right m  -> Right (k + l + m)

bindEither :: Either String a -> (a -> Either String b) -> Either String b
bindEither (Left err) _ = Left err
bindEither (Right x)  f = f x

foo' :: String -> String -> String -> Either String Int
foo' x y z = readEither x `bindEither` \k ->
             readEither y `bindEither` \l ->
             readEither z `bindEither` \m ->
             Right (k + l + m)

foo'' :: String -> String -> String -> Either String Int
foo'' x y z = threeInts (readEither x) (readEither y) (readEither z)
