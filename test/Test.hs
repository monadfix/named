{-# LANGUAGE OverloadedLabels, DataKinds, TypeOperators, ViewPatterns,
             PartialTypeSignatures #-}

{-# OPTIONS -fno-warn-partial-type-signatures #-}

module Main where

import Data.Maybe (fromMaybe)
import Data.Function ((&))
import Named

test1 ::
  String ->
  "a" :! Bool ->
  "b" :! Bool ->
  IO ()
test1 "str" (arg #a -> True) (arg #b -> False) = return ()
test1 _ _ _ = error "unexpected flags or str"

test1_1 =
  test1 "str"
    ! #a True
    ! #b False

test1_2 =
  test1 "str"
    ! #b False
    ! #a True

test1_3 =
  test1 "str"
    & with (#b False)
    & with (#a True)

test1_4 =
  with (#a True) $
  with (#b False) $
    test1 "str"

test1_5 =
  with (#a True) $
  test1 "str"! #b False

test1_6 =
  test1 "str"! #a True
    & with (#b False)

test2 :: "x" :! Int -> Int
test2 x = arg #x x * 2

test2_1 :: Int
test2_1 = test2 ! #x 5 + test2 ! #x 3

test3 (arg #a -> a) (arg #b -> b) = a + b

-- must not typecheck:
--     Couldn't match type ‘"a"’ with ‘"b"’
--     arising from the overloaded label ‘#b’
--
-- test3' :: _ => "a" :! _ -> _ :! "b" -> _
-- test3' (arg #b -> a) (arg #a -> b) = a + b

test4 ::
  "b" :! Bool ->
  NamedF _ Char "x" ->
  "y" :? Char ->
  Char
test4
  (arg #b -> b)
  (argDef #x 'x' -> x)
  (ArgF y)
  = if b then x else (fromMaybe 'y' y)

test4_1 = test4 ! #b True ! defaults
test4_2 = test4 ! #b False ! defaults
test4_3 = test4 ! #x 'z' ! #b True ! defaults
test4_4 = test4 ! defaults ! #b True
test4_5 = test4 ! paramF #x (Just 'q') ! #b True ! defaults
test4_6 = test4 ! paramF #x Nothing ! #b True ! #y '-'

test5_1 :: ("bar" :! Int -> ()) -> ()
test5_1 f = f ! #bar 3

test5_2 :: ("bar" :! Int -> ()) -> "bar" :! Int -> ()
test5_2 f x = f x

test6 :: Maybe ("x" :! Int -> Int) -> Int
test6 Nothing = 0
test6 (Just f) = f ! #x 42

main :: IO ()
main = do
  test1_1
  test1_2
  test1_3
  test1_4
  test1_5
  test1_6
  test2_1 `mustBe` 16
  test4_1 `mustBe` 'x'
  test4_2 `mustBe` 'y'
  test4_3 `mustBe` 'z'
  test4_4 `mustBe` 'x'
  test4_5 `mustBe` 'q'
  test4_6 `mustBe` 'x'

mustBe :: (Eq a, Show a) => a -> a -> IO ()
mustBe a b
  | a == b = return ()
  | otherwise = error $ show a ++ " must be " ++ show b
