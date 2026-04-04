{-# LANGUAGE OverloadedLabels, DataKinds, TypeOperators, ViewPatterns,
             PartialTypeSignatures, TemplateHaskell #-}

{-# OPTIONS -fno-warn-partial-type-signatures #-}

module Main where

import Data.Maybe (fromMaybe)
import Data.Functor (void)
import Data.Function ((&))
import Data.Generics.Labels () -- to ensure our instances won't conflict with IsLabel from generic-lens
import Named
import Test.Inspection

test1 ::
  String ->
  "a" :! Bool ->
  "b" :! Bool ->
  IO Bool
test1 "str" (arg #a -> True) (arg #b -> False) = return True
test1 _ _ _ = return False

test1_raw :: String -> Bool -> Bool -> IO Bool
test1_raw "str" True False = return True
test1_raw _ _ _ = return False

inspect $ 'test1 ==- 'test1_raw

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

test2_raw :: Int -> Int
test2_raw x = x * 2

inspect $ 'test2 ==- 'test2_raw

test2_1 :: Int
test2_1 = test2 ! #x 5 + test2 ! #x 3

test3 (arg #a -> a) (arg #b -> b) = a + b

test3_raw a b = a + b

inspect $ 'test3 ==- 'test3_raw

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

test4_raw :: Bool -> Maybe Char -> Maybe Char -> Char
test4_raw b (fromMaybe 'x' -> x) y = if b then x else fromMaybe 'y' y

inspect $ 'test4 ==- 'test4_raw

test4_1 = test4 ! #b True ! defaults
test4_2 = test4 ! #b False ! defaults
test4_3 = test4 ! #x 'z' ! #b True ! defaults
test4_4 = test4 ! defaults ! #b True
test4_5 = test4 ! paramF #x (Just 'q') ! #b True ! defaults
test4_6 = test4 ! paramF #x Nothing ! #b True ! #y '-'

test5_1 :: ("bar" :! Int -> ()) -> ()
test5_1 f = f ! #bar 3

test5_1_raw :: (Int -> ()) -> ()
test5_1_raw f = f 3

inspect $ 'test5_1 ==- 'test5_1_raw

test5_2 :: ("bar" :! Int -> ()) -> "bar" :! Int -> ()
test5_2 f x = f x

test5_2_raw :: (Int -> ()) -> Int -> ()
test5_2_raw f x = f x

inspect $ 'test5_2 ==- 'test5_2_raw

test6 :: Maybe ("x" :! Int -> Int) -> Int
test6 Nothing = 0
test6 (Just f) = f ! #x 42

test6_raw :: Maybe (Int -> Int) -> Int
test6_raw Nothing = 0
test6_raw (Just f) = f 42

inspect $ 'test6 ==- 'test6_raw

newtype M m a = M { runM :: m a }
  deriving (Functor, Applicative, Monad)

-- must typecheck:

test7_1 :: "x" :? Int -> M m Int
test7_1 = undefined

test7_2 :: M m Int
test7_2 = test7_1 ! defaults

test7_3 :: m Int
test7_3 = runM (test7_1 ! defaults)

-- doesn't typecheck (yet):
--
-- test7_4 :: "x" :? Int -> m Int
-- test7_4 = undefined

-- test7_5 :: m Int
-- test7_5 = test7_1 ! defaults

main :: IO ()
main = do
  void test1_1
  void test1_2
  void test1_3
  void test1_4
  void test1_5
  void test1_6
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
