{-# LANGUAGE OverloadedLabels, DataKinds, TypeOperators #-}

module Main where

import Data.Function ((&))
import Named

test1 ::
  String ->
  Flag "a" ->
  Bool `Named` "b" ->
  IO ()
test1 "str" (Flag True) (Flag False) = return ()
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
    & with #b False
    & with #a True

test1_4 =
  with #a True $
  with #b False $
    test1 "str"

test1_5 =
  with #a True $
  test1 "str"! #b False

test1_6 =
  test1 "str"! #a True
    & with #b False

test2 :: Named Int "x" -> Int
test2 x = unnamed x * 2

test2_1 :: Int
test2_1 = test2 ! #x 5 + test2 ! #x 3

main :: IO ()
main = do
  test1_1
  test1_2
  test1_3
  test1_4
  test1_5
  test1_6
  test2_1 `mustBe` 16

mustBe :: (Eq a, Show a) => a -> a -> IO ()
mustBe a b
  | a == b = return ()
  | otherwise = error $ show a ++ " must be " ++ show b
