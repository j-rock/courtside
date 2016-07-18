module Main where

import Game

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
   describe "Game" . parallel $ do

       it "passes trivial test" . property $
           \x xs -> head (x:xs) == (x :: Int)

