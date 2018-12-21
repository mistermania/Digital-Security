module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.Map.Strict as M

import Basics as B
import Inventory

basics :: SpecWith ()
basics = describe "basics" $ do
  describe "simple operations" $ do
    prop "add" (\a b -> add a b == a + b)
    prop "divide" (\a b -> b == 0 || divide a b == a / b)
    prop "divide' /= 0" (\a b -> b == 0 || divide' a b == DoubleSuccess (a / b))
    prop "divide' == 0" (\a -> divide' a 0 == DoubleFailure)
    prop "divide'' /= 0" (\a b -> b == 0 || divide'' a b == B.Success (a / b))
    prop "divide'' == 0" (\a -> divide'' a 0 == B.Failure)
  describe "eval" $ do
    prop "Value" (\a -> eval (Value a) == a)
    prop "Add" (\a b -> eval (Add (Value a) (Value b)) == a + b)
    prop "Sub" (\a b -> eval (Sub (Value a) (Value b)) == a - b)
    prop "Mul" (\a b -> eval (Mul (Value a) (Value b)) == a * b)
    it "test 1" (eval (Add (Mul (Value 5) (Value 3.4)) (Value 8)) `shouldBe` 5*3.4+8)
  describe "eval'" $ do
    prop "Value" (\a -> eval' (Value a) == B.Success a)
    prop "Add" (\a b -> eval' (Add (Value a) (Value b)) == B.Success (a + b))
    prop "Sub" (\a b -> eval' (Sub (Value a) (Value b)) == B.Success (a - b))
    prop "Mul" (\a b -> eval' (Mul (Value a) (Value b)) == B.Success (a * b))
    it "test 1" (eval' (Add (Mul (Value 5) (Value 3.4)) (Value 8)) `shouldBe` B.Success (5*3.4+8))
    it "test 2" (eval' (Add (Div (Value 5) (Value 3.4)) (Value 0)) `shouldBe` B.Success (5/3.4+0))
  describe "lists" $ do
    prop "listSum" (\l -> listSum (fromList l) == sum l)
    prop "listEq" (\l1 l2 -> listEq (fromList l1) (fromList l2) == ((l1 :: [Int]) == l2))
    prop "toList" (\l -> toList (fromList l) == (l :: [Int]))

fromList :: [a] -> B.List a
fromList = foldr Cons Empty

inventory :: SpecWith ()
inventory = describe "inventory" $ do
  describe "accessors" $ do
    prop "getName" (\n -> getName (ItemDesc n 0 Book) == n)
    prop "getCost" (\c -> getCost (ItemDesc "name" c Book) == c)
    it "getType" (getType (ItemDesc "name" 0 Book) `shouldBe` Book)
  describe "check" $ do
    it "check true" (check (Cost LT 300) (ItemDesc "foo" 200 Book) `shouldBe` True)
    it "check false" (check (Cost GT 300) (ItemDesc "foo" 200 Book) `shouldBe` False)
  describe "filterCatalog" $ do
    it "search 1" (filterCatalog (And [Cost LT 300, Cost GT 200]) sampleCatalog `shouldBe` M.singleton 3 (ItemDesc "Huawei P20 Lite" 247 HighTech)  )
    it "search 2" (filterCatalog (Or [TypeIs t | t <- [Clothing, HighTech, Book]]) sampleCatalog `shouldBe` sampleCatalog)
  describe "catalogItems" $ do
    it "test 1" $ catalogItems sampleCatalog `shouldBe` [1..5]
  describe "filterInventory" $ do
    it "test 1" $ filterInventory [1..5] sampleInventory `shouldBe` sampleInventory
    it "test 1" $ filterInventory [] sampleInventory `shouldBe` M.empty
  describe "search" $ do
    it "write your own test!" ("TODO!" `shouldBe` "TODO!!")

main :: IO ()
main = hspec $ do
  basics
  inventory
