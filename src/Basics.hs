module Basics where

add :: Double -> Double -> Double
add x y = x + y

divide :: Double -> Double -> Double
divide x y = x / y

data DoubleResult
    = DoubleSuccess Double
    | DoubleFailure
    deriving (Show, Eq)

divide' :: Double -> Double -> DoubleResult
divide' a b =  if b == 0
                 then DoubleFailure
                 else DoubleSuccess (a/b)


data Result a
    = Success a
    | Failure
    deriving (Show, Eq)

divide'' :: Double -> Double -> Result Double
divide'' a b = if b == 0
                 then Failure
                 else Success (a/b)

data Operation
    = Add Operation Operation
    | Sub Operation Operation
    | Mul Operation Operation
    | Div Operation Operation
    | Value Double
    deriving (Show, Eq)

op1 :: Operation -- 5 + 3*6
op1 = Add (Value 5) (Mul (Value 3) (Value 6))

eval :: Operation -> Double
eval o = case o of
            Add x y -> eval x + eval y
            Sub x y -> eval x - eval y
            Mul x y -> eval x * eval y
            Div x y -> eval x / eval y
            Value x -> x


example1 :: Double
example1 = eval (Add (Mul (Value 5) (Value 3.4)) (Value 8))

example2 :: Double
example2 = eval (Add (Div (Value 5) (Value 0)) (Value 5))

eval' :: Operation -> Result Double
eval' o = case o of
            Add x y ->
              case eval' x of
                  Failure -> Failure
                  Success resultx ->
                    case eval' y of
                      Failure -> Failure
                      Success resulty -> Success(resultx + resulty)
            Sub x y ->
               case eval' x of
                 Failure -> Failure
                 Success resultx ->
                   case eval' y of
                     Failure -> Failure
                     Success resulty -> Success(resultx -resulty)
            Mul x y ->
               case eval' x of
                 Failure -> Failure
                 Success resultx ->
                   case eval' y of
                     Failure -> Failure
                     Success resulty -> Success(resultx*resulty)
            Div x y ->
               case eval' x of
                 Failure -> Failure
                 Success resultx ->
                   case eval' y of
                     Failure -> Failure
                     Success resulty -> divide'' resultx resulty
            Value x -> Success x

example1' :: Result Double
example1' = eval' (Add (Mul (Value 5) (Value 3.4)) (Value 8))

example2' :: Result Double
example2' = eval' (Add (Div (Value 5) (Value 3.4)) (Value 0))


data List a
    = Empty
    | Cons a (List a)
    deriving Show

listHead :: List a -> Result a
listHead as = case as of
               Empty -> Failure
               Cons a _ -> Success a

head1 :: Result Int
head1 = listHead Empty -- Failure
head2 :: Result Int
head2 = listHead (Cons 5 (Cons 4 Empty)) -- success 5

listTail :: List a -> Result (List a)
listTail as = case as of
                Empty -> Failure
                Cons _ b -> Success b

tail1 :: Result (List Int)
tail1 = listTail Empty -- Failure
tail2 :: Result (List Int)
tail2 = listTail (Cons 5 (Cons 4 Empty)) -- success (Cons 4 Empty)

listSum :: List Int -> Int
listSum lst = case lst of
               Empty -> 0
               Cons a as -> a + listSum as

listEq :: Eq a => List a -> List a -> Bool
listEq l1 l2 = 
    case (l1, l2) of
      (Empty, Empty) -> True
      (Cons a as, Cons b  bs) -> if a == b
                                   then listEq as bs
                                   else False
      _ -> False

toList :: List a -> [a]
toList as = case as of
            Empty -> []
            Cons b bs ->  b : toList bs 



allEvenList :: [Int] -> Bool
allEvenList lst =
    case lst of
      [] -> True
      x:xs -> even x && allEvenList xs

myall :: (a -> Bool) -> [a] -> Bool
myall f lst =
    case lst of
      [] -> True
      x:xs -> f x && myall f xs



