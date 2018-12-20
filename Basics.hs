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
divide' a b  =
    if b == 0
      then DoubleFailure
      else DoubleSuccess ((/) a b)

data Result a
    = Success a
    | Failure
    deriving (Show, Eq)

divide'' :: Double -> Double -> Result Double
divide'' a b =
    if b == 0
      then Failure
      else Success ((/) a b)

data Operation
    = Add Operation Operation
    | Sub Operation Operation
    | Mul Operation Operation
    | Div Operation Operation
    | Value Double
    deriving (Show, Eq)

op1 :: Operation
op1 = Add (Value 5) (Mul (Value 3) (Value 6))

eval :: Operation -> Double
eval o = case o of
           Add x y -> eval x + eval y
           Sub x y -> eval x - eval y
           Mul x y -> eval x * eval y
           Div x y -> eval x / eval y
           Value xooo -> xooo

example1 :: Double
example1 = eval (Add (Mul (Value 5) (Value 3.4)) (Value 8))

example2 :: Double
example2 = eval (Add (Div (Value 5) (Value 0)) (Value 5))

eval' :: Operation -> Result Double
eval' o = case o of
            Add x y ->
              case eval' x of
                Failure -> Failure
                Success xu ->
                  case eval' y of
                    Failure -> Failure
                    Success yu -> Success (xu + yu)
            Sub x y ->
                case eval' x of
                  Failure -> Failure
                  Success xu ->
                    case eval' y of
                      Failure -> Failure
                      Success yu -> Success (xu - yu)
            Mul x y ->
                case eval' x of
                  Failure -> Failure
                  Success xu ->
                    case eval' y of
                      Failure -> Failure
                      Success yu -> Success (xu * yu)
            Div x y ->
                case eval' x of
                  Failure -> Failure
                  Success xu ->
                    case eval' y of
                      Failure -> Failure
                      Success yu ->
                        if yu == 0
                          then Success(0)
                          else Success((/) xu yu)
            Value xo -> Success(xo)

example1' :: Result Double
example1' = eval' (Add (Mul (Value 5) (Value 3.4)) (Value 8))

example2' :: Result Double
example2' = eval' (Add (Div (Value 5) (Value 3.4)) (Value 0))

data List a
    = Empty
    | Cons a (List a)
    deriving Show

listHead :: List a -> Result a
listHead = undefined

head1 :: Result Int
head1 = listHead Empty -- Failure
head2 :: Result Int
head2 = listHead (Cons 5 (Cons 4 Empty)) -- success 5

listTail :: List a -> Result (List a)
listTail = undefined

tail1 :: Result (List Int)
tail1 = listTail Empty -- Failure
tail2 :: Result (List Int)
tail2 = listTail (Cons 5 (Cons 4 Empty)) -- success (Cons 4 Empty)


listSum :: List Int -> Int
listSum = undefined

listEq :: Eq a => List a -> List a -> Bool
listEq = undefined

toList :: List a -> [a]
toList = undefined
