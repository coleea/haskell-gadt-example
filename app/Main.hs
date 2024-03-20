{-# LANGUAGE GADTs #-}

module Main where


-- Expr 타입은 GADT로 정의됩니다. 각 생성자는 Expr 타입을 가지지만, 내부적으로 서로 다른 타입을 가질 수 있습니다.
data Expr a where
    I   :: Int -> Expr Int
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq  :: Eq a => Expr a -> Expr a -> Expr Bool

-- 타입에 따라 다른 동작을 하는 eval 함수를 정의할 수 있습니다.
-- 인자로 GADT를 전달받습니다
-- 재귀적으로 입력받은 GADT 타입의 식을 파싱하여 계산합니다.
eval :: Expr a -> a
eval (I n)       = n
eval (B b)       = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq e1 e2)  = eval e1 == eval e2

-- 사용 예:
-- (5 + 3) * 2 == 16
exampleExpr :: Expr Int
exampleExpr = Mul (Add (I 5) (I 3)) (I 2)

-- 이 예제에서는 다음과 같이 eval 함수를 호출합니다:
-- eval exampleExpr == 16


main :: IO ()
main = do 
    putStrLn "Hello, Haskell!"
    putStrLn $ show $ (eval exampleExpr)
    putStrLn $ show $ (eval (Eq (I 1) (I 1)))
    putStrLn "== end of program =="
