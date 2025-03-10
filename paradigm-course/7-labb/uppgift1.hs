-- This exercise was made to understand how interpreters work. We are essentially writing our own mini imperative  
-- language in Haskell, that enables us to declare variables. This is done by taking a premade list of tokens you'd normally
-- get from a parser, and then interpret the list with the help of the functions below.
 
data IntExpr = Const Int -- this makes evalutaion of integer expressions possible
             | Var String
             | Add IntExpr IntExpr
             | Mul IntExpr IntExpr
  deriving (Eq,Show)

data BoolExpr = IntEq IntExpr IntExpr -- Lets us write and evaluate Boolean expressions
              | BoolNot BoolExpr
  deriving (Eq,Show)
-- extends the above data types to work together, and i.e. lets us assign variables to integer expressions,
-- create if/else statements, as well as While-loops.
data Instr = Assign String IntExpr 
           | If BoolExpr [Instr] [Instr] 
           | While BoolExpr [Instr] 
  deriving (Eq,Show)


findVar :: String -> [(String,Int)] -> Int  -- used to find a variable in a list and outputs its corr. value
findVar x (y : env) | x == fst y = snd y
                    | otherwise = findVar x env


evalIntExpr :: IntExpr -> [(String,Int)] -> Int  -- evaluates Integer Expression types with the help of the declared variables and outputs the final evaluation.
evalIntExpr (Const x) _ = x
evalIntExpr (Var x) env = findVar x env 
evalIntExpr (Add a b) env = evalIntExpr a env + evalIntExpr b env
evalIntExpr (Mul a b) env = evalIntExpr a env * evalIntExpr b env


evalBoolExpr :: BoolExpr -> [(String, Int)] -> Bool
evalBoolExpr (IntEq n1 n2) env = evalIntExpr n1 env == evalIntExpr n2 env
evalBoolExpr (BoolNot expr) env = not(evalBoolExpr expr env)


addToEnv :: (String,Int) -> [(String,Int)] -> [(String,Int)] --Add/changes the environment
addToEnv x [] = [x] 
addToEnv x (y:env) | fst x == fst y = x : env
                   | otherwise = y : addToEnv x env


eval :: [Instr] -> [(String,Int)] -> [(String,Int)]
eval [] env = env 
eval ((Assign x e) : xs) env = eval xs (addToEnv (x, evalIntExpr e env) env) 
eval ((If booexpr xs ys) : zs) env | evalBoolExpr booexpr env = eval (xs ++ zs) env
                                   | otherwise = eval (ys ++ zs) env 
eval ((While booexpr instr) : xs) env | evalBoolExpr booexpr env = eval (instr ++ (While booexpr instr : xs)) env
                                      | otherwise = eval xs env


run :: [Instr] -> [(String,Int)]
run xs = eval xs []


prog1 :: [Instr]
prog1 = [ Assign "x" (Const 1)
        , Assign "y" (Add (Var "x") (Var "x"))
        , Assign "y" (Mul (Var "y") (Add (Var "x") (Const 1)))
        , Assign "z" (Add (Var "x") (Mul (Var "y") (Const 4)))
        ]

prog2 :: [Instr]
prog2 = [ Assign "x" (Const 0)
        , If (IntEq (Var "x") (Const 0))
             [ Assign "y" (Const 1) ]
             [ Assign "y" (Const 2) ]
        , If (BoolNot (IntEq (Var "y") (Const 2)))
             [ Assign "z" (Const 2) ]
             [ Assign "z" (Const 1) ]
        ] 

prog3 :: [Instr]
prog3 = [ Assign "x" (Const 0)
        , Assign "y" (Const 1)
        , If (BoolNot (IntEq (Var "x") (Var "y")))
             [ Assign "x" (Var "y")
             , If (IntEq (Var "x") (Var "y"))
                  [ Assign "z" (Const 0) ]
                  [ Assign "z" (Const 1) ]
             , Assign "y" (Const 5) ]
             [ Assign "z" (Const 2) ]
        , Assign "w" (Var "z")
        ]


progfac :: [Instr]
progfac = [ Assign "x" (Const 5)
          , Assign "y" (Const 1)
          , While (BoolNot (IntEq (Var "x") (Const 0)))
                  [ Assign "y" (Mul (Var "y") (Var "x"))
                  , Assign "x" (Add (Var "x") (Const (-1)))
                  ]
          ]

