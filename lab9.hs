import Data.List
import Data.Char
import Data.Maybe
--lab 8.pdf

type Nume = String
data Prop
    = Var Nume   --Var p
    | F
    | T
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop
    deriving (Eq,Read)

p1::Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p3::Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))

instance Show Prop where
    show (Var x) = x
    show F = "F"
    show T = "T"
    show (Not formula) = "(~" ++ show formula ++ ")"
    show ( formula1 :|: formula2 )= "(" ++ show formula1 ++ "|" ++show formula2 ++")"
    show (formula1 :&: formula2) = "(" ++ show formula1 ++ "&" ++show formula2 ++")"

impureLookup :: Eq a => a -> [(a,b)] ->b
impureLookup a = fromJust . lookup a

type Env = [(Nume,Bool)]
eval :: Prop -> Env -> Bool
eval (Var x) env = impureLookup x env

eval (F) _ = False
eval (T) _ = True
eval (Not p) env = not (eval p env)
eval (a :|: b) env = (eval a env) || (eval b env)
eval (a :&: b) env = (eval a env) && (eval b env)

--variable :: Prop-> [Nume]
variabile :: Prop -> [Nume]
variabile (Var a) = [a]
variabile T = []
variabile F = []
variabile (Not a) = variabile a
variabile (a :|: b) = nub (variabile a ++ variabile b)
variabile (a :&: b) = nub (variabile a ++ variabile b)

satisfiabila :: Prop -> Bool
satisfiabila prop = or [eval prop x| x <- envs(variable prop)]

valida :: Prop -> Bool
valida prop = and [eval prop x | x <- envs(variable prop)]

tabelAdevar :: Prop -> String
tabelAdevar prop = intercalate " " (variable prop) ++ " " ++ showProp prop ++ "\n"


envrec :: [Nume] -> Int -> Int -> Env
envrec l i len
  | len > 0 = envrec l (i `div` 2) (len - 1) ++ [(l !! (len -1), e1)]
  | otherwise = []
  where
    e1 = odd i

envs :: [Nume] -> [Env]
envs l = [envrec l i (length l) | i <- [0 .. (2 ^ length l - 1)]]


--rezolvari date de moanga


tabelaAdevar1 :: Prop -> IO ()
tabelaAdevar1 = table

centre :: Int -> String -> String
centre w s = replicate h ' ' ++ s ++ replicate (w-n-h) ' '
where
n = length s
h = (w - n) `div` 2

dash :: String -> String
dash s = replicate (length s) '-'

fort :: Bool -> String
fort False = "F"
fort True = "T"

-- Prelude> unlines ["Hello", "World", "!"]
-- "Hello\nWorld\n!\n"
-- Prelude> zipWith (+) [1, 2, 3] [4, 5, 6]
-- [5,7,9]
-- Prelude> unwords ["Lorem", "ipsum", "dolor"]
-- "Lorem ipsum dolor"

showTable :: [[String]] -> IO ()
showTable tab = putStrLn (
unlines [ unwords (zipWith centre widths row) | row <- tab ] )
where
widths = map length (head tab)
-- Prelude> import Data.List
-- Prelude Data.List> nub [1,2,3,4,3,2,1,2,4,3,5]
-- [1,2,3,4,5]

table p = tables [p]

tables :: [Prop] -> IO ()
tables ps =
let xs = nub (concatMap variabile ps) in
showTable (
[ xs ++ ["|"] ++ [show p | p <- ps] ] ++
[ dashvars xs ++ ["|"] ++ [dash (show p) | p <- ps ] ] ++
[ evalvars e xs ++ ["|"] ++ [fort (eval p e) | p <- ps ] | e <- envs xs]
)
where dashvars xs = [ dash x | x <- xs ]
evalvars e xs = [ fort (eval (Var x) e) | x <- xs ]