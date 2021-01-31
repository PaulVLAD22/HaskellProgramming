import Data.Char
import Data.List
import Data.Maybe


type Name  = String 
type Quantity = Int 

data Ingredient = Ing Name Quantity
    deriving Show


data Reteta = R[ Ingredient]
    deriving Show

r1 = R[Ing "faina" 500, Ing "oua" 4,Ing "zahar" 500]
r2 = R[Ing "fAIna" 500,Ing "zahar" 500,Ing "Oua" 4]
r3= R[Ing "fAIna" 500,Ing "zahar" 500,Ing "Oua" 55]

--- facem instanta EQ PT Ingredient

--facem instanta Eq pt Reteta
--nu conteaza litere /mari din ingrediente name

--nu conteaza ordinea ingredientelor intr-o reteta

-- de exemplu r1 == r2

instance Eq Ingredient where
    (Ing name1 quantity1) == (Ing name2 quantity2) = map toLower name1==map toLower name2 && quantity1==quantity2

eqReteta:: Reteta->Reteta ->Bool 

eqReteta (R[])(R[])=True
eqReteta (R[]) _=False 
eqReteta _ (R[]) =False 
eqReteta (R(Ing n1 q1 :ings1)) (R(Ing n2 q2:ings2))
    | length ings1/=length ings2 =False
    | Ing n1 q1 == Ing n2 q2 = eqReteta(R ings1)(R ings2)
    | otherwise =False 

instance Eq Reteta where
    (R r1 )== (R r2) = eqReteta(R (sort r1)) (R (sort r2))

instance Ord Ingredient where
    Ing n1 q1 <= Ing n2 q2 = map toLower n1 <= map toLower n2 || ((map toLower n1 <= map toLower n2) && q1<=q2)

--Lab logica Propozitionala

type Nume = String 

data Prop
    =Var Nume
    | F
    | T
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop
    |Prop :->: Prop
    | Prop :<->:Prop
    deriving Eq
    
infixr 2 :|:
infixr 3 :&:

--ex 1
--scrieti urmatoarele formule ca expresii de tip Prop
--p1 = (P | Q & P & Q)
p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2= (Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not (Var "Q"))

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not(Var "Q")) :&: (Not (Var "P"):|: Not(Var "R")))

instance Show Prop where
    show(Var nume)= nume
    show F="F"
    show T="T"
    show(Not (Var nume))="~"++nume
    show(p1 :|: p2)="("++show p1++"|"++show p2++")"
    show (p1 :&: p2)="("++show p1++"&"++show p2++")"
    show (p1 :->: p2)=show p1++"->"++show p2
    show (p1 :<->: p2)=show p1++"<->"++show p2
 

type Env = [(Nume,Bool)]

impureLookUp :: Eq a => a -> [(a,b)] ->b

impureLookUp a env = fromJust (lookup a env)


env :: [([Char], Bool)]
env=[("P",True),("Q",False)]


eval :: Prop->Env->Bool 
eval (Var nume) env= impureLookUp nume env
eval (F) env =False 
eval (T) env=True 
eval (p1 :|: p2) env =eval (p1) env || eval (p2) env
eval (p1 :&: p2) env =eval (p1) env && eval (p2) env
eval (Not p) env = not (eval p env)
eval (p1 :->: p2) env = not (eval p1 env) || (eval p2 env)
eval (p1 :<->: p2) env =  eval (p1 :->: p2) env && eval (p2:->: p1) env


variabile ::Prop -> [Nume]
variabile (Var p) = [p]
variabile (F)=[]
variabile(T)=[]
variabile(Not p)=variabile p
variabile(p1 :|: p2)= nub(variabile p1 ++variabile p2)
variabile (p1 :&: p2)= nub(variabile p1++variabile p2)
variabile (p1 :->: p2)= nub(variabile p1++variabile p2)
variabile(p1 :<->:p2) =nub(variabile p1++variabile p2)

--primeste lista de stringuri variable propzitionale
--returneaza toate env posibile
envs :: [Nume] -> [Env]
envs vars = [zip vars environment | environment <- sequence (take (length vars) (repeat [False,True]))]


satisfiabila :: Prop->Bool 
satisfiabila form =or[eval form env | env<-envs(variabile(form))]


valida::Prop->Bool 
valida form = satisfiabila (Not form) == False 

p4::Prop
p4=Var "P" :|: (Not (Var "P"))

echivalenta :: Prop->Prop->Bool 
echivalenta p1 p2 = valida (p1 :<->: p2)

