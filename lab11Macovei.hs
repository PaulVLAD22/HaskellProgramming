-- Semigrup (M,<>), <> e asociativa
-- Monoid (M,<>,mempty) (M,<>) semigrup , mempty elem neutru la st si dr

import Test.QuickCheck
import Test.QuickCheck.Gen

semigroupAssoc:: (Eq m, Semigroup m) => m -> m->m->Bool 
semigroupAssoc a b c =(a<>(b<>c))==((a<>b)<>c)

monoidLeftIdentity :: (Eq m,Monoid m) => m->Bool 
monoidLeftIdentity a = (mempty <> a) ==a

monoidRightIdentity :: (Eq m,Monoid m) => m->Bool 
monoidRightIdentity a = (a <> mempty ) ==a
----------------------------
--ex1

data Trivial = Trivial
    deriving (Eq,Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial

instance Arbitrary Trivial where
    arbitrary = elements [Trivial]

type TrivialAssoc = Trivial-> Trivial ->Trivial ->Bool 
type TrivialIdentity = Trivial->Bool

test_trivial_assoc = quickCheck (semigroupAssoc :: TrivialAssoc)
test_trivial_ml = quickCheck (monoidLeftIdentity :: TrivialIdentity)
test_trivial_mr = quickCheck (monoidRightIdentity:: TrivialIdentity)


--ex2 

-- conjuctii

newtype BoolConj = BoolConj Bool 
    deriving (Eq,Show)

instance Semigroup BoolConj where
    (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Monoid BoolConj where
    mempty = BoolConj True 

instance Arbitrary BoolConj where
    arbitrary = fmap BoolConj arbitrary 

type BoolConjAssoc = BoolConj->BoolConj->BoolConj->Bool 
type BoolConjIdentity = BoolConj->Bool 


--ex 3

newtype BoolDisj = BoolDisj Bool 
    deriving (Eq,Show)

instance Semigroup BoolDisj where
    (BoolDisj x) <> (BoolDisj y) = BoolDisj (x || y)

instance Monoid BoolDisj where
    mempty = BoolDisj False 

instance Arbitrary BoolDisj where
    arbitrary = fmap BoolDisj arbitrary 

--ex 4

newtype Identity a = Identity a
    deriving (Eq,Show)

instance Semigroup a=>Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity(x <> y)

--- e instanta de semigroup deci deja are definita x<>y

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

instance Arbitrary a=>Arbitrary (Identity a) where
    arbitrary = fmap Identity arbitrary 


data Two a b= Two a b
    deriving (Eq,Show)
instance (Semigroup a, Semigroup b) =>Semigroup (Two a b) where 
    (Two x1 y1) <> (Two x2 y2) = Two (x1 <>x2) (y1 <>y2)

instance (Monoid a, Monoid b)=> Monoid(Two a b) where
    mempty = Two mempty mempty 

instance (Arbitrary a,Arbitrary b)=>Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary


data Or a b =Fst a |Snd b
    deriving (Eq,Show)

--trebuie sa implementez (<>) ai 
-- fst 1 <>snd 2 = snd 2
-- fst 1 <>fst 2 = fst 2
-- snd 1 <>fst 2 = snd 1
-- snd 1 <> snd 2 = snd 1

instance Semigroup (Or a b) where
    Fst _ <> x = x
    y     <> _ = y

-- nu ex niciun elem neutru mempty 
instance (Arbitrary a,Arbitrary b ) => Arbitrary  (Or a b) where
    arbitrary = oneof [arbitraryFst,arbitrarySnd]
        where
                arbitraryFst  = Fst <$> arbitrary 
                arbitrarySnd = Snd <$> arbitrary 


