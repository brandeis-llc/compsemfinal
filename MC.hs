module MC where

import Data.List
import FSynF
import Model

type LF = Formula Term

lfCommand :: Command -> LF
lfCommand (Command loc act) = (lfLoc loc) (lfAct act)  

lfLoc :: Location -> (Term -> LF) -> LF
lfLoc loc = \p -> p (Struct (show loc) [])

lfAct :: Action -> Term -> LF
lfAct (Action gest loc) = \loc1 -> lfLoc loc (\loc2 -> lfGest gest (loc1, loc2))

lfGest :: Gesture -> (Term, Term) -> LF
lfGest Move = \ (t1, t2) -> Atom "move" [t1, t2]

bInLF :: LF -> [Int]
bInLF (Atom _ _) = []

bInLFs :: [LF] -> [Int]
bInLFs = nub . concat . map bInLF

freshIndex :: [LF] -> Int
freshIndex lfs = i+1
	where i = maximum (0:(bInLFs lfs))

fresh :: [Term -> LF] -> Int
fresh preds = freshIndex (map ($ dummy) preds)
	where dummy = Struct "" []

type Lookup a = Variable -> a
type Interp a = String -> [a] -> Bool

change :: Lookup a -> Variable -> a -> Lookup a
change g x d = \v -> if x == v then d else g v

eval :: Eq a => 
	[a] ->
	Interp a ->
	Lookup a ->
	Formula Variable -> Bool

eval domain i = eval' where 
	eval' g (Atom str vs) = i str (map g vs)

int1 :: String -> [Int] -> Bool
int1 "R" = rconvert (<)
     where 
           rconvert :: (a -> a -> Bool) -> [a] -> Bool
           rconvert r [x,y] = r x y 

ass2 :: Variable -> Int
ass2 v = if v == x then 1 else if v == y then 2 else 0

type FInterp a = String -> [a] -> a

zero = Struct "zero" []

fint1 :: FInterp Int 
fint1 "zero"  []    = 0
fint1 "s"     [i]   = succ i
fint1 "plus"  [i,j] = i + j 
fint1 "times" [i,j] = i * j 

type TVal a = Term -> a

liftLookup :: FInterp a -> Lookup a -> TVal a 
liftLookup fint g (Var v)         = g v
liftLookup fint g (Struct str ts) = 
           fint str (map (liftLookup fint g) ts)

