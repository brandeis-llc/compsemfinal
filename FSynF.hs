module FSynF where

import Data.List

data Column = A' | B' | C' | D' | E' 
            | F' | G' | H' | I' | J'
              deriving (Eq,Ord,Show,Enum,Read) --can possibly reuse this column/row structure

type Row    = Int 
type Location = (Column,Row) -- renamed this location from Attack, locations are letter, number tuples

data Block = Block Int 
            deriving Show --changing from Ships to Blocks

instance Eq Block where 
	Block a == Block b = a == b

blocks :: [Block]
blocks = [Block 1, Block 2, Block 3, Block 4]

-- used to communicate what happens
data Reaction = NoBlock | Impossible | MovedBlock deriving Show 

type Turn = (Command, Reaction) --Currently only Move

data Colour   = Red | Yellow | Blue | Green | Orange 
                deriving (Eq,Show,Bounded,Enum)

data Answer   = Black | White deriving (Eq,Show)

data Term = Var Variable | Struct String [Term]
            deriving (Eq, Ord)

instance Show Term where 
  show (Var v)       = show v 
  show (Struct s []) = s
  show (Struct s ts) = s ++ show ts


instance Show Variable where 
  show (Variable name [])  = name
  show (Variable name [i]) = name ++ show i
  show (Variable name is ) = name ++ showInts is
     where showInts []     = "" 
           showInts [i]    = show i  
           showInts (i:is) = show i ++ "_" ++ showInts is

x, y, z :: Variable
x = Variable "x" []
y = Variable "y" []
z = Variable "z" []

tx, ty, tz :: Term 
tx = Var x
ty = Var y
tz = Var z

isVar :: Term -> Bool
isVar (Var _) = True
isVar _       = False

varsInTerm :: Term -> [Variable]
varsInTerm (Var v)       = [v]
varsInTerm (Struct s ts) = varsInTerms ts

varsInTerms :: [Term] -> [Variable]
varsInTerms = nub . concat . map varsInTerm

type Pattern  = [Colour]
type Feedback = [Answer] 

data Command = Command Location Action deriving Show
data Action = Action Gesture Location deriving Show

data Gesture = Move 
          deriving Show 

type Grid = [(Column, Row)]

type Name     = String 
type Index    = [Int]
data Variable = Variable Name Index deriving (Eq,Ord)
data Formula a = Atom String [a]
               deriving Eq

instance Show a => Show (Formula a) where 
  show (Atom s [])   = s
  show (Atom s xs)   = s ++ show xs 

sampleGrid :: Grid
sampleGrid = [(A',1), (A',2), (A',3), (A',4), (A',5),
              (B',1), (B',2), (B',3), (B',4), (B',5),
              (C',1), (C',2), (C',3), (C',4), (C',5),
              (D',1), (D',2), (D',3), (D',4), (D',5),
              (E',1), (E',2), (E',3), (E',4), (E',5)
              ]


