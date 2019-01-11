module Cont where

import Data.List.Split
import Data.List (find, dropWhileEnd)
import Data.Maybe
import Data.Text (toUpper, toLower)
import Data.Char (isSpace)
import Control.Monad.State.Lazy
import FSynF
import MC
import Model

mkc :: (a -> b) -> a -> (b -> r) -> r 
mkc f x next = next (f x)

raise :: a -> (a -> r) -> r 
raise x = \ f ->  f x

mkc' ::  (a -> b) -> a -> (b -> r) -> r 
mkc' f x = raise (f x)

type Cont a r = a -> r
type Comp a r = Cont a r -> r

cpsConst :: a -> Comp a r
cpsConst c = \ k -> k c

cpsConstAct :: (BoardState->a) -> BoardState -> Comp a r
cpsConstAct c bs = \k -> k (c bs)

cpsApply :: Comp (a -> b) r -> Comp a r -> Comp b r
cpsApply m n = \ k -> n (\ b -> m (\ a -> k (a b)))

intCommand_CPS :: BoardState -> Command -> Comp Bool Bool
intCommand_CPS bs (Command loc act) = -- here loc is the location to look for the block, action is a gesture and potential target location for moving the block
   cpsApply (intAct_CPS bs act) (intLoc_CPS loc) 

intLoc_CPS :: Location -> Comp Location Bool
intLoc_CPS loc = cpsConst loc

intAct_CPS :: BoardState -> Action -> Comp (Location -> Bool) Bool
intAct_CPS bs (Action gest loc) = cpsApply (intTAct_CPS bs gest) (intLoc_CPS loc)

intTAct_CPS :: BoardState -> Gesture -> Comp (Location -> Location -> Bool) Bool
intTAct_CPS bs Move = cpsConstAct move bs

compSent s bs = intCommand_CPS bs s id

b1 = (A', 2)
m1 = Action Move (A', 5)
ex1 = compSent (Command b1 m1)  

--currently input string must look like "A2 Move A4" or "Block 5 Move B6"
sentSplit :: String -> BoardState -> Command
sentSplit str bs
              | strLen == 3 = locCommand $ splitOn " " str
              | strLen == 4 = blockCommand bs (splitOn " " str)
              | otherwise = error ("invalid command")
               where strLen = length $ splitOn " " str

-- if the command has 3 words this function will be called
-- A2 move A4 -> Command (A', 2) Action Move (A', 4)
locCommand :: [String] -> Command
locCommand s = if length s /= 3
               then error "locCommand called with wrong arguments"
               else Command (strToLoc $ s!!0) (Action Move (strToLoc $ s!!2))

-- if the command has 4 words (the first two are the block) this function is called
blockCommand :: BoardState -> [String] -> Command
-- [Block, 1, Move, A2]
blockCommand bs s = if length s /= 4
                 then error "blockCommand called with wrong arguments"
                 -- getBlock 1   strToLoc A2
                 else Command (getBlock bs (s!!1)) (Action Move (strToLoc $ s!!3))

--BoardState is the current BoardState from Main
-- String is a string corresponding to an int used to construct the block
getBlock :: BoardState -> String -> Location
getBlock bs i = if isJust b
				then (snd (fromJust b))
				else error ("Block not found, try 'add'")
					where b = find (\x -> (fst x) == (Block (read i))) bs

strToLoc :: String -> Location
strToLoc st = if (length s) /= 2
              then error (s)
              else (read $ [s!!0]++ "'", read $ [s!!1])
              	where s = trim st


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
--https://stackoverflow.com/questions/6270324/in-haskell-how-do-you-trim-whitespace-from-the-beginning-and-end-of-a-string

getL1 :: Command -> Location
getL1 (Command loc1 (Action _ loc2)) = loc1

getL2 :: Command -> Location
getL2 (Command loc1 (Action _ loc2)) = loc2

mainLoop s = do
        print s --display current block state (curState)
        line <- getLine --get input from user, a location command, A1 Move A2
        unless (line == "quit") $ do
          if (compSent (sentSplit line s) s) --display success or failure
          then 
          	let newState = execState (moveBlock (getL1 $ sentSplit line s) (getL2 $ sentSplit line s)) s in
          	      mainLoop newState
          else if (isJust (findBlock (getL1 $ sentSplit line s) s)) --there is a block to be moved
               then print "Location occupied."
               else print "No block to move."

          putStrLn " "
          mainLoop s

main ::IO ()
main = mainLoop curState
