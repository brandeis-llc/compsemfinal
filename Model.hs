module Model where 

import Data.List
import Data.Maybe
import FSynF
import Control.Monad.State.Lazy


locations :: [Location]
locations =  sampleGrid

type TwoPlacePred   = BoardState -> Location -> Location -> Bool
move :: TwoPlacePred

--define move as a list of blocks and a list of open locations 
-- any block can be moved to any open location
-- CPSS will handle updating the location of the block after the evaluation of move returns true
move bs = curry (`elem` [ (x, y) | y <- occupied, x <- (filter (\x -> notElem x occupied) locations)])
    where occupied = (map (\x -> snd x) bs)

--finds the (block, location) tuple representing the block to be moved
-- 
findBlock :: Location -> BoardState -> Maybe (Block, Location)
findBlock loc bS = find (\x -> (snd x == loc)) bS

moveBlock :: Location -> Location -> State BoardState ()
moveBlock loc1 loc2 = state $ \g -> let index = findIndex (\x -> loc1 == (snd x)) g
                             			in let newLoc = (fst (fromJust (findBlock loc1 g)), loc2)
                             				in ((), take (fromJust index) g ++ newLoc : drop ((fromJust index) + 1) g)

testMove :: State BoardState ()
testMove = do
	moveBlock (A',2) (B',4)
	moveBlock (B',3) (A',2)


curState :: BoardState
curState = [((Block 1), (A', 2)), ((Block 2), (B', 3)), ((Block 3), (D', 5))]

type BoardState = [(Block, Location)]
--moveBlock :: Location -> Location -> 
--moveBlock loc1 loc2 = do 
--                    curState <- get
--                  if isJust srcLoc
--                      then do 
--                              curState ++ [(fst (fromJust srcLoc), loc2)]
--                              MovedBlock
--                      else Impossible
--                        where srcLoc = findBlock loc1

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x,y,z)

self ::  (a -> a -> b) -> a -> b
self p = \ x -> p x x 
