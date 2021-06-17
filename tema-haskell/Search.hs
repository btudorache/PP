{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = Node 
    { state         :: s,
      action        :: Maybe a,
      parentNode    :: Maybe (Node s a),
      depth         :: Int,
      heuristic     :: Float,
      children      :: [Node s a]
    } deriving Show 

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    (Node state1 _ _ _ _ _) == (Node state2 _ _ _ _ _) = state1 == state2

instance Ord s => Ord (Node s a) where
    (Node state1 _ _ _ _ _) <= (Node state2 _ _ _ _ _) = state1 <= state2

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState (Node state _ _ _ _ _) = state

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent (Node _ _ parent _ _ _) = parent

nodeDepth :: Node s a -> Int
nodeDepth (Node _ _ _ depth _ _) = depth

nodeChildren :: Node s a -> [Node s a]
nodeChildren (Node _ _ _ _ _ children) = children

nodeHeuristic :: Node s a -> Float
nodeHeuristic (Node _ _ _ _ heuristic _) = heuristic

nodeAction :: Node s a -> Maybe a
nodeAction (Node _ action _ _ _ _) = action

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = createStateSpaceHelper initialState Nothing Nothing 0

createStateSpaceHelper state action parent depth =  let children = map (\pair -> (createStateSpaceHelper (snd pair) (Just (fst pair)) (Just newNode) (depth + 1))) (successors state)
                                                        newNode = (Node state action parent depth (h state) children)
                                                    in newNode
{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => (PQ.PSQ k p) -> (k, (PQ.PSQ k p))
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}

suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> (S.Set s) -> [Node s a]
suitableSuccs node visited = filter (\node -> not (S.member (state node) visited)) (children node)

{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}

insertSucc :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node = let priority = (heuristic node) + (fromIntegral (depth node))
                           in PQ.insertWith (min) node priority frontier

{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insertSuccs :: (ProblemState s a, Ord s) => (Node s a) -> (PQ.PSQ (Node s a) Float) -> (S.Set s) -> (PQ.PSQ (Node s a) Float)
insertSuccs node frontier visited = let succs = suitableSuccs node visited
                                    in foldl (\pq node -> insertSucc pq node) frontier succs

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

astar' :: (ProblemState s a, Ord s) => (S.Set s) -> (PQ.PSQ (Node s a) Float) -> Node s a
astar' visited frontier = let frontierPair = deleteFindMin frontier
                              succ = fst frontierPair
                              newFrontier = snd frontierPair
                              newSet = (S.insert gameState visited)
                              gameState = state succ
                          in if (isGoal gameState) then succ
                             else (astar' newSet (insertSuccs succ newFrontier newSet))
                            

{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = let priority = (heuristic initialNode) + (fromIntegral (depth initialNode))
                    in astar' S.empty (PQ.insert initialNode priority PQ.empty)

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

-- extractPath :: Node s a -> [(a, s)]
-- extractPath goalNode = case (parentNode goalNode) of
--                             Nothing -> []
--                             Just parent -> case (action goalNode) of
--                                                 Just validAction ->  (extractPath parent) ++ [(validAction, (state goalNode))]
 
extractPath :: Node s a -> [(a, s)]
extractPath goalNode = let stream = (iterate (\node -> fromJust (parentNode node)) goalNode)
                           list = takeWhile (\node -> isJust (parentNode node)) stream
                       in reverse (map (\node -> ((fromJust (action node)), state node)) list)