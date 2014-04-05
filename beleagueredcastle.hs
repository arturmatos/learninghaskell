import Data.Sequence
import Data.Foldable

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq)
data Rank = RA | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK deriving (Show, Enum, Eq)

data Card = Card Rank Suit deriving (Show)

data Stack = Foundation [Card] | Row [Card] deriving (Show)

data Board = Board [Stack] deriving (Show)

rank :: Card -> Rank
rank (Card r _) = r

suit :: Card -> Suit
suit (Card _ s) = s

cards :: Stack -> [Card]
cards (Foundation lst) = lst
cards (Row lst) = lst

push :: Stack -> Card -> Stack
push (Foundation cards) new = (Foundation (new:cards))
push (Row cards) new = (Row (new:cards))

pop :: Stack -> (Card, Stack)
pop (Foundation (x:xs)) = (x, (Foundation xs))
pop (Row (x:xs)) = (x, (Row xs))

isLegalMove :: Stack -> Stack -> Bool
isLegalMove (Row (x:_)) (Row xs) = isLegalMoveRow x xs
isLegalMove (Row (x:_)) (Foundation xs) = isLegalMoveFoundation x xs
isLegalMove (Foundation (x:_)) (Row xs) = isLegalMoveRow x xs
isLegalMove _ _ = False

isLegalMoveRow :: Card -> [Card] -> Bool
isLegalMoveRow _ [] = True
isLegalMoveRow (Card RA _) _ = False
isLegalMoveRow (Card RK _) (x:_) = False
isLegalMoveRow card (topRowCard:_) = succ (rank card) == rank topRowCard

isLegalMoveFoundation :: Card -> [Card] -> Bool
isLegalMoveFoundation _ ((Card RK _):_) = False
isLegalMoveFoundation (Card x s1) ((Card y s2):_) = x == succ y && s1 == s2
isLegalMoveFoundation _ _ = False


-- move :: Board -> int -> int -> Board
-- move (Board stackList) index1 index2 =
--   let fromCards = cards stackList !! index1
--       toCards = cards stackList !! index2
--       elem:rem = fromCards
--       added = elem:toCards      
--   in (Board toList(update index2 added (update index1 rem (fromList stackList))))


