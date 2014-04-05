data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq)
data Rank = RA | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK deriving (Show, Enum, Eq)

data Card = Card Rank Suit deriving (Show)

data Foundation = Foundation [Card] [Card] [Card] [Card]  deriving (Show)

data Board = Board { foundation :: Foundation,
                     rows :: ([Card], [Card], [Card], [Card], [Card], [Card], [Card], [Card])
                   } deriving (Show)


rank :: Card -> Rank
rank (Card r _) = r

suit :: Card -> Suit
suit (Card _ s) = s

solvedBoard = 
  let dia   = [ Card x Diamonds | x <- [RA .. RK]]
      heart = [ Card x Hearts | x <- [RA .. RK]]
      clubs = [ Card x Clubs | x <- [RA .. RK]]
      spades= [ Card x Spades | x <- [RA .. RK]]
  in 
      Board {foundation = Foundation heart dia clubs spades,
             rows = ([],[],[],[],[],[],[],[])
            }


isLegalMoveRow :: Card -> [Card] -> Bool
isLegalMoveRow _ [] = True
isLegalMoveRow (Card RA _) _ = False
isLegalMoveRow (Card RK _) (x:_) = False
isLegalMoveRow card (topRowCard:_) = succ (rank card) == rank topRowCard

isLegalMoveFoundation :: Card -> [Card] -> Bool
isLegalMoveFoundation _ ((Card RK _):_) = False
isLegalMoveFoundation (Card x s1) ((Card y s2):_) = x == succ y && s1 == s2
isLegalMoveFoundation _ _ = False

generateMoves :: Board -> [Board]
generateMoves 


solve :: Board -> Maybe [Board]
solve board = Just []
