% Poker.#ext
% This file was produced from Poker.lit

% ABRHLibs -- a personal library of Haskell modules
% Copyright (C) 2007, 2008,  Andrew Rock
% 
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

\module{Poker} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Poker} provides basic stuff like
categorisation of hands, not tactics.

\begin{code}
module ABR.Poker (
      sortByRankSuit, sortBySuitRank, groupByRank,
      groupBySuit, areSuccRanks, HandType(..), handType,
      isGarbage, isPair, isTwoPair, isTriple, isStraight,
      isFlush, isFullHouse, isPoker, isStraightFlush,
      compareCards, compareGroups, compareHands, beats, 
      ties
   ) where
\end{code}

\begin{code}
import Data.List
\end{code}

\begin{code}
import ABR.Data.List
import ABR.PlayingCards
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Requires review.
   

\submodule{Presorting and grouping} %%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{sortByRankSuit}~$\mathit{cs}$ sorts
$\mathit{cs}$ by rank and then suit.\\
\highlighttt{sortBySuitRank}~$\mathit{cs}$ sorts
$\mathit{cs}$ by suit and then rank.

\begin{code}
sortByRankSuit, sortBySuitRank :: [Card] -> [Card]
sortByRankSuit = sortBy (\(Suit r s) (Suit r' s') ->
      case compare r r' of
         EQ -> compare s s'
         o  -> o
   )
sortBySuitRank = sortBy (\(Suit r s) (Suit r' s') -> 
      case compare s s' of
         EQ -> compare r r'
         o  -> o
   )
\end{code}

\noindent \highlighttt{groupByRank}~$\mathit{cs}$ groups
the cards in $\mathit{cs}$ by common ranks. Each group
will be sorted by suit. All the groups are sorted by
the length of the group.
\highlighttt{groupBySuit}~$\mathit{cs}$ groups
the cards in $\mathit{cs}$ by common suits. Each group
will be sorted by rank. All the groups are sorted by
the length of the group.

\begin{code}
groupByRank, groupBySuit :: [Card] -> [[Card]]
groupByRank = sortByLength 
   . groupBy (\(Suit r _) (Suit r' _) -> r == r')
   . sortByRankSuit
groupBySuit = sortByLength 
   . groupBy (\(Suit _ s) (Suit _ s') -> s == s')
   . sortBySuitRank
\end{code}

\noindent \highlighttt{areSuccRanks}~$r$~$r'$ returns {\tt
True} iff $r'$ is the next highest rank after $r$.

\begin{code}
areSuccRanks :: Rank -> Rank -> Bool
areSuccRanks r r' = case r of
   King -> case r' of Ace -> True
                      _   -> False
   _    -> succ r == r'
\end{code}

\noindent \highlighttt{allSuccRanks}~$H$ returns {\tt
True} iff all the cards in $H$ have consecutive ranks.

\begin{code}
allSuccRanks :: Hand -> Bool
allSuccRanks h = 
   let asr rs = and $ zipWith areSuccRanks rs (tail rs)
   in case sort $ map rank h of
      Ace : rs' -> asr (Ace : rs') || asr (rs' ++ [Ace])
      rs        -> asr rs
\end{code}

\submodule{Categorisation of hands} %%%%%%%%%%%%%%%%%%%%%%%

\noindent A \highlighttt{HandType} is one of (in order of 
increasing value):

\begin{enumerate}
   \item \highlighttt{Garbage} -- not any of the other 
      kinds, worth only the ranks of its cards;
   \item \highlighttt{Pair} -- two cards have the same rank
      and all of the other cards have different ranks;
   \item \highlighttt{TwoPair} -- there are two pairs of
     different ranks and the other card is yet another rank.
   \item \highlighttt{Triple} -- three cards have the same
      rank and the rest other different ranks;
   \item \highlighttt{Straight} -- the cards all have 
      sequential ranks (an ace can preceed a deuce or
      follow a king) and some cards have different suits;
   \item \highlighttt{Flush} - all cards are the same suit,
      but not with sequential ranks;
   \item \highlighttt{FullHouse} -- a triple and a pair;
   \item \highlighttt{Poker} -- four of a kind;
   \item \highlighttt{StraightFlush} -- all cards have 
      sequential ranks and the same suit. 
\end{enumerate}

\begin{code}
data HandType = Garbage | Pair | TwoPair | Triple 
   | Straight | Flush | FullHouse | Poker | StraightFlush
   deriving (Eq, Ord, Show, Enum)
\end{code}

\noindent \highlighttt{handType}~$H$ classifies $H$.

\begin{code}
handType :: Hand -> HandType
handType h = case groupByRank h of
   [[_], [_], [_], [_,_]]    -> Pair
   [[_], [_,_], [_,_]]       -> TwoPair
   [[_], [_], [_,_,_]]       -> Triple
   [[_,_], [_,_,_]]          -> FullHouse
   [[_], [_,_,_,_]]          -> Poker
   [[_], [_], [_], [_], [_]] -> 
      case groupBySuit h of
         [h'] | allSuccRanks h -> StraightFlush
              | otherwise      -> Flush
         _    | allSuccRanks h -> Straight
              | otherwise      -> Garbage
\end{code}

\noindent \highlighttt{isGarbage}, \highlighttt{isPair},
\highlighttt{isTwoPair}, \highlighttt{isStraight},
\highlighttt{isTriple}, \highlighttt{isStraight},
\highlighttt{isFlush}, \highlighttt{isFullHouse},
\highlighttt{isPoker} and \highlighttt{isStraightFlush}
each return {\tt True} iff some hand is that kind of hand.

\begin{code}
isGarbage, isPair, isTwoPair, isTriple, isStraight, 
   isFlush, isFullHouse, isPoker, isStraightFlush 
   :: Hand -> Bool
isGarbage       h = handType h == Garbage
isPair          h = handType h == Pair
isTwoPair       h = handType h == TwoPair
isTriple        h = handType h == Triple
isStraight      h = handType h == Straight
isFlush         h = handType h == Flush
isFullHouse     h = handType h == FullHouse
isPoker         h = handType h == Poker
isStraightFlush h = handType h == StraightFlush
isRoyalFlush    h = handType h == StraightFlush &&
   (let rs = map rank h
    in Ace `elem` rs && King `elem` rs)
\end{code}

\submodule{Comparison of hands} %%%%%%%%%%%%%%%%%%%%%%%%%%%

The ordering of cards for the purpose of comparing hands
is based solely on rank. Aces have the highest value.

\begin{code}
instance Ord Card where
\end{code}

\begin{code}
   compare (Suit r s) (Suit r' s') = case r of
      Ace -> case r' of Ace -> EQ
                        _   -> GT
      _   -> case r' of Ace -> LT
                        _   -> compare r r'
\end{code}

\noindent
\highlighttt{compareCards}~$\mathit{cs}$~$\mathit{cs}'$
compares two list of cards starting by comparing the
highest ranked cards.

\begin{code}
compareCards :: [Card] -> [Card] -> Ordering
compareCards cs cs' = 
   compare (sortBy (flip compare) cs)
           (sortBy (flip compare) cs')   
\end{code}

\noindent
\highlighttt{compareGroups}~$\mathit{css}$~$\mathit{css}'$
compares lists of lists of cards, considering the
corresponding lists in the order they come. Precondition:
$\mathit{css}$ and 
$\mathit{css}'$ are the same length.

\begin{code}
compareGroups :: [[Card]] -> [[Card]] -> Ordering
compareGroups (cs : css) (cs':css') = 
   case compareCards cs cs' of
      EQ -> compareGroups css css'
      o  -> o
compareGroups [] []                 = EQ
\end{code}


\noindent \highlighttt{compareHands}~$H$~$H'$ compares two
hands.

\begin{code}
compareHands :: Hand -> Hand -> Ordering
compareHands h h' = 
   let t  = handType h
       t' = handType h'
   in case compare t t' of
      EQ -> case t of
         Garbage       -> compareCards h h'
         Pair          -> 
            let [[c1], [c2], [c3], [c4,_]] = groupByRank h
                [[d1], [d2], [d3], [d4,_]] = groupByRank h'
            in compareGroups [[c4], [c1,c2,c3]]
                             [[d4], [d1,d2,d3]]
         TwoPair       -> 
            let [[c1], [c2,_], [c3,_]] = groupByRank h
                [[d1], [d2,_], [d3,_]] = groupByRank h'
            in compareGroups [[c2,c3], [c1]] 
                             [[d2,d3], [d1]]
         Triple        ->  
            let [[_], [_], [c1,_,_]] = groupByRank h
                [[_], [_], [d1,_,_]] = groupByRank h'
            in compare c1 d1
         Straight      -> compareCards h h'
         Flush         -> compareCards h h'
         FullHouse     ->             
            let [[c1,_], [c2,_,_]] = groupByRank h
                [[d1,_], [d2,_,_]] = groupByRank h'
            in compare c2 d2
         Poker         -> 
            let [[_], [c1,_,_,_]] = groupByRank h
                [[_], [d1,_,_,_]] = groupByRank h'
            in compare c1 d1
         StraightFlush -> compareCards h h'
      o  -> o
\end{code}

\noindent $H$~\highlighttt{beats}~$H'$ iff $H$ is a better
poker hand than $H'$. $H$~\highlighttt{ties}~$H'$ if they
have the same value.

\begin{code}
beats, ties :: Hand -> Hand -> Bool
beats h h' = compareHands h h' == GT
ties  h h' = compareHands h h' == EQ
\end{code}

\noindent \highlighttt{better}~$H$~$H'$ returns the better
of hands $H$ and $H'$.

\begin{code}
better :: Hand -> Hand -> Hand
better h h' | h `beats` h' = h
            | otherwise    = h'
\end{code}

\submodule{Hands with more than 5 cards} %%%%%%%%%%%%%%%%%%

\noindent \highlighttt{best5}~$H$ picks the highest scoring 5
cards from $H$. Precondition $H$ contains at least 5 cards.

\begin{code}
best5 :: Hand -> Hand
best5 = foldl1 better . combinations 5
\end{code}

\submodule{Tests} %%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
-- generate a random hand h, print h and f h
test :: Show a => (Hand -> a) -> IO ()
test f = do
   deck <- shuffle deck52
   let hand = take 5 deck
   print hand
   print $ f hand
\end{code}

\begin{code}
-- generate random hands h of size n, until p (f h).
-- Print h and f h
seek :: 
   Show a => Int -> (Hand -> a) -> (a -> Bool) -> IO ()
seek n f p = do
   deck <- shuffle deck52
   let hand = take n deck
   if p (f hand) then do print hand
                         print $ f hand
                 else seek n f p
\end{code}

\begin{code}
-- generate 2 random hands and compare
cmp :: IO ()
cmp = do
   d <- shuffle deck52
   let (h,d') = splitAt 5 d
       h' = take 5 d'
   putStrLn $ show h ++ " " ++ show h'
   putStrLn $ show (handType h) ++ " " ++ 
      show (handType h')
   print $ compareHands h h'
\end{code}

