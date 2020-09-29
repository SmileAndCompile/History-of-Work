% PlayingCards.#ext
% This file was produced from PlayingCards.lit

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

\module{Playing Cards} %%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.PlayingCards} provides basic data
types for card playing games and problems.

\begin{code}
module ABR.PlayingCards (
      Suit(..), Rank(..), suits, ranks, Card(..), Deck, 
      deck52, deck54, Hand, shuffle
   ) where
\end{code}

\begin{code}
import Data.List
import System.Random
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2017-01-17: Made all the {\tt -Wall}s go away
   

\submodule{Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Most cards are members of one of these \highlighttt{Suit}s:
\highlighttt{Clubs} ($\clubsuit$); \highlighttt{Diamonds}
({\color{red}$\diamondsuit$}); \highlighttt{Hearts}
({\color{red}$\heartsuit$}); \highlighttt{Spades}
($\spadesuit$).

\begin{code}
data Suit = Clubs |  Diamonds | Hearts | Spades
   deriving (Eq, Ord, Enum, Bounded)
\end{code}

\noindent Most cards have of one of these
\highlighttt{Rank}s: \highlighttt{Ace}; \highlighttt{R2};
\highlighttt{R3}; \highlighttt{R4}; \highlighttt{R5};
\highlighttt{R6}; \highlighttt{R7}; \highlighttt{R8};
\highlighttt{R9}; \highlighttt{R10}; \highlighttt{Jack};
\highlighttt{Queen}; \highlighttt{King}. 

\begin{code}
data Rank = Ace | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | 
            R10 | Jack | Queen | King
   deriving (Eq, Ord, Enum, Bounded)
\end{code}

\noindent \highlighttt{ranks} is the set of all ranks.
\highlighttt{suits} is the set of all suits.

\begin{code}
ranks :: [Rank]
ranks = [minBound..maxBound]
suits :: [Suit]
suits = [minBound..maxBound]
\end{code}

\noindent A \highlighttt{Card} is either a card belonging
to one of the \highlighttt{Suit}s (with a
\highlighttt{rank} and a \highlighttt{suit}), or is a
\highlighttt{Joker}.

\begin{code}
data Card =   Suit {rank :: Rank,
                    suit :: Suit}
            | Joker
   deriving (Eq)
\end{code}

\noindent A \highlighttt{Deck} of cards, or a 
\highlighttt{Hand} of cards:

\begin{code}
type Deck = [Card]
type Hand = [Card]
\end{code}

\submodule{Creating decks} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{deck52} returns all the cards in a standard
52-card deck. \highlighttt{deck54}  returns all the cards
in a standard 52-card deck plus 2 jokers.
      
\begin{code}
deck52, deck54 :: Deck
deck52 = [Suit r s | s <- suits, r <- ranks]
deck54 = deck52 ++ [Joker, Joker]
\end{code}

\noindent \highlighttt{shuffle}~$\mathit{deck}$ returns all
the cards in $\mathit{deck}$ in a new random order.
     
\begin{code}
shuffle :: Deck -> IO Deck
shuffle cs = s (length cs) cs []
   where
   s n cs' ds | n == 1 = return $ cs' ++ ds
             | otherwise = do
      i <- getStdRandom (randomR (0, n-1))
      let c = cs' !! i
      s (n - 1) (delete c cs') (c : ds)
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance Show Suit where
\end{code}

\begin{code}
   showsPrec _ s = case s of
      Clubs    -> showChar 'C'
      Diamonds -> showChar 'D'
      Hearts   -> showChar 'H'
      Spades   -> showChar 'S'
\end{code}

\begin{code}
instance Show Rank where
\end{code}

\begin{code}
   showsPrec _ s = case s of
      Ace   -> showChar 'A'
      R2    -> showChar '2'
      R3    -> showChar '3'
      R4    -> showChar '4'
      R5    -> showChar '5'
      R6    -> showChar '6'
      R7    -> showChar '7'
      R8    -> showChar '8'
      R9    -> showChar '9'
      R10   -> showString "10"
      Jack  -> showChar 'J'
      Queen -> showChar 'Q'
      King  -> showChar 'K'
\end{code}

\begin{code}
instance Show Card where
\end{code}

\begin{code}
   showsPrec _ c = case c of
      Suit r s -> shows r . shows s
      Joker    -> showString "Jkr"
\end{code}

\submodule{Tests} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{haddock}
testShuffle54 :: IO ()
testShuffle54 = do
   cs <- shuffle deck54
   print $ length cs
   print cs
\end{haddock}


