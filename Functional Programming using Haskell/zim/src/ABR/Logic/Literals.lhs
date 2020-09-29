% Literals.#ext
% This file was produced from Literals.lit

% ABRHLibs -- a personal library of Haskell modules
% Copyright (C) 2007, ... 2102  Andrew Rock
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

\module{Logic.Literals} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Logic.Literals} implements literals.

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
\end{code}

\begin{code}
module ABR.Logic.Literals (
      Literal(..), pLiteralP, Negatable(..),
      Complementable(..)
   ) where
\end{code}

\begin{code}
import Control.DeepSeq
\end{code}

\begin{code}
import ABR.Util.Pos
import ABR.Parser
import ABR.Logic.Constants
import ABR.Logic.Variables
import ABR.Logic.Atoms
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2013-11-24: Removed qualification.
   

\submodule{Data type} %%%%%%%%%%%%%

A \highlighttt{Literal} is any atom $a$
(\highlighttt{Pos}) or its negation $\neg a$
(\highlighttt{Neg}).

\begin{code}
data Literal = 
     Pos {
        lAtom :: Atom,
        lPos  :: Pos
     }
   | Neg {
        lAtom :: Atom,
        lPos  :: Pos
     }
\end{code}

\submodule{Parser} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \LogicEBNF{literal}

\noindent \highlighttt{pLiteralP} recognises literals.

\begin{code}
pLiteralP :: Parser Literal
pLiteralP = 
       atomP 
       @> (\a -> Pos a (getPos a))
   <|>     literalP "symbol" "~" 
       <&> pLiteralP 
       @> (\((_,_,p),l) -> neg (l {lPos = p}))
   <|>     literalP "symbol" "(" 
       <&> pLiteralP 
       <& literalP "symbol" ")"
       @> (\((_,_,p),l) -> l {lPos = p})
\end{code}

\submodule{Negation} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Class \highlighttt{Negatable} includes types that may
be logically negated with $\neg$.

\begin{code}
class Negatable a where
\end{code}

\noindent \highlighttt{neg}~$x$ negates $x$.
For example if $a$ is an atom, {\tt neg}~$a = \neg a$, and {\tt neg}~$\neg a = a$.

\begin{code}
   neg :: a -> a
\end{code}

\noindent \highlighttt{pos}~$x$ returns the ``positive'' $x$.
For example if $a$ is an atom, {\tt pos}~$a =$ {\tt pos}~$\comp a = a$.

\begin{code}
   pos :: a -> a
\end{code}

\noindent Class \highlighttt{Complementable} includes types that may
be logically complemented with $\comp$.

\begin{code}
class Complementable a where
\end{code}

\noindent \highlighttt{comp}~$x$ complements $x$.
For example if $a$ is an atom, {\tt comp}~$a = \comp a = \neg a$.

\begin{code}
   comp :: a -> a
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Comparing} %%%%%

\begin{code}
instance Eq Literal where
   (Pos a _) == (Pos a' _) = a == a'
   (Neg a _) == (Neg a' _) = a == a'
   _         == _          = False
\end{code}

\begin{code}
instance Ord Literal where
   compare l l' = case l of
      Pos a _ -> case l' of
         Pos a' _ -> compare a a'
         Neg _ _  -> GT
      Neg a _ -> case l' of
         Pos _ _  -> LT
         Neg a' _ -> compare a' a 
\end{code}

\subsubmodule{Positions} %%%%%%%

\begin{code}
instance HasPos Literal where
   getPos = lPos
\end{code}

\subsubmodule{Showing} %%%%%

\begin{code}
instance Show Literal where
   showsPrec p l = case l of
      Pos a _ -> shows a
      Neg a _ -> showChar '~' . shows a
\end{code}

\subsubmodule{Negation} %%%%%

\begin{code}
instance Negatable Literal where
   neg l = case l of
      Pos a p -> Neg a p
      Neg a p -> Pos a p
   pos l = case l of
      Pos a p -> Pos a p
      Neg a p -> Pos a p
\end{code}

\begin{code}
instance Complementable Literal where
   comp = neg
\end{code}

\subsubmodule{Collecting Atoms} %%%%%

\begin{code}
instance HasAtoms Literal where
   getAtoms l = case l of
      Pos a _ -> getAtoms a
      Neg a _ -> getAtoms a
\end{code}

\subsubmodule{Collecting constants} %%%%%%%

\begin{code}
instance HasConstants Literal where
   getConstants l cs = case l of
      Pos a _ -> getConstants a cs
      Neg a _ -> getConstants a cs
\end{code}

\subsubmodule{Collecting variables} %%%%%%%

\begin{code}
instance HasVariables Literal where
   getVariables l vs = case l of
      Pos a _ -> getVariables a vs
      Neg a _ -> getVariables a vs
\end{code}

\subsubmodule{Grounding} %%%%%%%

\begin{code}
instance Groundable Literal where
   ground1 v c l = case l of
      Pos a p -> Pos (ground1 v c a) p
      Neg a p -> Neg (ground1 v c a) p
   rename v v' l = case l of
      Pos a p -> Pos (rename v v' a) p
      Neg a p -> Neg (rename v v' a) p
\end{code}

\subsubmodule{DeepSeq} %%%%%%%

\begin{code}
instance NFData Literal where
   rnf l = case l of
      Pos a p -> a `deepseq` p `deepseq` ()
      Neg a p -> a `deepseq` p `deepseq` ()
\end{code}
