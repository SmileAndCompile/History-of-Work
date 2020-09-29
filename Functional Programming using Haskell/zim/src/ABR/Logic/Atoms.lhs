% Atoms.#ext
% This file was produced from Atoms.lit

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

\module{Logic.Atoms} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Logic.Atoms} implements atoms\index{atom}.

\begin{code}
module ABR.Logic.Atoms (
      Atom(..), atomNameP, atomP, HasAtoms(..)
   ) where
\end{code}

\begin{code}
import qualified Data.Set as S
import Control.DeepSeq
\end{code}

\begin{code}
import ABR.Util.Pos
import ABR.Parser
import ABR.Text.Showing
import ABR.Logic.Kinds
import ABR.Logic.Constants
import ABR.Logic.Variables
import ABR.Logic.Arguments
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2013-11-24: Removed qualification.
   

\submodule{Data type} %%%%%%%%%%%%%

An \highlighttt{Atom} is a proposition symbol,
\highlighttt{Prop}.
An atom may have a list of arguments\index{arguments!in atoms}.

\begin{code}
data Atom = 
   Prop {
      aName :: String,
      aArgs :: [Argument],
      aPos  :: Pos
   }
\end{code}

\submodule{Parsers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \LogicEBNF{atomName}

\noindent \highlighttt{atomNameP} recognises atom names.

\begin{code}
atomNameP :: Parser (String, Pos)
atomNameP =
   (     tagP "lName" 
     <|> tagP "uName"
   )
   @> (\(_,n,p) -> (n, p))
\end{code}

\noindent\index{arguments!in atoms}\LogicEBNF{argList}.

\noindent \highlighttt{argListP} recognises argument lists.

\begin{code}
argListP :: Parser [Argument]
argListP = 
   literalP "symbol" "(" 
    &> argumentP
   <&> many (literalP "symbol" "," &> argumentP)
   <&  nofail (literalP "symbol" ")")
   @> cons
\end{code}

\noindent .

\noindent\LogicEBNF{specialAtom}\index{arguments!in atoms}
	   
\noindent\LogicEBNF{atom}\index{arguments!in atoms}
	   
\noindent \highlighttt{atomP} recognises atoms.

\begin{code}
atomP :: Parser Atom
atomP = 
           argumentP 
       <&> (     literalP "symbol" "<"
             <|> literalP "symbol" "<="
             <|> literalP "symbol" "=="
           )
       <&> nofail argumentP
       @> (\(a1,((_,op,_),a2)) -> 
              Prop op [a1,a2] (getPos a1))
   <|>     atomNameP
       <&> optional argListP
       @> (\((n,p),ass) -> case ass of
             []   -> Prop n [] p
             [as] -> Prop n as p
          )
   <|> tagP "string"
       @> (\(_,s,p) -> Prop s [] p)
\end{code}

\submodule{Collecting atoms} %%%%%%%%%%%%%%%%%%%%%

It is required for various purposes to identify all
of the distinct atoms that occur in an object.
Atoms can be collected from instances of class
\highlighttt{HasAtoms}.

\begin{code}
class HasAtoms a where
\end{code}

\noindent \highlighttt{getAtoms}~$x~A$ adds any
atoms in $x$ to $A$.

\begin{code}
   getAtoms :: a -> S.Set Atom -> S.Set Atom
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Comparing} %%%%%%%

\begin{code}
instance Eq Atom where
   (Prop n as _) == (Prop n' as' _) = n == n' && as == as'
\end{code}

\begin{code}
instance Ord Atom where
    compare (Prop n as _) (Prop n' as' _) = 
       case compare n n' of
          LT -> LT
          EQ -> compare as as'
          GT -> GT
\end{code}

\subsubmodule{Positions} %%%%%%%

\begin{code}
instance HasPos Atom where
   getPos = aPos
\end{code}

\subsubmodule{Showing} %%%%%%%

\begin{code}
instance Show Atom where
   showsPrec _ (Prop "<"  [a1,a2] _) = 
      shows a1 . showString " < " . shows a2
   showsPrec _ (Prop "<=" [a1,a2] _) = 
      shows a1 . showString " <= " . shows a2
   showsPrec _ (Prop "==" [a1,a2] _) = 
      shows a1 . showString " == " . shows a2
   showsPrec _ (Prop n as _) = case as of
      [] -> showString n
      as -> showString n . showChar '(' . 
            showWithSep "," as . showChar ')' 
\end{code}

\subsubmodule{Collecting Atoms} %%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance HasAtoms Atom where
   getAtoms = S.insert
\end{code}

\subsubmodule{Collecting constants} %%%%%%%

\begin{code}
instance HasConstants Atom where
   getConstants (Prop _ as _) cs = foldr getConstants cs as
\end{code}

\subsubmodule{Collecting variables} %%%%%%%

\begin{code}
instance HasVariables Atom where
   getVariables (Prop _ as _) vs = foldr getVariables vs as
\end{code}

\subsubmodule{Grounding} %%%%%%%

\begin{code}
instance Groundable Atom where
   ground1 v c (Prop n as p) = 
      Prop n (map (ground1 v c) as) p
   rename v v' (Prop n as p) = 
      Prop n (map (rename v v') as) p
\end{code}

\subsubmodule{DeepSeq} %%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance NFData Atom where
   rnf (Prop n as p) = 
      n `deepseq` as `deepseq` p `deepseq` () 
\end{code}
