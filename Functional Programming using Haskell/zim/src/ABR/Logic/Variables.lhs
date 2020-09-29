% Variables.#ext
% This file was produced from Variables.lit

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

\module{Logic.Variables} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Logic.Variables} implements variables\index{variable}.

\begin{code}
module ABR.Logic.Variables (
      Variable(..), variableP, HasVariables(..), 
      Substitution(..), Groundable(..)
   ) where
\end{code}

\begin{code}
import qualified Data.Set as S
import Control.DeepSeq
\end{code}

\begin{code}
import ABR.Util.Pos
import ABR.Parser
import ABR.Logic.Kinds
import ABR.Logic.Constants
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2013-11-24: Removed qualification.
   

\submodule{Data type} %%%%%%%%%%%%%

An \highlighttt{Variable} is a token which may appear as 
an argument\index{arguments!in atoms} to an atom, to be
instatiated with constants.

\begin{code}
data Variable = 
   Variable {
      vName :: String,
      vPos  :: Pos
   }
\end{code}

\submodule{Parsers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent\LogicEBNF{variable}
	   
\noindent \highlighttt{variableP} recognises variables.

\begin{code}
variableP :: Parser Variable
variableP =
      tagP "lName" 
   @> (\(_,n,p) -> Variable n p)
\end{code}

\submodule{Collecting variables} %%%%%%%%%%%%%%%%%%%%%

It is required for various purposes to identify all
of the distinct variables that occur in an object.
Variables can be collected from instances of class
\highlighttt{HasVariables}.

\begin{code}
class HasVariables a where
\end{code}

\noindent \highlighttt{getVariables}~$x~V$ adds any
variables in $x$ to $V$.

\begin{code}
   getVariables :: a -> S.Set Variable -> S.Set Variable
\end{code}

\noindent \highlighttt{hasVariables}~$x$ returns {\tt True}
iff $x$ contains variables.

\begin{code}
   hasVariables :: a -> Bool
   hasVariables a = not $ S.null $ getVariables a S.empty
\end{code}

\submodule{Grounding} %%%%%%%%%%%%%%%%%%%%%

To \emph{ground} is to substitute a variable with a
constant.

A \highlighttt{Substitution} $v$~\highlighttt{:->-}~$c$
replaces a variable $v$ with a constant $c$. Substitutions
may be composed. $s_{1}$~\highlighttt{:>->}~$s_{2}$ first
performs $s_{1}$ and then $s_{2}$. \highlighttt{NullSub}
is the null substitution that does nothing.

\begin{code}
data Substitution =   NullSub
                    | Variable :->- Constant
                    | Substitution :>-> Substitution
                    deriving (Eq, Ord, Show)
\end{code}

\noindent Anything groundable should be an instance
of class \highlighttt{Groundable}.

\begin{code}
class Groundable a where
\end{code}

\noindent \highlighttt{ground1}~$v~c~x$ returns $x$
with all occurrences of variable $v$ replaced by
constant $c$.

\begin{code}
   ground1 :: Variable -> Constant -> a -> a
\end{code}

\noindent \highlighttt{ground}~$s~x$ applies substitution
$s$ to $x$.

\begin{code}
   ground :: Substitution -> a -> a
   ground s x = case s of
      NullSub    -> x
      v :->- c   -> ground1 v c x
      s1 :>-> s2 -> ground s2 $ ground s1 x 
\end{code}

\noindent \highlighttt{rename}~$v~v'~x$ returns $x$
with all occurrences of variable $v$ replaced by
another variable $v'$.

\begin{code}
   rename :: Variable -> Variable -> a -> a
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Comparing} %%%%%%%

\begin{code}
instance Eq Variable where
   (Variable n _) == (Variable n' _) = n == n'
\end{code}

\begin{code}
instance Ord Variable where
   compare (Variable n _) (Variable n' _) = compare n n'
\end{code}

\subsubmodule{Positions} %%%%%%%

\begin{code}
instance HasPos Variable where
   getPos = vPos
\end{code}

\subsubmodule{Showing} %%%%%%%

\begin{code}
instance Show Variable where
   showsPrec _ (Variable n _) = showString n 
\end{code}

\subsubmodule{Collecting variables} %%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance HasVariables Variable where
   getVariables = S.insert
\end{code}

\subsubmodule{Grounding} %%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance Groundable a => Groundable [a] where
   ground1 v c = map (ground1 v c)
   rename v v' = map (rename v v')
\end{code}

\subsubmodule{Kind inference} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance HasKind Variable where
   kindCheck c = case c of
      Variable _ _    -> return KUnknown
\end{code}

\subsubmodule{DeepSeq} %%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance NFData Variable where
   rnf (Variable v p) = v `deepseq` p `deepseq` ()
\end{code}
