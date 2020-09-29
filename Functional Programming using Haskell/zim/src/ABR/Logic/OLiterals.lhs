% OLiterals.#ext
% This file was produced from OLiterals.lit

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

\module{OLiterals} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlightModule{OLiterals} implements optimised
literals for CDL.

\begin{code}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
\end{code}

\begin{code}
module CDL.OLiterals (
      OLiteral, toOLiteral, toLiteral
   ) where
\end{code}

\begin{code}
import CDL.Atoms
import CDL.OAtoms
import CDL.Literals
\end{code}

\submodule{Data types} %%%%%%%%%%%%%

An \highlighttt{OLiteral} is a positive or negative integer
(never 0) that uniquely identifies a correspoding {\tt
Literal}.

\begin{code}
type OLiteral = Int
\end{code}

\submodule{Looking up literals} %%%%%%%%%%%%%

\highlighttt{toOLiteral}~$\mathit{am}~l$ returns {\tt
Literal} $l$'s corresponding {\tt OLiteral}.

\begin{code}
toOLiteral :: AtomMap -> Literal -> OLiteral
toOLiteral am l = case l of
   Pos a _ -> toOAtom am a
   Neg a _ -> negate $ toOAtom am a
\end{code}

\noindent
\highlighttt{toLiteral}~$\mathit{oam}~\mathit{ol}$ returns
{\tt OLiteral} $\mathit{ol}$'s corresponding {\tt Literal}.

\begin{code}
toLiteral :: OAtomMap -> OLiteral -> Literal
toLiteral oam ol
   | ol < 0    = Neg (toAtom oam $ abs ol) (-1,-1)
   | otherwise = Pos (toAtom oam ol) (-1,-1)
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%

\subsubmodule{Negation}

\begin{code}
instance Negatable OLiteral where
   neg = negate
   pos = abs
\end{code}

\begin{code}
instance Negatable [OLiteral] where
   neg = map neg
   pos = map pos
\end{code}
