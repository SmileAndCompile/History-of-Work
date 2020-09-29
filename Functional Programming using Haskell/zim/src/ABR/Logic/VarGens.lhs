% VarGens.#ext
% This file was produced from VarGens.lit

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

\module{VarGens} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlightModule{VarGens} implements variable 
generators for CDL. Variable generators bind a 
variable name to a type form which constants will
be drawn to instantiate the variable.

\begin{code}
module CDL.VarGens (
      VarGen(..), varGenP
   ) where
\end{code}

\begin{code}
import ABR.Parser.Pos
import ABR.Parser
\end{code}

\begin{code}
import CDL.Constants
import CDL.Variables
import CDL.Types
import CDL.Qualification
\end{code}

\submodule{Data types} %%%%%%%%%%%%%

A \highlighttt{VarGen} binds a Variable to
its type with \highlighttt{:<-}.

\begin{code}
data VarGen = 
   VarGen {
      vgVar  :: Variable,
      vgType :: Type,
      vgPos  :: Pos
   }
\end{code}

\submodule{Parsers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \syntrax{varGen}

\noindent\highlighttt{varGenP} recognises variable generators.

\begin{code}
varGenP :: Parser VarGen
varGenP =
       variableP 
   <*> literalP "symbol" "<-"
    *> typeP
    @> (\(v,t) -> VarGen v t (getPos v))
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Showing} %%%%%

\begin{code}
instance Show VarGen where
   showsPrec _ (VarGen v t _) = shows v .
      showString " <- " . shows t
\end{code}

\subsubmodule{Collecting constants} %%%%%%%

\begin{code}
instance HasConstants VarGen where
   getConstants (VarGen _ t _) = getConstants t
\end{code}

\subsubmodule{Collecting variables} %%%%%%%

\begin{code}
instance HasVariables VarGen where
   getVariables (VarGen v t _) vs = getVariables v $
      getVariables t vs
\end{code}

\subsubmodule{Grounding} %%%%%%%

\begin{code}
instance Groundable VarGen where
   ground1 v c (VarGen v' t p) =
      VarGen v' (ground1 v c t) p
   rename v v' (VarGen v'' t p)
      | v'' == v  = VarGen v' (rename v v' t) p
      | otherwise = VarGen v'' (rename v v' t) p
\end{code}

\subsubmodule{Collecting Orderings} %%%%%%%%

\begin{code}
instance HasTypes VarGen where
   getOrderings os (VarGen _ t _) = getOrderings os t
\end{code}

\subsubmodule{Qualification} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance Qualifiable VarGen where
   qualify n (VarGen v t p) = VarGen v (qualify n t) p
\end{code}
