% AtomTypeDecs.#ext
% This file was produced from AtomTypeDecs.lit

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

\module{AtomTypeDecs} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlightModule{AtomTypeDecs} implements 
atom type declarations for CDL. An atom type 
declatation declares the types for each argument
of an atom with matching name and arity.

\begin{code}
module CDL.AtomTypeDecs (
      AtomTypeDec(..), atomTypeDecP
   ) where
\end{code}

\begin{code}
import ABR.Parser.Pos
import ABR.Parser
import ABR.Showing
\end{code}

\begin{code}
import CDL.Constants
import CDL.Variables
import CDL.VarGens
import CDL.Atoms
import CDL.Types
import CDL.Qualification
\end{code}

\submodule{Data types} %%%%%%%%%%%%%

\noindent An \highlighttt{AtomTypeDec} asserts the type of
every argument\index{arguments!in atoms} to an atom

\begin{code}
data AtomTypeDec = 
   AtomTypeDec {
      atdName  :: String,
      atdVGens :: [VarGen],
      atdPos   :: Pos
   }
\end{code}

\submodule{Parsers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent\syntrax{atomTypeDec}

\noindent\highlighttt{atomTypeDecP} recognises atom type declarations. 

\begin{code}
atomTypeDecP :: Parser AtomTypeDec
atomTypeDecP = 
       atomNameP
   <*> literalP "symbol" "("
    *> varGenP
   <*> many (
             literalP "symbol" ","
          *> nofail' "variable generator expected" varGenP
       )
   <*  nofail' "')' expected" (literalP "symbol" ")")
    @> (\((n,p),(vg,vgs)) -> AtomTypeDec n (vg : vgs) p)
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Showing} %%%%%

\begin{code}
instance Show AtomTypeDec where
   showsPrec _ (AtomTypeDec n vgs p) = showString n . 
      showChar '(' . showWithSep ", " vgs . showChar ')'
\end{code}

\subsubmodule{Collecting constants} %%%%%%%

\begin{code}
instance HasConstants AtomTypeDec where
   getConstants (AtomTypeDec _ vgs _) cs = 
      foldr getConstants cs vgs
\end{code}

\subsubmodule{Collecting variables} %%%%%%%

\begin{code}
instance HasVariables AtomTypeDec where
   getVariables (AtomTypeDec _ vgs _) vs = 
      foldr getVariables vs vgs
\end{code}

\subsubmodule{Collecting Orderings} %%%%%%%%

\begin{code}
instance HasTypes AtomTypeDec where
   getOrderings os (AtomTypeDec _ vgs _) = 
      foldl getOrderings os vgs
\end{code}

\subsubmodule{Qualification} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance Qualifiable AtomTypeDec where
   qualify n (AtomTypeDec n' vgs p) = 
      AtomTypeDec n' (map (qualify n) vgs) p
\end{code}
