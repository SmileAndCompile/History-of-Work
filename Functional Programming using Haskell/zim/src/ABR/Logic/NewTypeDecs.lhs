% NewTypeDecs.#ext
% This file was produced from NewTypeDecs.lit

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

\module{NewTypeDecs} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlightModule{NewTypeDecs} implements type
declarations for CDL. A type declaration binds a new
name to a type.

\begin{code}
module CDL.NewTypeDecs (
      NewTypeDec(..), newTypeDecP
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

A \highlighttt{NewTypeDec} binds a type name to a type
with \highlighttt{:=}.

\begin{code}
data NewTypeDec = 
   NewTypeDec {
      ntName :: TypeName,
      ntType :: Type,
      ntPos  :: Pos
   }
   deriving (Eq, Ord)
\end{code}

\submodule{Parsers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent\syntrax{newTypeDec}

\noindent\highlighttt{newTypeDecP} recognises new type declarations.

\begin{code}
newTypeDecP :: Parser NewTypeDec
newTypeDecP = 
       typeNameP
   <*> literalP "symbol" "="
    *> typeP
    @> (\(n,t) -> NewTypeDec n t (getPos n))
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Showing} %%%%%

\begin{code}
instance Show NewTypeDec where
   showsPrec _ (NewTypeDec n t _) = shows n . 
      showString " = " . shows t
\end{code}

\subsubmodule{Collecting constants} %%%%%%%

\begin{code}
instance HasConstants NewTypeDec where
   getConstants d = getConstants (ntType d)
\end{code}

\subsubmodule{Collecting variables} %%%%%%%

\begin{code}
instance HasVariables NewTypeDec where
   getVariables d = getVariables (ntType d)
\end{code}

\subsubmodule{Collecting Orderings} %%%%%%%%

\begin{code}
instance HasTypes NewTypeDec where
   getOrderings os d = getOrderings os (ntType d)
\end{code}

\subsubmodule{Qualification} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance Qualifiable NewTypeDec where
   qualify n d = d {
         ntName = qualify n $ ntName d,
         ntType = qualify n $ ntType d
      }
\end{code}
