% Defaults.#ext
% This file was produced from Defaults.lit

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

\module{Defaults} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlightModule{Defaults} implements
default fact declarations. A default fact strictly
asserts a literal, but only if the negation of that
literal has not been strictly asserted. A default
fact would typically contain variables so that
it represents a shorthand for a default situation,
which has a few exceptions.

\begin{code}
module CDL.Defaults (
      Default(..), defaultP
   ) where
\end{code}

\begin{code}
import ABR.Parser.Pos
import ABR.Parser
\end{code}

\begin{code}
import CDL.Literals
import CDL.Qualification
import CDL.Constants
import CDL.Variables
\end{code}

\submodule{Data types} %%%%%%%%%%%%%

\noindent A \highlighttt{Default} fact is not really a type declaration,
but its purpose is purely to control instantiation, so
this is as good a place to define it as anywhere. A default
fact declaration is a literal to be instantiated to
obviate rules.

\begin{code}
data Default = 
   Default {
      dLit :: Literal,
      dPos :: Pos
   }
\end{code}

\submodule{Parsers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent Default facts are parsed by 
\highlighttt{defaultP}.

\syntrax{default}

\begin{code}
defaultP :: Parser Default
defaultP = 
       literalP "symbol" "?"
   <*> pLiteralP
   @> (\((_,_,p),l) -> Default l p)
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Showing} %%%%%

\begin{code}
instance Show Default where
   showsPrec _ (Default l p) = showString "? " . 
      shows l
\end{code}

\subsubmodule{Collecting constants} %%%%%%%

\begin{code}
instance HasConstants Default where
   getConstants (Default l _) = getConstants l
\end{code}

\subsubmodule{Collecting variables} %%%%%%%

\begin{code}
instance HasVariables Default where
   getVariables (Default l _) = getVariables l
\end{code}

\subsubmodule{Grounding} %%%%%%%

\begin{code}
instance Groundable Default where
   ground1 v c (Default l p) = Default (ground1 v c l) p
   rename v v' (Default l p) = Default (rename v v' l) p
\end{code}

\subsubmodule{Qualification} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance Qualifiable Default where
   qualify n (Default l p) = Default (qualify n l) p
\end{code}
