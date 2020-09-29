% Qualification.#ext
% This file was produced from Qualification.lit

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

\module{(Deprecated) Logic.Qualification} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Logic.Qualification} implements qualification for
CDL. 

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
\end{code}

\begin{code}
module ABR.Logic.Qualification
   {-# DEPRECATED "Ill-conceived, I think." #-}
   (
      Qualifiable(..)
   ) 
   where
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2013-11-24: Deprecated.
   

\submodule{Qualifiable class} %%%%%%%%%%%%%%%%%%%%%%%%

\noindent Class \highlighttt{Qualifiable} overloads methods for
qualifying names.

\begin{code}
class Qualifiable a where
\end{code}

\noindent \highlighttt{qualify}~$n~x$ prepends $n${\tt .} to
the name of $x$.

\begin{code}
   qualify :: String -> a -> a
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Qualification} %%%%%%%

\begin{code}
instance Qualifiable String where
   qualify n cs |'.' `elem` cs = cs
                | otherwise    = n ++ '.' : cs
\end{code}
