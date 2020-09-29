% Supply.#ext
% This file was produced from Supply.lit

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

\module{Data.Supply} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Data.Supply} implements a name supply using
a mutable variable in the IO monad.

\begin{code}
module ABR.Data.Supply (
      Supply, newSupply, supplyNext, peekNext
   ) where
\end{code}

\begin{code}
import Data.IORef
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2015-02-02. Passed {\tt hlint}.\\
Reviewed 2014-06-03: Made all the {\tt -Wall}s go away.\\
Reviewed 2013-11-22.
Reviewed 2012-11-24: Moved into {\tt ABR.Data}.
   

\submodule{Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A \highlighttt{Supply} is a value of any enumerated
type.

\begin{code}
type Supply a =
   IORef a
\end{code}

\submodule{Creating a supply} %%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{newSupply}~$\mathit{first}$ creates a
new {\tt Supply} that will commence with $\mathit{first}$.

\begin{code}
newSupply :: a -> IO (Supply a)
newSupply = newIORef
\end{code}

\submodule{Extracting values from a supply} %%%%%%%%%%%%%%

\noindent \highlighttt{supplyNext}~$\mathit{supply}$ returns
the next value from the $\mathit{supply}$.

\begin{code}
supplyNext :: Enum a => Supply a -> IO a
supplyNext r = do
   i <- readIORef r
   let i' = succ i
   writeIORef r i'
   return i
\end{code}

\noindent \highlighttt{peekNext}~$\mathit{supply}$ returns
the next value from the $\mathit{supply}$, but does not
change the supply, so that the next value extracted
will be the same.

\begin{code}
peekNext :: Supply a -> IO a
peekNext = readIORef
\end{code}

