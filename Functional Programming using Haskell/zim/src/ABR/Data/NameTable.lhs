% NameTable.#ext
% This file was produced from NameTable.lit

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

\module{Data.NameTable} %%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Data.NameTable} implements structures for the
efficient accumulation of names, assigning unique, sequential
integers to each name and mapping between them.

\begin{code}
module ABR.Data.NameTable (
      NameTable, newNameTable, insertName, lookupName,
      NameArray, makeNameArray
   ) where
\end{code}

\begin{code}
import Data.Array.IArray
import Data.Array.IO
import qualified Data.Map as M
\end{code}

\begin{code}
import ABR.Data.Supply
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2015-02-02. Passed {\tt hlint}.\\
Reviewed 2014-06-03: Made all the {\tt -Wall}s go away.\\
Reviewed 2013-11-21: Switch from hashtables to maps. Less need for IO functions.\\
Reviewed 2012-11-24: Moved into {\tt ABR.Data}.
   

\submodule{Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%

A \highlighttt{NameTable} is a pair of:

\begin{itemize}
   \item a map from strings to integers; and
   \item a supply of sequential integers starting from 0.
\end{itemize}

\begin{code}
type NameTable = 
   (M.Map String Int, Supply Int)
\end{code}

\noindent For the reverse mapping an array of names, a \highlighttt{NameArray}, is optimal.

\begin{code}
type NameArray = Array Int String
\end{code}

\submodule{Creating a name table} %%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{newNameTable}~$\mathit{size}$ creates a {\tt NameTable}.

\begin{code}
newNameTable :: IO NameTable
newNameTable = do
   s <- newSupply 0
   return (M.empty, s)
\end{code}

\noindent \highlighttt{insertName}~$t~n$ inserts name $n$ into the
name table $t$. If the name already exists, 
nothing happens. If the name is new, it is added
to the table and assigned the next sequence number.

\begin{code}
insertName :: NameTable -> String -> IO NameTable
insertName (m,s) n = case M.lookup n m of
   Just _  -> return (m, s)
   Nothing -> do
      i <- supplyNext s
      return (M.insert n i m, s)
\end{code}

\submodule{Looking up by name} %%%%%%%%%%%%%%%%%

\highlighttt{lookupName}~$t~n$ retrieves the sequence number
for the given name $n$ in name table $t$,
provided it exists.

\begin{code}
lookupName :: NameTable -> String -> Maybe Int
lookupName (m,_) n = M.lookup n m
\end{code}

\submodule{Creating a name array} %%%%%%%%%%%%%%%

\highlighttt{makeNameArray}~$t$ builds an array for mapping sequence
numbers back to names.

\begin{code}
makeNameArray :: NameTable -> IO NameArray
makeNameArray (m,s) = do
   size <- peekNext s
   a <- newArray (0, size - 1) "" :: IO (IOArray Int String)
   let add :: (String, Int) -> IO ()
       add (n,i) = writeArray a i n
   mapM_ add $ M.toList m
   freeze a
\end{code}

