% HashTables.#ext
% This file was produced from HashTables.lit

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

\module{(Deprecated) Data.HashTables} %%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Data.HashTables} implements hash tables in as
efficient a manner as I can, while retaining as much
polymorphism as possible. The efficiency is made 
possible by exploiting the mutable arrays built into
the IO monad. 

\begin{code}
module ABR.Data.HashTables 
   {-# DEPRECATED "Use Data.HashTable instead." #-}
   (
      HashTable, newHT, updateHT, lookupHT, dumpHT
   ) where
\end{code}

\begin{code}
import Data.Array.IO
import qualified Data.Map as M
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2015-02-09. Passed {\tt hlint}.\\
Reviewed 2012-11-07: Deprecated.
Removed the dependence on {\tt ABR.Data.BSTree} anyway.
   
Reviewed 2009-10-27: Changed to {\tt ABR.\emph{Data}.HashTables}.
   

\submodule{Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%

A \highlighttt{HashTable} is a mapping from {\tt key}s to
associated {\tt value}s. Access is speeded by distributing
the values across an array that can be accessed in constant
time using a hashing function to map the keys to {\tt index} 
values.

\begin{code}
type HashTable key index value =
   IOArray index (M.Map key value)
\end{code}

A hash table is an array of binary search trees.
Each tree stores all of the entries with the same
hash value. The array type used is the mutable
array type that may be threaded through the {\tt IO}
monad. Hence all the following functions are 
{\tt IO} functions.


\submodule{Creating a new hash table} %%%%%%%%%%%%%

\highlighttt{newHT}~$(\mathit{lo}, \mathit{hi})$ returns a new empty hash table,
where $(\mathit{lo}, \mathit{hi})$ is the bounds on the array and
therefore the range of the hashing function.

\begin{code}
newHT :: (Ix ix, Ord key) =>
   (ix,ix) -> IO (HashTable key ix value)
newHT loHi = newArray loHi M.empty
\end{code}


\submodule{Updating an existing hash table} %%%%%%%%%%%%%

\highlighttt{updateHT}~$\mathit{hashFun}~\mathit{updateFun}~\mathit{ht}~k~v$ updates the 
hash table $\mathit{ht}$ with the key $k$ and
associated value $v$. The function $\mathit{hashFun}$ 
maps keys to hashing values. The function $\mathit{updateFun}$
is used to combine the new value $v$ with any
existing value already associated with this $\mathit{key}$.
Use \verb"(\x _ -> x)" to merely replace the old value.

\begin{code}
updateHT :: (Ix ix, Ord key) =>
   (key -> ix) -> (value -> value -> value) ->
   HashTable key ix value -> key -> value -> IO ()
updateHT hashFun updateFun ht k v = do
   let i = hashFun k
   t <- readArray ht i
   writeArray ht i $ M.insertWith updateFun k v t
\end{code}

\submodule{Looking up in a hash table} %%%%%%%%%%%%%

\highlighttt{lookupHT}~$\mathit{hashFun}~k~\mathit{ht}$ returns {\tt Just}~$v$,
where $v$ is the value associated with
$k$ in the hash table $\mathit{ht}$. If $k$
is not in the hash table, {\tt Nothing} is returned.
The function $\mathit{hashFun}$ maps keys to hashing values.

\begin{code}
lookupHT :: (Ix ix, Ord key) =>
   (key -> ix) -> key -> HashTable key ix value ->
   IO (Maybe value)
lookupHT hashFun k ht = do
   t <- readArray ht $ hashFun k
   return $ M.lookup k t
\end{code}


\submodule{Dumping a hash table} %%%%%%%%%%%%%%%%%%%%%%

\highlighttt{dumpHT}~$\mathit{ht}$ prints the hash table $\mathit{ht}$ in a fairly
crude format, adequate for assessment of the hashing
function.

\begin{code}
dumpHT :: 
   (Ix ix, Enum ix, Ord key, Show key, Show value) =>
   HashTable key ix value -> IO ()
dumpHT ht = do
   let put ix = do
          t <- readArray ht ix
          print $ M.toList t
   (lo,hi) <- getBounds ht
   mapM_ put [lo..hi]
\end{code}


\submodule{Test harness} %%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt test}~$\mathit{path}$ reads a file named $\mathit{path}$ and counts
the frequency of all ``words''. Note the example hashing
function {\tt hf}.

\begin{haddock}
test :: FilePath -> IO ()
test path = do
   let size = 1009
   inp <- readFile path
   ht <- newHT (0, size - 1)
   let hf = (`mod` size) . sum . map fromEnum
       count1 w = updateHT hf (+) ht w 1
   mapM_ count1 $ words inp
   dumpHT ht
\end{haddock}

\noindent This version just uses plain {\tt BSTrees} for comparison.

\begin{haddock}
test' :: FilePath -> IO ()
test' path = do
   inp <- readFile path
   print $ M.toList 
      $ foldr (\w -> M.insertWith (+) w 1) M.empty 
      $ words inp
\end{haddock}

\noindent Hash tables using modulo type hashing functions as in the
example above work best if the size is prime. Use this to
pick suitable primes.

\begin{haddock}
primes :: [Int]
primes =
      sieve [2..]
   where
      sieve (x:xs) =
         x : sieve [x' | x' <- xs, x' `mod` x /= 0]
\end{haddock}

