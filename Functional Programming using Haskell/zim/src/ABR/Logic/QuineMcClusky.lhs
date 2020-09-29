% QuineMcClusky.#ext
% This file was produced from QuineMcClusky.lit

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

\module{Logic.QuineMcClusky} %%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Logic.QuineMcClusky} implements the
Quine-McClusky algorithm for simplifying boolean
expressions  as described in Rosen\cite{rosen:2003}.

\begin{code}
module ABR.Logic.QuineMcClusky (
      QMBit(..), qmSimplify
   ) where
\end{code}

\begin{code}
import Control.Arrow
import Data.List
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.IO
\end{code}

\begin{code}
import ABR.Data.List
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2016-06-02. Removed, for now, {\tt DeepSeq} instance
   that was not compiling.\\
Reviewed 2013-11-24. Removed dependence on deprecated modules.\\
Reviewed 2012-11-24: Moved into {\tt ABR.Logic}. It also
   needs to really be completed, and made to use standard
   data types.
   

\submodule{Data types} %%%%%%%%%%%%%%%%%

\noindent A Quine-McCluskey bit (\highlighttt{QMBit}) is
either zero (\highlighttt{Zer}), one (\highlighttt{One})
or a placholding dash (\highlighttt{Dsh}). A list of them
is a bit string. A list of bit strings is a formula. This
module's purpose is the simplification of such a formula.

\begin{code}
data QMBit = Zer | Dsh | One
     deriving (Eq, Show)
\end{code}

\noindent This is a label with which to tag a term, and
those derived from it. The original terms will be labelled
$0, 1, \ldots$

\begin{code}
type QMLbl = Int
\end{code}

\noindent A {\tt QMTree} stores a collection of
bit strings. A bit string is a sequence of zeroes, ones
and don't care dashes. The fork constructor
{\tt ZDO} starts branches that start with a zero,
dash or one respectively. A leaf constructor
{\tt N} indicates that that branch does not
represent a valid bit string, whereas the leaf constructor
{\tt Y} ends a valid bit string. The label that is
attached to each {\tt Y} idicates which of the original
terms this bit string either is or covers. The label is a
list of ints in strictly ascending order.

\begin{code}
data QMTree =   ZDO QMTree QMTree QMTree
              | Y [QMLbl]
              | N
              deriving (Show)
\end{code}

\noindent Example {\tt QMTree}s:

{\centering
 
{\tt 

\begin{tabular}{ll}
        & N                     \\ \hline

0: []   & Y [0]                 \\ \hline

0: [0]  & ZDO (Y [0]) N (Y [1]) \\
1: [1]  &                       \\ \hline

0: [00] & ZDO                   \\
1: [-0] & (ZDO (Y [0]) N N)     \\
2: [10] & (ZDO (Y [1]) N N)     \\
        & (ZDO (Y [2]) N N)     \\ \hline

0: [00] & ZDO                   \\
1: [10] & (ZDO (Y [0]) N N)     \\
        & N                     \\
        & (ZDO (Y [1]) N N)
\end{tabular}

}

}

\noindent A term is a list of bits, tagged by the list of
labels assigned to the original terms from which it is
derived.

\begin{code}
type QMTerm = ([QMLbl], [QMBit])
\end{code}

\submodule{Conversion from lists to trees} %%%%%%%%%%%%%%%%%

\noindent {\tt entree}~$\mathit{bss}~l$ returns $(t,l')$
where $t$ is the tree that is equivalent to the list of 
bit strings $\mathit{bss}$. $l$ is the first label to assign
to one of $\mathit{bss}$ and $l'$ is the next available
label.

\begin{code}
entree :: [[QMBit]] -> QMLbl -> (QMTree, QMLbl)
entree bss n = case bss of
   []   -> (N, n)
   [[]] -> (Y [n], n + 1)
   _    -> let (zs,ds,os) = partitionBs bss [] [] []
               (zs',n') = entree zs n
               (ds',n'') = entree ds n'
               (os',n''') = entree os n''
           in (ZDO zs' ds' os', n''')
\end{code}

\noindent {\tt partitionBs}~$\mathit{bss}$~{\tt []~[]~[]}
returns
$(\mathit{zs},\mathit{ds},\mathit{os})$ where
$\mathit{zs}$ are the tails of the strings in
$\mathit{bss}$ that started with zeroes, $\mathit{zs}$ are
the tails of the strings in $\mathit{bss}$ that started
dashes, and $\mathit{zs}$ are the tails of the strings in
$\mathit{bss}$ that started with ones.

\begin{code}
partitionBs :: 
   [[QMBit]] -> [[QMBit]] -> [[QMBit]] -> [[QMBit]] -> 
   ([[QMBit]], [[QMBit]], [[QMBit]])
partitionBs bss zs ds os = case bss of
   []     -> (zs, ds, os)
   bs:bss' -> case bs of
      Zer:bs' -> partitionBs bss' (bs':zs) ds os
      Dsh:bs' -> partitionBs bss' zs (bs':ds) os
      One:bs' -> partitionBs bss' zs ds (bs':os)
      []      -> error "partitionBs: this should not happen"
\end{code}

\submodule{Conversion from trees to lists} %%%%%%%%%%%%%%%%%

\noindent {\tt detree}~$t$ converts a tree to a list
of terms (labels and bit strings).

\begin{code}
detree :: QMTree -> [QMTerm]
detree t = case t of
   N         -> []
   Y l       -> [(l,[])]
   ZDO z d o -> 
         map (second ((:) Zer)) (detree z)
      ++ map (second ((:) Dsh)) (detree d)
      ++ map (second ((:) One)) (detree o)
\end{code}

\submodule{Combining QMTrees} %%%%%%%%%%%%%%%%%

\noindent {\tt tCombine}~$t~t'$ returns the bit
strings that result from successful combinations of the
bit strings in $t$ and $t'$. All the bit strings in $t$
and $t'$ have the same number of 1s and that number for
$t$ differs from that of $t'$ by 1. 

\begin{code}
tCombine :: QMTree -> QMTree -> QMTree
tCombine t t' = case t of
   ZDO z d o -> case t' of
      ZDO z' d' o' -> ZDO
         (tCombine z z')
         (tUnion (tCombine d d') 
            (tUnion (tSect z o') (tSect o z')))
         (tCombine o o')
      _            -> N
   _         -> N
\end{code}

\noindent {\tt tSect}~$t~t'$ returns the bit
strings that occur in $t$ and $t'$.

\begin{code}
tSect :: QMTree -> QMTree -> QMTree
tSect t t' = case t of
   N         -> N
   Y l       -> case t' of
      Y l'      -> Y (mnub l l')
      N         -> N
      ZDO {}    -> error 
         "ABR.QuineMcClusky.tSect: mismatched length"
   ZDO z d o -> case t' of
      ZDO z' d' o' -> case (tSect z z',
                            tSect d d',
                            tSect o o') of
         (N,   N,   N)   -> N
         (z'', d'', o'') -> ZDO z'' d'' o''
      _            -> N
\end{code}

\noindent {\tt tUnion}~$t~t'$ returns the bit
strings that occur in either $t$ or $t'$.

\begin{code}
tUnion :: QMTree -> QMTree -> QMTree
tUnion t t' = case t of
   N         -> t'
   Y l       -> case t' of
      N         -> t
      Y l'      -> Y (mnub l l')
      ZDO {}    -> error 
         "ABR.QuineMcClusky.tUnion: mismatched length"
   ZDO z d o -> case t' of
      N            -> t
      ZDO z' d' o' -> ZDO (tUnion z z')
                          (tUnion d d')
                          (tUnion o o')
      Y _          -> error 
         "ABR.QuineMcClusky.tUnion: mismatched length"
\end{code}

\noindent {\tt diff}~$t~t'$ returns the bit
strings that occur in $t$ but not in $t'$.

\begin{code}
diff :: QMTree -> QMTree -> QMTree
diff t t' = case t of
   N         -> N
   Y _       -> case t' of
      N         -> t
      Y _       -> N
      ZDO {}    -> error 
         "ABR.QuineMcClusky.diff: mismatched length"
   ZDO z d o -> case t' of
      N            -> t
      ZDO z' d' o' -> case (diff z z',
                            diff d d',
                            diff o o') of
         (N,   N,   N)   -> N
         (z'', d'', o'') -> ZDO z'' d'' o''
      Y _          -> error 
         "ABR.QuineMcClusky.tUnion: mismatched length"
\end{code}

\noindent {\tt unusedOne}~$t~t'$ returns the bit
strings that occur in $t$ but not used in $t'$. If it
were used in $t'$ then there would be a bit string in 
$t'$ that was exactly the same but for one one changed
to a dash.

\begin{code}
unusedOne :: QMTree -> QMTree -> QMTree
unusedOne t t' = case t of
   N         -> N
   Y _       -> t
   ZDO z d o -> case t' of
      N            -> t
      ZDO z' d' o' -> case (unusedOne z z',
                            unusedOne d d',
                            diff (unusedOne o o') 
                                 (tSect o d')) of
         (N,   N,   N)   -> N
         (z'', d'', o'') -> ZDO z'' d'' o''
      Y _          -> error 
         "ABR.QuineMcClusky.unusedOne: mismatched length"
\end{code}

\noindent {\tt unusedZero}~$t~t'$ returns the bit
strings that occur in $t$ but not used in $t'$. If it
were used in $t'$ then there would be a bit string in 
$t'$ that was exactly the same but for one zero changed
to a dash.

\begin{code}
unusedZero :: QMTree -> QMTree -> QMTree
unusedZero t t' = case t of
   N         -> N
   Y _       -> t
   ZDO z d o -> case t' of
      N            -> t
      ZDO z' d' o' -> case (diff (unusedZero z z') 
                                 (tSect z d'),
                            unusedZero d d',
                            unusedZero o o') of
         (N,   N,   N)   -> N
         (z'', d'', o'') -> ZDO z'' d'' o''
      Y _          -> error 
         "ABR.QuineMcClusky.unusedZero: mismatched length"
\end{code}

\submodule{Simplification} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent  {\tt countOnes}~$\mathit{bs}$ returns the
number of ones in $\mathit{bs}$.

\begin{code}
countOnes :: [QMBit] -> Int
countOnes bs = sum [1 | One <- bs]
\end{code}

\noindent {\tt groupByOnes}~$\mathit{bss}$ takes the
original set of bit strings and groups them by how many
ones they contain.

\begin{code}
groupByOnes :: [[QMBit]] -> IO (Array Int [[QMBit]])
groupByOnes bss = do
   let n = length $ head bss
       a :: Array Int [[QMBit]]
       a = accumArray (flip (:)) [] (0,n) 
              [(countOnes bs, bs) | bs <- bss]
   return a
\end{code}

\noindent {\tt makeTrees}~$\mathit{a}$ takes the
array of sets of bit strings, grouped by ones,
and creates a mutable array of trees. In doing so it
assigns labels.

\begin{code}
makeTrees :: Array Int [[QMBit]] -> IO (IOArray Int QMTree)
makeTrees a = do
   let (0,n) = bounds a
   a' <- newArray (0,n) N
   let entrees :: Int -> QMLbl -> IO QMLbl
       entrees i l 
          | i <= n    = do
             let (t,l') = entree (a!i) l
             writeArray a' i t
             entrees (i+1) l'
          | otherwise = return l
   _ <- entrees 0 1
   return a'
\end{code}

\noindent {\tt primeImplicants}~$a$ takes an array of
trees grouped by numbers of ones in each $a$, and returns
all those terms that could not be reduced by combination.

\begin{code}
primeImplicants :: IOArray Int QMTree -> IO [QMTerm]
primeImplicants a = do 
   (0,n) <- getBounds a
   unusedOnes <- mapArray id a
   pi1 a n 0 unusedOnes
\end{code}

\noindent {\tt pi1}~$a~n~i~\mathit{unusedOnes}$ is the outer
loop. $i$ ranges from $0$ to $n$. At $i = n$, there are no
more combinations possible, so just return the single
array element left as the irreducible terms. For $0 < i <
n$, We call {\tt fi2} to do a sweep that combines elements
$i + 1$ down to 0.

\begin{code}
pi1 :: IOArray Int QMTree -> Int -> Int 
   -> IOArray Int QMTree -> IO [QMTerm]
pi1 a n i unusedOnes
   | n == 0    = do
      t <- readArray a 0
      return $ detree t
   | i == n    = do 
      us <- readArray unusedOnes 1
      bs <- readArray a 1
      let irrs = detree $ tSect us bs 
      irrs' <- readArray a 0
      return $ irrs ++ detree irrs' 
   | otherwise = do
      irrs  <- pi2 a i unusedOnes
      irrs' <- pi1 a n (i+1) unusedOnes
      return $ irrs ++ irrs'
\end{code}

\noindent {\tt pi2}~$a~j$ performs a sweep of combinations
of elements from $j + 1$ down to 0.

\begin{code}
pi2 :: IOArray Int QMTree -> Int -> IOArray Int QMTree
   -> IO [QMTerm]
pi2 a j unusedOnes
   | j < 0    =  return []
   | otherwise = do
      t <- readArray a (j+1)
      t' <- readArray a j
      uos' <- readArray unusedOnes j
      let t'' = tCombine t t'
          uos = unusedOne t t''
          uzs = unusedZero t' t''
          irrs = detree $ tSect uos' uzs
      writeArray a j t''
      writeArray unusedOnes (j+1) uos
      writeArray unusedOnes j t''
      irrs' <- pi2 a (j-1) unusedOnes
      return $ irrs ++ irrs'
\end{code}

\noindent {\tt
findMinimalCoverage}~$\mathit{uncovered}~\mathit{coveredBy}~\mathit{nepiis}~\mathit{pia}$
finds the smallest selection from prime implicants (stored
in $\mathit{pia}$) that are not essential prime implicants
(with numbers in $\mathit{nepiis}$) and cover the original
terms not covered by essential prime implicants (their
labels are in $\mathit{uncovered}$). $\mathit{coveredBy}$
maps original term labels to the numbers of the
prime implicants that cover them.

% New breadth-first, really minimal solution that blows up

%\begin{code}
%findMinimalCoverage :: [QMLbl] -> Array QMLbl [Int] 
%   -> [Int] -> Array Int QMTerm -> IO [Int]
%findMinimalCoverage uncovered coveredBy nepiis pia = do
%      fmc init
%   where
%   init :: Queue ([QMLbl], [Int], [Int])
%   init = attachQ (uncovered, nepiis, []) emptyQ
%   fmc :: Queue ([QMLbl], [Int], [Int]) -> IO [Int]
%   fmc q 
%      | isEmptyQ q = 
%         error "ABR.QuineMcClusky.findMinimalCoverage: \
%            \No solution. (This should not happen.)"
%      | otherwise = do
%         let ((uncovered,nepiis,part),q') = detachQ q
%         case uncovered of
%            []   -> return part
%            i:is -> do
%               let -- Find the remaining inessential prime
%                   -- implicants that cover the next 
%                   -- uncovered original term (labelled i)
%                   pis = osect (reverse (coveredBy ! i)) 
%                         nepiis
%                   -- Any one of them could be part of a
%                   -- solution, but we try all of them
%                   -- looking for the minimal solution.
%                   add :: Queue ([QMLbl], [Int], [Int]) ->
%                      [Int] -> Either (Queue ([QMLbl], [Int], 
%                      [Int])) [Int]
%                   add q js = case js of
%                      []     -> Left q
%                      j : js ->
%                         let ls = fst (pia ! j)
%                             is' = odiff is ls
%                             nepiis' = delete j nepiis
%                             part' = j : part
%                         in case is' of 
%                            [] -> Right part'
%                            _  -> add (attachQ (is',
%                                     nepiis', part') q) js
%               case add q' pis of
%                  Left q''       -> fmc q''
%                  Right solution -> return solution
%\end{code}

% Old depth-first, not minimal solution

\begin{code}
findMinimalCoverage :: [QMLbl] -> Array QMLbl [Int] 
   -> [Int] -> Array Int QMTerm -> IO [Int]
findMinimalCoverage uncovered coveredBy nepiis_ pia =
   fmc uncovered nepiis_ [] 
   where
   fmc :: [QMLbl] -> [Int] -> [Int] -> IO [Int]
   fmc [] _ part = 
      return part
   fmc (i:is) nepiis part = do
      let pis = osect (reverse (coveredBy ! i)) nepiis
          j = head pis
          ls = fst (pia ! j)
          is' = odiff is ls
          nepiis' = delete j nepiis
          part' = j : part
      fmc is' nepiis' part'
\end{code}

\noindent \highlighttt{qmSimplify}~$\mathit{bss}$ 
simplifies $\mathit{bss}$. 

\begin{code}
qmSimplify :: [[QMBit]] -> IO [[QMBit]]
qmSimplify bss = do
   -- group bit strings by # 1s
   a <- groupByOnes bss
   -- turn them into trees and assign labels
   ts <- makeTrees a
   -- find the PIs
   pis <- primeImplicants ts
   let -- # original terms
       n_bss = length bss
       -- # PIs
       n_pis = length pis
       -- save PIs in an array with unique index with 
       -- PIs that cover the most original terms having
       -- lower indices
       pia :: Array Int QMTerm
       pia = array (1,n_pis) $ zip [1..] $ sortBy 
          (\(ls,_) (ls',_) -> 
             compare (length ls') (length ls)) pis
       -- each original term covered by what PIs?
       coveredBy :: Array QMLbl [Int]
       coveredBy = accumArray (flip (:)) [] (1,n_bss)
           [(l,i) | i <- [1..n_pis], l <- fst (pia ! i)]
       -- which PIs are EPIs
       epiis = snub [j 
          | i <- [1..n_bss], [j] <- [coveredBy ! i]]
       -- the EPI bits strings and labels
       (epils,epibs) = unzip [pia ! i | i <- epiis]
       -- all originals covered by EPIs
       covered = foldl mnub [] epils 
       -- all originals not covered by EPIs
       uncovered = odiff [1..n_bss] covered
       -- which PIs are not EPIs
       nepiis = odiff [1..n_pis] epiis
   nepiis' <- findMinimalCoverage uncovered coveredBy
      nepiis pia
   let -- the not essential PI bits strings needed
       nepibs = [snd (pia ! i) | i <- nepiis']
   return $ epibs ++ nepibs
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{DeepSeq} %%%%%%%%%%%%%%%

\begin{haddock}
instance Generic QMBit where { }
\end{haddock}

\begin{haddock}
instance GNFData QMBit where { }
\end{haddock}

\begin{haddock}
instance NFData QMBit where { }
\end{haddock}

%\submodule{Tests} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%\begin{code}
%test :: IO ()
%test = do
%   let bss = [[One,One,One,Zer],[One,Zer,One,One],[Zer,One,One,One],[One,Zer,One,Zer],[Zer,One,Zer,One],[Zer,Zer,One,One],[Zer,Zer,Zer,One]]
%              
%   a <- groupByOnes bss
%   print a
%   
%   t <- makeTrees a
%   ts <- getAssocs t
%   print ts
%
%   pis <- primeImplicants t
%   print pis
%              
%   bss' <- qmSimplify bss
%   print bss'
%\end{code}
%       x = entree bss 0
%   print x

