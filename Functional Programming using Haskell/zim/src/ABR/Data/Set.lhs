% Set.#ext
% This file was produced from Set.lit

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

\module{(Deprecated) Data.Set} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Data.Set} implements a set type where the elements
are orderable, but selected from too large a
domain to make an array implementation practical.
The sets are implemented with a list.

\begin{code}
module ABR.Data.Set 
   {-# DEPRECATED "Use Data.Set instead." #-}
   (
      Set, eset, set, set1, unset1, list, (.|), (.&), (.-),
      (.+), (.<-), (.<), (.<=), card, smap, snull, select,
      (.*), sprod, (.*.), sany, sall, sfoldl, sfoldl1, 
      sfoldr, sfoldr1, sunion, ssect
   ) where
\end{code}

\begin{code}
import ABR.Data.List
import ABR.Text.Showing
import ABR.DeepSeq
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2015-02-09. Passed {\tt hlint}.\\
Reviewed 2012-11-24: Deprecated.\\
Reviewed 2009-10-27: Changed to {\tt ABR.\emph{Data}.Set}.
   

\submodule{Data type} %%%%%%%%%%%%%%%%%%%

This representation of a \highlighttt{Set} is a list in 
\emph{strictly} ascending order.

\begin{code}
data Set a = 
   Set [a]
   deriving (Eq)
\end{code}


\submodule{Operations} %%%%%%%%%%%%%%%%%%%

\begin{code}
infixl 7 .&, .*, .*.
infixl 6 .|, .-, .+
infix  5 .<-, .<, .<=
\end{code}

\noindent \highlighttt{eset} is $\{\}$.

\begin{code}
eset :: Set a
eset = Set []
\end{code}

\noindent \highlighttt{set}~$\mathit{xs}$ returns the set of elements in 
$\mathit{xs}$.

\begin{code}
set :: (Ord a) => [a] -> Set a
set xs = Set (snub xs)
\end{code}

\noindent \highlighttt{set1}~$x$ returns $\{x\}$.

\begin{code}
set1 :: a -> Set a
set1 x = Set [x]
\end{code}

\noindent \highlighttt{unset1}~$\{x\}$ returns $x$.

\begin{code}
unset1 :: Set a -> a
unset1 (Set [x]) = x
\end{code}

\noindent \highlighttt{list}~$A$ returns the list of elements in 
$A$.

\begin{code}
list :: Set a -> [a]
list (Set xs) = xs
\end{code}

\noindent $A$~\highlightttNoindex{.\PIPE}\index{.\PIPE@{\tt .\PIPE}}~$B$ returns $A \cup B$.
$A$~\highlighttt{.\&}~$B$ returns $A \cap B$.
$A$~\highlighttt{.-}~$B$ returns $A - B$.

\begin{code}
(.|), (.&), (.-) :: (Ord a) => Set a -> Set a -> Set a
(Set as) .| (Set bs) = Set (umerge as bs)
   where
   umerge []     ys     = ys
   umerge xs     []     = xs
   umerge (x:xs) (y:ys)
      | x < y           = x : umerge xs     (y:ys)
      | x == y          =     umerge xs     (y:ys)
      | otherwise       = y : umerge (x:xs) ys
(Set as) .& (Set bs) = Set (imerge as bs)
   where
   imerge []     ys     = []
   imerge xs     []     = []
   imerge (x:xs) (y:ys)
      | x < y           =     imerge xs     (y:ys)
      | x == y          = x : imerge xs     ys
      | otherwise       =     imerge (x:xs) ys
(Set as) .- (Set bs) = Set (smerge as bs)
   where
   smerge []     ys     = []
   smerge xs     []     = xs
   smerge (x:xs) (y:ys)
      | x < y           = x : smerge xs     (y:ys)
      | x == y          =     smerge xs     ys
      | otherwise       =     smerge (x:xs) ys
\end{code}

\noindent $A$~\highlighttt{.+}~$x$ returns $A \cup \{x\}$.

\begin{code}
(.+) :: (Ord a) => Set a -> a -> Set a
(Set as) .+ x = Set (insert as)
   where
   insert []     = [x]
   insert (y:ys) 
      | x < y     = x : y : ys
      | x == y    = y : ys
      | otherwise = y : insert ys   
\end{code}

\noindent $x$~\highlighttt{.<-}~$A$ returns {\tt True} iff 
$x \in A$.

\begin{code}
(.<-) :: (Ord a) => a -> Set a -> Bool
x .<- (Set as) = elem as
   where
   elem []        = False
   elem (y:ys) 
      | x < y     = False
      | x == y    = True
      | otherwise = elem ys
\end{code}

\noindent $A$~\highlighttt{.<}~$B$ returns {\tt True} iff 
$A \subset B$.
$A$~\highlighttt{.<=}~$B$ returns {\tt True} iff $A \subseteq B$.

\begin{code}
(.<), (.<=) :: (Ord a) => Set a -> Set a -> Bool
Set as .< Set bs = Set as .<= Set bs  
   && length as < length bs
(Set as) .<= (Set bs) = iss as bs
   where
   iss []     ys  = True
   iss xs     []  = False
   iss (x:xs) (y:ys)
      | x < y     = False
      | x == y    = iss xs ys
      | otherwise = iss (x:xs) ys 
\end{code}

\noindent \highlighttt{card}~$A$ returns $|A|$.

\begin{code}
card :: Set a -> Int
card (Set xs) = length xs
\end{code}

\noindent \highlighttt{smap}~$f~A$ returns $\{f x : x \in A\}$.

\begin{code}
smap :: (Ord b) => (a -> b) -> Set a -> Set b
smap f (Set xs) = Set (snub (map f xs)) 
\end{code}

\noindent \highlighttt{snull}~$A$ returns {\tt True} iff $A = \{\}$.

\begin{code}
snull :: Set a -> Bool
snull (Set xs) = null xs
\end{code}

\noindent \highlighttt{select}~$P~A$ returns $\{x : x \in A$ and 
$P(x)\}$.

\begin{code}
select :: (a -> Bool) -> Set a -> Set a
select f (Set xs) = Set (filter f xs)
\end{code}

\noindent $A$~\highlighttt{.*}~$B$ returns $A \otimes B
= \bigotimes \{A, B\}$.

\begin{code}
(.*) :: Ord a => Set a -> Set a -> Set (Set a)
a .* b = sprod (set [a, b])
\end{code}

\noindent \highlighttt{sprod}~$\{A_{1}, \ldots A_{n}\}$
returns $\bigotimes \{A_{1}, \ldots A_{n}\}$, where
$\bigotimes \{\} = \{\{\}\}$, $\bigotimes \{A\} = 
\{\{a\} : a \in A\}$, and $\bigotimes \{A_{1}, 
\ldots A_{n}\} = A_{1} \otimes \ldots 
\otimes A_{n} = \{\{a_{1},\ldots a_{n}\} : 
(a_{1},\ldots a_{n}) \in A_{1} \times \ldots \times 
A_{n}\}$

\begin{code}
sprod :: Ord a => Set (Set a) -> Set (Set a)
sprod (Set []) = Set [Set []]
sprod (Set [Set xs]) = Set (map set1 xs)
sprod (Set as) = (set . map set . cartProd . map list) as 
\end{code}

\noindent $A$~\highlighttt{.*.}~$B$ returns $A \times B$.

\begin{code}
(.*.) :: (Ord a, Ord b) => 
   Set a -> Set b -> Set (a,b)
(Set as) .*. (Set bs) = Set [(a,b) | a <- as, b <- bs]
\end{code}

\noindent \highlighttt{sall}~$P~A$ returns {\tt True} iff
$\forall a \in A, \, P(a)$. \highlighttt{sany}~$P~A$ returns
{\tt True} iff $\exists a \in A, \, P(a)$.

\begin{code}
sall, sany :: (a -> Bool) -> Set a -> Bool
sall f (Set xs) = all f xs
sany f (Set xs) = any f xs
\end{code}

\noindent \highlighttt{sfoldl}, \highlighttt{sfoldr}, \highlighttt{sfoldl1}
and \highlighttt{sfoldr1} are analogues of {\tt foldl}, 
{\tt foldr}, {\tt foldl1} and {\tt foldr1} respectively.

\begin{code}
sfoldl :: (a -> b -> a) -> a -> Set b -> a
sfoldr :: (a -> b -> b) -> b -> Set a -> b
sfoldl1, sfoldr1:: (a -> a -> a) -> Set a -> a
sfoldl f z (Set xs) = foldl f z xs
sfoldr f z (Set xs) = foldr f z xs 
sfoldl1 f (Set xs) = foldl1 f xs
sfoldr1 f (Set xs) = foldr1 f xs
\end{code}

\noindent \highlighttt{sunion}~$\{A_{1}, \ldots , A_{n}\}$ returns
$\bigcup  \{A_{1}, \ldots , A_{n}\} = A_{1} \cup \ldots \cup A_{n}$.
\highlighttt{ssect}~$\{A_{1}, \ldots , A_{n}\}$ returns
$\bigcap  \{A_{1}, \ldots , A_{n}\} = A_{1} \cap \ldots \cap A_{n}$.

\begin{code}
sunion, ssect :: Ord a => Set (Set a) -> Set a
sunion = sfoldl (.|) eset  
ssect a | snull a   = eset 
        | otherwise = sfoldl1 (.&) a
\end{code}

\submodule{Instances} %%%%%%%%%%%%%%%%%%%

\subsubmodule{Ord}

\begin{code}
instance (Ord a) => Ord (Set a) where 
\end{code}

\begin{code}
   compare (Set as) (Set bs) = 
      case compare (length as) (length bs) of
         EQ -> compare as bs
         c  -> c
\end{code}


\subsubmodule{Showing}

\begin{code}
instance (Show a) => Show (Set a) where 
\end{code}

\begin{code}
   showsPrec p (Set xs) = showChar '{' 
      . showWithSep "," xs . showChar '}' 
\end{code}


\subsubmodule{DeepSeq}

\begin{code}
instance (DeepSeq a) => DeepSeq (Set a) where 
\end{code}

\begin{code}
   deepSeq (Set xs) = deepSeq xs
\end{code}


