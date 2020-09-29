\module{Matching}

The {\tt Matching} module provides the cross-matching of tokenised files, detecting any sequences of consecutive matching {\tt Tokens} between tokenized files whose length is greater than or equal to a minimum required length.

\begin{code}
module Zimm.Matching (
      Run(..), findRuns, filterRuns
   ) where
\end{code}

\begin{code}
import Data.List as L
import Data.Map.Strict as M
import Data.Set as S
\end{code}

\begin{code}
import ABR.Util.Pos
\end{code}

\begin{code}
import Zimm.Token
\end{code}

\noindent {\tt match ts ts'} compares 2 lists of {\tt Tokens} and determines 
the number of matching consecutive {\tt Tokens} from the start of each list, returning the (len, lastPos1, lastPos2) -- where len is the length of 
matching tokens and lastPos1 / lastPos2 are the last matching positions in the 
respective lists. If the first {\tt Tokens} in each list do not match, 
(0, (-1, -1), (-1,-1)) will be returned. When comparing {\tt Tokens}, 
identifiers are mapped to an index according to the order they appear within 
their respective list of {\tt Tokens}, which is a form of unification that allows the algorithm to detect matches even when identifier names have been 
changed between files.

\begin{code}
match :: [(Token, Pos)] -> [(Token, Pos)] -> (Int, Pos, Pos)
match ts ts' = 
   let len = match' 0 ts 0 M.empty ts' 0 M.empty
       lastPos1 | len == 0  = (-1, -1)
                | otherwise = snd $ ts !! (len - 1)
       lastPos2 | len == 0  = (-1, -1)
                | otherwise = snd $ ts' !! (len - 1)
   in (len, lastPos1, lastPos2)
   where
   match' :: Int -> [(Token, Pos)] -> Int -> M.Map Int Int
                 -> [(Token, Pos)] -> Int -> M.Map Int Int -> Int
   match' n t1s v1 m1 t2s v2 m2 
      | L.null t1s || L.null t2s = n
      | otherwise                = 
         let reassign :: Token -> Int -> M.Map Int Int ->
                         (Token, Int, M.Map Int Int)
             reassign t v m = case t of
                Id i -> case M.lookup i m of
                   Nothing -> (Id v, v + 1, M.insert i v m)
                   Just j  -> (Id j, v, m)
                _ -> (t, v, m)
             (t1, _) : tl1 = t1s
             (t2, _) : tl2 = t2s
             (t1', v1', m1') = reassign t1 v1 m1
             (t2', v2', m2') = reassign t2 v2 m2 
         in if t1' == t2' 
               then match' (n + 1) tl1 v1' m1' tl2 v2' m2'
               else n
\end{code}

\noindent A {\tt Run} represents a continuous stream of matching {\tt Tokens} that were found
between 2 files, where {\tt len} represents the number of matching {\tt Tokens} and
{\tt startPos1}, {\tt endPos1}, {\tt startPos2}, {\tt endPos2} represent the 
positions in the respective files where the stream of matching {\tt Tokens} start 
and finish.

\begin{code}
data Run = Run {
      len :: Int,
      startPos1 :: Pos,
      endPos1 :: Pos,
      startPos2 :: Pos,
      endPos2 :: Pos
   }
\end{code}

\noindent {\tt findRuns n f1 f2} returns all the matching runs of at least {\tt n} 
{\tt Tokens} from the tokenized files {\tt f1} and {\tt f2}. 

\begin{code}
findRuns :: Int -> [(Token, Pos)] -> [(Token, Pos)] -> [Run]
findRuns n f1 f2 = 
   [Run m p1 ep1 p2 ep2 
   | t1@((_, p1) : _) <- L.tails f1, t2@((_, p2) : _) <- L.tails f2, 
     let (m, ep1, ep2) = match t1 t2, m >= n]
\end{code}

%\begin{haddock}
%findRuns :: Int -> [(Token, Pos)] -> [(Token, Pos)] -> [Run]
%findRuns n f1 f2 = findRuns' S.empty (zip [0..] (tails f1)) 
%                   (zip [0..] (tails f2))
%   where
%   findRuns' :: S.Set (Int, Int) -> [(Int, [(Token, Pos)])]
%                -> [(Int, [(Token, Pos)])] -> [Run]
%   findRuns' s its1 its2 = case its1 of
%      []        -> []
%      it : its1' -> 
%         let (s', runs) = findRuns'' s it its2
%         in runs ++ findRuns' s' its1' its2
%   findRuns'' :: S.Set (Int, Int) -> (Int, [(Token, Pos)]) -> 
%      [(Int, [(Token, Pos)])] -> (Set (Int, Int), [Run])
%   findRuns'' s (i1, ts1) its2 = case its2 of
%      [] -> (s, [])
%      (i2, ts2) : its2'
%         | (i1 - 1, i2 - 1) `S.member` s -> 
%            findRuns'' (S.insert (i1, i2) s) (i1, ts1) its2' 
%         | otherwise -> 
%            let (m, ep1, ep2) = match ts1 ts2
%            in if m >= n then let (s', runs) = findRuns'' (S.insert (i1, i2) s) 
%                              (i1, ts1) its2' 
%                              in (s', Run m (snd (head ts1)) ep1 
%                                 (snd (head ts2)) ep2 : runs)
%                         else findRuns'' s (i1, ts1) its2' 
%\end{haddock}

\noindent {\tt runInclues r r'} checks whether the start and ending positions 
of run {\tt r'} are encapsulated within the run {\tt r} (meaning that {\tt r'} is effectively a 
redundant match), returning {\tt True} when the positions of {\tt r'} are encapsulated 
within the run {\tt r} and {\tt False} otherwise.

\begin{code}
runIncludes :: Run -> Run -> Bool
runIncludes r r' =
   len r' < len r &&
   startPos1 r < startPos1 r' && startPos2 r < startPos2 r' &&
   endPos1 r >= endPos1 r' && endPos2 r >= endPos2 r'
\end{code}

\noindent {\tt filterRuns runs} checks a list of runs for any that are 
redundant (any run having start and end positions which are encapsulated within 
the positions of another run), returning the list of runs where all redundant runs have been removed.

\begin{code}
filterRuns :: [Run] -> [Run]
filterRuns runs = case runs of
   []     -> []
   r : rs -> r : filterRuns (L.filter (\r' -> not (runIncludes r r')) rs)
\end{code}
