% Formulas.#ext
% This file was produced from Formulas.lit

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

\module{Formulas} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlightModule{Formulas} implements clauses and
formulas for Decisive Plausible Logic.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
\end{code}

\begin{code}
module CDL.Formulas (
\end{code}

\begin{haddock}
      Clause(..), CnfFormula(..), clauseP, cnfFormulaP,
      MaybeTautology(..), res, rsn
\end{haddock}

\begin{code}
   ) where
\end{code}

\begin{haddock}
import List
import Maybe
\end{haddock}

\begin{haddock}
import ABR.Parser
import ABR.List
import ABR.Showing
import ABR.Control.Check
import ABR.Data.BSTree
\end{haddock}

\begin{haddock}
import CDL.Constants
import CDL.Variables
import CDL.Atoms
import CDL.Literals
import CDL.LitSets
import CDL.Types
\end{haddock}

\submodule{Data types} %%%%%%%%%%%%%

\noindent A \highlighttt{Clause} is the disjunction
 of a set of literals, expressed 
either: 

\begin{itemize}
   \item comprehensively (\highlighttt{Clause}); or
   \item as a list (\highlighttt{Or}).
\end{itemize}

\begin{haddock}
data Clause =   Clause LitSet
              | Or [Literal]
              deriving (Eq, Ord)
\end{haddock}

\noindent A \highlighttt{CnfFormula} is the conjunction
(\highlighttt{CNF}) of a set of clauses.

\begin{haddock}
newtype CnfFormula = CNF [[Literal]]
\end{haddock}


\submodule{Parsers} %%%%%%%%%%%%%%%%%%%%%%%%

\noindent Clauses are based on sets of literals, they are
initially parsed as comprehension, by
\highlighttt{clauseP}, but ultimately must be transformed
to lists.

\syntrax{clause}

\begin{haddock}
clauseP :: Parser Clause
clauseP = 
       pLiteralP
       @> Clause . mkLitSet
   <|> literalP "symbol" "\\/"
       *> nofail' "literal set expected" litSetP
       @> Clause
\end{haddock}

\noindent \highlighttt{cnfFormulaP} parses cnf-formulas.

\syntrax{cnfFormula}

\begin{haddock}
clauseSetP :: Parser [Clause]
clauseSetP = 
       literalP "symbol" "{"
       *> clauseP
       <*> many (literalP "symbol" ","
                 *> nofail' "clause expected" clauseP)
       <* literalP "symbol" "}"
       @> snub . cons
   <|> literalP "symbol" "{"
       <*> nofail (literalP "symbol" "}")
       #> []
\end{haddock}

\begin{haddock}
cnfFormulaP :: Parser CnfFormula
cnfFormulaP = 
       clauseP
       @> (\(Or ls) -> CNF [ls]) . flatten' emptyBST
   <|> literalP "symbol" "/\\"
       *> nofail' "clause set expected" clauseSetP
       @> CNF . map ((\(Or ls) -> ls) . flatten' emptyBST)
\end{haddock}

\submodule{Tautologies} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{haddock}
class MaybeTautology a where
\end{haddock}

\highlighttt{isTautology}~$C$ returns {\tt True} iff
$C$ is a tautology.

\begin{haddock}
   isTautology :: a -> Bool
\end{haddock}
 
\submodule{Resolution} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
{\tt resolve}~$c$~$d$ returns {\tt Just} 
a clause that is a resolvent of clauses $c$ and $d$,
or {\tt Nothing}. Precondition: $c$ and $d$ must be
in strictly ascending order; e.g. $Or [\comp e, \comp b,
a, c]$. Postcondition: If there is a resolvent, 
it is returned with the same ordering.

\begin{haddock}
resolve :: Clause -> Clause -> Maybe Clause
resolve (Or cs) (Or ds) = resolve' cs (reverse ds) [] []
   where
   resolve', resolve'' :: [Literal] -> [Literal] -> 
      [Literal] -> [Literal] -> Maybe Clause
   resolve' _ [] _ _ = Nothing
   resolve' [] _ _ _ = Nothing
   resolve' (q:qs) (r:rs) qs' rs' = case q of
      Pos qa -> case r of
         Pos _  -> resolve' (q:qs) rs qs' (r:rs')
         Neg ra -> if qa == ra then
                      resolve'' qs rs qs' rs' 
                   else if qa < ra then
                      resolve' qs (r:rs) (q:qs') rs'
                   else 
                      resolve' (q:qs) rs qs' (r:rs')
      Neg qa -> case r of
         Pos ra -> if qa == ra then
                      resolve'' qs rs qs' rs'
                   else if qa < ra then
                      resolve' qs (r:rs) (q:qs') rs'
                   else 
                      resolve' (q:qs) rs qs' (r:rs') 
         Neg _  ->
            resolve' qs (r:rs) (q:qs') rs'
   resolve'' qs [] qs' rs' = 
      Just $ Or $ mnub (reverse qs' ++ qs) rs'
      
   resolve'' [] rs qs' rs' =
      Just $ Or $ mnub (reverse qs') (reverse rs ++ rs')
   
   resolve'' (q:qs) (r:rs) qs' rs' = case q of
      Pos qa -> case r of
         Pos _  -> resolve'' (q:qs) rs qs' (r:rs')
         Neg ra -> if qa == ra then
                      Nothing
                   else if qa < ra then 
                      resolve'' qs (r:rs) (q:qs') rs'
                   else
                      resolve'' (q:qs) rs qs' (r:rs') 
      Neg qa -> case r of 
         Pos ra -> if qa == ra then
                      Nothing
                   else if qa < ra then
                      resolve'' qs (r:rs) (q:qs') rs'
                   else 
                      resolve'' (q:qs) rs qs' (r:rs') 
         Neg _  -> resolve'' qs (r:rs) (q:qs') rs'
   {- build :: [Literal] -> [Literal] -> [Literal] ->
      Maybe Clause
   build qs qs' rs' = 
      Just $ Or $ mnub (reverse qs' ++ qs) rs' -}
\end{haddock}

\noindent \highlighttt{res}~$S$ returns $Res(S)$.
Precondition: $S$ and all elements of $S$ are
in strictly ascending order.

\begin{haddock}
res :: [Clause] -> [Clause]
res s =
   let res' = snub $ catMaybes [resolve c d |
                 c <- s, d <- s]
       res'' = res' \\ s
       res''' = snub $ s ++ res''
   in if null res'' then
         res'''
      else
         res res'''
\end{haddock}

\noindent \highlighttt{rsn}~$S$ returns $Rsn(S)$.
Precondition: $S$ and all elements of $S$ are
in strictly ascending order.

\begin{haddock}
rsn :: [Clause] -> [Clause]
rsn = (\\ [Or []]) . res
\end{haddock}


\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Showing} %%%%%

\begin{haddock}
instance Show Clause where
   showsPrec p c = case c of
      Clause ls ->  showString "\\/" . shows ls
      Or ls     -> case ls of
         [l] -> shows l
         ls  -> showString "\\/{" .
            showWithSep "," ls . showChar '}'
\end{haddock}

\begin{haddock}
instance Show CnfFormula where
   showsPrec p (CNF cs) = case cs of 
      [ls] -> shows (Or ls) 
      cs   -> showString "/\\{" .
          showWithSep "," (map Or cs) . showChar '}'
\end{haddock}

\subsubmodule{Collecting Atoms} %%%%%

\begin{haddock}
instance HasAtoms Clause where
    getAtoms (Or ls) as = foldr getAtoms as ls
\end{haddock}

\begin{haddock}
instance HasAtoms CnfFormula where
    getAtoms (CNF lss) as = 
       foldr getAtoms as (concat lss)
\end{haddock}

\subsubmodule{Collecting constants} %%%%%%%%%%%%%%%%%%%%%

\begin{haddock}
instance HasConstants Clause where
   getConstants c cs = case c of
      Clause ls -> getConstants ls cs
      Or ls     -> foldr getConstants cs ls
\end{haddock}

\begin{haddock}
instance HasConstants CnfFormula where
   getConstants (CNF lss) cs = 
      foldl (foldr getConstants) cs lss
\end{haddock}

\subsubmodule{Collecting variables} %%%%%%%%%%%%%%%%%%%%%

\begin{haddock}
instance HasVariables Clause where
   getVariables c vs = case c of
      Clause ls -> getVariables ls vs
      Or ls     -> foldr getVariables vs ls
\end{haddock}

\begin{haddock}
instance HasVariables CnfFormula where
   getVariables (CNF lss) vs = 
      foldl (foldr getVariables) vs lss
\end{haddock}

\subsubmodule{Grounding} %%%%%

\begin{haddock}
instance Groundable Clause where
   ground1 v c cl = case cl of
      Clause ls -> Clause $ ground1 v c ls
      Or ls     -> Or $ map (ground1 v c) ls
   rename v v' cl = case cl of
      Clause ls -> Clause $ rename v v' ls
      Or ls     -> Or $ map (rename v v') ls
\end{haddock}

\begin{haddock}
instance Groundable CnfFormula where
   ground1 v c (CNF lss) = 
      CNF (map (map (ground1 v c)) lss)
   rename v v' (CNF lss) = 
      CNF (map (map (rename v v')) lss)
\end{haddock}


\subsubmodule{Flattening} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{haddock}
instance HasLitSets Clause Clause where
   flatten tt c = case c of
      Clause ls -> case flatten tt ls of
         CheckPass ls' -> CheckPass $ Or ls'
         CheckFail msg -> CheckFail msg
      Or ls'    -> CheckPass $ Or ls'
\end{haddock}

\subsubmodule{Tautologies} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{haddock}
instance MaybeTautology Clause where
   isTautology (Or ls) = 
      or [neg l == l' | l : ls' <- tails ls, l' <- ls']
\end{haddock}

\begin{haddock}
instance (Eq a, Negatable a) => MaybeTautology [a] where
   isTautology ls = 
      or [neg l == l' | l : ls' <- tails ls, l' <- ls']
\end{haddock}

\subsubmodule{Collecting Orderings} %%%%%%%%

\begin{haddock}
instance HasTypes Clause where
   getOrderings os c = case c of
      Clause ls -> getOrderings os ls
      _         -> os
\end{haddock}

\subsubmodule{Qualification} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{haddock}
instance Qualifiable Constant where
   qualify n c = case c of
      CNamed n' p   -> CNamed (n ++ "." ++ n') p
      CIntegral _ _ -> c
      CString _ _   -> c
\end{haddock}
