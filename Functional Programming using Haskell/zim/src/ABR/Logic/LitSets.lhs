% LitSets.#ext
% This file was produced from LitSets.lit

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

\module{LitSets} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlightModule{LitSets} implements sets of and
literals for Decisive Plausible Logic.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
\end{code}

\begin{code}
module CDL.LitSets (
      LitSet(..), litSetP, mkLitSet, HasLitSets(..)
   ) where
\end{code}

\begin{code}
import ABR.Showing
import ABR.Parser
import ABR.Data.SparseSet
import ABR.Parser.Pos
\end{code}

\begin{haddock}
import ABR.Control.Check
\end{haddock}

\begin{code}
import CDL.Literals
import CDL.VarGens
import CDL.Types
import CDL.Variables
\end{code}

\begin{haddock}
import CDL.Constants
import CDL.Atoms
import CDL.TypeDecs
\end{haddock}

\submodule{Data types} %%%%%%%%%%%%%

\noindent A \highlighttt{LitSet} is either

\begin{itemize}
   \item a fully enumerated set of literals 
      (\highlighttt{LSEnum}, the ``flattened'' form);
   \item the union of two sets of literals 
      (\highlighttt{LSUnion});
   \item the intersection between two types 
      (\highlighttt{LSSect}); 
   \item the difference between two types 
      (\highlighttt{LSDiff}); or
   \item a comprehensive specification 
      (\highlighttt{LSComp}).
\end{itemize}

\begin{code}
data LitSet =
     LSEnum {
        lsEnum :: SparseSet Literal,
        lsPos  :: Pos
     }
   | LSUnion {
        ls1   :: LitSet,
        ls2   :: LitSet,
        lsPos :: Pos
     }
   | LSSect {
        ls1   :: LitSet,
        ls2   :: LitSet,
        lsPos :: Pos
     }
   | LSDiff {
        ls1   :: LitSet,
        ls2   :: LitSet,
        lsPos :: Pos
     }
   | LSComp {
        lsPat   :: Literal,
        lsGen :: [VarGen],
        lsPos :: Pos
     }
\end{code}  

\submodule{Parsers} %%%%%%%%%%%%%%%%%%%%%%%%

\noindent Types are parsed by \highlighttt{litSetP}.

\syntrax{litSet}

\begin{code}
litSetP :: Parser LitSet
litSetP = 
       litSetTermP 
   <*> litSetEndP
   @> (\(t,f) -> f t)
\end{code}

\syntrax{litSetEnd}

\begin{code}
litSetEndP :: Parser (LitSet -> LitSet)
litSetEndP =
           literalP "symbol" "+"
        *> nofail' "literal set expected" litSetTermP
       <*> litSetEndP
        @> (\(t, f) -> 
               (\t' -> LSUnion t' t (getPos t')) . f)
   <|>     literalP "symbol" "^"
        *> nofail' "literal set expected" litSetTermP
       <*> litSetEndP
        @> (\(t, f) -> 
               (\t' -> LSSect t' t (getPos t')) . f)
   <|>     literalP "symbol" "-"
        *> nofail' "literal set expected" litSetTermP
       <*> litSetEndP
        @> (\(t, f) -> 
               (\t' -> LSDiff t' t (getPos t')) . f)
   <|>     epsilonA
        #> id
\end{code}

\syntrax{litSetTerm}

\begin{code}
litSetTermP :: Parser LitSet
litSetTermP = 
           literalP "symbol" "("
        *> litSetP
       <*  nofail (literalP "symbol" ")")
   <|>     literalP "symbol" "{"
       <*> pLiteralP
       <*> literalP "symbol" "|"
        *> nofail' "variable generator expected" varGenP
       <*> many (
                 literalP "symbol" ","
              *> nofail' "variable generator expected" 
                    varGenP
           )
       <*  nofail (literalP "symbol" "}")
        @> (\((_,_,p),(l,(vg,vgs))) -> 
              let vgs' = vg : vgs
                  f = foldl1 (.)
                     [rename (Variable v p)
                             (Variable ("_VG_" ++ v) p)
                     | VarGen (Variable v p) _ _ <- vgs']
                  f' = foldl1 (.)
                     [rename (Variable v p)
                             (Variable ("_VG_" ++ v) p)
                     | VarGen (Variable v p) _ _ <- vgs']
              in LSComp (f l) (map f' vgs') p
           )
   <|>     literalP "symbol" "{"
       <*> optional (
                  pLiteralP
              <*> many (
                       literalP "symbol" ","
                    *> nofail' "literal expected" pLiteralP
                  )
           )
       <*  nofail (literalP "symbol" "}")
        @> (\((_,_,p),lss) -> LSEnum (case lss of
              []       -> emptySS
              [(l,ls)] -> list2SS (l : ls)
           ) p)
\end{code}

\submodule{Methods} %%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{mkLitSet}~$l$ makes a singleton
literal set from a literal $l$.

\begin{code}
mkLitSet :: Literal -> LitSet
mkLitSet l = LSEnum (mkSS l) (getPos l)
\end{code}

\submodule{Flattening} %%%%%%%%%%%%%%%%%%%%%%%%

Class \highlighttt{HasLitSets} includes all those types
which contain literal sets that must be flattened. The 
class is parameterized over both the unflattened and 
flattened types.

\begin{code}
class HasLitSets a b where
\end{code}

\noindent \highlighttt{flatten}~$\mathit{tt}~x$ is attempts
to flatten $x$, but may fail with a message. $\mathit{tt}$
is the table of named types.

\begin{code}
   flatten :: TypeTable -> a -> IO b
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Comparing} %%%%%%%

\begin{haddock}
instance Eq LitSet where
   { }
\end{haddock}

\begin{haddock}
instance Ord LitSet where
   { }
\end{haddock}

\subsubmodule{Positions} %%%%%%%

\begin{code}
instance HasPos LitSet where
   getPos = lsPos
\end{code}

\subsubmodule{Showing} %%%%%

\begin{code}
instance Show LitSet where
   showsPrec _ ls = case ls of
      LSEnum ls' _ -> showChar '{' . 
         showWithSep ", " (flattenSS ls') . showChar '}'
      LSUnion ls1 ls2 _ -> showChar '(' . shows ls1 . 
         showString " + " . shows ls2 . showChar ')'
      LSSect ls1 ls2 _ -> showChar '(' . shows ls1 . 
         showString " ^ " . shows ls2 . showChar ')'
      LSDiff ls1 ls2 _ -> showChar '(' . shows ls1 . 
         showString " - " . shows ls2 . showChar ')'
      LSComp l vgs _ -> showChar '{' . shows l . 
         showString " | " . showWithSep ", " vgs . 
         showChar '}'
\end{code}

\subsubmodule{Collecting constants} %%%%%%%

\begin{haddock}
instance HasConstants LitSet where
   getConstants ls cs = case ls of
      LEnum ls'   -> foldr getConstants cs $ flattenSS ls'
      ls1 ::+ ls2 -> getConstants ls1 $ getConstants ls2 cs
      ls1 ::^ ls2 -> getConstants ls1 $ getConstants ls2 cs
      ls1 ::- ls2 -> getConstants ls1 $ getConstants ls2 cs
      LComp l vgs -> getConstants l $
         foldr getConstants cs vgs
\end{haddock}

\subsubmodule{Collecting variables} %%%%%%%

\begin{haddock}
instance HasVariables LitSet where
   getVariables ls vs = case ls of
      LEnum ls'   -> foldr getVariables vs $ flattenSS ls'
      ls1 ::+ ls2 -> getVariables ls1 $ getVariables ls2 vs
      ls1 ::^ ls2 -> getVariables ls1 $ getVariables ls2 vs
      ls1 ::- ls2 -> getVariables ls1 $ getVariables ls2 vs
      LComp l vgs -> getVariables l $
         foldr getVariables vs vgs
\end{haddock}

\subsubmodule{Collecting Atoms} %%%%%

\begin{haddock}
instance HasAtoms LitSet where
    getAtoms ls as = case ls of
      LEnum ls'   -> foldr getAtoms as $ flattenSS ls'
      ls1 ::+ ls2 -> getAtoms ls1 $ getAtoms ls2 as
      ls1 ::^ ls2 -> getAtoms ls1 $ getAtoms ls2 as
      ls1 ::- ls2 -> getAtoms ls1 $ getAtoms ls2 as
      LComp l vgs -> getAtoms l as
\end{haddock}

\subsubmodule{Grounding} %%%%%%%

\noindent Precondtion: The variable being grounded must be
realatively free.

\begin{haddock}
instance Groundable LitSet where
   ground1 v c ls = case ls of
      LEnum ls'   -> LEnum $ list2SS $ map (ground1 v c) $
         flattenSS ls'
      ls1 ::+ ls2 -> ground1 v c ls1 ::+ ground1 v c ls2
      ls1 ::^ ls2 -> ground1 v c ls1 ::^ ground1 v c ls2
      ls1 ::- ls2 -> ground1 v c ls1 ::- ground1 v c ls2
      LComp l vgs -> 
         LComp (ground1 v c l) (ground1 v c vgs)
   rename v v' ls = case ls of
      LEnum ls'   -> LEnum $ list2SS $ map (rename v v') $
         flattenSS ls'
      ls1 ::+ ls2 -> rename v v' ls1 ::+ rename v v' ls2
      ls1 ::^ ls2 -> rename v v' ls1 ::^ rename v v' ls2
      ls1 ::- ls2 -> rename v v' ls1 ::- rename v v' ls2
      LComp l vgs -> LComp (rename v v' l) vgs
\end{haddock}

\subsubmodule{Flattening} %%%%%%%

\noindent Precondition: $ls$ must contain no variables
other than that appearing in comprehensions.

\begin{haddock}
instance HasLitSets LitSet [Literal] where
   flatten tt ls = case ls of
      LEnum ls'   -> CheckPass $ flattenSS ls'
      ls1 ::+ ls2 -> case flatten tt ls1 of
         CheckPass ls1' -> case flatten tt ls2 of
            CheckPass ls2' -> flatten tt $ LEnum
               $ unionSS (list2SS ls1') (list2SS ls2') 
            CheckFail msg  -> CheckFail msg 
         CheckFail msg  -> CheckFail msg
      ls1 ::^ ls2 -> case flatten tt ls1 of
         CheckPass ls1' -> case flatten tt ls2 of
            CheckPass ls2' -> flatten tt $ LEnum
               $ sectSS (list2SS ls1') (list2SS ls2') 
            CheckFail msg  -> CheckFail msg 
         CheckFail msg  -> CheckFail msg
      ls1 ::- ls2 -> case flatten tt ls1 of
         CheckPass ls1' -> case flatten tt ls2 of
            CheckPass ls2' -> flatten tt $ LEnum
               $ diffSS (list2SS ls1') (list2SS ls2') 
            CheckFail msg  -> CheckFail msg 
         CheckFail msg  -> CheckFail msg
      LComp l vgs       -> 
         let unfold :: [VarGen] -> [Substitution]
             unfold vgs = case vgs of
                []              -> [NullSub]
                (v :<- t) : vgs -> 
                   [s :>-> s'
                   | s <- [v :->- c 
                          | c <- evalType' tt t],
                     s' <- unfold (ground s vgs)
                   ]
         in CheckPass [ground s l | s <- unfold vgs]
\end{haddock}

\subsubmodule{Collecting Orderings} %%%%%%%%

\begin{haddock}
instance HasTypes LitSet where
   getOrderings os ls = case ls of
      LEnum ls'   -> os
      ls1 ::+ ls2 -> getOrderings (getOrderings os ls1) ls2
      ls1 ::^ ls2 -> getOrderings (getOrderings os ls1) ls2
      ls1 ::- ls2 -> getOrderings (getOrderings os ls1) ls2
      LComp l vgs -> foldl getOrderings os vgs
\end{haddock}

\subsubmodule{Qualification} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{haddock}
instance Qualifiable Constant where
   qualify n c = case c of
      CNamed n' p   -> CNamed (n ++ "." ++ n') p
      CIntegral _ _ -> c
      CString _ _   -> c
\end{haddock}

