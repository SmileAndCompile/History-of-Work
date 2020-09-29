% Descriptions.#ext
% This file was produced from Descriptions.lit

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

\module{Descriptions} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlightModule{Descriptions} implements descriptions
for Decisive Plausible Logic.

\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, 
             TypeSynonymInstances, OverlappingInstances #-}
\end{code}

\begin{code}
module DPL.Descriptions (
      Description(..), descriptionP, labelCheck, 
      groundCheck, obviateCheck, generateRuns, 
      generateAxioms, loadDescription, assertCheck
  ) where
\end{code}

\begin{code}
import Data.List
import Data.Maybe
\end{code}

\begin{code}
import ABR.Parser hiding (Tag)
import ABR.Parser.Checks
import ABR.Showing
import ABR.Control.Check
import ABR.List
import ABR.Data.BSTree
import ABR.Data.SparseSet
import ABR.DeepSeq
import ABR.Parser.Lexers
\end{code}

\begin{code}
import DPL.Constants
import DPL.Variables
import DPL.Arguments
import DPL.Atoms
import DPL.Literals
import DPL.LitSets
import DPL.Formulas
import DPL.Rules
import DPL.Priorities
import DPL.Tags
import DPL.Types
import DPL.TypeDecs
import DPL.DPLLexer
\end{code}


\submodule{Data type definitions} %%%%%%%%%%%%%%%%%%%%%%%%

A plausible \highlighttt{Description} consists of:

\begin{itemize}
   \item a list of new type declarations 
      (\highlighttt{dnt});
   \item a set of axioms\index{axioms} (\highlighttt{dax});
   \item a set of default facts (\highlighttt{ddef});
   \item a set of plausible rules (\highlighttt{drp});
   \item a set of defeater rules (\highlighttt{drd});
   \item and a set of priorities (\highlighttt{dpri}).
\end{itemize}

\noindent Additionally we associate
with a description: 

\begin{itemize}
   \item a mapping from names to fully evaluated types 
      (\highlighttt{dtt});
   \item the sets of input literals to use as inputs 
      (\highlighttt{din});
   \item the sets of input literals to ignore in
      combination (\highlighttt{dig});
   \item the sets of input literals specifically asserted
      (\highlighttt{das});
   \item a set of tagged cnf-formulas to attempt proofs
      of (\highlighttt{dout}); 
   \item a name to uniquely identify descriptions and
      theories (\highlighttt{dnam}).
   \item some modules to import (\highlighttt{dimp}); and
   \item some predefined tokens in some target language
      (\highlighttt{dpre}).
\end{itemize}

\begin{code}
data Description = Description {
      dnt  :: [NewTypeDec],
      dtt  :: TypeTable,
      dat  :: [AtomTypeDec],
      dax  :: [Clause],
      ddef :: [Default],
      drp  :: [Rule],
      drd  :: [Rule],
      dpri :: [Priority],
      din  :: [[Literal]],
      dig  :: [[Literal]],
      das  :: [[Literal]],
      dout :: [(TaggedCnfFormula Literal, Maybe String)],
      dnam :: String,
      dimp :: [String],
      dpre :: [String]
   }
\end{code}

\noindent A {\tt Statement} is an intermediate data
structure used while parsing.

\begin{code}
data Statement =   NT  NewTypeDec
                 | AT  AtomTypeDec
                 | Ax  Clause
                 | Def Default
                 | Rul Rule
                 | Pri Priority
                 | In  [Literal]
                 | Ig  [[Literal]]
                 | As  [Literal]
                 | Out (TaggedCnfFormula Literal,
                        Maybe String)
                 | Nam String
                 | Imp String
                 | Pre String
\end{code}

\submodule{Parser} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Gode generation hints} %%%%%%

\syntrax{nameDec}

\begin{code}
nameDecP :: Parser String
nameDecP = 
        literalP "lName" "name" 
     *> literalP "symbol" "{"
     *> tagP "uName" 
    <*  nofail (literalP "symbol" "}")
     @> (\(_,n,_) -> n)
\end{code}

\syntrax{importDec}

\begin{code}
importDecP :: Parser String
importDecP = 
        literalP "lName" "import" 
     *> literalP "symbol" "{"
     *> tagP "string" 
    <*  nofail (literalP "symbol" "}")
     @> (\(_,cs,_) -> cs)
\end{code}

\syntrax{predefDec}

\begin{code}
predefDecP :: Parser String
predefDecP = 
        literalP "lName" "predefined" 
     *> literalP "symbol" "{"
     *> tagP "string" 
    <*  nofail (literalP "symbol" "}")
     @> (\(_,cs,_) -> cs)
\end{code}

\subsubmodule{Inputs} %%%%%%

\syntrax{someLits}

\begin{code}
someLitsP :: Parser [Literal]
someLitsP = 
       literalP "symbol" "{"
     *> pLiteralP 
    <*> many (   literalP "symbol" ","
              *> nofail pLiteralP
        )
    <*  nofail (literalP "symbol" "}")
     @> cons
\end{code}

\syntrax{input}

\begin{code}
inputP :: Parser [Literal]
inputP = 
      literalP "lName" "input" 
   *> someLitsP
\end{code}

\subsubmodule{Ignores} %%%%%%

\syntrax{ignore}

\begin{code}
ignoreP :: Parser [[Literal]]
ignoreP = 
       literalP "lName" "ignore" 
    *> optional (
             tagP "cardinal"
          <* nofail (literalP "lName" "of")
       )
   <*> someLitsP
    @> (\(ns,ls) -> case ns of
          []          -> [ls]
          [(_,n,(l,_))] -> 
             let n' = read n
             in if n' > length ls then
                error $ "Not enough literals for combina\
                   \tions at line " ++ show (l + 1)
             else
                combinations n' ls
       )
\end{code}

\subsubmodule{Asserts} %%%%%%

\syntrax{assert}

\begin{code}
assertP :: Parser [Literal]
assertP = 
       literalP "lName" "assert" 
    *> someLitsP
\end{code}

\subsubmodule{Outputs} %%%%%%

\syntrax{output}

\begin{code}
outputP :: Parser (TaggedCnfFormula Literal, Maybe String)
outputP =   
   (   literalP "lName" "output" 
    *> (    literalP "symbol" "{"
         *> nofail taggedFormulaP 
        <*> optional (
                  literalP "symbol" ","
               *> tagP "string"
            )
        <* nofail (literalP "symbol" "}")
       )
   ) @> (\(tf,ss) -> (tf, case ss of
           []        -> Nothing
           [(_,s,_)] -> Just s
        ))
\end{code}

\subsubmodule{Descriptions} %%%%%%

\noindent \highlighttt{descriptionP} parses a description
and the input, ignore and output specifications.

\syntrax{statement}

\begin{code}
statementP :: Parser Statement
statementP = (
          newTypeDecP  @> NT
      <|> atomTypeDecP @> AT
      <|> defaultP     @> Def
      <|> nameDecP     @> Nam
      <|> importDecP   @> Imp
      <|> predefDecP   @> Pre
      <|> inputP       @> In
      <|> ignoreP      @> Ig
      <|> assertP      @> As
      <|> outputP      @> Out 
      <|> ruleP        @> Rul
      <|> priorityP    @> Pri
      <|> clauseP      @> Ax
   ) <* nofail (literalP "symbol" ".")
\end{code}

\syntrax{description}

\begin{code}
descriptionP :: Parser Description
descriptionP =
      many statementP
      @> fix . foldl addStatement 
            (Description [] emptyBST [] [] defaultDefaults 
               [] [] [] [] [] [] [] "/DEFAULT/" [] [])
   where 
   defaultDefaults = [
         Default (Neg (Prop "<" [Var (Variable "x"), 
                                 Var (Variable "y")])),
         Default (Neg (Prop "<=" [Var (Variable "x"), 
                                  Var (Variable "y")])),
         Default (Neg (Prop "==" [Var (Variable "x"), 
                                  Var (Variable "y")]))
      ]
   addStatement :: Description -> Statement
      -> Description
   addStatement d s = case s of
      NT  nt  -> d {dnt = nt : dnt d}
      AT  at  -> d {dat = at : dat d}
      Ax  c   -> d {dax = c : dax d} 
      Rul r   -> case r of
         Plaus  {} -> d {drp = r : drp d} 
         Defeat {} -> d {drd = r : drd d} 
      Pri p   -> d {dpri = p : dpri d} 
      In  ls  -> d {din = ls : din d} 
      Ig  lss -> d {dig = lss ++ dig d} 
      As  ls  -> d {das = ls : das d} 
      Out ts  -> d {dout = ts : dout d}
      Def df  -> d {ddef = df : ddef d}
      Nam n   -> d {dnam = n}
      Imp cs  -> d {dimp = cs : dimp d}
      Pre cs  -> d {dpre = cs : dpre d}
   fix :: Description -> Description
   fix d = 
      let cs :: SparseSet Constant
          cs = getConstants d emptySS
          u :: NewTypeDec
          u = TypeName "Universe" := TEnum (list2SS 
             (map Const (flattenSS cs)))
          os = concat [
                  foldl getOrderings [] (dnt d),
                  foldl getOrderings [] (dat d),
                  foldl getOrderings [] (dax d),
                  foldl getOrderings [] (drp d),
                  foldl getOrderings [] (drd d)
               ]
          d' :: Description
          d' = d {
                dnt  = snub $ u : dnt d ,
                dat  = snub $ dat d,
                dax  = snub $ dax d ++ assertOrderings os,
                drp  = snub $ drp d,
                drd  = snub $ drd d,
                dpri = snub $ dpri d,
                din  = snub $ din d,
                dig  = snub $ dig d,
                das  = reverse $ das d,
                dout = snub $ dout d
             }
      in d' {
            dtt  = case evalTypes (map (\(n := t) -> (n,t))
               (dnt d')) of
            Left tt   -> tt
            Right msg -> error msg
         }
\end{code}

\submodule{Semantic checks} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{labelCheck} is a {\tt Check} that all of the
labels occuring in priorities also occur in rules. It passes
the description through if OK, or returns an error message.

\begin{code}
labelCheck :: Check Description Description String
labelCheck d = 
   let rls = snub $ map rlbl $ drp d ++ drd d
       pls = snub $ concatMap (\(l :> l') -> [l, l']) $ 
          dpri d
       undefs = filter (`notElem` rls) pls 
   in case undefs of
      [] -> CheckPass d
      _  -> CheckFail $ "Undefined labels: " ++ 
         unwords undefs
\end{code}

\submodule{Asserting Orderings} %%%%%%%%%%%%%%%%%%%

\begin{code}
assertOrderings :: [[Constant]] -> [Clause]
assertOrderings = concatMap ao
   where
   ao :: [Constant] -> [Clause]
   ao cs = case cs of
      []     -> []
      c : cs -> eq c : le c c : map (lt c) cs ++ 
                map (le c) cs ++ ao cs
   eq c    = mc $ Pos $ Prop "==" [Const c, Const c]
   lt c c' = mc $ Pos $ Prop "<"  [Const c, Const c']
   le c c' = mc $ Pos $ Prop "<=" [Const c, Const c']
   mc = Clause . LEnum . mkSS 
\end{code}

\submodule{Grounding all variables} %%%%%%%%%%%%%%%%%%%%%%

The \highlighttt{groundCheck} passes a description
if it can replace all axioms\index{axioms} and rules with ground
instances generated from the constants appearing in
the description. If there are variables, but no
constants the check fails.

\begin{code}
groundCheck :: Check Description Description String
groundCheck d 
   | not (hasVariables d) = CheckPass d
   | not (hasConstants d) = CheckFail "Can not ground desc\
      \ription. There are variables, but there are no cons\
      \tants."
   | otherwise = CheckPass d {
           dax = nub $ 
              concatMap (instances (dtt d) (dat d)) 
              (dax d),
           drp = nub $
              concatMap (instances (dtt d) (dat d))
              (drp d),
           drd = nub $
              concatMap (instances (dtt d) (dat d))
              (drd d),
           ddef = nub $
              concatMap (instances (dtt d) (dat d))
              (ddef d),
           din = nub $
              concatMap (instances (dtt d) (dat d))
              (din d),
           dig = nub $
              concatMap (instances (dtt d) (dat d))
              (dig d),
           das = nub $ map (nub . 
              concatMap (instances (dtt d) (dat d)))
              (das d),
           dout = nub $
              concatMap (instances (dtt d) (dat d))
              (dout d)
        }
\end{code}

\submodule{Checking the asserts against the inputs} %%%%%%%%%%%%%%%%%%%%%%

The \highlighttt{assertCheck} passes a description
if every assert (after grounding) completely covers all
of the declared inputs.

\begin{code}
assertCheck :: Check Description Description String
assertCheck d
   | null (das d) = CheckPass d
   | otherwise    = case gen (din d) (dig d) of
      []     -> CheckPass d
      r1 : _ -> 
         if and [null (ls \\ ls') && null (ls' \\ ls)
                | let ls = map pos r1, 
                  ls' <- map (map pos) (das d)]
         then CheckPass d
         else CheckFail "Assert does not match the\
            \ declared inputs. Hint: Inspect the\
            \ grounded description to fix this."
\end{code}

\submodule{Removing strictly useless rules} %%%%%%%%%%%%%%%%%%%%%%

If a rule has a literal in its antecedent\index{antecedent} that is
asserted as a fact, then its strict provability is
assured and it can be omitted from the antecedent\index{antecedent}.
If the negation of the literal in an antecedent\index{antecedent} has been
asserted as a fact, then that rule is useless and
can be omitted.  

The \highlighttt{obviateCheck} passes a description
after removing such obviously true literals from 
antecedents\index{antecedent} and such obviously useless rules.

\begin{code}
obviateCheck :: Check Description Description String
obviateCheck d = 
   let facts = [l | Or [l] <- dax d]
       defaults = [l | Default l <- ddef d]
       test :: Literal -> Maybe Bool
       test l 
          | l `elem` facts     = Just True
          | neg l `elem` facts = Just False
          | l `elem` defaults  = Just True
          | neg l `elem` defaults  = Just False
          | otherwise          = Nothing
       check :: [Literal] -> Maybe [Literal]
       check as = case as of
          []     -> Just []
          a : as' -> case test a of
             Just True  -> check as'
             Just False -> Nothing
             Nothing    -> case check as' of
                Nothing   -> Nothing
                Just as'' -> Just (a : as'')
       obviateP, obviateD :: Rule -> Maybe Rule
       obviateP (Plaus l c (Right as)) = case check as of
          Nothing  -> Nothing
          Just as' -> Just (Plaus l c (Right as'))
       obviateD (Defeat l c (Right as)) = case check as of
          Nothing  -> Nothing
          Just as' -> Just (Defeat l c (Right as'))
   in CheckPass d {
          drp = nub $ mapMaybe obviateP $ drp d,
          drd = nub $ mapMaybe obviateD $ drd d
       }
\end{code}

\submodule{Generating runs} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{generateRuns}~$d$ returns the list of
extra axioms\index{axioms} given the inputs and ignores in
$d$. If there are asserts, they are used instead of the inputs/ignores.

\begin{code}
generateRuns :: Description -> [[Literal]]
generateRuns d 
   | null (das d) = gen (din d) (dig d) 
   | otherwise = das d
\end{code}

\noindent
{\tt gen}~$\mathit{inss}~\mathit{igss}$
returns the lists of extra axioms\index{axioms} generated from remaining
sets of mutually exclusive inputs $\mathit{inss}$ and the
remaining ignore sets $\mathit{igss}$. This does not use the 
asserts.

\begin{code}
gen :: [[Literal]] -> [[Literal]] -> [[Literal]]
gen inss igss = case inss of
   []          -> [[]]
   ins : inss' -> 
      let qss = case ins of 
             [q] -> [[q], [neg q]]
             _   -> (map (\(b,e,a) -> 
                reverse (map neg b) ++ [e] ++ map neg a)
                . fragments) ins
          genSeq igss qs = case qs of
             []      -> gen inss' igss
             q : qs' -> case applyIgnores q igss of
                Nothing    -> []
                Just igss' ->
                   map (q :) $ genSeq igss' qs'
      in concatMap (genSeq igss) qss
\end{code}

\noindent
{\tt applyIgnores}~$l~\mathit{igss}$ returns either:
{\tt Nothing}, when the literal $l$ is knocked out
by an element of $\mathit{igss}$ that equals $[l]$; or
{\tt Just}~$\mathit{igss}'$, where $\mathit{igss}'$
contains any elements of $\mathit{igss}$ not knocked
out because they contain $\neg l$.

\begin{code}
applyIgnores :: Literal -> [[Literal]] 
   -> Maybe [[Literal]]
applyIgnores l igss = case igss of
   []                    -> Just []
   igs : igss'
      | igs == [l]       -> Nothing
      | neg l `elem` igs -> applyIgnores l igss'
      | otherwise        -> case applyIgnores l igss' of
         Nothing     -> Nothing
         Just igss'' -> Just $ delete l igs : igss''
\end{code}

\noindent \highlighttt{generateAxioms}~$d$~$ls$
returns the description $d$ augmented by the axioms\index{axioms}
$ls$.

\begin{code}
generateAxioms :: Description -> [Literal] -> Description
generateAxioms d ls = d {
      dax = snub $ dax d ++ map (Or . (:[])) ls,
      din = [],
      dig = []
   }
\end{code}

\submodule{Loading a description} %%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{loadDescription}~$\mathit{path}$ reads the
description file from $\mathit{path}$ and processes it 
ready for subsequent proofs, for example with 
\verb+Inference.doProof+. It either returns the description
or an error message.

\begin{code}
loadDescription :: FilePath -> 
   IO (CheckResult Description String)
loadDescription path = do
   source <- readFile path
   case (checkParse ((dropWhite . nofail .total) lexerL)
      (total descriptionP) &? labelCheck) source of
      CheckFail msg -> return $ CheckFail msg
      CheckPass d   -> return $ (groundCheck &? 
         flatten (dtt d) &? obviateCheck) d
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Showing} %%%%%%%

\begin{code}
instance Show Description where
\end{code}

\begin{code}
   showsPrec p d =
        showString "% new type declarations:\n"
      . showWithTerm ".\n" (dnt d) 
      . showString "% evaluated types:\n"
      . showWithTerm ".\n" (map (uncurry (:=)) 
           (flattenBST  (dtt d)))
      . showString "% atom type assertions:\n"
      . showWithTerm ".\n" (dat d)
      . showString "% axioms:\n"
      . showWithTerm ".\n" (dax d) 
      . showString "% default facts:\n"
      . showWithTerm ".\n" (ddef d) 
      . showString "% plausible rules:\n"
      . showWithTerm ".\n" (drp d) 
      . showString "% defeaters:\n"
      . showWithTerm ".\n" (drd d)
      . showString "% priorities:\n"
      . showWithTerm ".\n" (dpri d)
      . showString "% inputs:\n"
      . showWithTerm ".\n" (din d)
      . showString "% ignores:\n"
      . showWithTerm ".\n" (dig d)
      . showString "% asserts:\n"
      . showWithTerm ".\n" (das d)
      . showString "% outputs:\n"
      . showWithTerm ".\n" (dout d)
      . showString "% name: "
      . showString (dnam d)
      . showString "\n% imports:\n"
      . shows (dimp d)
      . showString "% predefined tokens:\n"
      . shows (dpre d)
\end{code}


\subsubmodule{Collecting constants} %%%%%%%%

\begin{code}
instance HasConstants Description where
\end{code}

\begin{code}
    getConstants d cs = 
       let cs1 = foldr getConstants cs $ dnt d
           cs2 = foldr getConstants cs1 $ ranBST $ dtt d
           cs3 = foldr getConstants cs2 $ dat d
           cs4 = foldr getConstants cs3 $ dax d
           cs5 = foldr getConstants cs4 $ drp d
           cs6 = foldr getConstants cs5 $ drd d
           cs7 = foldr getConstants cs6 $ concat $ din d
           cs8 = foldr getConstants cs7 $ concat $ dig d
           cs9 = foldr (getConstants . fst) cs8 (dout d)
           cs10 = foldr getConstants cs9 $ ddef d
       in cs10
\end{code}

\subsubmodule{Collecting variables} %%%%%%%%

\begin{code}
instance HasVariables Description where
\end{code}

\begin{code}
    getVariables d vs = 
       let vs1 = foldr getVariables vs $ dnt d
           vs2 = foldr getVariables vs1 $ ranBST $ dtt d
           vs3 = foldr getVariables vs2 $ dat d
           vs4 = foldr getVariables vs3 $ dax d
           vs5 = foldr getVariables vs4 $ drp d
           vs6 = foldr getVariables vs5 $ drd d
           vs7 = foldr getVariables vs6 $ concat $ din d
           vs8 = foldr getVariables vs7 $ concat $ dig d
           vs9 = foldr (getVariables . fst) vs8 (dout d)
           -- vs10 = foldr getVariables vs9 $ ddef d
           -- Don't count the variables in defaults as they
           -- can falsely indicate a description needs 
           -- grounding when it really doesn't.
       in vs9
\end{code}

\subsubmodule{Collecting Atoms} %%%%%%%%

\begin{code}
instance HasAtoms Description where
\end{code}

\begin{code}
    getAtoms d as = 
       let as1 = foldr getAtoms as $ dax d
           as2 = foldr getAtoms as1 $ drp d
           as3 = foldr getAtoms as2 $ drd d
           as4 = foldr getAtoms as3 $ concat $ din d
           as5 = foldr getAtoms as4 $ concat $ dig d
           as6 = foldr (getAtoms . fst) as5 (dout d)
       in as6
\end{code}

\subsubmodule{Flattening} %%%%%%%%

\begin{code}
instance HasLitSets Description Description where
\end{code}

\begin{code}
    flatten tt d = 
       let chax = map (flatten tt) $ dax d
           chrp = map (flatten tt) $ drp d
           chrd = map (flatten tt) $ drd d
           fs = [msg | CheckFail msg <- chax] ++ 
                [msg | CheckFail msg <- chrp] ++ 
                [msg | CheckFail msg <- chrd]
           pax = [ax | CheckPass ax <- chax]
           prp = [rp | CheckPass rp <- chrp]
           prd = [rd | CheckPass rd <- chrd]
       in if null fs then
          CheckPass d {dax = pax,
                       drp = prp,
                       drd = prd
                    }
       else 
          CheckFail $ unlines fs
\end{code}

\subsubmodule{Instantiating} %%%%%%%%

\begin{code}
instance Instantiable Argument where
\end{code}

\begin{code}
   instantiate tt ats a = [(a, NullSub)]
\end{code}

\begin{code}
instance Instantiable Type where
\end{code}

\begin{code}
   instantiate tt ats t = case t of 
      TEnum as -> 
         [(TEnum (list2SS as'), s)
         | (as', s) <- instantiate tt ats (flattenSS as)
         ]
      TName n  -> [(TName n, NullSub)]
      t1 :+ t2 ->
         [(t1' :+ t2', s1 :>-> s2)
         | (t1', s1) <- instantiate tt ats t1,
           (t2', s2) <- instantiate tt ats (ground s1 t2)
         ]
      t1 :^ t2 -> 
         [(t1' :^ t2', s1 :>-> s2)
         | (t1', s1) <- instantiate tt ats t1,
           (t2', s2) <- instantiate tt ats (ground s1 t2)
         ]
      t1 :- t2 -> 
         [(t1' :- t2', s1 :>-> s2)
         | (t1', s1) <- instantiate tt ats t1,
           (t2', s2) <- instantiate tt ats (ground s1 t2)
         ]
\end{code}

\begin{code}
instance Instantiable VarGen where
\end{code}

\begin{code}
   instantiate tt ats (v :<- t) = 
      [(v :<- t', s) 
      | (t', s) <- instantiate tt ats t
      ]
\end{code}

\begin{code}
instance Instantiable Default where
\end{code}

\begin{code}
   instantiate tt ats (Default l) = 
      [(Default l', s) 
      | (l', s) <- instantiate tt ats l
      ]
\end{code}

\begin{code}
instance Instantiable [(Argument, VarGen)] where
\end{code}

\begin{code}
   instantiate tt ats avgs = case avgs of
      []                   -> [([], NullSub)]
      (a, vg@(v :<- t)) : avgs' -> case a of
         Const c ->
            [((a, vg) : avgs, s') 
            | c `elem` evalType' tt t,
              let s = v :->- c,
              (avgs, s') <- instantiate tt ats 
                 [(a, ground s vg) | (a,vg) <- avgs']
            ]
         Var v'@(Variable n)
            | "_VG_" `isPrefixOf` n -> 
               [((a, vg) : avgs, s) 
               | (avgs, s) <- instantiate tt ats  avgs'
               ]
            | otherwise             -> 
               [((ground s a, vg) : avgs, s :>-> s'')
               | (s,s') <- [(v' :->- c, v :->- c)
                           | c <- evalType' tt t],
                 (avgs, s'') <- instantiate tt ats 
                    [(ground s a, ground (s :>-> s') vg) 
                    | (a,vg) <- avgs']
               ]
\end{code}

\begin{code}
instance Instantiable Atom where
\end{code}

\begin{code}
   instantiate tt ats (Prop n as) = 
      let len :: Int
          len = length as
          tss :: [[VarGen]]
          tss = [ts | AtomTypeDec n' ts <- ats, 
                      n' == n, length ts == len]
          ts :: [VarGen]
          ts = case tss of
             []   -> [Variable ("D_" ++ show i) :<- u
                     | let u = TName (TypeName "Universe"),
                       i <- [0 .. len - 1]
                     ]
             [ts] -> ts
             _    -> error $ "Too many type assertions for\
                \ " ++ n ++ "/" ++ show n
      in [(Prop n as', s)
         | (avgs,s) <- instantiate tt ats (zip as ts),
           let as' = map fst avgs
         ]
\end{code}

\begin{code}
instance Instantiable Literal where
\end{code}

\begin{code}
   instantiate tt ats l = case l of
      Pos a -> [(Pos a', s)
               | (a', s) <- instantiate tt ats a] 
      Neg a -> [(Neg a', s)
               | (a', s) <- instantiate tt ats a] 
\end{code}

\begin{code}
instance (Instantiable a, Groundable a) =>
   Instantiable [a] where
\end{code}

\begin{code}
   instantiate tt ats xs = case xs of
      []     -> [([], NullSub)]
      x : xs -> [ (x' : xs', s :>-> s')
                | (x',s) <- instantiate tt ats x,
                  (xs', s') <- instantiate tt ats 
                     (map (ground s) xs)
                ]
\end{code} 

\begin{code}
instance Instantiable LitSet where
\end{code}

\begin{code}
   instantiate tt ats ls = case ls of
      LEnum ls    -> 
         [(LEnum (list2SS ls'), s)
         | (ls', s) <- instantiate tt ats (flattenSS ls)
         ]
      ls1 ::+ ls2 -> 
         [(ls1' ::+ ls2', s1 :>-> s2)
         | (ls1', s1) <- instantiate tt ats ls1,
           (ls2', s2) <- instantiate tt ats (ground s1 ls2)
         ]
      ls1 ::^ ls2 -> 
         [(ls1' ::^ ls2', s1 :>-> s2)
         | (ls1', s1) <- instantiate tt ats ls1,
           (ls2', s2) <- instantiate tt ats (ground s1 ls2)
         ]
      ls1 ::- ls2 -> 
         [(ls1' ::- ls2', s1 :>-> s2)
         | (ls1', s1) <- instantiate tt ats ls1,
           (ls2', s2) <- instantiate tt ats (ground s1 ls2)
         ]
      LComp l vgs -> 
         [ (LComp l' vgs', s :>-> s')
         | (l', s) <- instantiate tt ats l,
           (vgs', s') <- instantiate tt ats (ground s vgs)
         ]
\end{code} 

\begin{code}
instance Instantiable Clause where
\end{code}

\begin{code}
   instantiate tt ats c = case c of       
      Clause ls -> 
         [(Clause ls', s)
         | (ls', s) <- instantiate tt ats ls
         ]
      Or ls     -> 
         [(Or ls', s)
         | (ls', s) <- instantiate tt ats ls
         ]
\end{code}

\begin{code}
instance Instantiable Rule where
\end{code}

\begin{code}
   instantiate tt ats r = case r of 
      Strict   c a -> case a of
         Left  ls -> 
            [(Strict c' (Left ls'), s :>-> s')
            | (ls', s) <- instantiate tt ats ls,
              (c', s') <- instantiate tt ats (ground s c)
            ]
         Right ls -> 
            [(Strict c' (Right ls'), s :>-> s')
            | (ls', s) <- instantiate tt ats ls,
              (c', s') <- instantiate tt ats (ground s c)
            ]
      Plaus  l c a -> case a of
         Left  ls -> 
            [(Plaus l c' (Left ls'), s :>-> s')
            | (ls', s) <- instantiate tt ats ls,
              (c', s') <- instantiate tt ats (ground s c)
            ]
         Right ls -> 
            [(Plaus l c' (Right ls'), s :>-> s')
            | (ls', s) <- instantiate tt ats ls,
              (c', s') <- instantiate tt ats (ground s c)
            ]
      Defeat l c a -> case a of
         Left  ls -> 
            [(Defeat l c' (Left ls'), s :>-> s')
            | (ls', s) <- instantiate tt ats ls,
              (c', s') <- instantiate tt ats (ground s c)
            ]
         Right ls -> 
            [(Defeat l c' (Right ls'), s :>-> s')
            | (ls', s) <- instantiate tt ats ls,
              (c', s') <- instantiate tt ats (ground s c)
            ]
\end{code}

\begin{code}
instance Instantiable (TaggedCnfFormula Literal, 
   Maybe String) where
   instantiate tt ats (Tag tag lss, ms) = case ms of
      Just _ -> error "Can't instantiate outout with \
         \string"
      Nothing -> [((Tag tag lss', Nothing), s) 
                 | (lss',s) <- instantiate tt ats lss]
\end{code}
