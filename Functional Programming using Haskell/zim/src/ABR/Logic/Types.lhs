% Types.#ext
% This file was produced from Types.lit

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

\module{Types} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlightModule{Types} implements types for CDL.

\begin{code}
module CDL.Types (
      TypeName(..), Type(..), TypeTable, typeNameP, typeP,
      evalTypes, evalType, evalType', HasTypes(..)
   ) where
\end{code}

\begin{code}
import Data.List
import Control.Monad
\end{code}

\begin{code}
import ABR.Parser
import ABR.Data.SparseSet
import ABR.Parser.Pos
import ABR.Showing
import ABR.Data.BSTree
import ABR.Data.List
\end{code}

\begin{code}
import CDL.Constants
import CDL.Variables
import CDL.Arguments
import CDL.Utils
import CDL.Kinds
import CDL.Qualification
\end{code}

\submodule{Data types} %%%%%%%%%%%%%

A \highlighttt{TypeName} is a string.

\begin{code}
data TypeName = 
   TypeName {
      tnNam :: String,
      tnPos :: Pos
   }
\end{code}

\noindent A \highlighttt{Type} is either

\begin{itemize}
   \item an enumerated set of arguments\index{arguments!in atoms}
      (\highlighttt{TEnum});
   \item an enumerated set of constants that defines a total ordering
      (\highlighttt{TOrd});
   \item an intergal subrange (\highlighttt{TSubr});
   \item a named type (\highlighttt{TName}); 
   \item the union of two types (\highlighttt{TUnion}); 
   \item the intersection of two types (\highlighttt{TSect}); 
   \item the difference between two types (\highlighttt{TDiff}); or
   \item the cartesian product of a sequence of types (\highlighttt{TCart}).
\end{itemize}

\begin{code}
data Type =   
     TEnum {
        tEnum :: SparseSet Argument,
        tPos  :: Pos
     }
   | TOrd {
        tOrd :: [Constant],
        tPos  :: Pos
     }
   | TSubr {
        tFirst  :: Integer,
        tSecond :: Maybe Integer,
        tLimit  :: Integer,
        tPos    :: Pos
     }
   | TName {
        tName :: TypeName,
        tPos  :: Pos
     }
   | TUnion {
        tArg1 :: Type,
        tArg2 :: Type,
        tPos  :: Pos
     }
   | TSect {
        tArg1 :: Type,
        tArg2 :: Type,
        tPos  :: Pos
     }
   | TDiff {
        tArg1 :: Type,
        tArg2 :: Type,
        tPos  :: Pos
     }
   | TCart {
        tArgs :: [Type],
        tPos  :: Pos
     }
\end{code}

\noindent A \highlighttt{TypeTable} is a mapping from named
types to their values.

\begin{code}
type TypeTable = BSTree TypeName Type
\end{code}

\submodule{Parsers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent\syntrax{typeName}

\noindent\highlighttt{typeNameP} recognises type names.

\begin{code}
typeNameP :: Parser TypeName
typeNameP = tagP "uName" 
            @> (\(_,n,p) -> TypeName n p)
\end{code}

\noindent\syntrax{type}

\noindent \highlighttt{typeP} recognises types.

\begin{code}
typeP :: Parser Type
typeP = 
       typeTermP 
   <*> typeEndP
   @> (\(t,f) -> f t)
\end{code}

\noindent\syntrax{typeEnd}

\begin{code}
typeEndP :: Parser (Type -> Type)
typeEndP =
           literalP "symbol" "+"
        *> nofail' "type expected" typeTermP
       <*> typeEndP
        @> (\(t, f) ->
               (\t' -> TUnion t' t (getPos t')) . f)
   <|>     literalP "symbol" "^"
        *> nofail' "type expected" typeTermP
       <*> typeEndP
        @> (\(t, f) ->
               (\t' -> TSect t' t (getPos t')) . f)
   <|>     literalP "symbol" "-"
        *> nofail' "type expected" typeTermP
       <*> typeEndP
        @> (\(t, f) ->
               (\t' -> TDiff t' t (getPos t')) . f)
   <|> epsilonA
       #> id
\end{code}

\noindent\syntrax{typeTerm}

\begin{code}
typeTermP :: Parser Type
typeTermP = 
       typeFactorP 
   <*> many (
             literalP "symbol" "*"
          *> typeFactorP
       )
   @> (\(t,ts) -> if null ts
          then t
          else TCart (t:ts) (getPos t))
\end{code}

\noindent\syntrax{typeFactor}

\begin{code}
typeFactorP :: Parser Type
typeFactorP = 
           literalP "symbol" "("
       <*> nofail' "type expected" typeP
       <*  nofail (literalP "symbol" ")")
       @> (\((_,_,p),t) -> t {tPos = p})
   <|>     literalP "symbol" "{"
       <*> optional (
                  argumentP
              <*> many (
                        literalP "symbol" ","
                     *> nofail' "argument expected" 
                           argumentP
                  )
           )
       <*  nofail (literalP "symbol" "}")
       @> (\((_,_,p),es) -> case es of 
             []  -> TEnum emptySS p
             [x] -> TEnum (list2SS $ cons x) p
          )
   <|>     literalP "symbol" "["
       <*> integerP
       <*> optional (
                 literalP "symbol" ","
              *> integerP
           )
       <*>  literalP "symbol" ".."
        *> nofail' "integer expected" integerP
       <* nofail (literalP "symbol" "]")
       @> (\((_,_,p),((i,_),(ips,   (i',_)     ))) -> TSubr {
             tFirst = i,
             tSecond = case ips of
                []      -> Nothing
                [(i,_)] -> Just i,
             tLimit = i',
             tPos = p
          })
   <|>     literalP "symbol" "["
       <*> optional (
                  constantP
              <*> many (
                        literalP "symbol" ","
                     *> nofail' "constant expected" 
                           constantP
                  )
           )
       <* literalP "symbol" "]"
       @> (\((_,_,p),es) -> case es of 
             []  -> TOrd [] p
             [x] -> TOrd (cons x) p
          )
   <|> typeNameP
       @> (\tn-> TName tn (getPos tn))
\end{code}

\submodule{Evaluating the named types} %%%%%%%%%%%%%%%%%%%

A description will contain zero or more declarations of
named types. These types may be defined in terms of each 
other. Before anything else happens these named types must
be fully evaluated. \highlighttt{evalTypes}~$\mathit{nts}$,
where $\mathit{nts}$ is a list of pairs $(n,t)$ associating
a name with a type, returns either {\tt Left} $\mathit{tt}$
or {\tt Right} $\mathit{msg}$, where $\mathit{tt}$ is a
{\tt TypeTable} mapping names to fully evaluated types and
$\mathit{msg}$ is an error message.

\begin{code}
evalTypes :: String -> [(TypeName, Type)] -> IO TypeTable
evalTypes source nts = 
   let ns = map fst nts 
       dups = nub [n | (n:ns') <- tails ns, n `elem` ns']
       tt = pairs2BST nts
   in if not (null dups)
      then fatalError' (getPos $ head dups) ("Type(s) " ++ 
         show (head dups) ++ concatMap (\n -> ", " ++
         show n) (tail dups) ++ " is/are multiply \
         \defined") source
      else foldM (evaln []) tt ns 
   where  
   evaln :: 
      [TypeName] -> TypeTable -> TypeName -> IO TypeTable
   -- evaln ns tt n forces the full evaluation of the type 
   -- named n, updating tt.
   -- ns = list of type names already pending evaluation
   evaln ns tt n
      | n `elem` ns = fatalError' (getPos n) ("Definition \
         \ of type " ++ show n ++ " is cyclic.") source
      | otherwise   = case lookupBST n tt of
            Nothing -> fatalError' (getPos n) ("Type " ++
               show n ++ " is used but not declared.")
               source
            Just t  -> do
               (t',tt') <- evalt t tt (n : ns)
               return $ updateBST const n t' tt'
   evalt :: Type -> TypeTable -> [TypeName] 
      -> IO (Type, TypeTable)
   -- evalt t tt ns returns t fully evaluated to an 
   -- enumeration, and an updated tt. 
   -- ns = list of type names already pending evaluation
   evalt t tt ns = do
      let merge :: Type -> Type -> String -> 
            (SparseSet Argument -> SparseSet Argument
             -> SparseSet Argument) -> 
             IO (Type, TypeTable)
          merge t1 t2 verb f = do
             (t1'@(TEnum as1 _),tt') <- evalt t1 tt ns
             (t2'@(TEnum as2 _),tt'') <- evalt t2 tt' ns
             k1 <- kindCheck source t1' 
             k2 <- kindCheck source t2'
             if k1 == k2
                then return (TEnum (f as1 as2) p, tt'')
                else fatalError' (getPos t1) ("Can not form \
                   \the " ++ verb ++ " of two types with \
                   \incompatible kinds: " ++ show k1 ++ 
                   " and " ++ show k2) source
      case t of
         TEnum as _ -> 
            if not (nullSS (getVariables t emptySS))
            then fatalError' (getPos t) ("Type " ++ show t
               ++ " in the declaration of a named type \
               \contains variables.") source
            else return (t, tt)
         TOrd cs p ->evalt (TEnum (list2SS $ 
            map (\c -> Const c (getPos c)) cs) p) tt ns
         TSubr f ms l p -> do
            let is = case ms of
                   Nothing -> [f .. l]
                   Just s  -> [f, s .. l]
                wrap i = Const (CIntegral i p) p
            evalt (TEnum (list2SS $ map wrap is) p) tt ns
         TName n _ -> do
            tt' <- evaln ns tt n
            case lookupBST n tt' of
               Nothing -> fatalError' (getPos n) ("Type " ++ 
                  show n ++ " is used but not declared.")
                  source
               Just t  -> return (t, tt')
         TUnion t1 t2 p -> merge t1 t2 "union" unionSS
         TSect t1 t2 p -> merge t1 t2 "intersection" sectSS
         TDiff t1 t2 p -> merge t1 t2 "difference" diffSS
         TCart ts p -> do
            let f :: [Type] -> TypeTable
                     -> IO ([[Argument]], TypeTable)
                f ts tt = case ts of
                   [t] -> do 
                      (TEnum as _,tt') <- evalt t tt ns
                      return (map (: []) (flattenSS as), tt')
                   (t:ts') -> do
                      (TEnum as _,tt') <- evalt t tt ns
                      (ass, tt'') <- f ts' tt'
                      return ([a : as' | a <- flattenSS as,
                                         as' <- ass], tt'')
            (ass, tt') <- f ts tt
            let tups = map
                   (\as -> Tuple as (getPos $ head as)) ass
            return (TEnum (list2SS tups) p, tt')
\end{code}

\submodule{Evaluating a type} %%%%%%%%%%%%%%%%%%%

At the time an object is instantiated, it should
be possible to fully evaluate its type, that is,
reduce it to an enumeration, {\tt TEnum}.
\highlighttt{evalType}$\mathit{source}$~$\mathit{tt}~t$
returns the enumeration equivalent to $t$ and its kind,
where $\mathit{tt}$ is the table of named types.

\begin{code}
evalType :: String -> TypeTable -> Type -> IO (Type, Kind)
evalType source tt t = case t of
   TEnum cs p -> do
      k <- kindCheck source t
      return (t, k)
   TOrd cs p -> do
      k <- kindCheck source t
      return (TEnum (list2SS $
         map (\c -> Const c (getPos c)) cs) p, k)
   TSubr f ms l p -> do
      k <- kindCheck source t
      let is = case ms of
             Nothing -> [f .. l]
             Just s  -> [f, s .. l]
          wrap i = Const (CIntegral i p) p
      return (TEnum (list2SS $ map wrap is) p, k)
   TName n p -> case lookupBST n tt of
      Nothing -> fatalError' p ("Named type, " ++ show n
         ++ ", is undefined.") source
      Just t' -> do
         (t'',k) <- evalType source tt t'
         return (t'', k)
   TUnion t1 t2 p -> do
      (t1'@(TEnum as1 _),k1) <- evalType source tt t1
      (t2'@(TEnum as2 _),k2) <- evalType source tt t2
      case kUnify k1 k2 of
         Nothing -> fatalError' p ("can not form the \
            \union of two types with incompatible kinds, "
            ++ show k1 ++ " and " ++ show k2) source
         Just k3 -> return (TEnum (unionSS as1 as2) p, k3)
   TSect t1 t2 p -> do
      (t1'@(TEnum as1 _),k1) <- evalType source tt t1
      (t2'@(TEnum as2 _),k2) <- evalType source tt t2
      case kUnify k1 k2 of
         Nothing -> fatalError' p ("can not form the \
            \intersection of two types with incompatible \
            \kinds, " ++ show k1 ++ " and " ++ show k2)
            source
         Just k3 -> return (TEnum (sectSS as1 as2) p, k3) 
   TDiff t1 t2 p -> do
      (t1'@(TEnum as1 _),k1) <- evalType source tt t1
      (t2'@(TEnum as2 _),k2) <- evalType source tt t2
      case kUnify k1 k2 of
         Nothing -> fatalError' p ("can not form the \
            \difference of two types with incompatible \
            \kinds, " ++ show k1 ++ " and " ++ show k2)
            source
         Just k3 -> return (TEnum (diffSS as1 as2) p, k3)
   TCart ts p ->  do
      tks <- mapM (evalType source tt) ts
      let (ts',ks) = unzip tks
      return (TEnum (list2SS $
         map (\as -> Tuple as (getPos $ head as)) $
         cartProd $ map (flattenSS . tEnum) ts') p,
         KTuple ks)
\end{code}

\noindent \highlighttt{evalType'}$\mathit{tt}~t$ returns $t$
reduced to a list of constants, where $\mathit{tt}$
is the table of named types.

\begin{code}
evalType' :: String -> TypeTable -> Type -> IO [Constant]
evalType' source tt t = do
   (TEnum as p,_) <- evalType source tt t
   let cs = [c | Const c _ <- flattenSS as]
       vs = [v | Var v _ <- flattenSS as]
   if null vs
      then return cs
      else fatalError' p "evalType': variables left in\
         \type." source
\end{code}

\submodule{Collecting Orderings} %%%%%%%%%%%%%%%%%%%

Class \highlighttt{HasTypes} overloads things do with data structures
that contain {\tt Type}s.

\begin{code}
class HasTypes a where
\end{code}

\noindent \highlighttt{getOrderings}~$\mathit{os}$~$x$ adds
any orderings in $x$ to $\mathit{os}$. The orderings are the
square-bracket delimited sequences of constants that appear
in types. They need to be collected and used to generate
the facts that define those total ordering relations.

\begin{code}
   getOrderings :: [[Constant]] -> a -> [[Constant]]
   getOrderings os _ = os
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Comparing} %%%%%%%

\begin{code}
instance Eq TypeName where
   (TypeName n _) == (TypeName n' _) = n == n'
\end{code}

\begin{code}
instance Ord TypeName where
   compare (TypeName n _) (TypeName n' _) = compare n n'
\end{code}

\begin{code}
instance Eq Type where
   (TEnum  as     _) == (TEnum  as'       _) = as == as'
   (TOrd   cs     _) == (TOrd   cs'       _) = cs == cs'
   (TSubr  f ms l _) == (TSubr  f' ms' l' _) = 
      let is = case ms of
             Nothing -> [f .. l]
             Just s  -> [f, s .. l]
          is' = case ms' of
             Nothing -> [f' .. l']
             Just s' -> [f', s' .. l']
      in is == is'
   (TName  tn     _) == (TName  tn'       _) = tn == tn'
   (TUnion t1 t2  _) == (TUnion t1' t2'   _) = t1 == t1' && 
                                               t2 == t2'
   (TSect  t1 t2  _) == (TSect  t1' t2'   _) = t1 == t1' && 
                                               t2 == t2'
   (TDiff  t1 t2  _) == (TDiff  t1' t2'   _) = t1 == t1' && 
                                               t2 == t2'
   (TCart  ts     _) == (TCart  ts'       _) = ts == ts'
   _                 == _                    = False
\end{code}

\begin{code}
instance Ord Type where
   compare t t' = case t of
      TEnum as _ -> case t' of
         TEnum as' _       -> compare as as'
         TOrd cs' _        -> LT
         TSubr f' ms' l' _ -> LT
         TName tn' _       -> LT
         TUnion t1' t2' _  -> LT
         TSect t1' t2' _   -> LT
         TDiff t1' t2' _   -> LT
         TCart ts' _       -> LT
      TOrd cs _ -> case t' of
         TEnum as' _       -> GT
         TOrd cs' _        -> compare cs cs'
         TSubr f' ms' l' _ -> LT
         TName tn' _       -> LT
         TUnion t1' t2' _  -> LT
         TSect t1' t2' _   -> LT
         TDiff t1' t2' _   -> LT
         TCart ts' _       -> LT
      TSubr  f ms l _ -> case t' of
         TEnum as' _       -> GT
         TOrd cs' _        -> GT
         TSubr f' ms' l' _ -> 
            let is = case ms of
                   Nothing -> [f .. l]
                   Just s  -> [f, s .. l]
                is' = case ms' of
                   Nothing -> [f' .. l']
                   Just s' -> [f', s' .. l']
            in compare is is'
         TName tn' _       -> LT
         TUnion t1' t2' _  -> LT
         TSect t1' t2' _   -> LT
         TDiff t1' t2' _   -> LT
         TCart ts' _       -> LT
      TName  tn    _ -> case t' of
         TEnum as' _       -> GT
         TOrd cs' _        -> GT
         TSubr f' ms' l' _ -> GT
         TName tn' _       -> compare tn tn'
         TUnion t1' t2' _  -> LT
         TSect t1' t2' _   -> LT
         TDiff t1' t2' _   -> LT
         TCart ts' _       -> LT
      TUnion t1 t2 _ -> case t' of
         TEnum as' _       -> GT
         TOrd cs' _        -> GT
         TSubr f' ms' l' _ -> GT
         TName tn' _       -> GT
         TUnion t1' t2' _  -> case compare t1 t1' of
            LT -> LT
            EQ -> compare t2 t2'
            GT -> GT
         TSect t1' t2' _   -> LT
         TDiff t1' t2' _   -> LT
         TCart ts' _       -> LT
      TSect  t1 t2 _ -> case t' of
         TEnum as' _       -> GT
         TOrd cs' _        -> GT
         TSubr f' ms' l' _ -> GT
         TName tn' _       -> GT
         TUnion t1' t2' _  -> GT
         TSect t1' t2' _   -> case compare t1 t1' of
            LT -> LT
            EQ -> compare t2 t2'
            GT -> GT
         TDiff t1' t2' _   -> LT
         TCart ts' _       -> LT
      TDiff  t1 t2 _ -> case t' of
         TEnum as' _       -> GT
         TOrd cs' _        -> GT
         TSubr f' ms' l' _ -> GT
         TName tn' _       -> GT
         TUnion t1' t2' _  -> GT
         TSect t1' t2' _   -> GT
         TDiff t1' t2' _   ->  case compare t1 t1' of
            LT -> LT
            EQ -> compare t2 t2'
            GT -> GT
         TCart ts' _       -> LT
      TCart  ts _ -> case t' of
         TEnum as' _       -> GT
         TOrd cs' _        -> GT
         TSubr f' ms' l' _ -> GT
         TName tn' _       -> GT
         TUnion t1' t2' _  -> GT
         TSect t1' t2' _   -> GT
         TDiff t1' t2' _   -> GT
         TCart ts' _       -> case compare ts ts' of
            LT -> LT
            EQ -> compare ts ts'
            GT -> GT
\end{code}

\subsubmodule{Positions} %%%%%%%

\begin{code}
instance HasPos TypeName where
   getPos = tnPos
\end{code}

\begin{code}
instance HasPos Type where
   getPos = tPos
\end{code}

\subsubmodule{Showing} %%%%%

\begin{code}
instance Show TypeName where
   showsPrec _ (TypeName n _) = showString n
\end{code}

\begin{code}
instance Show Type where
   showsPrec _ t = case t of
      TEnum as _     -> showChar '{' . 
         showWithSep ", " (flattenSS as) . showChar '}'
      TOrd cs _      -> showChar '[' . 
         showWithSep ", " cs . showChar ']'
      TSubr f ms l _ -> showChar '[' . shows f . 
         (case ms of
            Nothing -> id
            Just s  -> showChar ',' . shows s) . 
            showString ".." . shows l . showChar ']'
      TName n _      -> shows n
      TUnion t1 t2 _ -> showChar '(' . shows t1 . 
         showString " + " . shows t2 . showChar ')'
      TSect  t1 t2 _ -> showChar '(' . shows t1 . 
         showString " ^ " . shows t2 . showChar ')'
      TDiff  t1 t2 _ -> showChar '(' . shows t1 . 
         showString " - " . shows t2 . showChar ')'
      TCart  ts    _ -> showChar '(' .
         showWithSep " * " ts . shows ts . showChar ')'
\end{code}

\subsubmodule{Collecting constants} %%%%%%%

\begin{code}
instance HasConstants Type where
   getConstants t cs = case t of
      TEnum as _     -> 
         foldr getConstants cs $ flattenSS as
      TOrd as _      -> foldr getConstants cs as
      TSubr f ms l p -> 
         let is = case ms of
                Nothing -> [f .. l]
                Just s  -> [f, s .. l]
             wrap i = Const (CIntegral i p) p
         in foldr (getConstants . wrap) cs is
      TName n _      -> cs
      TUnion t1 t2 _ -> 
         getConstants t2 (getConstants t1 cs)
      TSect  t1 t2 _ -> 
         getConstants t2 (getConstants t1 cs)
      TDiff  t1 t2 _ -> 
         getConstants t2 (getConstants t1 cs)
      TCart  ts    _ -> foldr getConstants cs ts
\end{code}

\subsubmodule{Collecting variables} %%%%%%%

\begin{code}
instance HasVariables Type where
   getVariables t cs = case t of
      TEnum as _     -> 
         foldr getVariables cs $ flattenSS as
      TOrd {}        -> cs
      TSubr {}       -> cs
      TName n _      -> cs
      TUnion t1 t2 _ -> 
         getVariables t2 (getVariables t1 cs)
      TSect  t1 t2 _ -> 
         getVariables t2 (getVariables t1 cs)
      TDiff  t1 t2 _ -> 
         getVariables t2 (getVariables t1 cs)
      TCart  ts    _ -> foldr getVariables cs ts
\end{code}

\subsubmodule{Grounding} %%%%%%%

\begin{code}
instance Groundable Type where
   ground1 v c t = case t of
      TEnum as p     -> TEnum (list2SS $ 
         map (ground1 v c) $ flattenSS as) p
      TOrd cs p      -> TOrd cs p
      TSubr f ms l p -> TSubr f ms l p
      TName n p      -> TName n p
      TUnion t1 t2 p -> 
         TUnion (ground1 v c t1) (ground1 v c t2) p
      TSect  t1 t2 p -> 
         TSect (ground1 v c t1) (ground1 v c t2) p
      TDiff  t1 t2 p -> 
         TDiff (ground1 v c t1) (ground1 v c t2) p
      TCart  ts p -> 
         TCart (map (ground1 v c) ts) p
   rename v v' t = case t of
      TEnum as p     -> TEnum (list2SS $ map (rename v v')
         $ flattenSS as) p
      TOrd cs p      -> TOrd cs p
      TSubr f ms l p -> TSubr f ms l p
      TName n p      -> TName n p
      TUnion t1 t2 p -> 
         TUnion (rename v v' t1) (rename v v' t2) p
      TSect  t1 t2 p -> 
        TSect (rename v v' t1) (rename v v' t2) p
      TDiff  t1 t2 p -> 
         TDiff (rename v v' t1) (rename v v' t2) p
      TCart  ts p    -> 
         TCart (map (rename v v') ts) p
\end{code}

\subsubmodule{Collecting Orderings} %%%%%%%%

\noindent Note: don't collect subranges as the ordering of
integers is fixed and implicit.

\begin{code}
instance HasTypes Type where
   getOrderings os t = case t of
      TEnum {}       -> os
      TOrd cs _      -> cs : os
      TSubr {}       -> os
      TName {}       -> os
      TUnion t1 t2 p -> 
         getOrderings (getOrderings os t1) t2
      TSect  t1 t2 p -> 
         getOrderings (getOrderings os t1) t2
      TDiff  t1 t2 p -> 
         getOrderings (getOrderings os t1) t2
      TCart ts p -> foldl getOrderings os ts
\end{code}

\subsubmodule{Kind inference} %%%%%%%%

\begin{code}
instance HasKind Type where
   kindCheck source t = case t of
      TEnum  as    p -> kindCheckList source $ flattenSS as
      TOrd   cs    p -> kindCheckList source cs
      TSubr {}       -> return KIntegral
      TName  tn    p -> return KUnknown
      TUnion t1 t2 p -> do
         k1 <- kindCheck source t1
         k2 <- kindCheck source t2
         case kUnify k1 k2 of
            Just k  -> return k
            Nothing -> fatalError' (getPos t1) ("Can not \
               \form the union of two types with \
               \incompatible kinds: " ++ show  k1 ++
               " and " ++ show k2) source
      TSect  t1 t2 p -> do
         k1 <- kindCheck source t1
         k2 <- kindCheck source t2
         case kUnify k1 k2 of
            Just k  -> return k
            Nothing -> fatalError' (getPos t1) ("Can not \
               \form the intersection of two types with \
               \incompatible kinds: " ++ show  k1 ++
               " and " ++ show k2) source
      TDiff  t1 t2 p ->  do
         k1 <- kindCheck source t1
         k2 <- kindCheck source t2
         case kUnify k1 k2 of
            Just k  -> return k
            Nothing -> fatalError' (getPos t1) ("Can not \
               \form the difference of two types with \
               \incompatible kinds: " ++ show  k1 ++
               " and " ++ show k2) source
      TCart  ts    p -> do
         ks <- mapM (kindCheck source) ts
         return $ KTuple ks
\end{code}

\subsubmodule{Qualification} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance Qualifiable TypeName where
   qualify n n' = n' {tnNam = qualify n $ tnNam n'}
\end{code}

\begin{code}
instance Qualifiable Type where
   qualify n t = case t of
      TEnum {}      -> t {
            tEnum = list2SS $ map (qualify n) $ flattenSS
               $ tEnum t
         }
      TOrd {}       -> t {
            tOrd = map (qualify n) $ tOrd t
         }
      TSubr {}      -> t 
      TName {}      -> t {
            tName = qualify n $ tName t
         }
      TUnion {}     -> t {
            tArg1 = qualify n $ tArg1 t,
            tArg2 = qualify n $ tArg2 t
         }
      TSect {}      -> t {
            tArg1 = qualify n $ tArg1 t,
            tArg2 = qualify n $ tArg2 t
         }
      TDiff {}      -> t {
            tArg1 = qualify n $ tArg1 t,
            tArg2 = qualify n $ tArg2 t
         }
      TCart {}      -> t {
            tArgs = map (qualify n) $ tArgs t
         }
\end{code}
