% Kinds.#ext
% This file was produced from Kinds.lit

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

\module{Logic.Kinds} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{Kinds} implements kinds\index{kind} for 
arguments in logic atoms.

\begin{code}
module ABR.Logic.Kinds (
      Kind(..), HasKind(..), kindCheckList, kUnify
   ) where
\end{code}

\begin{code}
import Data.Maybe
\end{code}

\begin{code}
import ABR.Util.Pos
import ABR.Text.Showing
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2013-11-24.\\
Reviewed 2012-11-23: Saved from the CDL project for RL/PL.
   

\submodule{Data type} %%%%%%%%%%%%%

An \highlighttt{Kind} is the essential structure of the elements of a type.
Only types of the same kind may be combined in certain ways, for example
union.
The traditional kind of type is the one that consist of named
constants (\highlighttt{KNamed}). Types may also be subsets
of the integers (\highlighttt{KIntegral}), or sets of strings
(\highlighttt{KString}). Cartesian 
products of types yield tuple kinds (\highlighttt{KTuple}).
There should never be less than two elements
in a tuple kind. A with variables in it has an unknown kind 
(\highlighttt{KUnknown}).

\begin{code}
data Kind =   KNamed
            | KIntegral
            | KString
            | KTuple [Kind]
            | KUnknown
            deriving (Eq, Ord)
\end{code}

\noindent Kinds are not declared. They are inferred. So
no parser is necessary.

\submodule{Unification} %%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{kUnify}~$k$~$k'$ returns the unification
of $k$ and $k'$ if there is one.

\begin{code}
kUnify :: Kind -> Kind -> Maybe Kind
kUnify k k' = case k of
   KNamed    -> case k' of
      KNamed     -> Just KNamed
      KUnknown   -> Just KNamed
      _          -> Nothing
   KIntegral -> case k' of
      KIntegral  -> Just KIntegral
      KUnknown   -> Just KIntegral
      _          -> Nothing
   KString   -> case k' of
      KString    -> Just KString
      KUnknown   -> Just KString
      _          -> Nothing
   KTuple ks -> case k' of
      KTuple ks' ->
         let n = length ks
             ks'' = catMaybes $ zipWith kUnify ks ks'
         in if n == length ks' && n == length ks''
            then Just $ KTuple ks''
            else Nothing
      KUnknown   -> Just $ KTuple ks
      _          -> Nothing
   KUnknown  -> Just k'
\end{code}

\submodule{Kind inference} %%%%%%%%%%%%%%%%%%%%%%%%

\noindent Class \highlighttt{HasKind}~overload functions
pertaining to kinds.

\begin{code}
class (Show a, HasPos a) => HasKind a where
\end{code}

\noindent \highlighttt{kindCheck}~$x$ infers the kind of $x$.

\begin{code}
   kindCheck :: a -> IO Kind
   kindCheck x = ioError $ userError $ 
      "Unimplemented kindCheck for thing:\n"
      ++ show x ++ "\nat " ++ show (getPos x) 
\end{code}

\noindent \highlighttt{kindCheckList}~$\mathit{xs}$ infers the 
unified kind of $\mathit{xs}$.
\emph{precondition}: length of $\mathit{xs} \ge 2$.

\begin{code}
kindCheckList :: HasKind a => [a] -> IO Kind
kindCheckList xs = do
   ks <- mapM kindCheck xs
   let fold :: HasKind a => [(Kind, a)] -> IO Kind
       fold ((k,x):(k',x'):kxs) = case kUnify k k' of
          Just k'' -> if null kxs
             then return k''
             else fold $ (k'',x') : kxs
          Nothing -> ioError $ userError $
             "Thing:\n" ++ show x ++ "\nat " ++ 
             show (getPos x') ++ "\nhas kind " ++ show k' 
             ++ ",\nthat is incompatible with expected "
             ++ show k
   fold $ zip ks xs
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Showing} %%%%%%%

A named kind is represented as \verb"*". An integral 
kind is represented as \verb"#". A string 
kind is represented as \verb"$". Tuples are formed with
parentheses and commas. An unknown kind (what a variable must have)
is represented as \verb"?".

\begin{code}
instance Show Kind where
   showsPrec _ k = case k of
      KNamed    -> showChar '*' 
      KIntegral -> showChar '#'
      KString   -> showChar '$'
      KTuple ks -> showChar '(' . showWithSep "," ks
                   . showChar ')'
      KUnknown  -> showChar '?'
\end{code}

