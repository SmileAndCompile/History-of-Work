% Arguments.#ext
% This file was produced from Arguments.lit

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

\module{Logic.Arguments} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Logic.Arguments} implements
arguments\index{argument!in atoms} for logic systems.

\begin{code}
module ABR.Logic.Arguments (
      Argument(..), argumentP
   ) where
\end{code}

\begin{code}
import Control.DeepSeq
\end{code}

\begin{code}
import ABR.Util.Pos
import ABR.Parser
import ABR.Text.Showing
import ABR.Logic.Kinds
import ABR.Logic.Constants
import ABR.Logic.Variables
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2013-11-24: Removed qualification.
   

\submodule{Data type} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

An \highlighttt{Argument} of an atom\index{atom} may be either:

\begin{itemize}
   \item a constant\index{constant} (\highlighttt{Const});
   \item a variable (\highlighttt{Var}); or
   \item a tuple containing constants and/or variables.
\end{itemize}

\begin{code}
data Argument =
     Const {
        arConst :: Constant,
        arPos   :: Pos
     }
   | Var {
        arVar :: Variable,
        arPos :: Pos
     }
   | Tuple {
        arElems :: [Argument],
        arPos   :: Pos
     }
\end{code}

\noindent The order of a tuple's elements must be maintained.
A tuple has at least 2 elements. A nested tuple is equivalent
to a flat one.

\[ ((x,y),z) = (x,y,x) = (x,(y,z)) \]

\submodule{Parsers}\index{parsers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent\LogicEBNF{argument}
	   
\noindent \highlighttt{argumentP} recognises arguments.

\begin{code}
argumentP :: Parser Argument
argumentP = 
          constantP @> (\c -> Const c (getPos c))
      <|> variableP @> (\v -> Var v (getPos v))
      <|>     literalP "symbol" "("
          <&> nofail' "argument expected" argumentP
          <&> some (
                    literalP "symbol" ","
                 &> nofail' "argument expected" argumentP
              )
          <&  nofail (literalP "symbol" ")")
          @> (\((_,_,p),(a,as)) -> Tuple (a:as) p)
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Positions} %%%%%%%

\begin{code}
instance HasPos Argument where
   getPos = arPos
\end{code}

\subsubmodule{Comparing} %%%%%%%

\begin{code}
instance Eq Argument where
   Const c  _ == Const c'  _ = c == c'
   Var   v  _ == Var   v'  _ = v == v'
   Tuple as _ == Tuple as' _ = as == as'
\end{code}

\begin{code}
instance Ord Argument where
   compare a1 a2 = case a1 of
      Const c _ -> case a2 of
         Const c' _ -> compare c c'
         Var   _  _ -> LT
         Tuple _  _ -> LT
      Var   v  _ -> case a2 of
         Const _  _ -> GT
         Var   v' _ -> compare v v'
         Tuple _  _ -> LT
      Tuple as _ -> case a2 of
         Const _   _ -> GT
         Var   _   _ -> GT
         Tuple as' _ -> compare as as'
\end{code}

\subsubmodule{Showing}\index{Showing} %%%%%%%

\begin{code}
instance Show Argument where
   showsPrec _ a = case a of
      Const c  _ -> shows c
      Var   v  _ -> shows v
      Tuple as _ -> showChar '(' . showWithSep "," as .
         showChar ')'
\end{code}

\subsubmodule{Collecting constants}\index{collecting!constants}\index{constants!collecting} %%%%%%%

\begin{code}
instance HasConstants Argument where
   getConstants a cs = case a of
      Const c _  -> getConstants c cs
      Var   _ _  -> cs
      Tuple as _ -> foldr getConstants cs as
\end{code}

\subsubmodule{Collecting variables}\index{collecting!variables}\index{variables!collecting} %%%%%%%

\begin{code}
instance HasVariables Argument where
   getVariables a vs = case a of
      Const _ _  -> vs
      Var   v _  -> getVariables v vs
      Tuple as _ -> foldr getVariables vs as
\end{code}

\subsubmodule{Grounding}\index{Grounding} %%%%%%%

\begin{code}
instance Groundable Argument where
   ground1 v c a = case a of
      Const c' p             -> Const c' p
      Var   v' p | v == v'   -> Const c  p
                 | otherwise -> Var   v' p
      Tuple as p             ->
         Tuple (map (ground1 v c) as) p 
   rename v v' a = case a of
      Const c   p             -> Const c  p
      Var   v'' p | v == v''  -> Var v'  p
                  | otherwise -> Var v'' p
      Tuple as p              ->
         Tuple (map (rename v v') as) p
\end{code}

\subsubmodule{Kind inference} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance HasKind Argument where
   kindCheck a = case a of
      Const c  _ -> kindCheck c
      Var   v  _ -> kindCheck v
      Tuple as _ -> do
         ks <- mapM kindCheck as
         return $ KTuple ks
\end{code}

\subsubmodule{DeepSeq}\index{DeepSeq} %%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance NFData Argument where
   rnf a = case a of
      Const c  p -> c  `deepseq` p `deepseq` ()
      Var   v  p -> v  `deepseq` p `deepseq` ()
      Tuple as p -> as `deepseq` p `deepseq` ()
\end{code}
