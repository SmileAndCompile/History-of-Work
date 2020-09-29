% Constants.#ext
% This file was produced from Constants.lit

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

\module{Logic.Constants} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{Logic.Constants} implements constants\index{constant}.

\begin{code}
module ABR.Logic.Constants (
      Constant(..), constantP, integerP, HasConstants(..)
   ) where
\end{code}

\begin{code}
import qualified Data.Set as S
import Control.DeepSeq
\end{code}

\begin{code}
import ABR.Util.Pos
import ABR.Parser
import ABR.Logic.Kinds
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2013-11-24: Removed qualification.
   

\submodule{Data types} %%%%%%%%%%%%%

An \highlighttt{Constant} is a fixed token which may appear
as an argument\index{arguments!in atoms} to an atom.
Constants may be the traditional named kind (\highlighttt{CNamed}),
an integer (\highlighttt{CIntegral}), or a string (\highlighttt{CString}).

\begin{code}
data Constant =   
     CNamed {
        cName :: String,
        cPos  :: Pos
     }
   | CIntegral {
        cInt :: Integer,
        cPos :: Pos
     }
   | CString {
        cStr :: String,
        cPos :: Pos
     }
\end{code}

\submodule{Parsers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent\LogicEBNF{integer}
	   
\noindent \highlighttt{integerP} recognises integers, returning
the integer value and the position it occured at.

\begin{code}
integerP :: Parser (Integer, Pos)
integerP = 
       optional (literalP "symbol" "-")
   <&> tagP "cardinal"
   @> (\(ms,(_,c,p)) -> case ms of
          []         -> (read c, p)
          [(_,_,p')] -> (negate $ read c, p'))
\end{code}

\noindent\LogicEBNF{constant}
	   
\noindent \highlighttt{constantP} recognises constants.

\begin{code}
constantP :: Parser Constant
constantP =
       tagP "uName"  @> (\(_,n,p) -> CNamed n p)
   <|> integerP      @> uncurry CIntegral
   <|> tagP "string" @> (\(_,s,p) -> CString s p)
\end{code}

\submodule{Collecting constants} %%%%%%%%%%%%%%%%%%%%%

It is required for various purposes to identify all
of the distinct constants that occur in an object.
Constants can be collected from instances of class
\highlighttt{HasConstants}.

\begin{code}
class HasConstants a where
\end{code}

\noindent \highlighttt{getConstants}~$x~C$ adds any
constants in $x$ to $C$.

\begin{code}
   getConstants :: a -> S.Set Constant -> S.Set Constant
\end{code}

\noindent \highlighttt{hasConstants}~$x$ returns {\tt True}
iff $x$ contains constants.

\begin{code}
   hasConstants :: a -> Bool
   hasConstants a = not $ S.null $ getConstants a S.empty
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Comparing} %%%%%%%

\begin{code}
instance Eq Constant where
   (CNamed n _)    == (CNamed n' _)    = n == n'
   (CIntegral i _) == (CIntegral i' _) = i == i'
   (CString s _)   == (CString s' _)   = s == s'
   _               == _                = False
\end{code}

\begin{code}
instance Ord Constant where
   compare c c' = case c of
      CNamed n _    -> case c' of
         CNamed n' _    -> compare n n'
         CIntegral _ _  -> LT
         CString _ _    -> LT
      CIntegral i _ -> case c' of
         CNamed _ _     -> GT
         CIntegral i' _ -> compare i i'
         CString _ _    -> LT
      CString s _ -> case c' of
         CNamed _ _     -> GT
         CIntegral _ _  -> GT
         CString s' _   -> compare s s'
\end{code}

\subsubmodule{Positions} %%%%%%%

\begin{code}
instance HasPos Constant where
   getPos = cPos
\end{code}

\subsubmodule{Showing} %%%%%%%

\begin{code}
instance Show Constant where
   showsPrec _ c = case c of
      CNamed n _    -> showString n 
      CIntegral i _ -> shows i
      CString s _   -> showString s
\end{code}

\subsubmodule{Collecting constants} %%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance HasConstants Constant where
   getConstants = S.insert
\end{code}

\subsubmodule{Kind inference} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance HasKind Constant where
   kindCheck c = case c of
      CNamed _ _    -> return KNamed
      CIntegral _ _ -> return KIntegral
      CString _ _   -> return KString
\end{code}

\subsubmodule{DeepSeq} %%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance NFData Constant where
   rnf c = case c of
      CNamed    n p -> n `deepseq` p `deepseq` ()
      CIntegral i p -> i `deepseq` p `deepseq` ()
      CString   s p -> s `deepseq` p `deepseq` ()
\end{code}
