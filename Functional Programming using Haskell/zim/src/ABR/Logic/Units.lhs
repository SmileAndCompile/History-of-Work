% Units.#ext
% This file was produced from Units.lit

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

\module{Units} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlightModule{Units} implements the top level 
parser for the CDL tool.

\begin{code}
module CDL.Units (
      Unit(..), cdlSourceP, namedUnits, processUnit
   ) where
\end{code}

\begin{code}
import Control.Monad
\end{code}

\begin{code}
import ABR.Parser.Pos
import ABR.Parser
import ABR.Args
import ABR.Showing
import ABR.Data.SparseSet
\end{code}

\begin{haddock}

import ABR.List
import ABR.DeepSeq
\end{haddock}

\begin{code}
import CDL.Constants
import CDL.Variables
import CDL.Kinds
import CDL.Arguments
import CDL.Atoms
import CDL.Literals
import CDL.Types
import CDL.VarGens
import CDL.NewTypeDecs
import CDL.AtomTypeDecs
import CDL.Defaults
import CDL.LitSets
import CDL.Utils
import CDL.Instantiation
\end{code}

\submodule{Data type} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A \highlighttt{Uses} is either a request to import another named
unit, maybe qualified, or the other actual unit, qualified when
requested.

\begin{code}
data Uses = Uses String Bool Pos
\end{code}

An \highlighttt{Unit} is the lest chunk the CDL
language accepts as a complete source.

\begin{code}
data Unit =   
     EchoInteger {
        uInt :: Integer,
        uUses :: [Uses],
        uUnits :: [Unit],
        uPos :: Pos
     }
   | EchoConstant {
        uConst :: Constant,
        uUses :: [Uses],
        uUnits :: [Unit],
        uPos :: Pos
     }
   | EchoVariable {
        uVar :: Variable,
        uUses :: [Uses],
        uUnits :: [Unit],
        uPos :: Pos
     }
   | EchoArgument {
        uArg :: Argument,
        uUses :: [Uses],
        uUnits :: [Unit],
        uPos :: Pos
     }
   | EchoAtom {
        uAtm :: Atom,
        uUses :: [Uses],
        uUnits :: [Unit],
        uPos :: Pos
     }
   | EchoLiteral {
        uLit :: Literal,
        uUses :: [Uses],
        uUnits :: [Unit],
        uPos :: Pos
     }
   | EchoTypeName {
        uTN :: TypeName,
        uUses :: [Uses],
        uUnits :: [Unit],
        uPos :: Pos
     }
   | EchoType {
        uTyp :: Type,
        uUses :: [Uses],
        uUnits :: [Unit],
        uPos :: Pos
     }
   | EchoNewTypeDec {
        uNTD :: NewTypeDec,
        uUses :: [Uses],
        uPos :: Pos
     }
   | EchoVarGen {
        uVG :: VarGen,
        uUses :: [Uses],
        uUnits :: [Unit],
        uPos :: Pos
     }
   | EchoAtomTypeDec {
        uATD :: AtomTypeDec,
        uUses :: [Uses],
        uUnits :: [Unit],
        uPos :: Pos
     }
   | EchoDefault {
        uDef :: Default,
        uUses :: [Uses],
        uUnits :: [Unit],
        uPos :: Pos
     }
   | EchoLitSet {
        uLS :: LitSet,
        uUses :: [Uses],
        uUnits :: [Unit],
        uPos :: Pos
     }
   | TypesUnit {
        uName :: String,
        uNTDs :: [NewTypeDec],
        uUses :: [Uses],
        uUnits :: [Unit],
        uPos :: Pos
     }
\end{code}

\submodule{Parsers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \syntrax{usesClauses}

\begin{code}
usesClausesP :: Parser [Uses]
usesClausesP = many (
          literalP "lName" "uses"
      <*> optional (literalP "lName" "qualified")
      <*> tagP "uName"
      @> (\((_,_,p),(qs,(_,n,_))) ->
            Uses n (not $ null qs) p)
   )
\end{code}

\noindent \syntrax{echoTestUnit}

\begin{code}
echoTestUnitP :: Parser Unit
echoTestUnitP = 
           literalP "lName" "echo"
       <*> literalP "lName" "integer"
        *> usesClausesP    
       <*> nofail (literalP "symbol" "{")
        *> nofail integerP
       <*  nofail (literalP "symbol" "}")
       @> (\((_,_,p),(us,(i,_))) -> EchoInteger i us [] p)
   <|>     literalP "lName" "echo"
       <*> literalP "lName" "constant"
        *> usesClausesP    
       <*> nofail (literalP "symbol" "{")
        *> nofail constantP
       <*  nofail (literalP "symbol" "}")
       @> (\((_,_,p),(us,c)) -> EchoConstant c us [] p)
   <|>     literalP "lName" "echo"
       <*> literalP "lName" "variable"
        *> usesClausesP    
       <*> nofail (literalP "symbol" "{")
        *> nofail variableP
       <*  nofail (literalP "symbol" "}")
       @> (\((_,_,p),(us,v)) -> EchoVariable v us [] p)
   <|>     literalP "lName" "echo"
       <*> literalP "lName" "argument"
        *> usesClausesP    
       <*> nofail (literalP "symbol" "{")
        *> nofail argumentP
       <*  nofail (literalP "symbol" "}")
       @> (\((_,_,p),(us,a)) -> EchoArgument a us [] p)
   <|>     literalP "lName" "echo"
       <*> literalP "lName" "atom"
        *> usesClausesP    
       <*> nofail (literalP "symbol" "{")
        *> nofail atomP
       <*  nofail (literalP "symbol" "}")
       @> (\((_,_,p),(us,a)) -> EchoAtom a us [] p)
   <|>     literalP "lName" "echo"
       <*> literalP "lName" "literal"
        *> usesClausesP    
       <*> nofail (literalP "symbol" "{")
        *> nofail pLiteralP
       <*  nofail (literalP "symbol" "}")
       @> (\((_,_,p),(us,l)) -> EchoLiteral l us [] p)
   <|>     literalP "lName" "echo"
       <*> literalP "lName" "typeName"
        *> usesClausesP    
       <*> nofail (literalP "symbol" "{")
        *> nofail typeNameP
       <*  nofail (literalP "symbol" "}")
       @> (\((_,_,p),(us,n)) -> EchoTypeName n us [] p)
   <|>     literalP "lName" "echo"
       <*> literalP "lName" "type"
        *> usesClausesP    
       <*> nofail (literalP "symbol" "{")
        *> nofail typeP
       <*  nofail (literalP "symbol" "}")
       @> (\((_,_,p),(us,t)) -> EchoType t us [] p)
   <|>     literalP "lName" "echo"
       <*> literalP "lName" "newTypeDec"
        *> usesClausesP    
       <*> nofail (literalP "symbol" "{")
        *> nofail newTypeDecP
       <*  nofail (literalP "symbol" "}")
       @> (\((_,_,p),(us,d)) -> EchoNewTypeDec d us [] p)
   <|>     literalP "lName" "echo"
       <*> literalP "lName" "varGen"
        *> usesClausesP    
       <*> nofail (literalP "symbol" "{")
        *> nofail varGenP
       <*  nofail (literalP "symbol" "}")
       @> (\((_,_,p),(us,v)) -> EchoVarGen v us [] p)
   <|>     literalP "lName" "echo"
       <*> literalP "lName" "atomTypeDec"
        *> usesClausesP    
       <*> nofail (literalP "symbol" "{")
        *> nofail atomTypeDecP
       <*  nofail (literalP "symbol" "}")
       @> (\((_,_,p),(us,d)) -> EchoAtomTypeDec d us [] p)
   <|>     literalP "lName" "echo"
       <*> literalP "lName" "default"
        *> usesClausesP    
       <*> nofail (literalP "symbol" "{")
        *> nofail defaultP
       <*  nofail (literalP "symbol" "}")
       @> (\((_,_,p),(us,d)) -> EchoDefault d us [] p)
   <|>     literalP "lName" "echo"
       <*> literalP "lName" "litSet"
        *> usesClausesP    
       <*> nofail (literalP "symbol" "{")
        *> nofail litSetP
       <*  nofail (literalP "symbol" "}")
       @> (\((_,_,p),(us,s)) -> EchoLitSet s us [] p)
\end{code}

\noindent \syntrax{typesUnit}

\begin{code}
typesUnitP :: Parser Unit
typesUnitP = 
       literalP "lName" "types"
   <*> nofail (tagP "uName")
   <*> usesClausesP
   <*> nofail (literalP "symbol" "{")
    *> many newTypeDecP
   <*  nofail (literalP "symbol" "}")
   @> (\((_,_,p),((_,n,_),(us,ds))) ->
         TypesUnit n ds us p)
\end{code}

\noindent \syntrax{unit}

\begin{code}
unitP :: Parser Unit
unitP = echoTestUnitP <|> typesUnitP    
\end{code}

\noindent \syntrax{cdlSource}

\noindent \highlighttt{cdlSourceP} recognises CDL sources.

\begin{code}
cdlSourceP :: Parser [Unit]
cdlSourceP = many unitP
\end{code}

\submodule{Getting named units} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{unitName}~$u$ returns the $u$'s name if
there is one.

\begin{code}
unitName :: Unit -> Maybe String
unitName u = case u of
   TypesUnit n _ _ _ -> Just n
   _                 -> Nothing
\end{code}

\noindent 
\highlighttt{namedUnits}~$\mathit{source}~\mathit{us}$
returns units in $\mathit{us}$ paired with their names.
If names are not unique, it is an error.

\begin{code}
namedUnits :: String -> [Unit] -> IO [(String,Unit)]
namedUnits source = foldM f []
   where
   f t u = case unitName u of
      Nothing -> return t
      Just n -> case lookup n t of 
         Nothing -> return $ (n, u) : t
         Just _  -> fatalError' (getPos u) 
            ("Duplicate unit name: \"" ++ n ++ "\"") source
\end{code}

\noindent 
\highlighttt{populateUnits}~$\mathit{source}~\mathit{nus}~u$
returns u 

\begin{code}
populateUnits = undefined
\end{code}

\submodule{Processing a unit} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent
\highlighttt{processUnit}~$\mathit{source}~\mathit{options}~\mathit{us}~u$
does what ever the unit $u$ and the $\mathit{options}$ imply.
The $\mathit{source}$ is required to print error messages.
$\mathit{us}$ is a list of named units that might be imported.

\begin{code}
processUnit ::
   String -> Options -> [(String,Unit)] -> Unit -> IO ()
processUnit source options us u = 
   case u of
      EchoInteger {} -> 
         print u
      EchoConstant c _ _ -> do
         print u
         dumpKind c
         dumpConstants c
      EchoVariable v _ _ -> do
         print u
         dumpKind v
         dumpVariables v
      EchoArgument a _ _ -> do
         print u
         dumpKind a
         dumpConstants a
         dumpVariables a
      EchoAtom a _ _ -> 
         print u
      EchoLiteral l _ _ -> 
         print u
      EchoTypeName n _ _ -> 
         print u
      EchoType t _ _ -> 
         print u
      EchoNewTypeDec d _ _ -> 
         print u
      EchoVarGen v _ _ -> 
         print u
      EchoAtomTypeDec d _ _ -> 
         print u
      EchoDefault d _ _ -> 
         print u
      EchoLitSet s _ _ -> 
         print u
      _ -> return ()
   where
   dumpKind x = do 
      k <- kindCheck source x
      putStrLn $ "   kind = " ++ show k
   dumpConstants x = do
      let cs = getConstants x emptySS
      putStrLn $ "   constants = " ++ show (flattenSS cs)
   dumpVariables x = do
      let vs = getVariables x emptySS
      putStrLn $ "   variables = " ++ show (flattenSS vs)
\end{code}

\submodule{Instance Declarations} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Positions} %%%%%%%

\begin{code}
instance HasPos Unit where
   getPos = uPos
\end{code}

\subsubmodule{Showing} %%%%%

\begin{code}
instance Show Uses where
   showsPrec _ (Uses n q _) = showString "uses" .
      (if q then showString "qualified " else id) .
      showString n
\end{code}

\begin{code}
instance Show Unit where
   showsPrec _ u = case u of
         EchoInteger i us p ->
            showString "\necho integer " . su us . 
            showString "{ " . shows i . showString " }" 
         EchoConstant c us p -> 
            showString "\necho constant " . su us . 
            showString "{ " . shows c . showString " }" 
         EchoVariable v us p -> 
            showString "\necho variable " . su us . 
            showString "{ " . shows v . showString " }"
         EchoArgument a us p -> 
            showString "\necho argument " . su us . 
            showString "{ " . shows a . showString " }"
         EchoAtom a us p -> 
            showString "\necho atom " . su us . 
            showString "{ " . shows a . showString " }"
         EchoLiteral l us p -> 
            showString "\necho literal " . su us . 
            showString "{ " . shows l . showString " }"
         EchoTypeName n us p -> 
            showString "\necho typeName " . su us . 
            showString "{ " . shows n . showString " }"
         EchoType t us p -> 
            showString "\necho type " . su us . 
            showString "{ " . shows t . showString " }"
         EchoNewTypeDec d us p -> 
            showString "\necho newTypeDec " . su us . 
            showString "{ " . shows d . showString " }"
         EchoVarGen v us p -> 
            showString "\necho varGen " . su us . 
            showString "{ " . shows v . showString " }"
         EchoAtomTypeDec d us p -> 
            showString "\necho atomTypeDec " . su us . 
            showString "{ " . shows d . showString " }"
         EchoDefault d us p -> 
            showString "\necho default " . su us . 
            showString "{ " . shows d . showString " }"
         EchoLitSet s us p -> 
            showString "\necho litSet " . su us . 
            showString "{ " . shows s . showString " }"
      where
      su = showWithTerm " "
\end{code}

\subsubmodule{Collecting constants} %%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance HasConstants Unit where
   getConstants u cs = case u of
         EchoInteger i us p -> usc us cs
         EchoConstant c us p -> usc us cs
         EchoVariable v us p -> usc us cs
         EchoArgument a us p -> usc us cs
         EchoAtom a us p -> usc us cs
         EchoLiteral l us p -> usc us cs
         EchoTypeName n us p -> usc us cs
         EchoType t us p -> usc us cs
         EchoNewTypeDec d us p -> usc us cs
         EchoVarGen v us p -> usc us cs
         EchoAtomTypeDec d us p -> usc us cs
         EchoDefault d us p -> usc us cs
         EchoLitSet s us p -> usc us cs
      where
      usc us cs = foldr getConstants cs us
\end{code}

\subsubmodule{Qualification} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{haddock}
instance Qualifiable Constant where
   qualify n c = case c of
      CNamed n' p   -> CNamed (n ++ "." ++ n') p
      CIntegral _ _ -> c
      CString _ _   -> c
\end{haddock}
