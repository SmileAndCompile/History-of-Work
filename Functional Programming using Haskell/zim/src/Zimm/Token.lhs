\module{Token}

The {\tt Token} module provides the functions required for the tokenization of program sources. Tokenization involves converting the lexemes of a file into {\tt Tokens}, which encapsulate all the lexemes of a language into one of 3 types (keywords, literal values and identifiers) -- which allow for more effective comparison of files to detect similarities. 

\begin{code}
module Zimm.Token (
      Token(..), makeMapsKey, makeMapsLiteral, makeMapsId, tokenizeFile
   ) where
\end{code}

\begin{code}
import Data.List as L
import Data.Map.Strict as M
\end{code}

\begin{code}
import ABR.Util.Pos
import ABR.Parser
\end{code}


\noindent A {\tt Token} is used to identify the type of a lexeme (keywords, literal 
values and identifiers). The keyword variant includes all the keywords, separators and operators associated with the language. 

\begin{code}
data Token =   Key Int 
             | Literal Int 
             | Id Int 
   deriving(Eq, Ord, Show)
\end{code}

\noindent A {\tt TokenMap} is a map between the Lexemes of a file and their associated {\tt Token}.

\begin{code}
type TokenMap = M.Map Lexeme Token
\end{code}

\noindent {\tt makeMapsKey lexs} returns a {\tt TokenMap} for all of the keywords, 
separators, and operators of the language - where {\tt lexs} contains
a list of all the keywords, separators and operators for the language.
All files will share the same tokens for these language elements.

\begin{code}
makeMapsKey :: [Lexeme] -> TokenMap
makeMapsKey lexs = fromList $ L.zip lexs $ L.map Key [0..]
\end{code}

\noindent {\tt makeLiteralToken n m (tag, lex)} adds a new {\tt Token} to the {\tt TokenMap} if 
{\tt (tag, lex)} is a new numeric constant - where {\tt m} is the {\tt TokenMap} and {\tt n} is the 
next token value to use. {\tt makeLiteralToken} then returns the new map and 
the next token value to use.
All files should have common tokens for numeric constants.

\begin{code}
makeLiteralToken :: Int -> TokenMap -> (Tag, Lexeme) -> (Int, TokenMap)
makeLiteralToken n m (tag, lex) = 
   let addOne = case M.lookup lex m of
          Nothing -> (n + 1, M.insert lex (Literal n) m)
          Just _  -> (n, m)
   in case tag of
      "integerLiteral"  -> addOne 
      "floatingLiteral" -> addOne
      _                 -> (n, m)
\end{code}

\noindent {\tt makeMapsLiteral m tlpss} returns a new {\tt TokenMap} where all of the 
constants from all lexed files have been added to the map, where {\tt m} is the 
incomplete {\tt TokenMap} and {\tt tlpss} are the {\tt TLPs} from all files that were lexed.

\begin{code}
makeMapsLiteral :: TokenMap -> [[((Tag, Lexeme), Pos)]] -> TokenMap
makeMapsLiteral m tlpss = 
   let addOne (n, m) tlp = makeLiteralToken n m (fst tlp)
       addFile (n, m) tlps = L.foldl addOne (n, m) tlps
   in snd $ L.foldl addFile (1, m) tlpss
\end{code}

\noindent {\tt makeIdToken n m (tag, lex)} adds a new {\tt Token} to the {\tt TokenMap} if 
{\tt (tag, lex)} is a new identifer - where {\tt m} is the incomplete {\tt TokenMap} and {\tt n} is 
the next token value to use. {\tt makeIdToken} then returns the new {\tt TokenMap} 
and the next token value to use.

\begin{code}
makeIdToken :: Int -> TokenMap -> (Tag, Lexeme) -> (Int, TokenMap)
makeIdToken n m (tag, lex) = 
   let addOne = case M.lookup lex m of
          Nothing -> (n + 1, M.insert lex (Id n) m)
          Just _  -> (n, m)
   in case tag of
      "identifier"  -> addOne
      _            -> (n, m) 
\end{code}

\noindent {\tt makeMapsId m tlps} returns a new {\tt TokenMap} where all of the 
identifiers for a single lexed file have been added to the map, where {\tt m} is 
the incomplete {\tt TokenMap} and {\tt tlps} are the tagged lexemes / positions 
for the associated file.

\begin{code}
makeMapsId :: TokenMap -> [((Tag, Lexeme), Pos)] -> TokenMap
makeMapsId m tlps = 
   let addOne (n, m) tlp = makeIdToken n m (fst tlp)
   in snd $ L.foldl addOne (0, m) tlps
\end{code}

\noindent {\tt tokenizeFile m tlps} returns the list of {\tt Tokens} and their 
positions for a single lexed file, where {\tt m} is a completed {\tt TokenMap} linking all 
the lexemes for a single file to their respective {\tt Tokens} and {\tt tlps} are the 
tagged lexemes / positions of the file to be tokenized.
String literals all have the same token (Literal 0) (not in the map).

\begin{code}
tokenizeFile :: TokenMap -> [((Tag, Lexeme), Pos)] -> [(Token, Pos)]
tokenizeFile m tlps = 
   let tokenize ((tag, lex), pos) = case tag of
         "stringLiteral" -> (Literal 0, pos)
         _               -> case M.lookup lex m of
            Nothing -> error $ "tokenizeFile: unknown lex: "++ show tag ++ 
               show lex ++ show pos
            Just t  -> (t, pos)
   in L.map tokenize tlps
\end{code}
