% JSON.#ext
% This file was produced from JSON.lit

% ABRHLibs -- a personal library of Haskell modules
% Copyright (C) 2014  Andrew Rock
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

\module{Text.JSON} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The \highlighttt{ABR.Text.JSON} library provides functions to
parse, construct, and interrogate JSON data.

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
\end{code}

\begin{code}
module ABR.Text.JSON (
   JPropertyName, JValue(..),
   jmString, jmNumber, jmTrue, jmFalse, jmNull,
   jmArray, jmObject,
   lexerL, valueP,
   JQuery (..), JResult (..)
  ) where
\end{code}

\begin{code}
import qualified Data.Map as M
import qualified Data.Array.IArray as A
\end{code}

\begin{code}
import ABR.Util.Pos
import ABR.Text.Showing
import ABR.Parser
import ABR.Parser.Lexers hiding (stringL)
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2015-02-10. Passed {\tt hlint}.\\
Reviewed 2014-05-30: Added more queries, and made all the
{\tt -Wall}s go away.\\
New 2014-05-16. 
   
\submodule{Language tweaks} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
error' :: String -> a
error' label = error $
   "Error in module ABR.Text.JSON, \
   \ with label \"" ++ label ++ "\"."
\end{code}

\submodule{Basic data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A distinction must be made between string data that appears in the
JSON test (delimited with double quotes, and with special character escaping)
and string data that is ready to use in an application (without all that).
In this document, \emph{escaping} is the process of adding quotes and escaping
special characters, and \emph{descaping} is the reverse process.

A {\tt String} is always the string data after descaping.

A {\tt JString} is a JSON string, as lexed, not yet descaped.

\begin{code}
type JString = String
\end{code}

\noindent A {\tt JNumber} is still a string. It has been lexed as string (without 
delimiters or escaping), but awaits conversion to {\tt Int} or {\tt Double}.

\begin{code}
type JNumber = String
\end{code}

\noindent A \highlighttt{JPropertyName} was lexed as a JSON string and has been descaped.

\begin{code}
type JPropertyName = String
\end{code}

\noindent A {\tt JProperty} binds a {\tt JPropertyName} to a {\tt JValue}.
Useful as an intermediate.

\begin{code}
data JProperty = JProperty JPropertyName JValue
\end{code}

\noindent A {\tt JObject} is a mapping from {\tt JPropertyName}s to
{\tt JValue}s.

\begin{code}
type JObject = M.Map JPropertyName JValue
\end{code}

\noindent A {\tt JArray} is a mapping from {\tt Ints}s to
{\tt JValue}s.

\begin{code}
type JArray = A.Array Int JValue
\end{code}

\noindent A \highlighttt{JValue} is a lump of JSON data.

\begin{code}
data JValue = 
     JString {
        jString :: String,
        jPos :: Pos
     }
   | JNumber {
        jNumber :: JNumber,
        jPos :: Pos
     }
   | JObject {
        jObject :: JObject,
        jPos :: Pos
     }
   | JArray {
        jArray :: JArray,
        jPos :: Pos
     }
   | JTrue {
        jPos :: Pos
     }
   | JFalse {
        jPos :: Pos
     }
   | JNull {
        jPos :: Pos
     }
\end{code}

\submodule{Escaping/descaping} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt escape}~$\mathit{cs}$ adds the special character escapes and delimiting
double quotes.

\begin{code}
escape :: String -> JString
escape = ('"' :) . (++ "\"") . concatMap (\c -> case c of
      '"'  -> "\\\""
      '\\' -> "\\\\"
      '/'  -> "\\/"
      '\b' -> "\\b"
      '\f' -> "\\f"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      _    -> [c]
   )
   -- missing hex codes
\end{code}

\noindent {\tt descape}~$\mathit{cs}$ removes the special character escapes and 
delimiting double quotes.

\begin{code}
descape :: JString -> String
descape ('"' : cs) = des cs
   where
   des cs' = case cs' of
      "\""      -> ""
      '\\' : cs'' -> case cs'' of
         '"'  : cs''' -> '\"' : des cs'''
         '\\' : cs''' -> '\\' : des cs'''
         '/'  : cs''' -> '/'  : des cs'''
         'b'  : cs''' -> '\b' : des cs'''
         'f'  : cs''' -> '\f' : des cs'''
         'n'  : cs''' -> '\n' : des cs'''
         'r'  : cs''' -> '\r' : des cs'''
         't'  : cs''' -> '\t' : des cs'''
         _            -> 
            error' "descape bad escape sequence"
      c : cs''   -> c : des cs''
      []         -> 
         error' "descape missing trailing quotes"
descape _ = error' "descape missing leading quotes"
-- note, unimplemented hex codes
\end{code}

\submodule{Construction} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The {\tt dummyPos} is the fake position assigned to constructed
(rather than parsed) JSON values.

\begin{code}
dummyPos :: Pos
dummyPos = (-666,-666)
\end{code}

\noindent \highlighttt{jmString}~$\mathit{cs}$ constructs a JSON string value containing
$\mathit{cs}$.

\begin{code}
jmString :: String -> JValue
jmString cs = JString cs dummyPos
\end{code}

\noindent \highlighttt{jmNumber}~$\mathit{x}$ constructs a JSON number value containing
$\mathit{x}$.

\begin{code}
jmNumber :: (Num a, Show a) => a -> JValue
jmNumber x = JNumber (show x) dummyPos
\end{code}

\noindent \highlighttt{jmTrue} constructs the JSON value {\tt true}.
\highlighttt{jmFalse} constructs the JSON value {\tt false}.
\highlighttt{jmNull} constructs the JSON value {\tt null}.

\begin{code}
jmTrue, jmFalse, jmNull :: JValue
jmTrue = JTrue dummyPos
jmFalse = JFalse dummyPos
jmNull = JNull dummyPos
\end{code}

\noindent \highlighttt{jmArray}~$\mathit{vs}$ constructs a JSON array from 
$\mathit{vs}$.

\begin{code}
jmArray :: [JValue] -> JValue
jmArray vs = 
   JArray (A.listArray (0,length vs - 1) vs) dummyPos
\end{code}

\noindent \highlighttt{jmObject}~$\mathit{nvs}$ constructs a JSON object from 
$\mathit{nvs}$, where $\mathit{nvs}$ is a list of pairs, $(n,v)$,
$n$ is a property name, and $v$ is its value.

\begin{code}
jmObject :: [(JPropertyName,JValue)] -> JValue
jmObject nvs = JObject (M.fromList nvs) dummyPos
\end{code}

\submodule{Lexer} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\InputEBNF{JSON}{digit19}

\begin{code}
digit19L :: Lexer
digit19L = satisfyL (\c -> '1' <= c && c <= '9') "digit19"
\end{code}

\InputEBNF{JSON}{digit}

\begin{code}
digitL :: Lexer
digitL = 
   (    literalL '0'
    <|> digit19L
   ) 
   %> "digit"
\end{code}

\InputEBNF{JSON}{hexDigit}

\begin{code}
hexDigitL :: Lexer
hexDigitL = 
   (    digitL
    <|> satisfyL (\c -> 'a' <= c && c <= 'f' || 
                        'A' <= c && c <= 'F') ""
   )
   %> "hexDigit"
\end{code}

\InputEBNF{JSON}{string}

\begin{code}
stringL :: Lexer
stringL =
        literalL '\"'
   <&&> (many (
                   satisfyL (`notElem` "\"\\") ""
               <|> escapeSequenceL     
              ) &%> "")
   <&&> nofail (literalL '\"')
   %> "string"
\end{code}

\InputEBNF{JSON}{escapeSequence}

\begin{code}
escapeSequenceL :: Lexer
escapeSequenceL = 
        literalL '\\'
   <&&> (     literalL '\"'
          <|> literalL '\\'
          <|> literalL '/'
          <|> literalL 'b'
          <|> literalL 'f'
          <|> literalL 'n'
          <|> literalL 'r'
          <|> literalL 't'
          <|> hexDigitL <&&> hexDigitL <&&> hexDigitL <&&>
              hexDigitL
        )
   %> "excapeSequence"
\end{code}

\InputEBNF{JSON}{number}

\begin{code}
numberL :: Lexer
numberL = 
        soft (optional (literalL '-'))
   <&&> (    literalL '0'
         <|> digit19L <&&> (many digitL &%> "")
        )
   <&&> soft (optional (
                     literalL '.' 
                <&&> (many digitL &%> "")
             ))
   <&&> soft (optional (
                     (literalL 'e' <|> literalL 'E')
                <&&> soft (optional (literalL '+' <|> 
                                      literalL '-'))
                <&&> soft (some digitL)
             ))
   %> "number"
\end{code}

\InputEBNF{JSON}{separator}

\begin{code}
separatorL :: Lexer
separatorL = 
   (
         literalL ','
     <|> literalL ':'
     <|> literalL '{'
     <|> literalL '}'
     <|> literalL '['
     <|> literalL ']'
   )
   %> "separator"
\end{code}

\InputEBNF{JSON}{keyword}

\begin{code}
keywordL :: Lexer
keywordL = 
   (
         tokenL "true"
     <|> tokenL "false"
     <|> tokenL "null"
   )
   %> "keyword"
\end{code}

\noindent \highlighttt{lexerL} lexes a JSON source.

\InputEBNF{JSON}{lexer}

\begin{code}
lexerL :: Lexer
lexerL = dropWhite $ nofail $ total $ listL [whitespaceL, 
      separatorL, stringL, numberL, keywordL]
\end{code}

\submodule{Parser} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{valueP} parses a JSON source.

\InputEBNF{JSON}{value}

\begin{code}
valueP :: Parser JValue
valueP = 
       tagP "string"
       @> (\(_,l,p) -> JString (descape l) p)
   <|> tagP "number"
       @> (\(_,l,p) -> JNumber l p)
   <|> objectP
   <|> arrayP
   <|> literalP "keyword" "true"
       @> (\(_,_,p) -> JTrue p)
   <|> literalP "keyword" "false"
       @> (\(_,_,p) -> JFalse p)
   <|> literalP "keyword" "null"
       @> (\(_,_,p) -> JNull p)
\end{code}

\InputEBNF{JSON}{object}

\begin{code}
objectP :: Parser JValue
objectP =
       literalP "separator" "{"
   <&> optional (
              propertyP
          <&> many (
                    literalP "separator" ","
                 &> nofail' "property expected" propertyP
              )
          @> cons
       )
   <&  nofail (literalP "separator" "}")
   @> (\((_,_,p), pss) -> JObject {
         jObject = M.fromList 
            (map (\(JProperty n v) -> (n, v)) (concat pss)),
         jPos = p
      })
\end{code}
  
\InputEBNF{JSON}{property}

\begin{code}
propertyP :: Parser JProperty
propertyP = 
       tagP "string"
   <&> nofail (literalP "separator" ":")
    &> nofail' "value expected" valueP
   @> (\((_,l,_),v) -> JProperty (descape l) v)
\end{code}

\InputEBNF{JSON}{array}

\begin{code}
arrayP :: Parser JValue
arrayP = 
       literalP "separator" "["
   <&> optional (
              valueP
          <&> many (
                    literalP "separator" ","
                 &> nofail' "value expected" valueP
              )
          @> cons
       )
   <&  nofail (literalP "separator" "]")
   @> (\((_,_,p), vss) -> JArray {
         jArray = let vs = concat vss
                  in A.listArray (0, length vs - 1) vs,
         jPos = p
      })
\end{code}

\submodule{Interrogation} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A \highlighttt{JQuery} is a request to project out a part of a 
{\tt JValue}. It has the following values:

\begin{itemize}
    \item \highlighttt{JQStr} -- The value is a string, return it as a {\tt String}.
    \item \highlighttt{JQInt} -- The value is a number, return it as an {\tt Int}.
    \item \highlighttt{JQDbe} -- The value is a number, return it as a {\tt Double}.
    \item \highlighttt{JQBool} -- The value is {\tt true} or {\tt false}, return it as a {\tt Bool}.
    \item \highlighttt{JQProp}~$n$ -- The value is an object, return the property named $n$.
    \item \highlighttt{JQProps} -- The value is an object, return all of the the properties as a list of $(\mathit{name},\mathit{value})$ pairs.
    \item \highlighttt{JQElem}~$i$ -- The value is an array, return the $i$th element.
    \item \highlighttt{JQElems} -- The value is an array, return all the elements, in order, in a list; or 
                                    the value is an object, return all the property values.
    \item \highlighttt{JQIsNull} -- The value might be {\tt null}, return {\tt True} if it is.
    \item ${q_1}$~\highlighttt{:->}~${q_2}$ -- This query is really a sequence of two queries to be 
       applied in the order ${q_1}$ then ${q_2}$.
\end{itemize}

\begin{code}
infixl 9 :->
\end{code}

\begin{code}
data JQuery = 
     JQStr
   | JQInt
   | JQDbe
   | JQBool
   | JQProp JPropertyName
   | JQProps
   | JQElem Int
   | JQElems
   | JQIsNull
   | JQuery :-> JQuery
\end{code}

\noindent Class \highlighttt{JResult} overloads {\tt jGet}.

\highlighttt{jGet}~$q~v$ applies the query $q$ to value $v$. It may fail
with {\tt Nothing}.

\begin{code}
class JResult a where
   jGet :: JQuery -> JValue -> Maybe a
\end{code}

\submodule{Instances} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Showing} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent The {\tt Show} instances reconstruct valid JSON syntax.

\begin{code}
instance Show JProperty where
   showsPrec _ (JProperty k v) = 
      showString (escape k) . 
      showString " : " .
      shows v
\end{code}

\begin{code}
instance Show JValue where
   showsPrec _ v = case v of
      JString cs  _ ->
         showString (escape cs)
      JNumber cs  _ ->
         showString cs
      JObject obj _ ->
         showString "{\n" .
         showWithSep ",\n" 
            (map (uncurry JProperty) (M.toList obj)) .
         showString "\n}"
      JArray arr  _ ->
         showString "[\n" .
         showWithSep ",\n" (A.elems arr) .
         showString "\n]"
      JTrue       _ ->
         showString "true"
      JFalse      _ ->
         showString "false"
      JNull       _ ->
         showString "null"
\end{code}

\subsubmodule{Interrogation} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance JResult String where
   jGet JQStr (JString cs _) = Just cs
   jGet (q1 :-> q2) v = case jGet q1 v of
      Nothing -> Nothing
      Just v' -> jGet q2 v'
   jGet _ _ = Nothing
\end{code}

\begin{code}
instance JResult Int where
   jGet JQInt (JNumber cs _) = Just (read cs)
   jGet (q1 :-> q2) v = case jGet q1 v of
      Nothing -> Nothing
      Just v' -> jGet q2 v'
   jGet _ _ = Nothing
\end{code}

\begin{code}
instance JResult Double where
   jGet JQDbe (JNumber cs _) = Just (read cs)
   jGet (q1 :-> q2) v = case jGet q1 v of
      Nothing -> Nothing
      Just v' -> jGet q2 v'
   jGet _ _ = Nothing
\end{code}

\begin{code}
instance JResult Bool where
   jGet JQBool (JTrue _) = Just True
   jGet JQBool (JFalse _) = Just False
   jGet JQIsNull (JNull _) = Just True
   jGet JQIsNull _ = Just False
   jGet (q1 :-> q2) v = case jGet q1 v of
      Nothing -> Nothing
      Just v' -> jGet q2 v'
   jGet _ _ = Nothing
\end{code}

\begin{code}
instance JResult JValue where
   jGet (JQProp k) (JObject m _) = M.lookup k m
   jGet (JQElem i) (JArray a _) = 
      let (l,h) = A.bounds a
      in if l <= i && i <= h then Just (a A.! i) 
                             else Nothing
   jGet (q1 :-> q2) v = case jGet q1 v of
      Nothing -> Nothing
      Just v' -> jGet q2 v'
   jGet _ _ = Nothing
\end{code}

\begin{code}
instance JResult [JValue] where
   jGet JQElems (JObject m _) = Just (M.elems m)
   jGet JQElems (JArray a _) = Just (A.elems a)
   jGet (q1 :-> q2) v = case jGet q1 v of
      Nothing -> Nothing
      Just v' -> jGet q2 v'
   jGet _ _ = Nothing
\end{code}

\begin{code}
instance JResult [(JPropertyName,JValue)] where
   jGet JQProps (JObject m _) = Just (M.assocs m)
   jGet (q1 :-> q2) v = case jGet q1 v of
      Nothing -> Nothing
      Just v' -> jGet q2 v'
   jGet _ _ = Nothing
\end{code}

