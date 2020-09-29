% Configs.#ext
% This file was produced from Configs.lit

% ABRHLibs -- a personal library of Haskell modules
% Copyright (C) 2007, 2008,  Andrew Rock
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

\module{Text.Configs} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Text.Configs} provides a type, parser
and pretty printer for a sequence of configuration
settings, as might be found in a configuration file.
This kind of data could be stored in XML or JSON, but this format
is nicer to edit by hand. Its design was inspired by the configuration
files for Apache.

\begin{code}
module ABR.Text.Configs (
      Config(..), Configs, configsL, configsP, stringL,
      showConfigs, read', lookupConfig, updateConfig,
      lookupFlag, lookupParam, getParam, getValueList,
      popTemplate
   ) where
\end{code}

\begin{code}
import Data.Char
import Data.List
import Data.Maybe
\end{code}

\begin{code}
import ABR.Parser
import ABR.Parser.Lexers
import ABR.Text.String
import ABR.Text.Showing
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2018-08-29. More explanatory documentation and lookup functions.\\
Reviewed 2018-08-24. Fields have names now.\\
Reviewed 2018-08-23. Flags may be strings.\\
Reviewed 2015-02-10. Passed {\tt hlint}.\\
Reviewed 2014-05-30: Made all the {\tt -Wall}s go away.\\
Reviewed 2013-11-22.\\
Reviewed 2009-04-13: Changed to {\tt ABR.\emph{Text}.Configs};
   added design for templates; refactored un/enString out
   to {\tt ABR.Text.String}.

\submodule{Language tweaks} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
undefined' :: String -> a
undefined' label = error $
   "Intentional undefined at module ABR.Text.Configs, \
   \ with label \"" ++ label ++ "\"."
\end{code}

\begin{code}
error' :: String -> a
error' label = error $
   "ABR.Text.Configs." ++ label ++ "."
\end{code}

\submodule{Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A configuration, \highlighttt{Config}, is one of:

\begin{description}
   \item[\highlighttt{CFlag}] a flag that is set by the
      presence of its name, \highlighttt{cName}, (or just an item of data);
   \item[\highlighttt{CParam}] a parameter with name, \highlighttt{cName}, and an
      associated value, \highlighttt{cValue};
   \item[\highlighttt{CSet}] a parameter with a with name, \highlighttt{cName}, and an
      associated set of configurations, \highlighttt{cSet}; or
   \item[\highlighttt{CList}] a parameter with a with name, \highlighttt{cName}, and an
      associated list of configurations, \highlighttt{cList}.
\end{description}

\begin{code}
data Config =   CFlag {
                   cName  :: String
                }
              | CParam {
                   cName  :: String,
                   cValue :: String
                }
              | CSet {
                   cName  :: String,
                   cSet   :: Configs
                }
              | CList {
                   cName  :: String,
                   cList  :: [Configs]
                }
              deriving (Eq, Ord)
\end{code}

\noindent A \highlighttt{Configs} is a list of
configurations.

\begin{code}
type Configs = [Config]
\end{code}

\noindent A config file will normally contain a {\tt Configs}, that is
a sequence of {\tt Config}s not separated by commas. For
example, this is just a sequence of flags, turned on or 
off by commenting them out.

\VerbatimInput[frame=single]{Text/configExamples/flags.config}

\noindent The {\tt lookupConfig} function, below, implements a
query language for {\tt Configs}. For the {\tt flags.config}
example, {\tt lookupConfig} with query {\tt "verbose"} 
would return {\tt Just (CFlag "verbose")}, and {\tt Nothing} for
{\tt "optimise"}.

For these kind of flags, use the function {\tt lookupFlag} which
will return a more convenient {\tt Bool}.

Another type of {\tt Config} is a parameter. A
parameter has a name and an associated value. 

\VerbatimInput[frame=single]{Text/configExamples/param.config}

\noindent The name permits the value for a parameter to be 
looked up with a query. {\tt lookupConfig} with query {\tt "level"}
would return {\tt Just (CParam "level" "3")}. The more convenient
function {\tt lookupParam} would return {\tt Just "3"}.

Where related {\tt Config}s may be logically grouped, 
use a set. Sets have names like parameters and the values are
{\tt Configs} delimited by a set of braces.

\VerbatimInput[frame=single]{Text/configExamples/set.config}

\noindent Access members of a set with a period. Example
query: {\tt "site.level"}.

Where there will be multiple instances of {\tt Configs}
with all of the same {\tt Config}s, use a named list.

\VerbatimInput[frame=single]{Text/configExamples/list.config}

\noindent Access elements of a list with the indexing operator
\verb"!". For example {\tt lookupConfig "site.courses"} returns
{\tt Just} the whole {\tt CList}. {\tt lookupParam "site.courses!1.code"}
would return {\tt Just "1801ICT"}. Note that that all queries must
end in a name, and not with indexing {\tt !\emph{n}}, as the elements
of a list are {\tt Configs}, not {\tt Config}s. Remember that 
a {\tt Configs} is a synomyn for {\tt [Config]}.

Note that the elements of a list are {\tt Configs},
and therefore may be a sequence of {\tt Config}s, not separated
by commas. Commas are \emph{only} used to separate the
list elements.

A list might contain just a sequence of strings, numbers, or
flags, all of which will be parsed as flags.

\VerbatimInput[frame=single]{Text/configExamples/values.config}

\noindent See {\tt getValueList} below.

\submodule{Lexer} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Comments in configuration files start with \verb+#+ and
extend to the end of the line. Comments are treated as
whitespace. There can be any amount of whitespace between
tokens. Aside from inside strings, and to separate names,
whitespace is not significant.

\InputEBNF{Configs}{comment}

\begin{code}
commentL :: Lexer
commentL = 
   literalL '#'
   <&&> (many (satisfyL (/= '\n') "") &%> "")
   <&&> (optional (literalL '\n') &%> "")
   %> " "
\end{code}

\noindent Names in configuration files may contain
letters, digits, plus and minus signs, underscores,
periods, bangs, at-signs, and slashes. Note that a number can lex as a 
{\tt name}, as could many file paths. 
Names are case sensitive.

\InputEBNF{Configs}{name}

\begin{code}
nameL :: Lexer
nameL = some (satisfyL nameChar "") &%> "name"
   where
   nameChar :: Char -> Bool
   nameChar c = 
      isAlpha c || isDigit c || c `elem` "+-_.!@/"
\end{code}

\noindent Strings are delimited by double quotes and may
extend across many lines. Use two double quotes for one,
{\it \`{a} la} Pascal. 

\InputEBNF{Configs}{string}

\noindent The other symbols used are:

\begin{description}
   \item[{\tt =}] to bind a name to a value (either a {\tt
      name} or a {\tt string}), configuration set or
      configuration list;
   \item[{\tt \char`\{}] to start a configuration set;
   \item[{\tt \char`\}}] to close a configuration set;
   \item[{\tt [}] to start a configuration list;
   \item[{\tt ]}] to close a configuration list; and
   \item[{\tt ,}] to separate items in a configuration
      list.
\end{description}

\noindent \highlighttt{configsL} is the lexer that will
tokenize a configuration source.

\InputEBNF{Configs}{configsL}

\begin{code}
configsL :: Lexer
configsL = dropWhite $ nofail $ total $ listL [
      commentL, nameL, stringL, 
      tokenL "=" %> "symbol", tokenL "{" %> "symbol",
      tokenL "}" %> "symbol", tokenL "[" %> "symbol",
      tokenL "]" %> "symbol", tokenL "," %> "symbol",
      whitespaceL
   ]
\end{code}

\submodule{Parser} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent A {\tt value} is either a {\tt name} or a {\tt
string}.

\InputEBNF{Configs}{value}

\begin{code}
valueP :: Parser String 
valueP = (tagP "name" <|> tagP "string")
         @> (\(_,v,_) -> v)
\end{code}

\noindent A {\tt configs} is a sequence of whitespace
separated 
{\tt config}s, parsed by \highlighttt{configsP}.

\InputEBNF{Configs}{configs}

\begin{code}
configsP :: Parser Configs
configsP = many configP
\end{code}

\noindent A {\tt configSet} is a {\tt configs} in braces.

\InputEBNF{Configs}{configSet}

\begin{code}
configSetP :: Parser Configs
configSetP = 
   literalP "symbol" "{"
   &> nofail' "configs expected" configsP
   <& nofail (literalP "symbol" "}")
\end{code}

\noindent A {\tt configList} is a comma separated sequence
of {\tt configs} in brackets.

\InputEBNF{Configs}{configList}

\begin{code}
configListP :: Parser [Configs]
configListP = 
           literalP "symbol" "["
       <&> literalP "symbol" "]"
       #> []
   <|>     literalP "symbol" "["
        &> configsP
       <&> some (
                 literalP "symbol" ","
              &> nofail' "configs expected"
                    configsP
           )
       <&  nofail (literalP "symbol" "]")
       @> cons
   <|>     literalP "symbol" "["
        &> nofail configsP
       <&  nofail (literalP "symbol" "]")
       @> (: [])
\end{code}

\noindent A {\tt config} is either: a binding of a name to
a {\tt configSet}, a {\tt configList}, or a {\tt value};
or just a {\tt name}.

\InputEBNF{Configs}{config}

\begin{code}
configP :: Parser Config
configP = 
           tagP "name"
       <&> literalP "symbol" "="
       <&> valueP
       @> (\((_,n,_),(_,v)) -> CParam n v)
   <|>     tagP "name"
       <&> literalP "symbol" "="
       <&> configSetP
       @> (\((_,n,_),(_,cs)) -> CSet n cs)
   <|>     tagP "name"
       <&> literalP "symbol" "="
        &> nofail configListP
       @> (\((_,n,_),css) -> CList n css)
   <|> valueP
       @> CFlag
\end{code}

\submodule{Showing} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance Show Config where
\end{code}

\begin{code}
    showsPrec _ c = case c of
       CFlag n     -> showString n
       CParam n v  -> showString n 
          . showString " = " . showString v
       CSet n cs   -> showString n 
          . showString " = {\n" 
          . showWithTerm "\n" cs
          . showString "}"
       CList n css -> showString n 
          . showString " = [\n"
          . showString (intercalate ",\n" 
             (map (unlines . map show) css))
          . showString "]"
\end{code}

\highlighttt{showConfigs}~$\mathit{cs}$ shows a list of
configs.

\begin{code}
showConfigs :: Configs -> String
showConfigs = concatMap ((++ "\n") . show)
\end{code}

\submodule{Reading} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{read'}~$s$ may be used to read a
parameter value $s$, removing quotes first.

\begin{code}
read' :: Read a => String -> a
read' = read . trim . unString
\end{code}

\submodule{Accessing} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{configPaths}

\noindent A \emph{configPath} is a string that selects a {\tt Config}
from within some {\tt Configs}. It can be: the name of a
{\tt Config}, eg {\tt name}; of the form {\tt name.name} to
select from inside a {\tt CSet}; or of the form {\tt
name!digits.name} to select from inside a {\tt CList}; or
of combinations like {\tt class!3.student!7.name.family}.

Note this description. While the names in a {\tt Config}
may be any double-quote-delimited string, those names are
not useable in a configPath.

\subsubmodule{Lookup functions}

\noindent \highlighttt{lookupConfig}~$n~\mathit{cs}$ tries
to find the named config or configs $n$ in $\mathit{cs}$, returning
{\tt Just c} if the first match is a config,
or {\tt Nothing}. $n$ is a configPath.

\begin{code}
lookupConfig :: String -> Configs -> Maybe Config
lookupConfig n cs = case cs of
   [] -> 
      Nothing
   CFlag n' : cs'
      | n == n' -> 
         Just (CFlag n)
      | otherwise ->
         lookupConfig n cs'
   CParam n' v : cs'
      | n == n' ->
         Just (CParam n v)
      | otherwise ->
         lookupConfig n cs'
   CSet n' cs' : cs''
      | n == n' ->
         Just (CSet n cs')
      | (n' ++ ".") `isPrefixOf` n ->
         lookupConfig (drop (length n' + 1) n) cs'
      | otherwise ->
         lookupConfig n cs''
   CList n' css : cs' 
      | n == n' ->
         Just (CList n css)
      | (n' ++ "!") `isPrefixOf` n ->
         let (i, n'') = 
                span isDigit (drop (length n' + 1) n)
         in if null n''
            then error' "lookupConfig:\
               \ query ends with !nn, which would return\
               \ Configs, not a Config." 
            else lookupConfig (tail n'') (css !! read i)
      | otherwise ->
         lookupConfig n cs'
\end{code}

\noindent \highlighttt{updateConfig}~$n~c~\mathit{cs}$
tries to replace the named config $n$ in $\mathit{cs}$
with $c$. $n$ is a configPath.
If the named config does not exist it will be created.
THIS IS INCOMPETELY IMPLEMENTED and is the wrong way to build
{\tt Configs} anyway.

\begin{code}
updateConfig :: String -> Config -> Configs -> Configs
updateConfig n c cs = case cs of
   [] ->
      [c] -- does not handle complex names when they need 
          -- to be inserted as lists and sets
   c'@(CFlag n') : cs'
      | n == n' ->
         c : cs'
      | otherwise ->
         c' : updateConfig n c cs'
   c'@(CParam n' _) : cs'
      | n == n' ->
         c : cs'
      | otherwise ->
         c' : updateConfig n c cs'
   c'@(CSet n' cs') : cs'' 
      | n == n' ->
         c : cs''
      | (n' ++ ".") `isPrefixOf` n ->
         CSet n' (updateConfig (drop (length n' + 1) n) c
         cs') : cs''
      | otherwise ->
         c' : updateConfig n c cs''
   c'@(CList n' css) : cs' 
      | n == n' ->
         c : cs'
      | (n' ++ "!") `isPrefixOf` n ->
         let (i,n'') = 
                span isDigit (drop (length n' + 1) n)
             (css',cs'':css'') = splitAt (read i) css
         in CList n' (css' ++ updateConfig (tail n'') c cs'' 
            : css'') : cs'
      | otherwise ->
         c' : updateConfig n c cs'
\end{code}

\noindent \highlighttt{lookupFlag}~$n~\mathit{cs}$ tries
to find the named flag $n$ in $\mathit{cs}$,
returning {\tt True} iff it exists as a flag.
$n$ is a configPath.

\begin{code}
lookupFlag :: String -> Configs -> Bool
lookupFlag n cs = case lookupConfig n cs of
   Just (CFlag _) -> True
   _              -> False
\end{code}

\noindent \highlighttt{lookupParam}~$n~\mathit{cs}$ tries
to find the named parameter $n$ in $\mathit{cs}$,
returning {\tt Just} the first value or {\tt Nothing}.


\begin{code}
lookupParam :: String -> Configs -> Maybe String
lookupParam n cs = case lookupConfig n cs of
   Nothing            -> Nothing
   Just (CParam _ v) -> Just v
   _ -> undefined' "lookupParam"
\end{code}

\noindent \highlighttt{getParam}~$n~\mathit{cs}$ tries to
return the value for the named {\tt CParam}. 
It is an error if the parameter
can not be found. 

\begin{code}
getParam:: String -> Configs -> String
getParam name cs = 
   let mp = lookupParam name cs
   in fromMaybe (error' $ ".getParam: Missing config: "
         ++ name) mp 
\end{code}

\noindent \highlighttt{getValueList}~$n~\mathit{cs}$ tries to
find the named {\tt CList} and return the list of single values in it.
It is an error if the list can not be found or if any of the 
elements of the list are not a single {\tt CFlag}.

\begin{code}
getValueList:: String -> Configs -> [String]
getValueList name cs = case lookupConfig name cs of
   Nothing ->
      error' $ "getValueList: Missing list: " ++ name
   Just (CList _ css) ->
      let singleFlag cs' = length cs' == 1 && 
             (case head cs' of
                CFlag _ -> True
                _       -> False)
      in if not (all singleFlag css)
         then error' $ "getValueList: " ++ name ++ " \
            \is not a list of only single flags."
         else map cName $ map head css
   _ -> error' $ "getValueList: " ++ name ++ " is not a list." 
\end{code}

\submodule{Templates} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{\tt Configs} are a structured data type like XML and can
be used to populate a template. The power of templates in
part comes from being able to handle variations in the data
that they might get populated with.

\subsubmodule{Simple Template Markup Language}

\begin{itemize}
   \item A template is a {\tt String} containing any kind 
      of text, marked up as HTML or anything else.
      
   \item The hash character (\verb"#") is the special escape 
      character in a template. It it \emph{always} treated
      specially. 
      
   \item To output a hash, use two, eg \verb"##".

   \item The sequence \verb"#"\emph{configPath}\verb"#"
      is replaced in the output by the value of the {\tt
      Config}.
      
   \item The output value of a {\tt Config} is:
   
      \begin{itemize}
         \item \verb"_UNDEFINED_" if the {\tt Config} does
            not exist;
            
         \item the unStringed text of the flag if the {\tt Config} is a 
            flag;
         
         \item the unStringed text of a parameter;
         
         \item \verb"_SET_" if the {\tt Config} is a set; 
            or
          
         \item \verb"_LIST_" if the {\tt Config} is a list.
      \end{itemize}
      
   \item In the following sequences, no extra whitespace
      is permitted.
    
   \item The sequence 
      \verb"#ifdef#"\emph{configPath}\verb"#"\emph{text}\verb"#end#"
      outputs the \emph{text} iff \emph{configPath} leads
      to a {\tt Config} of any kind that exists, otherwise
      outputs nothing. The \emph{text} has any template
      markup in it processed as usual.

   \item The sequence 
      \verb"#ifundef#"\emph{configPath}\verb"#"\emph{text}\verb"#end#"
      outputs the \emph{text} iff \emph{configPath} does
      not lead a {\tt Config} that exists, otherwise
      outputs nothing. The \emph{text} has any template
      markup in it processed as usual.

   \item The sequence 
      \verb"#with#"\emph{configPath}\verb"#"\emph{text}\verb"#end#"
      outputs the \emph{text} iff \emph{configPath} leads
      to a set {\tt Config} that exists. The \emph{text}
      has any template markup in it processed using the set
      as the new root {\tt Configs}. If the
      \emph{configPath} does not lead to a {\tt Config}
      that exists, the output is \verb"_UNDEFINED_SET_". If
      the \emph{configPath} does not leads to a {\tt
      Config} that exists, but is not a set, the output is
      \verb"_NOT_A_SET_".

   \item The sequence 
      \verb"#foreach#"\emph{configPath}\verb"#"\emph{text}\verb"#end#"
      outputs the \emph{text} for each element of the list
      iff \emph{configPath} leads to a list {\tt Config}
      that exists. The \emph{text} has any template markup
      in it processed using each element as its root {\tt
      Configs}. If the \emph{configPath} does not lead to a
      {\tt Config} that exists, the output is
      \verb"_UNDEFINED_LIST_". If the \emph{configPath}
      does not leads to a {\tt Config} that exists, but is
      not a list, the output is \verb"_NOT_A_LIST_".
\end{itemize}

\subsubmodule{Populating templates}

\highlighttt{popTemplate}~$\mathit{configs}~\mathit{template}$
returns the text of the $\mathit{template}$ populated with
the data from the $\mathit{configs}$. NOT TESTED

\begin{code}
data Mode = Quiet | Print | Reprint
\end{code}

\begin{code}
popTemplate :: Configs -> String -> String
popTemplate configs = pt [(configs,Print)]
   where
   pt :: [(Configs,Mode)] -> String -> String
   -- every #end# pops a context. in each tuple is the 
   -- current Configs and whether to output
   pt [] "" = ""
   pt [_] "" = ""
   pt (_:_:_) "" = "popTemplate: not enough #end#s."
   pt [] _ = error "popTemplate: too many #end#s."
   pt contexts@((g,m):gvs) template = case template of
      '#':'#':cs -> case m of
         Quiet -> pt contexts cs
         _     -> '#' : pt contexts cs
      '#':'e':'n':'d':'#':cs -> case m of
         Reprint -> ""
         _       -> pt gvs cs
      '#':'i':'f':'d':'e':'f':'#':cs -> 
         case span (/= '#') cs of
            ([],_) -> 
               error "popTemplate: empty path (1)"
            (_,[]) -> 
               error "popTemplate: non-terminated path (1)"
            (path,'#':cs') -> case m of
               Quiet -> pt ((g,Quiet):contexts) cs'
               _     -> case lookupConfig path g of
                  Nothing -> pt ((g,Quiet):contexts) cs'
                  Just _  -> pt ((g,Print):contexts) cs'
            _ -> undefined' "pt 1"
      '#':'i':'f':'u':'n':'d':'e':'f':'#':cs -> 
         case span (/= '#') cs of
            ([],_) -> 
               error "popTemplate: empty path (2)"
            (_,[]) -> 
               error "popTemplate: non-terminated path (2)"
            (path,'#':cs') -> case m of
               Quiet -> pt ((g,Quiet):contexts) cs'
               _     -> case lookupConfig path g of
                  Nothing -> pt ((g,Print):contexts) cs'
                  Just _  -> pt ((g,Quiet):contexts) cs'
            _ -> undefined' "pt 2"
      '#':'w':'i':'t':'h':'#':cs -> 
         case span (/= '#') cs of
            ([],_) -> 
               error "popTemplate: empty path (3)"
            (_,[]) -> 
               error "popTemplate: non-terminated path (3)"
            (path,'#':cs') -> case m of
               Quiet -> pt ((g,Quiet):contexts) cs'
               _     -> case lookupConfig path g of
                  Nothing -> "_UNDEFINED_SET_" ++ 
                     pt ((g,Quiet):contexts) cs'
                  Just (CSet _ g') ->
                     pt ((g',Print):contexts) cs'
                  Just _ -> "_NOT_A_SET_" ++
                        pt ((g,Quiet):contexts) cs'
            _ -> undefined' "pt 3"
      '#':'f':'o':'r':'e':'a':'c':'h':'#':cs -> 
         case span (/= '#') cs of
            ([],_) -> 
               error "popTemplate: empty path (4)"
            (_,[]) -> 
               error "popTemplate: non-terminated path (4)"
            (path,'#':cs') -> case m of
               Quiet -> pt ((g,Quiet):contexts) cs'
               _     -> case lookupConfig path g of
                  Nothing ->
                     "_UNDEFINED_LIST_" ++ 
                        pt ((g,Quiet):contexts) cs'
                  Just (CList _ g') -> case g' of
                     [] -> pt (([],Quiet):contexts) cs'
                     es -> concat [pt 
                           ((e,Reprint):contexts) cs' |
                           e <- init es] ++
                        pt ((last es,Print):contexts) cs' 
                  Just _ ->
                     "_NOT_A_List_" ++
                        pt ((g,Quiet):contexts) cs'
            _ -> undefined' "pt 4"
      '#':cs -> 
         case span (/= '#') cs of
            ([],_) -> 
               error "popTemplate: empty path (5)"
            (_,[]) -> 
               error "popTemplate: non-terminated path (5)"
            (path,'#':cs') -> case m of
               Quiet -> pt contexts cs'
               _     -> case lookupConfig path g of
                  Nothing ->
                     "_UNDEFINED_" ++ pt contexts cs'
                  Just (CFlag v) ->
                     unString v ++ pt contexts cs'
                  Just (CParam _ v) ->
                     unString v ++  pt contexts cs'
                  Just (CSet _ _) ->
                     "_SET_" ++ pt contexts cs'
                  Just (CList _ _) ->
                     "_LIST_" ++ pt contexts cs'
            _ -> undefined' "pt 5"
      c:cs -> case m of
         Quiet -> pt contexts cs
         _     -> c : pt contexts cs
      _ -> undefined' "pt 6"
\end{code}
