% Parser.#ext
% This file was produced from Parser.lit

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

\module{Parser} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\label{parserModule}

The \highlighttt{ABR.Parser} module provides a framework for lexical
analysis and parsing using parser combinators
\cite{hutton:92,fokker:95}. 

\begin{code}
module ABR.Parser (
      Msg, Could(Fail, Error, OK), Analyser, succeedA,
      epsilonA, failA, errorA, ( <|> ), ( <&> ), ( @> ), (
      #> ), cons, some, many, optional, someUntil,
      manyUntil, ( &> ), ( <& ), alsoSat, alsoNotSat,
      dataSatisfies, dataSatisfies', total, nofail,
      nofail', preLex, Lexeme, Tag, Lexer, TLP, TLPs,
      satisfyL, literalL, ( %> ), ( <&&> ), ( <++> ), ( &%>
      ), soft, tagFilter, tokenL, endL, listL, Parser,
      tagP, lineNo, literalP, errMsg, warnMsg
   ) where
\end{code}

\begin{code}
import ABR.Util.Pos
\end{code}

\begin{code}
infixr 6 <&>, &>, <&, <&&>, <++>
infixr 5 @>, #>, %>, &%>
infixr 4 <|>
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2017-01-17: Made all the {\tt -Wall}s go away.\\
Reviewed 2015-02-03. Passed {\tt hlint}.\\
Reviewed 2014-05-30: Made all the {\tt -Wall}s go away.\\
Reviewed 2013-11-22.\\
Reviewed 2009-04-13: Split up.
   

\submodule{Error messages} %%%%%%%%%%%%%%%%%%%%%%%%

An error message, \highlighttt{Msg}, generated by an
analyser is a {\tt String}.

\begin{code}
type Msg = String
\end{code}

\submodule{Results} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

An analyser could succeed, fail or generate an
error. The \highlighttt{Could} type wraps around
any other type to indicate success (with
\highlighttt{OK}), failure (with
\highlighttt{Fail}), or an immediately identifiable
error (with \highlighttt{Error}). Failure or error
values return a diagnostic message, and a position
in the source. Failure means: ``It's not that, try
something else''. An error is unrecoverable.

\begin{code}
data Could a = Fail Pos Msg | Error Pos Msg | OK a
               deriving (Eq, Ord, Show)
\end{code}

\submodule{Analysers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent An \highlighttt{Analyser} is a
higher-level abstraction of both lexers and
parsers. An analyser is a function that tries to
accept a list of inputs of type $a$ with their
positions, and return a value constructed from
consumed inputs of type $b$ (a parse tree for
example), and any unconsumed inputs. Alternately it
could fail or generate an error. By convention,
functions that are analysers have names that end
with a capital {\tt A}.

\begin{code}
type Analyser a b = [(a,Pos)] -> Could (b, [(a,Pos)])
\end{code}

\submodule{Elementary analysers} %%%%%%%%%%%%%%%%%%%%%%%%%

This is the simplest analyser.
\highlighttt{succeedA}~$v$ succeeds with a
predetermined value $v$ and does not consume any
input.

\begin{code}
succeedA :: b -> Analyser a b
succeedA v xs = OK (v, xs)
\end{code}

\noindent \highlighttt{epsilonA} is the trivial case
of {\tt succeedA}. It always succeeds and returns
the trivial value {\tt ()}. This implements
$\epsilon$, the symbol that stands for an empty
character sequence in grammars.

\begin{code}
epsilonA :: Analyser a ()
epsilonA = succeedA ()
\end{code}

\noindent \highlighttt{failA}~$\mathit{msg}$ always
fails with a diagnostic message $\mathit{msg}$ and
the position of the next input returned.

\begin{code}
failA :: Msg -> Analyser a b
failA msg []          = Fail (-1,-1) msg
failA msg ((_,pos):_) = Fail pos msg
\end{code}

\noindent \highlighttt{errorA}~$\mathit{msg}$ always
returns an error with a diagnostic message
$\mathit{msg}$ and the position of the next input.

\begin{code}
errorA :: Msg -> Analyser a b
errorA msg []          = Error (-1,-1) msg
errorA msg ((_,pos):_) = Error pos msg
\end{code}

\noindent \highlighttt{endA} succeeds if there is no
input left and returns the trivial value 
{\tt ()}.\footnote{Thanks to Chris English for
suggesting this strategy.}

\begin{code}
endA :: Analyser a ()
endA = total epsilonA
\end{code}

\submodule{Elementary analyser combinators} %%%%%%%%%%%%%%

These combinators allow the composition of analysers.

\highlightttNoindex{<|>}\index{<\PIPE>@{\tt <\PIPE>}} is
the alternation combinator. $a_{1}$~\verb"<|>"~$a_{2}$
returns the result of $a_{1}$, or $a_{2}$ if $a_{1}$ failed.

\begin{code}
( <|> ) :: Analyser a b -> Analyser a b -> Analyser a b
( <|> ) a1 a2 xs 
   = case a1 xs of
        Fail  _   _   -> a2 xs 
        Error pos msg -> Error pos msg 
        OK stuff      -> OK stuff
\end{code}

\noindent \highlighttt{<\&>} is the sequence
combinator. $a_{1}$~\verb"<&>"~$a_{2}$ returns a
pair formed from the results of $a_{1}$ and $a_{2}$.

\begin{code}
( <&> ) ::
   Analyser a b -> Analyser a c -> Analyser a (b,c)
( <&> ) a1 a2 xs 
   = case a1 xs of
        Fail  pos1 msg1 -> Fail  pos1 msg1
        Error pos1 msg1 -> Error pos1 msg1
        OK (v1,ys)      -> case a2 ys of
           Fail  pos2 msg2 -> Fail  pos2 msg2
           Error pos2 msg2 -> Error pos2 msg2
           OK (v2,zs)      -> OK ((v1,v2),zs)
\end{code}

\submodule{Analyser result modifiers} %%%%%%%%%%%%%%%%%%%%

These functions modify an analyser by modifying
the type of value it returns.

$a$~\highlightttNoindex{@>}\index{"@>@{\tt "@>}}~$f$
changes the value returned by analyser $a$ by applying
function $f$ to it.

\begin{code}
( @> ) :: Analyser a b -> (b -> c) -> Analyser a c
( @> ) a f xs 
   = case a xs of
        Fail  pos msg -> Fail  pos msg
        Error pos msg -> Error pos msg
        OK (v,ys)     -> OK (f v, ys)
\end{code}

\noindent $a$~\highlighttt{\#>}~$v$ changes the value
returned by analyser $a$ by replacing it with $v$.

\begin{code}
( #> ) :: Analyser a b -> c -> Analyser a c
a #> v = a @> const v
\end{code}

\submodule{More analyser combinators} %%%%%%%%%%%%%%%%%%%%

This definition of \highlighttt{cons} as an uncurried form
of {\tt :} is used below.

\begin{code}
cons :: (a,[a]) -> [a]
cons = uncurry (:)
\end{code}

\noindent \highlighttt{some}~$a$ changes analyser
$a$ which recognizes one thing into an analyser
that recognizes a sequence of \emph{one} or more
things, returned in a list. 
\highlighttt{many}~$a$ changes analyser $a$ which
recognizes one thing into an analyser that
recognizes a sequence of \emph{zero} or more
things, returned in a list.
\highlighttt{optional}~$a$ changes analyser $a$
which recognizes one thing into an analyser that
recognizes either zero or one things. An empty or
singleton list of things is returned.

\begin{code}
some, many, optional:: Analyser a b -> Analyser a [b]
some     a = a <&> many a @> cons
many     a = some a <|> succeedA []
optional a = a @> (: []) <|> succeedA []
\end{code}

\noindent \highlighttt{someUntil}~$a_{1}~a_{2}$
creates an analyser that recognizes a sequence of
one or more of the things recognized by analyser
$a_{1}$, like {\tt some}, but stops consuming input
when a second analyser $a_{2}$ would also work. 
\highlighttt{manyUntil}~$a_{1}~a_{2}$ creates an
analyser that recognizes a sequence of zero or more
of the things recognized by analyser $a_{1}$, like
{\tt many}, but stops consuming input when a second
analyser $a_{2}$ would also work.

\begin{code}
someUntil, manyUntil :: 
   Analyser a b -> Analyser a c -> Analyser a [b]
someUntil a1 a2 input = case a2 input of
   OK _ ->
      failA "unexpected text" input
   Fail _ _ ->
      ((a1 <&> manyUntil a1 a2) @> cons) input
   Error _ _ ->
      ((a1 <&> manyUntil a1 a2) @> cons) input
manyUntil a1 a2 = someUntil a1 a2 <|> succeedA []
\end{code}

\noindent \highlighttt{\&>} is the same as
\verb"<&>", but discards the first value.

\begin{code}
( &> ) :: Analyser a b -> Analyser a c -> Analyser a c
a1 &> a2 = a1 <&> a2 @> snd
\end{code}

\noindent \highlighttt{<\&} is the same as
\verb"<&>", but discards second value.

\begin{code}
( <& ) :: Analyser a b -> Analyser a c -> Analyser a b
a1 <& a2 = a1 <&> a2 @> fst
\end{code}

\noindent \highlighttt{alsoSat}~$a_{1}~a_{2}$
permits analyser $a_{1}$ to succeed and consume
input iff $a_{2}$ would also succeed.
\highlighttt{alsoNotSat}~$a_{1}~a_{2}$ permits
analyser $a_{1}$ to succeed and consume input iff
$a_{2}$ would not succeed.

\begin{code}
alsoSat, alsoNotSat :: 
   Analyser a b -> Analyser a c -> Analyser a b
alsoSat a1 a2 input = case a2 input of
   OK    _       -> a1 input
   Fail  msg pos -> Fail msg pos
   Error msg pos -> Error msg pos
alsoNotSat a1 a2 [] = case a2 [] of
   OK    _       -> Fail (-1,-1) "unexpected text"
   Fail  _   _   -> a1 []
   Error _   _   -> a1 []
alsoNotSat a1 a2 ((x,p):xps) = case a2 ((x,p):xps) of
   OK    _       -> Fail p "unexpected text"
   Fail  _   _   -> a1 ((x,p):xps)
   Error _   _   -> a1 ((x,p):xps)
\end{code}

\noindent
\highlighttt{dataSatisfies}~$a~\mathit{test}$
permits analyser $a$ to succeed and consume input
only if an auxiliary $\mathit{test}$ performed on
the data returned by $a$ returns {\tt True}.

\begin{code}
dataSatisfies :: 
   Analyser a b -> (b -> Bool) -> Analyser a b
dataSatisfies _ _ []
   =  Fail (-1,-1) "end of input"
dataSatisfies a test ((x,p):xps)
   = case a ((x,p):xps) of
        OK (dat,stuff) -> 
           if test dat then OK (dat, stuff)
                       else Fail p "unexpected text"
        Fail  pos msg -> 
           Fail  pos msg
        Error pos msg -> 
           Error pos msg
\end{code}

\noindent
\highlighttt{dataSatisfies'}~$a~\mathit{test}~\mathit{msg}$
permits analyser $a$ to succeed and consume input
only if an auxiliary $\mathit{test}$ performed on
the data returned by $a$ returns {\tt True}. If this
test fails, a {\tt Fail} is returned with $\mathit{msg}$.

\begin{code}
dataSatisfies' :: 
   Analyser a b -> (b -> Bool) -> Msg -> Analyser a b
dataSatisfies' _ _ _ []
   =  Fail (-1,-1) "end of input"
dataSatisfies' a test msg ((x,p):xps)
   = case a ((x,p):xps) of
        OK (dat,stuff) -> 
           if test dat then OK (dat, stuff)
                       else Fail p msg
        Fail pos msg' -> 
           Fail pos msg'
        Error pos msg' -> 
           Error pos msg'
\end{code}

\noindent \highlighttt{total}~$a$ forces analyser
$a$ to fail if it can't consume all the inputs.

\begin{code}
total :: Analyser a b -> Analyser a b
total a xs
   = case a xs of
        Error pos msg    -> Error pos msg
        Fail  pos msg    -> Fail  pos msg
        OK (v,[])        -> OK (v,[])
        OK (_,(_,pos):_) -> Fail pos "unexpected text"
\end{code}

\noindent \highlighttt{nofail}~$a$ forces analyser
$a$ to return an error when otherwise it would
merely fail.

\begin{code}
nofail :: Analyser a b -> Analyser a b
nofail a xs = case a xs of
   Error pos msg -> Error pos msg
   Fail  pos msg -> Error pos msg
   OK stuff      -> OK stuff
\end{code}

\noindent \highlighttt{nofail'}~$a~\mathit{msg}$
forces analyser $a$ to return an error when
otherwise it would merely fail and overrides the
error message with $\mathit{msg}$.

\begin{code}
nofail' :: Msg -> Analyser a b -> Analyser a b
nofail' msg' a xs = case a xs of
   Error pos msg -> Error pos msg
   Fail  pos _   -> Error pos msg'
   OK stuff      -> OK stuff
\end{code}

\submodule{Lexers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Lexing is the process of breaking the input stream
of characters up into a stream of lexemes (tokens).
Before lexing can take place, the locations must be
added.

\highlighttt{preLex}~$\mathit{cs}$ returns all of the
characters in $\mathit{cs}$ paired with its position.

\begin{code}
preLex :: String -> [(Char,Pos)]
preLex = pl (0,0)
   where
   pl _ [] = []
   pl (line,column) ('\n':cs) 
      = ('\n', (line, column)) : pl (line + 1, 0) cs
   pl (line,column) ('\t':cs) 
      = ('\t', (line,column)) 
        : pl (line, (column `div` 8 + 1) * 8) cs
   pl (line,column) (c:cs) 
      = (c, (line,column)) : pl (line, column + 1) cs
\end{code}

\noindent Each \highlighttt{Lexeme} must be
identified as belonging to one of an expected set
of classes of lexemes. This information will be
passed from a lexer to a parser by use of a
\highlighttt{Tag}.

\begin{code}
type Lexeme = String
type Tag    = String
\end{code}

\noindent The input to a \highlighttt{Lexer}
function is a list of characters and their
positions. The output from a lexer is a list of
lexemes with their tags and positions and the list
of unconsumed characters and their positions. The
output could also be an error or failure message.
By convention, a function that is a lexer has a
name that ends with a capital {\tt L}.

\begin{code}
type Lexer = Analyser Char [((Tag,Lexeme),Pos)]
\end{code}

\noindent Lexers produce streams of tagged lexemes.
These shorthand type synonyms, \highlighttt{TLP}
and \highlighttt{TLPs} are useful when writing
functions that process lexed sources. 

\begin{code}
type TLP = ((Tag,Lexeme),Pos)
type TLPs = [TLP]
\end{code}

\submodule{Elementary lexers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{satisfyL}~$p~\mathit{tag}$
succeeds if the first input character passes test
$p$. On success the lexeme returned is the
string containing just that character and the 
tag returned is $\mathit{tag}$. On failure the
message {\tt "}$\mathit{tag}$ {\tt expected."} is
returned.

\begin{code}
satisfyL :: (Char -> Bool) -> Tag -> Lexer
satisfyL _ wantTag [] = failA (wantTag ++ " expected.") []
satisfyL p wantTag ((c,pos):cps)
   | p c
      = succeedA [((wantTag, [c]), pos)] cps 
   | otherwise
      = failA (wantTag ++ " expected.") ((c,pos):cps)
\end{code}

\noindent \highlighttt{literalL}~$c$ succeeds if
the first input is $c$. On success the lexeme
returned is {\tt [$c$]} with the tag {\tt "'$c$'"}
On failure the message {\tt "'$c$' expected."} is
returned.

\begin{code}
literalL :: Char -> Lexer
literalL wantChar
   = satisfyL (== wantChar) ('\'' : wantChar : "'")
\end{code}

\noindent These lexers are only capable of
accepting single characters. To recognize and
return tokens made up of multiple characters, we
use the combinators described below.

\submodule{Special combinators for lexers} %%%%%%%%%%%%%%%

$l$~\highlighttt{\%>}~$\mathit{tag}$ overrides the
tag produced by lexer $l$ with $\mathit{tag}$.

\begin{code}
( %> ) :: Lexer -> Tag -> Lexer
l %> newTag = l
   @> (\[((_,lexeme),pos)] -> [((newTag,lexeme),pos)])
\end{code}

\noindent $l_{1}$~\highlighttt{<\&\&>}~$l_{2}$
``hard''-sequences two lexers $l_{1}$ and $l_{2}$.
The tag returned is the space-separated catenation
of the two tags, the lexeme returned is the
catenation of the two lexemes, and the position
returned is the first position.

\begin{code}
( <&&> ) :: Lexer -> Lexer -> Lexer
l1 <&&> l2 = (l1 <&> l2) @> f
   where 
   f (x,[]) = x
   f ([],x) = x
   f ([((t,l),p)],[((t',l'),_)]) =
      [((t ++ " " ++ t', l ++ l'), p)]
   f x = error $ "<&&> non singleton lists: " ++ show x
\end{code}

\noindent $l_{1}$~\highlighttt{<++>}~$l_{2}$
``soft''-sequences two lexers $l_{1}$ and $l_{2}$.
The combined lexer returns the catenation of the
lists of lexemes produced by each Lexer.

\begin{code}
( <++> ) :: Lexer -> Lexer -> Lexer
l1 <++> l2 = l1 <&> l2 @> uncurry (++)
\end{code}

\noindent $l$~\highlighttt{\&\%>}~$\mathit{tag}$
modifies a lexer $l$ by catenation of all its
returned lexemes and returning the supplied
$\mathit{tag}$. The position returned is the first.
This permits the use of the combinators {\tt some},
{\tt many} and {\tt optional} (above).  

\begin{code}
( &%> ) :: 
   Analyser Char [[((Tag,Lexeme),Pos)]] -> Tag -> Lexer
a &%> newTag = (a @> f) %> newTag 
   where 
   f xs = [(("", concatMap (snd.fst.head) xs),
            (snd.head.head) xs)]
\end{code}

\noindent \highlighttt{soft}~$(k~l)$ returns a
lexer by soft catenating the result of some
combinator $k \in \{${\tt some},~{\tt many},~{\tt
optional},~$\ldots\}$ applied to a lexer $l$. 

\begin{code}
soft :: Analyser Char [[((Tag,Lexeme),Pos)]] -> Lexer
soft = ( @> concat)
\end{code}

\noindent \highlighttt{tagFilter}~$\mathit{tag}~l$
modifies lexer $l$ by making it throw out lexemes
with a specified $\mathit{tag}$.

\begin{code}
tagFilter :: Tag -> Lexer -> Lexer
tagFilter dropTag = ( @> filter ((/= dropTag).fst.fst))
\end{code}

\submodule{Frequently used lexers} %%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{tokenL}~$\mathit{token}$
recognizes a particular $\mathit{token}$.

\begin{code}
tokenL :: String -> Lexer
tokenL = foldr ((<&&>) . literalL) (succeedA [])
\end{code}

\noindent \highlighttt{endL} succeeds at the end of input,
returning no lexemes.

\begin{code}
endL :: Lexer
endL = endA #> []
\end{code}

\noindent This is a common lexical structure for
sources:

\InputEBNF{Parser}{source}

\noindent \highlighttt{listL}~$\mathit{ls}$ builds
a lexer for {\tt source} out of a list of
alternative lexers $\mathit{ls}$.

\begin{code}
listL :: [Lexer] -> Lexer
listL =
   soft . many . foldr (<|>) (failA "unexpected text")
\end{code}

\submodule{Parsing} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Parsing is the process of transforming the stream
of lexemes into a parse tree. The input to a
\highlighttt{Parser} is the list of lexemes with
their tags and positions. The output from a parser
is the parse tree (of type $a$ and the list of
unconsumed lexemes, or failure or error. By
convention, a function that is a parser has a name
that ends with a capital {\tt P}.

\begin{code}
type Parser a = Analyser (Tag,Lexeme) a
\end{code}

\submodule{Elementary parsers} %%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{tagP}~$\mathit{tag}$ creates a parser
that succeeds if the first lexeme has the supplied
$\mathit{tag}$. On failure, the message returned is
{\tt "$\mathit{tag}$ expected."}.

\begin{code}
tagP :: Tag -> Parser (Tag,Lexeme,Pos)
tagP wantTag [] = failA (wantTag  ++ " expected.") []
tagP wantTag (((tag,lexeme),pos):xs) 
   | wantTag == tag
      = succeedA (tag,lexeme,pos) xs
   | otherwise
      = failA (wantTag ++ " expected.")
              (((tag,lexeme),pos):xs)
\end{code}

\noindent
\highlighttt{literalP}~$\mathit{tag}~\mathit{lexeme
}$ creates a parser that succeeds if the first tag
and lexeme match the supplied $\mathit{tag}$ and
$\mathit{lexeme}$. On failure, the message returned
is {\tt "$\mathit{tag}$ "$\mathit{lexeme}$"
expected."}.
    
\begin{code}
literalP :: Tag -> Lexeme -> Parser (Tag,Lexeme,Pos)
literalP wantTag wantLex []
   = failA (wantTag  ++ " \"" ++ wantLex
            ++ "\" expected.") []
literalP wantTag wantLex (((tag,lexeme),pos):xs) 
   | wantTag == tag && wantLex == lexeme
       = succeedA (tag,lexeme,pos) xs
   | otherwise
       = failA (wantTag ++ " \"" ++ wantLex
                ++ "\" expected.") (((tag,lexeme),pos):xs)
\end{code}

\submodule{Parser result modifiers} %%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{lineNo}~$\mathit{p}$ extracts
just the line number. $p$ is a parser with the same
result type as {\tt tagP}.

\begin{code}
lineNo :: Parser (Tag,Lexeme,Pos) -> Parser Int
lineNo p = p @> (\(_,_,(l,_)) -> l)
\end{code}

\submodule{Error reporting} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{errMsg}~$\mathit{position}~\mathit{message}~\mathit{source}$
generates an error message that reports the error
$\mathit{message}$ and the offending
$\mathit{position}$ in the $\mathit{source}$. 
\highlighttt{warnMsg} does the same, but tags the output as 
only a warning.

\begin{code}
errMsg, warnMsg :: Pos -> Msg -> String -> String
errMsg  = errMsg' "Error"
warnMsg = errMsg' "Warning"
errMsg' :: String -> Pos -> Msg -> String -> String
errMsg' we (-1,_) msg source = 
   errMsg' we (sl, length (sourcelines !! sl) + 1) msg source
   where
   sourcelines = lines source
   sl = length sourcelines -1
errMsg' we _ msg [] =
   we ++ ": " ++ msg ++ "\n"
errMsg' we (line,col) msg source =
   we ++ " on line " ++ show (line + 1) ++ ": " ++ msg
   ++ "\n" ++ displaylines ++ replicate col' ' '
   ++ "^\n" ++ replicate col' ' ' ++ "|\n"
   where
   col' | col > 0   = col
        | otherwise = 0
   sourcelines = lines source
   displaylines 
      | line == 0 =
         head sourcelines ++ "\n"
      | line == 1 = 
         unlines (take 2 sourcelines)
      | otherwise =
         (unlines . take 3 . drop (line-2)) sourcelines
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance HasPos (Could a) where
\end{code}

\begin{code}
   getPos c = case c of
      Fail p _  -> p
      Error p _ -> p
      OK _      -> 
         error "Parser.HasPos.pos: OK has no Pos."
\end{code}

