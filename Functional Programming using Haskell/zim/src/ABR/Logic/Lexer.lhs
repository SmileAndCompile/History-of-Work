% Lexer.#ext
% This file was produced from Lexer.lit

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

\module{Lexer} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Various elements of the CDL system parse textual
representations of atoms, literals, formulas, rules and
descriptions, etc. Module \highlightModule{Lexer}
implements the functions for lexical analysis of plausible
sources.

\begin{code}
module CDL.Lexer (lexerL) where
\end{code}

\begin{code}
import Data.Char
\end{code}

\begin{code}
import ABR.Parser
import ABR.Parser.Lexers
\end{code}


\submodule{Comments} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Comments in plausible sources follow the Prolog
conventions. \syntrax{comment1}

\begin{code}
comment1L :: Lexer
comment1L =
   tokenL "%"
   <**> (many (satisfyL (/= '\n') "") *%> "")
   <**> (optional (literalL '\n') *%> "")
   %> " "
\end{code}

\noindent\syntrax{comment2}

\syntrax{comment2end}

\begin{code}
comment2L :: Lexer
comment2L = 
   tokenL "/*" <**> comment2Lend %> " "
   where
   comment2Lend :: Lexer
   comment2Lend =
          tokenL "*/"
      <|> satisfyL (const True) "" <**> comment2Lend
\end{code}

\submodule{Names} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Literals, rule labels, constants and variables are all 
instances of names that occur in plausible sources.
Two types are distinguished: \syntrax{lName}

\begin{code}
lNameL :: Lexer
lNameL = 
   (satisfyL isLower "lower-case letter" <**> 
    many (satisfyL isNameChar "letter, digit, _")
     *%> "")
   %> "lName"
   where
   isNameChar c = isAlpha c || isDigit c || c == '_'
\end{code}

\noindent\syntrax{uName}

\begin{code}
uNameL :: Lexer
uNameL = 
   (satisfyL isUpper "upper-case letter" <**>
    many (satisfyL isNameChar "letter, digit, _")
     *%> "")
   %> "uName"
   where
   isNameChar c = isAlpha c || isDigit c || c == '_'
\end{code}

\submodule{Cardinals} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent\syntrax{cardinal}

\noindent {\tt cardinalL} is implemented in 
{\tt ABR.Parser.Lexers}.

\submodule{Strings} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Atoms may be defined as arbitrary strings. \syntrax{string}

\noindent {\tt stringL} is implemented in 
{\tt ABR.Parser.Lexers}.

\submodule{Symbols} %%%%%%%%%%%%%%

\syntrax{symbol}

\begin{code}
symbolL :: Lexer
symbolL = operatorL <|> separatorL
\end{code}

\noindent\syntrax{separator}

\begin{code}
separatorL :: Lexer
separatorL = 
       tokenL ":"   %> "symbol"
   <|> tokenL ","   %> "symbol"
   <|> tokenL "{"   %> "symbol" 
   <|> tokenL "}"   %> "symbol"
   <|> tokenL "["   %> "symbol" 
   <|> tokenL "]"   %> "symbol"
   <|> tokenL "("   %> "symbol"
   <|> tokenL ")"   %> "symbol"
   <|> tokenL ".."  %> "symbol" 
   <|> tokenL "."   %> "symbol" 
   <|> tokenL "?"   %> "symbol" 
\end{code}

\noindent\syntrax{operator}

\noindent\syntrax{arrow}

\noindent\syntrax{logicOp}

\noindent\syntrax{setOp}

\noindent\syntrax{relOp}

\begin{code}
operatorL :: Lexer
operatorL = 
       tokenL "->"  %> "symbol"  
   <|> tokenL "=>"  %> "symbol"
   <|> tokenL "~>"  %> "symbol" 
   <|> tokenL "<-"  %> "symbol"
   <|> tokenL "/\\" %> "symbol" 
   <|> tokenL "\\/" %> "symbol"
   <|> tokenL "<="  %> "symbol" 
   <|> tokenL "<"   %> "symbol" 
   <|> tokenL "=="  %> "symbol" 
   <|> tokenL ">"   %> "symbol" 
   <|> tokenL "~"   %> "symbol" 
   <|> tokenL "+"   %> "symbol" 
   <|> tokenL "-"   %> "symbol"
   <|> tokenL "*"   %> "symbol"
   <|> tokenL "="   %> "symbol" 
   <|> tokenL "^"   %> "symbol"
   <|> tokenL "&"   %> "symbol"  
   <|> tokenL "|"   %> "symbol"  
\end{code} 

\submodule{Whitespace} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent\syntrax{whitespace}

\noindent {\tt whitespaceL} is implemented in 
{\tt ABR.Parser.Lexers}.

\submodule{CDL source} %%%%%%%%%%%%%%

\noindent \highlighttt{lexerL} performs the lexical analysis
of a CDL source. \syntrax{lexer}

\begin{code}
lexerL :: Lexer
lexerL = listL [comment1L, comment2L, symbolL, lNameL,
      uNameL, cardinalL, stringL, whitespaceL
   ]
\end{code}
