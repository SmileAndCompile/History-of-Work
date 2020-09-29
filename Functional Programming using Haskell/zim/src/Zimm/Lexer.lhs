\module{Lexer}

The {\tt Lexer} module provides the lexical analysis
of a JavaScript source through grouping the characters of a file into the
lexemes associated with the JavaScript language. The Lexer module utilizes the ABR.Parser, ABR.Util.Pos and ABR.Parser.Lexers libraries to lex files and provide the positions for the lexemes \cite{ABRLibraries}.

\begin{code}
module Zimm.Lexer (
      lexerL, keywords, separators, operators
   ) where
\end{code}

\begin{code}
import Data.Char
\end{code}

\begin{code}
import ABR.Util.Pos
import ABR.Parser
import ABR.Parser.Lexers
\end{code}

\submodule{Overall lexer} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent {\tt lexerL} is the lexer that recognises
all the tokens in a JavaScript program.

\VerbatimInput[lastline=2, frame=single, rulecolor=\color{blue}]{syntax/source.ebnf}

{\centering

   \includegraphics{syntax/source}

}

\begin{code}
lexerL :: Lexer
lexerL =
   tagKeywords keywords $ dropWhite $ nofail $ total $ 
      listL [whitespaceL, commentL, regexL, mLiteralL, identifierL,
         separatorL, operatorL]
\end{code}

\submodule{Language Element Lexers} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\VerbatimInput[lastline=1, frame=single, rulecolor=\color{blue}]{syntax/letter.ebnf}

{\centering

   \includegraphics{syntax/letter}

}

\begin{code}
letterL :: Lexer
letterL = satisfyL isAlpha "letter"
\end{code}

\VerbatimInput[lastline=1, frame=single, rulecolor=\color{blue}]{syntax/digit.ebnf}

{\centering

   \includegraphics{syntax/digit}

}

\begin{code}
digitL :: Lexer
digitL = satisfyL isDigit "digit"
\end{code}

\VerbatimInput[lastline=1, frame=single, rulecolor=\color{blue}]{syntax/octalDigit.ebnf}

{\centering

   \includegraphics{syntax/octalDigit}

}

\begin{code}
octalDigitL :: Lexer
octalDigitL = satisfyL isOctDigit "octalDigit"
\end{code}

\VerbatimInput[lastline=1, frame=single, rulecolor=\color{blue}]{syntax/hexDigit.ebnf}

{\centering

   \includegraphics{syntax/hexDigit}

}

\begin{code}
hexDigitL :: Lexer
hexDigitL = satisfyL isHexDigit "hexDigit"
\end{code}



\noindent Implemented in {\tt ABR.Parser}.

\VerbatimInput[lastline=1, frame=single, rulecolor=\color{blue}]{syntax/comment.ebnf}

{\centering

   \includegraphics{syntax/comment}

}

\begin{code}
commentL :: Lexer
commentL = (cCommentL <|> eolCommentL) %> " "
\end{code}

\VerbatimInput[lastline=1, frame=single, rulecolor=\color{blue}]{syntax/cComment.ebnf}

{\centering

   \includegraphics{syntax/cComment}

}

\begin{code}
cCommentL :: Lexer
cCommentL = 
        tokenL "/*" 
   <&&> nofail' "incomplete comment" cCommentEndL 
   %> "cComment"
   where
   cCommentEndL :: Lexer
   cCommentEndL =
          tokenL "*/"
      <|>      satisfyL (const True) "" 
          <&&> cCommentEndL
\end{code}

\VerbatimInput[lastline=1, frame=single, rulecolor=\color{blue}]{syntax/eolComment.ebnf}

{\centering

   \includegraphics{syntax/eolComment}

}

\begin{code}
eolCommentL :: Lexer
eolCommentL = 
   tokenL "//"
   <&&> (many (satisfyL (/= '\n') "") &%> "")
   <&&> (optional (literalL '\n') &%> "")
   %> "eolComment"
\end{code}

\VerbatimInput[lastline=1, frame=single, rulecolor=\color{blue}]{syntax/identifier.ebnf}

{\centering

   \includegraphics{syntax/identifier}

}

\begin{code}
identifierL :: Lexer
identifierL = 
   ((letterL <|> literalL '_' <|> literalL '$') <&&> 
    (many (letterL <|> digitL <|> literalL '_' <|> literalL '$')
     &%> ""))
   %> "identifier"
\end{code}


\noindent Implement boolean literals as keywords.

\VerbatimInput[lastline=2, frame=single, rulecolor=\color{blue}]{syntax/literal.ebnf}

{\centering

   \includegraphics{syntax/literal}

}

\begin{code}
mLiteralL :: Lexer
mLiteralL =     floatingLiteralL
            <|> integerLiteralL 
            <|> stringLiteral1L
            <|> stringLiteral2L
            <|> stringLiteral3L
\end{code}

\VerbatimInput[lastline=1, frame=single, rulecolor=\color{blue}]{syntax/integerLiteral.ebnf}

{\centering

   \includegraphics{syntax/integerLiteral}

}

\begin{code}
integerLiteralL :: Lexer
integerLiteralL =  
   (     hexNumeralL
     <|> octalNumeralL
     <|> decimalNumeralL
   )
   %> "integerLiteral"
\end{code}

\VerbatimInput[lastline=1, frame=single, rulecolor=\color{blue}]{syntax/decimalNumeral.ebnf}

{\centering

   \includegraphics{syntax/decimalNumeral}

}

\begin{code}
decimalNumeralL :: Lexer
decimalNumeralL = (
          literalL '0'
      <|>      digitL `alsoNotSat` literalL '0'
          <&&> (many digitL &%> "")
   ) %> "decimalNumeral"
\end{code}

\VerbatimInput[lastline=1, frame=single, rulecolor=\color{blue}]{syntax/hexNumeral.ebnf}

{\centering

   \includegraphics{syntax/hexNumeral}

}

\begin{code}
hexNumeralL :: Lexer
hexNumeralL = 
        tokenL "0x"
   <&&> (many hexDigitL &%> "")
   %> "hexNumeral"
\end{code}

\VerbatimInput[lastline=1, frame=single, rulecolor=\color{blue}]{syntax/octalNumeral.ebnf}

{\centering

   \includegraphics{syntax/octalNumeral}

}

\begin{code}
octalNumeralL :: Lexer
octalNumeralL = 
        literalL '0'
   <&&> (many octalDigitL &%> "")
   %> "octalNumeral"
\end{code}

\VerbatimInput[lastline=2, frame=single, rulecolor=\color{blue}]{syntax/floatingLiteral.ebnf}

{\centering

   \includegraphics{syntax/floatingLiteral}

}

\begin{code}
floatingLiteralL :: Lexer
floatingLiteralL = 
        (some digitL &%> "")
   <&&> literalL '.'
   <&&> (many digitL &%> "")
   <&&> (optional (
                (literalL 'e' <|> literalL 'E')
           <&&> (optional (     literalL '+' 
                            <|> literalL '-')
                 &%> "")
           <&&> (nofail' "exponent expected." 
                   (some digitL) &%> "")
        ) &%> "")
   %> "floatingLiteral"
\end{code}



\noindent Implement boolean literals as keywords.

\VerbatimInput[lastline=1, frame=single, rulecolor=\color{blue}]{syntax/escapeSequence.ebnf}

{\centering

   \includegraphics{syntax/escapeSequence}

}

\begin{code}
escapeSequenceL :: Lexer
escapeSequenceL = 
        literalL '\\' 
   <&&> nofail' "bad character escape sequence" (
               literalL 'n'
           <|> literalL 't'
           <|> literalL '\''
           <|> literalL '"'
           <|> literalL '\\'
           <|> literalL 'u'
           <|> literalL 'r'
        ) 
   %> "escapeSequence"
\end{code}

\VerbatimInput[lastline=1, frame=single, rulecolor=\color{blue}]{syntax/regex.ebnf}

{\centering

   \includegraphics{syntax/regex}

}

\begin{code}
regexL :: Lexer
regexL = 
        literalL '/'
   <&&> (many (
                  satisfyL (`notElem` "\n/") ""
              <|> escapeSequenceL
           ) &%> "")
   <&&> (literalL '/')
   %> " "
\end{code}

\VerbatimInput[lastline=2, frame=single, rulecolor=\color{blue}]{syntax/stringLiteral1.ebnf}

{\centering

   \includegraphics{syntax/stringLiteral1}

}

\begin{code}
stringLiteral1L :: Lexer
stringLiteral1L = 
        literalL '\''
   <&&> (many (
                  satisfyL (`notElem` "\'\\\n\r") ""
              <|> escapeSequenceL
           ) &%> "")
   <&&> nofail (literalL '\'')
   %> "stringLiteral"
\end{code}

\VerbatimInput[lastline=2, frame=single, rulecolor=\color{blue}]{syntax/stringLiteral2.ebnf}

{\centering

   \includegraphics{syntax/stringLiteral2}

}

\begin{code}
stringLiteral2L :: Lexer
stringLiteral2L = 
        literalL '\"'
   <&&> (many (
                  satisfyL (`notElem` "\"\\\n\r") ""
              <|> escapeSequenceL
           ) &%> "")
   <&&> nofail (literalL '\"')
   %> "stringLiteral"
\end{code}

\VerbatimInput[lastline=2, frame=single, rulecolor=\color{blue}]{syntax/stringLiteral3.ebnf}

{\centering

   \includegraphics{syntax/stringLiteral3}

}

\begin{code}
stringLiteral3L :: Lexer
stringLiteral3L = 
        literalL '`'
   <&&> (many (
                  satisfyL (`notElem` "`\\\n\r") ""
              <|> escapeSequenceL
           ) &%> "")
   <&&> nofail (literalL '`')
   %> "stringLiteral"
\end{code}

\VerbatimInput[lastline=1, frame=single, rulecolor=\color{blue}]{syntax/separator.ebnf}

{\centering

   \includegraphics{syntax/separator}

}

\begin{code}
separatorL :: Lexer
separatorL = (
          literalL '('
      <|> literalL ')'
      <|> literalL '{'
      <|> literalL '}'
      <|> literalL '['
      <|> literalL ']'
      <|> literalL ';'
      <|> literalL ','
   ) %> "separator"
\end{code}

\VerbatimInput[lastline=5, frame=single, rulecolor=\color{blue}]{syntax/operator.ebnf}

{\centering

   \includegraphics{syntax/operator}

}

\begin{code}
operatorL :: Lexer
operatorL = (
          tokenL ">>>="
      <|> tokenL "==="
      <|> tokenL "!=="
      <|> tokenL "<<="
      <|> tokenL ">>="
      <|> tokenL "**="
      <|> tokenL ">>>"
      <|> tokenL "++"
      <|> tokenL "--"
      <|> tokenL "+="
      <|> tokenL "-="
      <|> tokenL "*="
      <|> tokenL "/="
      <|> tokenL "%="
      <|> tokenL "!="
      <|> tokenL "<="
      <|> tokenL ">="
      <|> tokenL "&="
      <|> tokenL "|="
      <|> tokenL "^="
      <|> tokenL "&&"
      <|> tokenL "||"
      <|> tokenL "**"
      <|> tokenL "=="
      <|> tokenL "<<"
      <|> tokenL ">>"
      <|> literalL '\\'
      <|> literalL '+'
      <|> literalL '-'
      <|> literalL '*'
      <|> literalL '/'
      <|> literalL '%'
      <|> literalL '='
      <|> literalL '>'
      <|> literalL '<'
      <|> literalL '!'
      <|> literalL '&'
      <|> literalL '|'
      <|> literalL '~'
      <|> literalL '^'
      <|> literalL '?'
      <|> literalL ':'
      <|> literalL '.'
      <|> literalL '#'
   ) %> "operator"
\end{code}


\submodule{Keywords} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent {\tt keywords}, {\tt separators} and {\tt operators} are the 
keywords, operators and separators within the JavaScript environment language 
respectively -- which are used when generating the {\tt Token} maps for the JavaScript language, allowing these elements to be identified and mapped to their respective {\tt Tokens} when tokenizing a file.

\begin{code}
keywords :: [String]
keywords = 
   ["await", "break", "case", "catch", "class", "const", "continue",
    "debugger", "default", "delete", "do", "else", "enum", "export",
    "extends", "false", "finally", "for", "function", "if", "implements",
    "import", "in", "instanceof", "interface", "let", "new", "null", "package",
    "private", "protected", "public", "return", "super", "switch", "this",
    "throw", "try", "true", "typeof", "var", "void", "while", "with",
    "yield"]
\end{code}

\begin{code}
separators :: [String]
separators = 
   ["(", ")", "{", "}", "[", "]", ";", ","]
\end{code}

\begin{code}
operators :: [String]
operators = [">>>=", "===", "!==", "<<=", ">>=", "**=", ">>>", "++", 
             "--", "+=", "-=", "*=", "/=", "%=", "!=", "<=", ">=", "&=", 
             "|=",  "^=", "&&", "||", "**", "==", "<<", ">>", "+", "-",
             "*", "/", "%", "=", ">", "<", "!", "&", "|", "~", "^", "?",
             ":", ".", "\\", "#"]
\end{code}

\noindent {\tt tagKeywords} retags identifiers as 
keywords.

\begin{code}
tagKeywords :: [String] -> Lexer -> Lexer
tagKeywords keywords = (@> map tagKeyword)
   where
   tagKeyword :: ((Tag,Lexeme),Pos) -> ((Tag,Lexeme),Pos)
   tagKeyword tlp = case tlp of
      (("identifier", l), p) 
         | l `elem` keywords -> (("keyword", l), p)
         | otherwise         -> tlp
      _                      -> tlp
\end{code}

