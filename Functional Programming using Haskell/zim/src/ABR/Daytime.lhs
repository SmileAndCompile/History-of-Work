% Daytime.#ext
% This file was produced from Daytime.lit

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

\module{Daytime} %%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Daytime} provides time of day and weekday 
manipulations.

\begin{code}
module ABR.Daytime (
      Daytime(..), Weekday(..), daytimeL, weekdayP,
      daytimeP, dayAndTimeP, showDT24, inInterval,
      tomorrow, yesterday
   ) where
\end{code}

\begin{code}
import Data.Char
\end{code}

\begin{code}
import ABR.Parser
import ABR.Parser.Lexers
import ABR.Text.String
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2014-06-09: Made all the {\tt -Wall}s go away.\\
Reviewed 2013-11-24.
   
\submodule{Language tweaks} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
undefined' :: String -> a
undefined' label = error $
   "Intentional undefined at module ABR.Daytime, \
   \ with label \"" ++ label ++ "\"."
\end{code}

\submodule{Data types} %%%%%%%%%%%%

A \highlighttt{Daytime} consists of: hours,
\highlighttt{dtHrs}; minutes, \highlighttt{dtMins};
and seconds, \highlighttt{dtSecs}.

\begin{code}
data Daytime = Daytime {
      dtHrs  :: Int,
      dtMins :: Int,
      dtSecs :: Int
   } deriving (Eq, Ord)
\end{code}

\noindent A \highlighttt{Weekday} is one of:
\highlighttt{Sunday}; \highlighttt{Monday};
\highlighttt{Tuesday}; \highlighttt{Wednesday};
\highlighttt{Thursday}; \highlighttt{Friday}; or
\highlighttt{Saturday}.

\begin{code}
data Weekday = 
   Sunday | Monday | Tuesday | Wednesday | Thursday | 
   Friday | Saturday
   deriving (Eq, Ord, Enum, Show)
\end{code}

\submodule{Lexing} %%%%%%%%%%%%

\highlighttt{daytimeL} recognizes the numbers words
and symbols that might occur in a day and/or time
specification.

\begin{code}
daytimeL :: Lexer
daytimeL = dropWhite $ listL [
      whitespaceL,
      cardinalL,
      literalL ':' %> ":", 
      literalL '.' %> ".",
      literalL ',' %> " ",
      (some (satisfyL isAlpha "") &%> "")
         @> (\[((_,w),p)] -> [(("w", map toLower w),p)])
   ]
\end{code}

\submodule{Parsing} %%%%%%%%%%%%

A weekday has this case-insensitive syntax:

\InputEBNF{Daytime}{weekday}

\noindent \highlighttt{weekdayP} parses a {\tt Weekday}.

\begin{code}
weekdayP :: Parser Weekday
weekdayP = 
       literalP "w" "su"        #> Sunday
   <|> literalP "w" "sun"       #> Sunday
   <|> literalP "w" "sunday"    #> Sunday
   <|> literalP "w" "m"         #> Monday
   <|> literalP "w" "mo"        #> Monday
   <|> literalP "w" "mon"       #> Monday
   <|> literalP "w" "monday"    #> Monday
   <|> literalP "w" "tu"        #> Tuesday
   <|> literalP "w" "tues"      #> Tuesday
   <|> literalP "w" "tuesday"   #> Tuesday
   <|> literalP "w" "w"         #> Wednesday
   <|> literalP "w" "we"        #> Wednesday
   <|> literalP "w" "wed"       #> Wednesday
   <|> literalP "w" "wednesday" #> Wednesday
   <|> literalP "w" "th"        #> Thursday
   <|> literalP "w" "thurs"     #> Thursday
   <|> literalP "w" "thursday"  #> Thursday
   <|> literalP "w" "f"         #> Friday
   <|> literalP "w" "fr"        #> Friday
   <|> literalP "w" "fri"       #> Friday
   <|> literalP "w" "friday"    #> Friday
   <|> literalP "w" "sa"        #> Saturday
   <|> literalP "w" "sat"       #> Saturday
   <|> literalP "w" "saturday"  #> Saturday
\end{code}

\noindent This type is used to store an AM/PM
designation.

\begin{code}
data AMPM = AM | PM deriving (Eq, Ord, Show)
\end{code}

\noindent An AM/PM designation has this
case-insensitive syntax:

\InputEBNF{Daytime}{ampm}

\begin{code}
ampmP :: Parser AMPM
ampmP = 
       literalP "w" "am" #> AM
   <|>     literalP "w" "a" 
       <&> literalP "." "."
       <&> literalP "w" "m"
       <&> literalP "." "."
       #> AM
   <|> literalP "w" "pm" #> PM
   <|>     literalP "w" "p" 
       <&> literalP "." "."
       <&> literalP "w" "m"
       <&> literalP "." "."
       #> PM
\end{code}

\noindent The hours in a daytime are either
in 12 or 24 hour formats. Minutes and seconds are
preceded by either a colon or a period and
are between 0 and 59.

\InputEBNF{Daytime}{hours12}

\begin{code}
hours12P :: Parser Int
hours12P = (tagP "cardinal" 
            @> (\(_,h,_) -> read h))
           `dataSatisfies` (\h -> 1 <= h && h <= 12)
\end{code}

\InputEBNF{Daytime}{hours24}

\begin{code}
hours24P :: Parser Int
hours24P = (tagP "cardinal" 
            @> (\(_,h,_) -> read h))
            `dataSatisfies` (\h -> 0 <= h && h <= 23)
\end{code}

\InputEBNF{Daytime}{minSec}

\begin{code}
minSecP :: Parser Int
minSecP =
   (literalP ":" ":" <|> literalP "." ".") &>
   (tagP "cardinal" 
    @> (\(_,m,_) -> read m))
    `dataSatisfies` (\m -> 0 <= m && m <= 59)
\end{code}

\noindent A daytime has this syntax:

\InputEBNF{Daytime}{daytime}
 
\noindent \highlighttt{daytimeP} parses a daytime.

\begin{code}
daytimeP :: Parser Daytime
daytimeP = 
       hours12P
       <&> optional (minSecP <&> optional minSecP)
       <&> ampmP
       @> (\(h,msa) -> case msa of
             ([],AM)        -> 
                Daytime (h `mod` 12) 0 0
             ([],PM)        -> 
                Daytime (h `mod` 12 + 12) 0 0
             ([(m,[])],AM)  -> 
                Daytime (h `mod` 12) m 0
             ([(m,[])],PM)  -> 
                Daytime (h `mod` 12 + 12) m 0
             ([(m,[s])],AM) -> 
                Daytime (h `mod` 12) m s
             ([(m,[s])],PM) -> 
                Daytime (h `mod` 12 + 12) m s
             _ -> undefined' "daytimeP 1"
          )
   <|> hours24P 
       <&> optional (minSecP <&> optional minSecP)
       @> (\(h,ms) -> case ms of
             []        -> Daytime h 0 0
             [(m,[])]  -> Daytime h m 0
             [(m,[s])] -> Daytime h m s
             _ -> undefined' "daytimeP 2"
          )
\end{code}

\noindent A day and time has this syntax:

\InputEBNF{Daytime}{dayAndTime}
 
\noindent \highlighttt{dayAndTimeP} parses a day and a time.

\begin{code}
dayAndTimeP :: Parser (Weekday,Daytime)
dayAndTimeP = 
       weekdayP <&> daytimeP
   <|> daytimeP <&> weekdayP
       @> (\(d,w) -> (w,d))
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%

\subsubmodule{Showing}

\begin{code}
instance Show Daytime where

   showsPrec _ (Daytime h m s) = showPad h
      . showChar '.' . showPad m
      . showChar '.' . showPad s
      where
      showPad = showString . rJustify' '0' 2 . show
\end{code}

\subsubmodule{Arithmetic}

\begin{code}
instance Num Daytime where
   
   (Daytime h m s) + (Daytime h' m' s') = 
      let ts = s + s'
          tm = m + m' + ts `div` 60
      in Daytime 
         (h + h' + tm `div` 60)
         (tm `mod` 60)
         (ts `mod` 60)

   (Daytime h m s) - (Daytime h' m' s') = 
      let ts = s - s'
          tm = m - m' - ts `div` 60
      in Daytime 
         (h - h' - tm `div` 60)
         (tm `mod` 60)
         (ts `mod` 60)

   _ * _ = undefined' "Num.*"
      
   signum _ = undefined' "Num.signum"
      
   abs _ = undefined' "Num.abs"
      
   fromInteger _ = undefined' "Num.fromInteger"
\end{code}

\submodule{Weekday methods} %%%%%%%%%%%%

\noindent \highlighttt{tomorrow}~$d$ returns the
weekday after $d$. \highlighttt{yesterday}~$d$
returns the weekday after $d$.

\begin{code}
tomorrow, yesterday :: Weekday -> Weekday
tomorrow d = toEnum ((fromEnum d + 1) `mod` 7)
yesterday d = toEnum ((fromEnum d - 1) `mod` 7)
\end{code}

\submodule{Daytime methods} %%%%%%%%%%%%

\noindent
\highlighttt{inInterval}~$\mathit{start}~\mathit{duration}~\mathit{time}$
returns {\tt True} iff $\mathit{start} \leq \mathit{time} < \mathit{start} +
\mathit{duration}$.

\begin{code}
inInterval :: Daytime -> Daytime -> Daytime -> Bool
inInterval start duration time = 
   start <= time && time < (start + duration)
\end{code}

\noindent \highlighttt{showDT24}~$t$ shows the
daytime $t$ in 24 hour format suppressing the
seconds.

\begin{code}
showDT24 :: Daytime -> String
showDT24 (Daytime h m _) = 
   pad h ++ "." ++ pad m
   where
   pad = rJustify' '0' 2 . show
\end{code}

