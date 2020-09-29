% Showing.#ext
% This file was produced from Showing.lit

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

\module{Text.Showing} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The \highlighttt{ABR.Text.Showing} library provides functions to
help write new instances of class {\tt Show}, and to
get control of numeric precision.

\begin{code}
module ABR.Text.Showing (
   showWithSep, showWithTerm, FormattedDouble(..), 
   makeFormattedDouble, showFD
) where
\end{code}

\begin{code}
import Numeric
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2015-02-10. Passed {\tt hlint}.\\
Reviewed 2014-05-30: Made all the {\tt -Wall}s go away.\\
Reviewed 2013-11-22.\\
Reviewed 2012-11-24: Moved into {\tt ABR.Text}.
   
\submodule{Language tweaks} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
error' :: String -> a
error' label = error $
   "Error in module ABR.Text.Showing, \
   \ with label \"" ++ label ++ "\"."
\end{code}

\submodule{Adding Delimiters} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent
\highlighttt{showWithSep}~$\mathit{sep}~\mathit{xs}$
shows the elements of $\mathit{xs}$ separated by
$\mathit{sep}$.
\highlighttt{showWithTerm}~$\mathit{term}~\mathit{xs}$
shows the elements of $\mathit{xs}$ terminated by
terminator $\mathit{term}$. (Adapted from Mark Jones's
Mini Prolog.)

\begin{code}
showWithSep, showWithTerm ::
   Show a => String -> [a] -> ShowS
showWithSep s xs = case xs of
   []      -> id
   [x]     -> shows x
   (x:xs') -> 
      shows x . showString s . showWithSep s xs'
showWithTerm s xs = case xs of 
   []  -> id
   xs' -> foldr1 (.) [shows x . showString s | x <- xs']
\end{code}

\submodule{Controlling Precision} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A \highlighttt{FormattedDouble} is a {\tt Double} bound
to a desired format. The format is one of:
\highlighttt{FD} for no exponent; \highlighttt{ED} for
an exponent; or \highlighttt{GD} for the best choice
between the two. An optional integer specifies the
number of digits after the decimal point.

\begin{code}
data FormattedDouble =   FD (Maybe Int) Double
                       | ED (Maybe Int) Double
                       | GD (Maybe Int) Double
\end{code}

\noindent This {\tt Show} instance applies the
formatting to the {\tt Double}.

\begin{code}
instance Show FormattedDouble where
\end{code}

\begin{code}
   showsPrec _ fd = case fd of
      FD md x -> showFFloat md x
      ED md x -> showEFloat md x
      GD md x -> showGFloat md x
\end{code}

\noindent
\highlighttt{makeFormattedDouble}~$\mathit{format}~x$
makes a {\tt FormattedDouble} from a {\tt Double} $x$
and a string that describes the format,
$\mathit{format}$, of the form 
\verb+("f" | "e" | "g") {digit}+, {\it e.g.} \verb+"f2"+.

\begin{code}
makeFormattedDouble :: String -> Double -> FormattedDouble
makeFormattedDouble format = case format of
   "f"    -> FD Nothing
   "e"    -> ED Nothing
   "g"    -> GD Nothing
   'f':cs -> FD (Just (read cs))
   'e':cs -> ED (Just (read cs))
   'g':cs -> GD (Just (read cs))
   _      -> 
      error' "makeFormattedDouble bad format"
\end{code}

\noindent
\highlighttt{showFD}~$\mathit{format}~x$
shows $x$ using the format described by $\mathit{format}$.

\begin{code}
showFD :: String -> Double -> String
showFD format x = show (makeFormattedDouble format x) 
\end{code}

