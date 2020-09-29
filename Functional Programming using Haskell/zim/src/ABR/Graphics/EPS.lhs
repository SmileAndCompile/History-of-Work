% EPS.#ext
% This file was produced from EPS.lit

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

\module{Graphics.EPS} %%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Graphics.EPS} provides support for the
composition of Encapsulated PostScript (EPS).

\begin{code}
module ABR.Graphics.EPS (
      EPS, PS, BPS, bpsToEps, EPSDrawable(..),
      joinBPS, setUpFonts, 
      times10Width, times10ItalWidth, helvetica10Width,
      helvetica10ObliqueWidth, helvetica10BoldWidth,
      helvetica10BoldObliqueWidth, symbol10Width,
      FontTag(..), FontString(..), ftWidth, fsWidth,
      (++-++), MakeFontTags(..), FontBlock(..), 
      wrapWithinWidth, psStr, newpath, moveto, lineto,
      closepath, stroke, show_, arc, gsave, grestore,
      translate, box
   ) where
\end{code}

\begin{code}
import ABR.Util.Args
import ABR.Graphics.Geometry
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2014-06-06: Made all the {\tt -Wall}s go away.\\
Reviewed 2013-11-24.\\
Reviewed 2012-11-24: Moved into {\tt ABR.Graphics}.\\
Reviewed 2012-05-23: Added underlining and more fonts.
   

\submodule{Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{EPS} is plain text, consisting of
some header comments followed by drawing commands in
PostScript. The header comments are very important as they
identify the text as EPS and specify a bounding box in
which the figure appears. Any drawing outside of the
bounding box is clipped.

\begin{code}
type EPS = String
\end{code}

\noindent A \highlighttt{PS} is a sequence lines of
PostScript code \emph{in reverse order}. We build up a
figure in reverse order initially to avoid a lot of use of
\verb"++".

\begin{code}
type PS = [String]
\end{code}

\noindent A \highlighttt{BPS} is a figure in construction
with its {\tt PS} code and a list of bounding boxes that
enclose the elements of the figure. 

\begin{code}
type BPS = ([Box Double], PS) 
\end{code}

\submodule{Finalizing to EPS} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{bpsToEps}~$b$ finalizes a {\tt BPS}
figure by reversing it and constructing the EPS header
comment including the bounding box.

\begin{code}
bpsToEps :: BPS -> EPS
bpsToEps (bs, css) = 
   let (l,b,r,t) = netBox bs
   in unlines $
      ["%!PS-Adobe-2.0 EPSF-1.2",
       "%%BoundingBox: " ++ unwords (map show 
          [floor (l - 1) :: Int, floor (b - 1),
          ceiling (r + 1),  ceiling (t + 1)]),
       "%%EndComments"
      ] ++ reverse css ++ ["showpage"]
\end{code}

\submodule{Drawing in BPS}

\noindent \highlighttt{epsDraw}~$\mathit{options}~x$
renders $x$ as a {\tt BPS}, where $x$ has a data type
which is an instance of \highlighttt{EPSDrawable} and
$\mathit{options}$ contains settings that might affect the
rendering.

\begin{code}
class EPSDrawable a where
\end{code}

\begin{code}
   epsDraw :: Options -> a -> BPS
\end{code}

\submodule{Merging BPS components}

\highlighttt{joinBPS}~$a~b~\Delta_x~\Delta_y$ puts
figure $b$ over figure $a$ displaced by $\Delta_x$
and $\Delta_y$.

\begin{code}
joinBPS :: BPS -> BPS -> Double -> Double -> BPS
joinBPS (bs,ps) (bs',ps') dx dy = 
   let bs'' = [(l + dx, b + dy, r + dx, t + dy) |
          (l,b,r,t) <- bs']
   in (bs ++ bs'', [grestore] ++ ps' ++ [
      unwords [gsave, show dx, show dy, translate]
      ] ++ ps)
\end{code}

\submodule{Drawing text}

\subsubmodule{Switching fonts efficiently}

\noindent \highlighttt{setUpFonts} is {\tt PS} code to
find the fonts and define procedures for switching to
them. 

\begin{code}
setUpFonts :: PS
setUpFonts = [
      "/symbol10 { fs10 setfont } def",
      "/times10 { ftr10 setfont } def",
      "/times10ital { fti10 setfont } def",
      "/helv10 { fh10 setfont } def",
      "/helv10obl { fho10 setfont } def",
      "/helv10bld { fhb10 setfont } def",
      "/helv10bldobl { fhbo10 setfont } def",
      "/ftr10 /Times-Roman findfont 10 scalefont def",
      "/fti10 /Times-Italic findfont 10 scalefont def",
      "/fh10 /Helvetica findfont 10 scalefont def",
      "/fho10 /Helvetica-Oblique findfont 10 scalefont def",
      "/fhb10 /Helvetica-Bold findfont 10 scalefont def",
      "/fhbo10 /Helvetica-BoldOblique findfont 10 scalefont\
         \ def",
      "/fs10 /Symbol findfont 10 scalefont def"
   ]
\end{code}

\subsubmodule{Font metrics}

\noindent \highlighttt{times10Width}~$c$ returns the width
of a character $c$ in Times-Roman 10 point, in 72 dpi
pixels. (not exhaustive)

\begin{code}
times10Width :: Char -> Double
times10Width c = case c of
   ' '    -> 2.5
   '!'    -> 3.32969
   '"'    -> 4.07969
   '#'    -> 5.0
   '$'    -> 5.0
   '%'    -> 8.32969
   '&'    -> 7.77969
   '\''   -> 3.32969
   '('    -> 3.32969
   ')'    -> 3.32969
   '*'    -> 5.0
   '+'    -> 5.63984
   ','    -> 2.5
   '-'    -> 3.32969
   '.'    -> 2.5
   '/'    -> 2.77969
   '0'    -> 5.0
   '1'    -> 5.0
   '2'    -> 5.0
   '3'    -> 5.0
   '4'    -> 5.0
   '5'    -> 5.0
   '6'    -> 5.0
   '7'    -> 5.0
   '8'    -> 5.0
   '9'    -> 5.0
   ':'    -> 2.77969
   ';'    -> 2.77969
   '<'    -> 5.63984
   '='    -> 5.63984
   '>'    -> 5.63984
   '?'    -> 4.43984
   '@'    -> 9.20977
   'A'    -> 7.21992
   'B'    -> 6.66992
   'C'    -> 6.66992
   'D'    -> 7.21992
   'E'    -> 6.10977
   'F'    -> 5.55977
   'G'    -> 7.21992
   'H'    -> 7.21992
   'I'    -> 3.32969
   'J'    -> 3.88984
   'K'    -> 7.21992
   'L'    -> 6.10977
   'M'    -> 8.88984
   'N'    -> 7.21992
   'O'    -> 7.21992
   'P'    -> 5.55977
   'Q'    -> 7.21992
   'R'    -> 6.66992
   'S'    -> 5.55977
   'T'    -> 6.10977
   'U'    -> 7.21992
   'V'    -> 7.21992
   'W'    -> 9.43984
   'X'    -> 7.21992
   'Y'    -> 7.21992
   'Z'    -> 6.10977
   '['    -> 3.32969
   '\\'   -> 2.77969
   ']'    -> 3.32969
   '^'    -> 4.68984
   '_'    -> 5.0
   '`'    -> 3.32969
   'a'    -> 4.43894
   'b'    -> 5.0
   'c'    -> 4.43894
   'd'    -> 5.0
   'e'    -> 4.43894
   'f'    -> 3.32969
   'g'    -> 5.0
   'h'    -> 5.0
   'i'    -> 2.77969
   'j'    -> 2.77969
   'k'    -> 5.0
   'l'    -> 2.77969
   'm'    -> 7.77969
   'n'    -> 5.0
   'o'    -> 5.0
   'p'    -> 5.0
   'q'    -> 5.0
   'r'    -> 3.32969
   's'    -> 3.88984
   't'    -> 2.77969
   'u'    -> 5.0
   'v'    -> 5.0
   'w'    -> 7.21992
   'x'    -> 5.0
   'y'    -> 5.0
   'z'    -> 4.43894
   '{'    -> 4.8
   '|'    -> 2.0
   '}'    -> 4.8
   '~'    -> 5.40977
   '\170' -> 4.43984 -- open double quote
   '\177' -> 5.0 -- dash/minus
   '\186' -> 4.43984 -- close double quote
   _      -> 2.5
\end{code}

\noindent \highlighttt{times10ItalWidth}~$c$ returns the
width of a character $c$ in Times-Italic 10 point, in 72
dpi pixels. (not exhaustive)

\begin{code}
times10ItalWidth :: Char -> Double
times10ItalWidth c = case c of
   '0'    -> 5.0
   '1'    -> 5.0
   '2'    -> 5.0
   '3'    -> 5.0
   '4'    -> 5.0
   '5'    -> 5.0
   '6'    -> 5.0
   '7'    -> 5.0
   '8'    -> 5.0
   '9'    -> 5.0
   'A'    -> 6.10999
   'B'    -> 6.10999
   'C'    -> 6.67
   'D'    -> 7.22
   'E'    -> 6.10999
   'F'    -> 6.10999
   'G'    -> 7.22
   'H'    -> 7.22
   'I'    -> 3.32998
   'J'    -> 4.43999
   'K'    -> 6.67
   'L'    -> 5.55999
   'M'    -> 8.32998
   'N'    -> 6.67
   'O'    -> 7.22
   'P'    -> 6.10999
   'Q'    -> 7.22
   'R'    -> 6.10999
   'S'    -> 5.0
   'T'    -> 5.55999
   'U'    -> 7.22
   'V'    -> 6.10999
   'W'    -> 8.32998
   'X'    -> 6.10999
   'Y'    -> 5.55999
   'Z'    -> 5.55999
   '_'    -> 5.0
   'a'    -> 5.0
   'b'    -> 5.0
   'c'    -> 4.43999
   'd'    -> 5.0
   'e'    -> 4.43999
   'f'    -> 2.77998
   'g'    -> 5.0
   'h'    -> 5.0
   'i'    -> 2.77998
   'j'    -> 2.77998
   'k'    -> 4.43999
   'l'    -> 2.77998
   'm'    -> 7.22
   'n'    -> 5.0
   'o'    -> 5.0
   'p'    -> 5.0
   'q'    -> 5.0
   'r'    -> 3.88999
   's'    -> 3.88999
   't'    -> 2.77998
   'u'    -> 5.0
   'v'    -> 4.43999
   'w'    -> 6.67
   'x'    -> 4.43999
   'y'    -> 4.43999
   'z'    -> 3.88999
   _      -> 2.5
\end{code}

\noindent \highlighttt{helvetica10Width}~$c$ returns the width
of a character $c$ in Helvetica 10 point, in 72 dpi
pixels. (not exhaustive)

\begin{code}
helvetica10Width :: Char -> Double
helvetica10Width c = case c of
   ' '    -> 2.77832
   '!'    -> 2.77832
   '"'    -> 3.54981
   '#'    -> 5.56152
   '$'    -> 5.56152
   '%'    -> 8.8916
   '&'    -> 6.66992
   '\''   -> 2.22168
   '('    -> 3.33008
   ')'    -> 3.33008
   '*'    -> 3.8916
   '+'    -> 5.83984
   ','    -> 2.77832
   '-'    -> 3.33008
   '.'    -> 2.77832
   '/'    -> 2.77832
   '0'    -> 5.56152
   '1'    -> 5.56152
   '2'    -> 5.56152
   '3'    -> 5.56152
   '4'    -> 5.56152
   '5'    -> 5.56152
   '6'    -> 5.56152
   '7'    -> 5.56152
   '8'    -> 5.56152
   '9'    -> 5.56152
   ':'    -> 2.77832
   ';'    -> 2.77832
   '<'    -> 5.83984
   '='    -> 5.83984
   '>'    -> 5.83984
   '?'    -> 5.56152
   '@'    -> 10.1514
   'A'    -> 6.66992
   'B'    -> 6.66992
   'C'    -> 7.22168
   'D'    -> 7.22168
   'E'    -> 6.66992
   'F'    -> 6.1084
   'G'    -> 7.77832
   'H'    -> 7.22168
   'I'    -> 2.77832
   'J'    -> 5.0
   'K'    -> 6.66992
   'L'    -> 5.56152
   'M'    -> 8.33008
   'N'    -> 7.22168
   'O'    -> 7.77832
   'P'    -> 6.66992
   'Q'    -> 7.77832
   'R'    -> 7.22168
   'S'    -> 6.66992
   'T'    -> 6.1084
   'U'    -> 7.22168
   'V'    -> 6.66992
   'W'    -> 9.43848
   'X'    -> 6.66992
   'Y'    -> 6.66992
   'Z'    -> 6.1084
   '['    -> 2.77832
   '\\'   -> 2.77832
   ']'    -> 2.77832
   '^'    -> 4.69238
   '_'    -> 5.56152
   '`'    -> 2.22168
   'a'    -> 5.56152
   'b'    -> 5.56152
   'c'    -> 5.0
   'd'    -> 5.56152
   'e'    -> 5.56152
   'f'    -> 2.77832
   'g'    -> 5.56152
   'h'    -> 5.56152
   'i'    -> 2.22168
   'j'    -> 2.22168
   'k'    -> 5.0
   'l'    -> 2.22168
   'm'    -> 8.33008
   'n'    -> 5.56152
   'o'    -> 5.56152
   'p'    -> 5.56152
   'q'    -> 5.56152
   'r'    -> 3.33008
   's'    -> 5.0
   't'    -> 2.77832
   'u'    -> 5.56152
   'v'    -> 5.0
   'w'    -> 7.22168
   'x'    -> 5.0
   'y'    -> 5.0
   'z'    -> 5.0
   '{'    -> 3.33984
   '|'    -> 2.59766
   '}'    -> 3.33984
   '~'    -> 5.83984
   '\170' -> 3.33008 -- open double quote
   '\177' -> 5.56152 -- dash/minus
   '\186' -> 3.33008 -- close double quote
   _      -> 6.33789
\end{code}

\noindent \highlighttt{helvetica10ObliqueWidth}~$c$ returns the width
of a character $c$ in Helvetica 10 point oblique, in 72 dpi
pixels. (not exhaustive)

\begin{code}
helvetica10ObliqueWidth :: Char -> Double
helvetica10ObliqueWidth = 
   helvetica10Width -- all the same!
\end{code}

\noindent \highlighttt{helvetica10BoldWidth}~$c$ returns the width
of a character $c$ in Helvetica 10 point bold, in 72 dpi
pixels. (not exhaustive)

\begin{code}
helvetica10BoldWidth :: Char -> Double
helvetica10BoldWidth c = case c of
   ' '    -> 2.77832
   '!'    -> 3.33008
   '"'    -> 4.74121
   '#'    -> 5.56152
   '$'    -> 5.56152
   '%'    -> 8.8916
   '&'    -> 7.22168
   '\''   -> 2.77832
   '('    -> 3.33008
   ')'    -> 3.33008
   '*'    -> 3.8916
   '+'    -> 5.83984
   ','    -> 2.77832
   '-'    -> 3.33008
   '.'    -> 2.77832
   '/'    -> 2.77832
   '0'    -> 5.56152
   '1'    -> 5.56152
   '2'    -> 5.56152
   '3'    -> 5.56152
   '4'    -> 5.56152
   '5'    -> 5.56152
   '6'    -> 5.56152
   '7'    -> 5.56152
   '8'    -> 5.56152
   '9'    -> 5.56152
   ':'    -> 3.33008
   ';'    -> 3.33008
   '<'    -> 5.83984
   '='    -> 5.83984
   '>'    -> 5.83984
   '?'    -> 6.1084
   '@'    -> 9.75098
   'A'    -> 7.22168
   'B'    -> 7.22168
   'C'    -> 7.22168
   'D'    -> 7.22168
   'E'    -> 6.66992
   'F'    -> 6.1084
   'G'    -> 7.77832
   'H'    -> 7.22168
   'I'    -> 2.77832
   'J'    -> 5.56152
   'K'    -> 7.22168
   'L'    -> 6.1084
   'M'    -> 8.33008
   'N'    -> 7.22168
   'O'    -> 7.77832
   'P'    -> 6.66992
   'Q'    -> 7.77832
   'R'    -> 7.22168
   'S'    -> 6.66992
   'T'    -> 6.1084
   'U'    -> 7.22168
   'V'    -> 6.66992
   'W'    -> 9.43848
   'X'    -> 6.66992
   'Y'    -> 6.66992
   'Z'    -> 6.1084
   '['    -> 3.33008
   '\\'   -> 2.77832
   ']'    -> 3.33008
   '^'    -> 5.83984
   '_'    -> 5.56152
   '`'    -> 2.77832
   'a'    -> 5.56152
   'b'    -> 6.1084
   'c'    -> 5.56152
   'd'    -> 6.1084
   'e'    -> 5.56152
   'f'    -> 3.33008
   'g'    -> 6.1084
   'h'    -> 6.1084
   'i'    -> 2.77832
   'j'    -> 2.77832
   'k'    -> 5.56152
   'l'    -> 2.77832
   'm'    -> 8.8916
   'n'    -> 6.1084
   'o'    -> 6.1084
   'p'    -> 6.1084
   'q'    -> 6.1084
   'r'    -> 3.8916
   's'    -> 5.56152
   't'    -> 3.33008
   'u'    -> 6.1084
   'v'    -> 5.56152
   'w'    -> 7.77832
   'x'    -> 5.56152
   'y'    -> 5.56152
   'z'    -> 5.0
   '{'    -> 3.8916
   '|'    -> 2.79785
   '}'    -> 3.8916
   '~'    -> 5.83984
   '\170' -> 5.0 -- open double quote
   '\177' -> 5.56152 -- dash/minus
   '\186' -> 5.0 -- close double quote
   _      -> 7.22168
\end{code}

\noindent \highlighttt{helvetica10BoldObliqueWidth}~$c$ returns the width
of a character $c$ in Helvetica 10 point bold oblique, in 72 dpi
pixels. (not exhaustive)

\begin{code}
helvetica10BoldObliqueWidth :: Char -> Double
helvetica10BoldObliqueWidth = 
   helvetica10BoldWidth -- all the same!
\end{code}

\noindent \highlighttt{symbol10Width}~$c$ returns the
width of a character $c$ in Symbol 10 point, in 72 dpi
pixels. (not exhaustive)

\begin{code}
symbol10Width :: Char -> Double
symbol10Width c = case c of
   '!'    -> 3.32969
   '\34'  -> 7.12969 -- for all
   '#'    -> 5.0
   '\36'  -> 5.48984 -- there exists
   '%'    -> 8.32969
   '&'    -> 7.77969
   '\39'  -> 4.38984 -- backwards var epsilon
   '('    -> 3.32969
   ')'    -> 3.32969
   '*'    -> 5.0
   '+'    -> 5.48984
   '\45'  -> 5.48984 -- dash
   '/'    -> 2.77969
   '0'    -> 5.0
   '1'    -> 5.0
   '2'    -> 5.0
   '3'    -> 5.0
   '4'    -> 5.0
   '5'    -> 5.0
   '6'    -> 5.0
   '7'    -> 5.0
   '8'    -> 5.0
   '9'    -> 5.0
   ':'    -> 2.77969
   ';'    -> 2.77969
   '<'    -> 5.48984
   '='    -> 5.48984
   '>'    -> 5.48984
   '?'    -> 4.43984
   '\64'  -> 9.20977 -- ~ over =
   'A'    -> 7.21992 -- Alpha
   'B'    -> 6.66992 -- Beta
   'C'    -> 7.21992 -- Chi
   'D'    -> 6.11992 -- Delta 
   'E'    -> 6.10977 -- Epsilon
   'F'    -> 7.62969 -- Phi
   'G'    -> 6.02969 -- Gamma
   'H'    -> 7.21992 -- Eta
   'I'    -> 3.32969 -- Iota
   'J'    -> 6.30977 -- var theta
   'K'    -> 7.21992 -- Kappa
   'L'    -> 6.85977 -- Lambda
   'M'    -> 8.88984 -- Mu
   'N'    -> 7.21992 -- Nu
   'O'    -> 7.21992 -- Omicron
   'P'    -> 7.67969 -- Pi
   'Q'    -> 7.40977 -- Theta
   'R'    -> 5.55977 -- Rho
   'S'    -> 5.91992 -- Sigma
   'T'    -> 6.10977 -- Tau
   'U'    -> 6.9     -- Upsilon
   'V'    -> 4.38984 -- var zeta
   'W'    -> 7.67969 -- Omega
   'X'    -> 6.45    -- Xi
   'Y'    -> 7.95    -- Psi
   'Z'    -> 6.10977 -- Zeta
   '['    -> 3.32969
   '\92'  -> 2.77969 -- therefore
   ']'    -> 3.32969 
   '\94'  -> 6.57969 -- perp
   '\95'  -> 5.0     -- heavy underscore
   '\96'  -> 5.0     -- overscore
   'a'    -> 6.30977 -- alpha
   'b'    -> 5.48984 -- beta
   'c'    -> 5.48984 -- chi
   'd'    -> 4.93984 -- delta
   'e'    -> 4.43894 -- epsilon
   'f'    -> 5.20977 -- phi
   'g'    -> 4.10977 -- gamma
   'h'    -> 6.02969 -- eta
   'i'    -> 3.28984 -- iota
   'j'    -> 6.02969 -- var phi
   'k'    -> 5.48984 -- kappa
   'l'    -> 5.48984 -- lambda
   'm'    -> 5.75977 -- mu
   'n'    -> 5.20977 -- nu
   'o'    -> 5.48984 -- omicron
   'p'    -> 5.48984 -- pi
   'q'    -> 5.20977 -- theta
   'r'    -> 5.48984 -- rho
   's'    -> 6.02969 -- sigma
   't'    -> 4.38984 -- tau
   'u'    -> 5.75977 -- upsilon
   'v'    -> 7.12969 -- var pi
   'w'    -> 6.85977 -- omega
   'x'    -> 4.92969 -- xi
   'y'    -> 6.85977 -- psi
   'z'    -> 4.93984 -- zeta
   '{'    -> 4.8
   '|'    -> 2.0
   '}'    -> 4.8
   '~'    -> 5.48984 -- longer tilde
   '\160' -> 7.61992 -- Euro
   '\163' -> 5.48984 -- le 
   '\172' -> 9.86992 -- leftarrow
   '\179' -> 5.48984 -- ge 
   '\180' -> 5.48984 -- times 
   '\184' -> 5.48984 -- div
   '\185' -> 5.48984 -- ne
   '\216' -> 7.12969 -- not
   '\217' -> 6.02969 -- wedge/and
   '\218' -> 6.02969 -- vee/or
   _     -> 2.5
\end{code}

\subsubmodule{Font tags}

\noindent Type \highlighttt{FontTag} tags a string (\highlighttt{ftStr}) with
either:

\begin{itemize}
   \item \highlighttt{Space} -- a space between symbols at which lines may be broken.
   \item \highlighttt{Times10} -- Times font, roman face, 10 point; 
   \item \highlighttt{Times10Ital} -- Times font, italic face, 10 point;
   \item \highlighttt{Helvetica10} -- Helvetica font, 10 point; 
   \item \highlighttt{Helvetica10Oblique} -- Helvetica font, oblique face, 10 point; 
   \item \highlighttt{Helvetica10Bold} -- Helvetica font, bold face, 10 point; 
   \item \highlighttt{Helvetica10BoldOblique} -- Helvetica font, bold-olbique face, 10 point; or
   \item \highlighttt{Symbol10} -- Symbol font, 10 point.
\end{itemize}

\noindent Each tag may also optionally be underlined (\highlighttt{ftUnder}).

\begin{code}
data FontTag =   
     Space { 
        ftUnder :: Bool
     }
   | Times10 { 
        ftUnder :: Bool,
        ftStr   :: String
     }
   | Times10Ital { 
        ftUnder :: Bool,
        ftStr   :: String
     }
   | Helvetica10 { 
        ftUnder :: Bool,
        ftStr   :: String
     }
   | Helvetica10Oblique { 
        ftUnder :: Bool,
        ftStr   :: String
     }
   | Helvetica10Bold { 
        ftUnder :: Bool,
        ftStr   :: String
     }
   | Helvetica10BoldOblique { 
        ftUnder :: Bool,
        ftStr   :: String
     }
   | Symbol10 { 
        ftUnder :: Bool,
        ftStr   :: String
     }
   deriving Show
\end{code}

\noindent \highlighttt{ftWidth}~$f$ returns the total
width of {\tt FontTag} $f$.

\begin{code}
ftWidth :: FontTag -> Double
ftWidth f = case f of
   Space                  _    -> 
      times10Width ' '
   Times10                _ cs -> 
      sum $ map times10Width cs
   Times10Ital            _ cs -> 
      sum $ map times10Width cs
   Helvetica10            _ cs -> 
      sum $ map helvetica10Width cs
   Helvetica10Oblique     _ cs -> 
      sum $ map helvetica10ObliqueWidth cs
   Helvetica10Bold        _ cs -> 
      sum $ map helvetica10BoldWidth cs
   Helvetica10BoldOblique _ cs -> 
      sum $ map helvetica10BoldObliqueWidth cs
   Symbol10               _ cs ->
      sum $ map symbol10Width cs
\end{code}

\subsubmodule{Font strings}

\noindent Type \highlighttt{FontString} is a sequence of
Strings, tagged by the font they are to be rendering in.

\begin{code}
data FontString = FontString [FontTag]
   deriving Show
\end{code}

\noindent \highlighttt{fsWidth}~$f$ returns the total
width of {\tt FontString} $f$.

\begin{code}
fsWidth :: FontString -> Double
fsWidth (FontString fs) = sum $ map ftWidth fs
\end{code}

\noindent $f$~\highlighttt{++-++}~$f'$ catenates
{\tt FontString}s $f$ and $f'$, with a space added at
the join. Iff both of the {\tt FontTag}s at the joining
ends are underlined, the joining space will also be underlined.

\begin{code}
(++-++) :: FontString -> FontString -> FontString
(FontString ts) ++-++ (FontString ts') = 
   FontString (ts ++ Space  (not (null ts) && 
      not (null ts') && ftUnder (last ts) && 
      ftUnder (head ts')) : ts')
\end{code}

\noindent {\tt fsWords}~$f$ groups a {\tt
FontString} into the {\tt Space}-separated 
{\tt FontString}s.

\begin{code}
fsWords :: FontString -> [FontString]
fsWords (FontString fs) = reverse $ w [] [] fs
   where
   w :: [FontString] -> [FontTag] -> [FontTag] -> 
      [FontString]
   w fss [] []              = fss
   w fss fs' []              = FontString (reverse fs') : fss
   w fss [] (Space _ : fs') = w fss [] fs'
   w fss fs' (Space _ : fs'') = 
      w (FontString (reverse fs') : fss) [] fs''
   w fss fs' (f:fs'')         = w fss (f : fs') fs''
\end{code}

\noindent Instances of class \highlighttt{MakeFontTags}
may be encoded as {\tt FontString}s.

\begin{code}
class MakeFontTags a where
\end{code}

\noindent \highlighttt{makeFontTags}~$x$ renders $x$ as
a {\tt FontString}

\begin{code}
   makeFontTags :: a -> [FontTag]
   makeFontTags = makeFontTagsPrec 0
\end{code}

\noindent \highlighttt{makeFontTagsPrec}~$p~x$ renders 
$x$ as a {\tt FontString}, in parentheses if $p$ is greater
that then precedence of object $x$ (as per {\tt showsPrec}.

\begin{code}
   makeFontTagsPrec :: Int -> a -> [FontTag]
   makeFontTagsPrec _ = makeFontTags
\end{code}

\subsubmodule{Font blocks}

\noindent Type \highlighttt{FontBlock} is a sequence of
{\tt FontString}s to be drawn as a block.

\begin{code}
data FontBlock = FontBlock [FontString]
   deriving Show
\end{code}

\noindent \highlighttt{wrapWithinWidth}~$w~f$ wraps
{\tt FontString} $f$ at the {\tt Space}s it contains
to within maximum width $w$ if possible, returning
the wrapped {\tt FontBlock}.

\begin{code}
wrapWithinWidth :: Double -> FontString -> FontBlock
wrapWithinWidth w = 
   FontBlock . wrap 0 (FontString []) . fsWords 
   where
   wrap :: Double -> FontString -> [FontString] 
      -> [FontString] 
   wrap _ (FontString []) []      = []
   wrap _ fs              []      = [fs]
   wrap n fs              (f:fs') = 
      let n' = fsWidth f
          n'' = n + times10Width ' ' + n'
      in if n < 0.01 then
            wrap n' f fs'
         else if n'' <= w then
            wrap n'' (fs ++-++ f) fs'
         else
            fs : wrap n' f fs'
\end{code}

% \noindent \highlighttt{wrapToAspect}~$f~a$ wraps {\tt
% FontString} $f$ at the {\tt Space}s it contains to as
% close to the desired aspect ratio $a$ as possible, where
% an aspect ratio is the height divided by the width,
% returning the wrapped {\tt FontBlock}.
% 
% \begin{code}
% wrapToAspect :: FontString -> Double -> FontBlock
% [-EPS.tex]
% wrapToAspect fs a = undefined
% [+EPS.tex]
% \end{code}

\subsubmodule{Text encoding}

\noindent \highlighttt{psStr}~$\mathit{cs}$ encodes a
string for inclusion in PostScript as a literal.

\begin{code}
psStr :: String -> String
psStr cs = '(' : concatMap f cs ++ ")"
   where
   f c = case c of
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      '\b' -> "\\b"
      '\f' -> "\\f"
      '\\' -> "\\\\"
      '('  -> "\\("
      ')'  -> "\\)"
      _    -> [c]
\end{code}

\submodule{Conveniences}

\noindent Some PostScript operators:
\highlighttt{newpath}; \highlighttt{moveto};
\highlighttt{lineto}; \highlighttt{closepath};
\highlighttt{stroke}; \highlighttt{show\_};
\highlighttt{arc}; \highlighttt{gsave};
\highlighttt{grestore}; \highlighttt{translate}.

\begin{code}
newpath, moveto, lineto, closepath, stroke, show_,
   arc, gsave, grestore, translate :: String
newpath   = "newpath"
moveto    = "moveto"
lineto    = "lineto"
closepath = "closepath"
stroke    = "stroke"
show_     = "show"
arc       = "arc"
gsave     = "gsave"
grestore  = "grestore"
translate = "translate"
\end{code}

\noindent Draw a \highlighttt{box}.

\begin{code}
box :: Double -> Double -> Double -> Double -> BPS
box l b r t = 
   let [l', b', r', t'] = map show [l, b, r, t]
   in ([(l, b, r, t)], [
         unwords [newpath, l', b', moveto, l', t', lineto,
            r', t', lineto, r', b', lineto, closepath,
            stroke]
      ])
\end{code}

\submodule{Instance declarations}

\subsubmodule{EPSDrawable}

\begin{code}
instance EPSDrawable FontTag where
\end{code}

{

\centering

\epsfig{figure=figures/EPS/FontTag}

}

\begin{code}
   epsDraw _ ft = 
      let w = ftWidth ft
          s = psStr (case ft of
             Space _ -> " "
             _       -> ftStr ft)
          f = case ft of
             Space                  _   -> "times10"
             Times10                _ _ -> "times10"
             Times10Ital            _ _ -> "times10ital"
             Helvetica10            _ _ -> "helv10"
             Helvetica10Oblique     _ _ -> "helv10obl"
             Helvetica10Bold        _ _ -> "helv10bld"
             Helvetica10BoldOblique _ _ -> "helv10bldobl"
             Symbol10               _ _ -> "symbol10"
     in ([(0, -2, w, 10)], (
            "0 0 moveto " ++ f ++ " " ++ s ++ " show" 
         ) : ["0.4 setlinewidth newpath 0 -1.2 moveto " 
              ++ show w ++ " -1.2 lineto stroke"
             | ftUnder ft ])   
\end{code}

\begin{code}
instance EPSDrawable FontString where
\end{code}

{

\centering

\epsfig{figure=figures/EPS/FontString}

}

\begin{code}
   epsDraw options (FontString fs) = 
      let p :: [FontTag] -> BPS
          p fs' = case fs' of
             []         -> ([(0,0,0,0)],[])
             (f : fs'') ->
                let ([(l,b,r,t)],ps) = epsDraw options f
                    ([(_,_,r',_)],ps') = p fs''
                in ([(l, b, r + r', t)], ps ++ [grestore] ++
                      ps' ++ [show r ++ " 0 translate"] ++
                      [gsave])
      in p fs 
\end{code}

\begin{code}
instance EPSDrawable FontBlock where
\end{code}

{

\centering

\epsfig{figure=figures/EPS/FontBlock}

}

\begin{code}
   epsDraw options (FontBlock fs) = case fs of
      []      -> ([(0,0,0,0)],[])
      [f]     -> epsDraw options f
      f : fs' -> 
         let ([(l,_,r,t)],ps) = epsDraw options f
             ([(_,b',r',_)],ps') = 
                epsDraw options (FontBlock fs')
         in ([(l, b' - 11, max r r', t)], 
             ps ++ [grestore] ++ ps' ++ 
             ["gsave 0 -11 translate"])
\end{code}

