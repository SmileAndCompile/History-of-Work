% Geometry.#ext
% This file was produced from Geometry.lit

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

\module{Graphics.Geometry} %%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Graphics.Geometry} implements some basic 
geometric calculations.

\begin{code}
{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances #-}
\end{code}

\begin{code}
module ABR.Graphics.Geometry (
      Point, Box, Angle,
      GeoNum(
         netBox, shiftBoxes, leastRightShift,
         placeAroundOval, iGeo, iPoint, iBox,
         insetBox
      )
   ) where
\end{code}

\begin{code}
import Data.List
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2014-06-03: Made all the {\tt -Wall}s go away.\\
Reviewed 2013-11-24.\\
Reviewed 2012-11-24: Moved into {\tt ABR.Graphics}.
   

\submodule{Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent A \highlighttt{Point} on the plane in
Cartesian coordinates $(x,y)$. It is assumed that the
coordinate system in conventional, with the $y$-axis
the right way up, unlike most screen graphics
coordinate systems. The actual numeric type is not 
specified, and where possible functions will be written
to accomodate both any of {\tt Float}, {\tt Double},
{\tt Int}, or {\tt Integer} or {\tt Rational}. See class
{\tt GeoNum}, below.

\begin{code}
type Point a = (a, a)
\end{code}

\noindent A \highlighttt{Box} $(l,b,r,t)$ is a
rectangle defined by its left $l$, bottom $b$, right
$r$ and top $t$. It is assumed that $l \le r$ and $b
\le t$.

\begin{code}
type Box a = (a, a, a, a)
\end{code}

\noindent \highlighttt{Angle}s are represented in
degrees. Absolute angles are measured anticlockwise
from the positive $x$-axis.

\begin{code}
type Angle a = a
\end{code}

\noindent \highlighttt{Line}s are represented by the 
coefficients of the general formula for a line $(A,B,C)$ in:

\[ Ax + by + C = 0 \]

\begin{code}
type Line a = (a, a, a)
\end{code}

\noindent \highlighttt{LineSeg}ments are represented by the 
start and end points.

\begin{code}
type LineSeg a = (Point a, Point a)
\end{code}

\submodule{Geometric computations} %%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{GeoNum} overloads functions
which perform geometric computations. 

\noindent 
\begin{code}
class (Ord a, Num a) => GeoNum a where
\end{code}

\noindent \highlighttt{netBox}~$\mathit{boxes}$ returns
the smallest box that encloses all of $\mathit{boxes}$.

\begin{code}
   netBox :: [Box a] -> Box a
   netBox boxes = 
      let (ls, bs, rs, ts) = unzip4 boxes
      in (minimum ls, minimum bs, maximum rs, maximum ts)
\end{code}

\noindent 
\highlighttt{shiftBoxes}~$\mathit{boxes}~\Delta_x~\Delta_y$ 
returns the $\mathit{boxes}$ displaced by $\Delta_x$
and $\Delta_y$.

\begin{code}
   shiftBoxes :: [Box a] -> a -> a -> [Box a]
   shiftBoxes bs dx dy = [(l + dx, b + dy, r + dx, t + dy)
      | (l,b,r,t) <- bs]
\end{code}

\noindent
\highlighttt{leastRightShift}~$\mathit{as}~\mathit{bs}$
returns the least horizontal displacement so that list of
boxes $\mathit{bs}$ no longer overlap list of boxes
$\mathit{as}$, as in figure~\ref{fig:leastRightShift}.

\begin{figure}[ht]
{

\centering

\epsfig{figure=figures/Geometry/leastRightShift}

\caption{\label{fig:leastRightShift} The least right shift
to stop two sets of boxes overlapping.}
}
\end{figure}

\begin{code}
   leastRightShift :: [Box a] -> [Box a] -> a
   leastRightShift as bs = case [r - l' |
         (_, b, r, t) <- as, (l', b', _, t') <- bs,
         t >= b' && b <= t'] of
      [] -> 0
      xs -> maximum xs
\end{code}

\noindent \highlighttt{distAroundOval}~$n~o~\phi$ returns a
list of $n$ points distributed around the oval inscribed in
box $o$. The points will be equally distributed by angle
of separation wrt to the centre of the oval, clockwise, and
starting from angle $\phi$.

\begin{code}
   placeAroundOval :: Int -> Box a -> Angle a -> [Point a]
   placeAroundOval = undefined
\end{code}

\noindent Working: An ellipse is defined by either:

\[\frac{x^2}{a^2} + \frac{y^2}{b^2} = 1\]

\noindent or the parametric equations:

\[x = a \cos t~\mathrm{and}~y = b \sin t\]

\begin{figure}[ht]
{

\centering

\epsfig{figure=figures/Geometry/ellipse}

\caption{\label{fig:ellipse} An ellipse.}
}
\end{figure}

\noindent \highlighttt{iGeo}~$x$ converts a {\tt GeoNum} 
$x$ to an {\tt Int}.

\begin{code}
   iGeo :: a -> Int
\end{code}

\noindent \highlighttt{iPoint}~$p$ converts the coordinates
of point $p$ to {\tt Int}s.

\begin{code}
   iPoint :: Point a -> Point Int
   iPoint (x,y) = (iGeo x, iGeo y)
\end{code}

\noindent \highlighttt{iBox}~$b$ converts the coordinates
of box $b$ to {\tt Int}s.

\begin{code}
   iBox :: Box a -> Box Int
   iBox (l,b,r,t) = (iGeo l, iGeo b, iGeo r, iGeo t)
\end{code}

\noindent \highlighttt{insetBox}~$d~(l,b,r,t)$ reduces
box $(l,b,r,t)$ all around by distance $d$. It is assumed
that $2 d \le r - l$ and $2 d \le t - b$, that is that the
original box was big enough to do this.

\begin{code}
   insetBox :: a -> Box a -> Box a
   insetBox d (l,b,r,t) = (l + d, b + d, r - d, t - d) 
\end{code}

\noindent \highlighttt{segToLine}~$((x_1,y_1),(x_2,y_2))$ computes $(A,B,C)$
for segment $((x_1,y_1),(x_2,y_2))$. Working:

\[ A = y_2 - y_1,~B=x_2-x_1,~C = -(A x_1 + B y_1) \]

\begin{code}
   segToLine :: LineSeg a -> Line a
   segToLine ((x1,y1),(x2,y2)) = 
      let a = y2 - y1
          b = x1 - x2
      in (a, b, -(a * x1 + b * y1))
\end{code}

\noindent \highlighttt{insetSeg}~$d~((x_1,y_1),(x_2,y_2))$ 
clips a distance $d$ from both ends of the line segment.
It is assumed that the line segment is long enough to
do this.

\begin{haddock}
   insetSeg :: a -> LineSeg a -> LineSeg a
   segToLine d ((x1,y1),(x2,y2)) = undefined
\end{haddock}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance GeoNum Float where
\end{code}

\begin{code}
   iGeo = round
\end{code}

\begin{code}
instance GeoNum Double where
\end{code}

\begin{code}
   iGeo = round
\end{code}

\begin{code}
   placeAroundOval n (l,b,r,t) p = 
      let p' = p * pi / 180.0
          g = 2.0 * pi / fromIntegral n
          a = (r - l) / 2.0
          b' = (t - b) / 2.0
      in [(l + a + a * cos t', b + b' + b' * sin t')
         | i <- [0..n-1], let t' = p' - fromIntegral i * g]
\end{code}

\begin{code}
instance GeoNum Int where
\end{code}

\begin{code}
   iGeo = id
\end{code}

\begin{code}
instance GeoNum Integer where
\end{code}

\begin{code}
   iGeo = fromIntegral
\end{code}

\begin{code}
instance GeoNum Rational where 
\end{code}

\begin{code}
   iGeo = round
\end{code}

