% OAtoms.#ext
% This file was produced from OAtoms.lit

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

\module{OAtoms} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlightModule{OAtoms} implements optimised atoms for
CDL.

\begin{code}
module CDL.OAtoms (
      OAtom, AtomMap, OAtomMap, mkAtomMaps, toOAtom, toAtom
   ) where
\end{code}

\begin{code}
import Data.Array.IArray
\end{code}

\begin{code}
import ABR.Data.BSTree
import ABR.Data.SparseSet
\end{code}

\begin{code}
import CDL.Atoms
\end{code}

\submodule{Data types} %%%%%%%%%%%%%

An \highlighttt{OAtom} is a \emph{positive}  integer
that uniquely identifies a correspoding {\tt Atom}.

\begin{code}
type OAtom = Int
\end{code}

\noindent An \highlighttt{AtomMap} maps {\tt Atom}s to
their corresponding {\tt OAtom}s.

\begin{code}
type AtomMap = BSTree Atom OAtom
\end{code}

\noindent An \highlighttt{OAtomMap} maps {\tt OAtom}s to
their corresponding {\tt Atom}s.

\begin{code}
type OAtomMap = Array Int Atom
\end{code}

\submodule{Building atom maps} %%%%%%%%%%%%%

\highlighttt{mkAtomMaps}~$x$ returns $(N,
\mathit{am},\mathit{oam})$, where $x$ is something that
contains $N$ distinct {\tt Atoms}, and for each of them,
$(\mathit{am}$ maps them to a corresponding {\tt OAtom},
and $\mathit{oam}$ maps them back.

\begin{code}
mkAtomMaps :: HasAtoms a => a -> (Int, AtomMap, OAtomMap)
mkAtomMaps x = 
   let as = flattenSS $ getAtoms x emptySS
       n = length as
       am = pairs2BST $ zip as [1..n]
       oam = array (1,n) $ zip [1..n] as
   in (n, am, oam)
\end{code}

\submodule{Looking up atoms} %%%%%%%%%%%%%

\highlighttt{toOAtom}~$\mathit{am}~a$ returns {\tt Atom}
$a$'s corresponding {\tt OAtom}.

\begin{code}
toOAtom :: AtomMap -> Atom -> OAtom
toOAtom am a = fromMaybe
   (error $ "CDL ERROR: toOAtom " ++ show a)
   (lookupBST a am)
\end{code}

\noindent \highlighttt{toAtom}~$\mathit{oam}~\mathit{oa}$
returns {\tt OAtom} $\mathit{oa}$'s corresponding {\tt
Atom}.

\begin{code}
toAtom :: OAtomMap -> OAtom -> Atom
toAtom = (!)
\end{code}
