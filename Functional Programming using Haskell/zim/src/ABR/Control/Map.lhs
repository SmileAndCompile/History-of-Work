% Map.#ext
% This file was produced from Map.lit

% ABRHLibs -- a personal library of Haskell modules
% Copyright (C) 2012 Andrew Rock
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

\module{Control.Map} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Control.Map} implements control 
abstractions involving {\tt Data.Map.Map}s.

\begin{code}
module ABR.Control.Map (lookupGuard) where
\end{code}

\begin{code}
import qualified Data.Map as M
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2015-02-02. Passed {\tt hlint}.\\
Reviewed 2014-05-28: Made all {\tt -Wall}s go away.\\
Reviewed 2013-11-10.\\
New 2012-11-01: replacement for {\tt ABR.Data.BSTree.lookupGuard}.
   

\submodule{lookupGuard} %%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{lookupGuard}~$\mathit{m}~\mathit{keys}~\mathit{handler}~\mathit{process}$
tries to look up the $\mathit{keys}$. If any are missing the $\mathit{handler}$
is applied to the first missing key otherwise the $\mathit{process}$
is applied to the list of values successfully looked up.

\begin{code}
lookupGuard :: Ord a => M.Map a b -> [a] -> (a -> c)
                        -> ([b] -> c) -> c
lookupGuard m keys handler process
   = lookupGuard' keys []
     where
     lookupGuard' [] vals
        = process vals
     lookupGuard' (k:ks) vals
        = case M.lookup k m of
             Nothing    ->
                handler k
             Just stuff ->
                lookupGuard' ks (vals ++ [stuff]) 
\end{code}

