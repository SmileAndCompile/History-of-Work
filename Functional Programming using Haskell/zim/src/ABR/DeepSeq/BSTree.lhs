% BSTree.#ext
% This file was produced from BSTree.lit

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

\module{(Deprecated) DeepSeq.BStree} %%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Data.BSTree} implements a depth/height balanced
(AVL) binary search tree abstract data type.

\begin{code}
module ABR.DeepSeq.BStree 
   {-# DEPRECATED 
      "Use Data.Map and Control.DeepSeq instead." #-}
   where
\end{code}

\begin{code}
import ABR.DeepSeq
import ABR.Data.BSTree
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2009-04-08: Separated from {\tt ABR.Data.BSTree}.
   

\submodule{Instance declaration} %%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
instance (DeepSeq k, DeepSeq v, Ord k) => 
      DeepSeq (BSTree k v) where
\end{code}

\begin{code}
   deepSeq t x = case t of
      Empty          -> x
      Node k v l r s -> deepSeq k $ deepSeq v $
         deepSeq l $ deepSeq r $ deepSeq s x
\end{code}

