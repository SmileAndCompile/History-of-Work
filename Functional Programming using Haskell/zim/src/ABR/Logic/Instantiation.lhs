% Instantiation.#ext
% This file was produced from Instantiation.lit

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

\module{Instantiation} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlightModule{Instantiation} implements instantiation for
CDL. 

\begin{code}
module CDL.Instantiation (
      Instantiable(..)
   ) where
\end{code}

\begin{code}
import CDL.Constants
import CDL.Variables
import CDL.Types
import CDL.NewTypeDecs
import CDL.AtomTypeDecs
\end{code}

\submodule{Instantiable class} %%%%%%%%%%%%%%%%%%%%%%%%

\noindent Class \highlighttt{Instantiable} has instances that
can be instantiated by the grounding of variables.

\begin{code}
class Instantiable a where
\end{code}

\noindent
\highlighttt{instantiate}~$\mathit{tt}~\mathit{ats}~x$
creates as many instances of $x$ as grounding of all the
free variables demands and the substitution that created
each instance. $\mathit{tt}$ is the table of named types.
$\mathit{ats}$ is the list of atom type assertions. For
each instance the substitution that defines that instance
is also returned

\begin{code}
   instantiate :: TypeTable -> [AtomTypeDec] -> a -> 
      [(a, Substitution)]
\end{code}

\noindent
\highlighttt{instances}~$\mathit{tt}~\mathit{ats}~x$
creates as many instances of $x$ as grounding of all the
free variables demands. $\mathit{tt}$ is the table of
named types. $\mathit{ats}$ is the list of atom type
assertions.

\begin{code}
   instances :: TypeTable -> [AtomTypeDec] -> a -> [a]
   instances tt ats = map fst . instantiate tt ats
\end{code}

\submodule{Instance declarations} %%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Instantiating} %%%%%%%%

\begin{code}
instance Instantiable Constant where
   instantiate tt ats c = [(c, NullSub)]
\end{code}

\begin{haddock}
instance Instantiable Argument where
   instantiate tt ats a = [(a, NullSub)]
\end{haddock}

\begin{haddock}
instance Instantiable Type where
   instantiate tt ats t = case t of 
      TEnum as -> 
         [(TEnum (list2SS as'), s)
         | (as', s) <- instantiate tt ats (flattenSS as)
         ]
      TName n  -> [(TName n, NullSub)]
      t1 :+ t2 ->
         [(t1' :+ t2', s1 :>-> s2)
         | (t1', s1) <- instantiate tt ats t1,
           (t2', s2) <- instantiate tt ats (ground s1 t2)
         ]
      t1 :^ t2 -> 
         [(t1' :^ t2', s1 :>-> s2)
         | (t1', s1) <- instantiate tt ats t1,
           (t2', s2) <- instantiate tt ats (ground s1 t2)
         ]
      t1 :- t2 -> 
         [(t1' :- t2', s1 :>-> s2)
         | (t1', s1) <- instantiate tt ats t1,
           (t2', s2) <- instantiate tt ats (ground s1 t2)
         ]
\end{haddock}

\begin{haddock}
instance Instantiable VarGen where
   instantiate tt ats (v :<- t) = 
      [(v :<- t', s) 
      | (t', s) <- instantiate tt ats t
      ]
\end{haddock}

\begin{haddock}
instance Instantiable Default where
   instantiate tt ats (Default l) = 
      [(Default l', s) 
      | (l', s) <- instantiate tt ats l
      ]
\end{haddock}

\begin{haddock}
instance Instantiable [(Argument, VarGen)] where
   instantiate tt ats avgs = case avgs of
      []                   -> [([], NullSub)]
      (a, vg@(v :<- t)) : avgs' -> case a of
         Const c ->
            [((a, vg) : avgs, s') 
            | c `elem` evalType' tt t,
              let s = v :->- c,
              (avgs, s') <- instantiate tt ats 
                 [(a, ground s vg) | (a,vg) <- avgs']
            ]
         Var v'@(Variable n)
            | "_VG_" `isPrefixOf` n -> 
               [((a, vg) : avgs, s) 
               | (avgs, s) <- instantiate tt ats  avgs'
               ]
            | otherwise             -> 
               [((ground s a, vg) : avgs, s :>-> s'')
               | (s,s') <- [(v' :->- c, v :->- c)
                           | c <- evalType' tt t],
                 (avgs, s'') <- instantiate tt ats 
                    [(ground s a, ground (s :>-> s') vg) 
                    | (a,vg) <- avgs']
               ]
\end{haddock}

