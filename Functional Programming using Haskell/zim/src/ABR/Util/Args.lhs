


% Args.#ext
% This file was produced from Args.lit


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

\module{Util.Args} %%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Util.Args} provides a way to pick apart the
meanings of command line arguments.

\begin{code}
{-# LANGUAGE CPP #-}
\end{code}

\begin{code}
module ABR.Util.Args (
      OptSpec(..), OptVal(..), Options, findOpts,
      assertFlagPlus, assertFlagMinus, deleteFlag,
      insertParam, deleteParam, emptyOptions,
      lookupFlag, lookupParam, lookupQueue
# ifdef NO_GLOB
-- legacy for hobbit
# else 
      , glob
# endif
   ) where
\end{code}


\begin{code}
import qualified Data.List as L
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Map as M
\end{code}

\noindent Hackage package {\tt Glob}:

\begin{code}
# ifdef NO_GLOB
-- legacy for hobbit
# else 
import qualified System.FilePath.Glob as G
# endif
\end{code}



\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2017-12-28: Updated {\tt glob} call.\\
Reviewed 2016-05-28: Added {\tt glob} dependency note.\\
Reviewed 2015-02-12: Made globbing conditional.\\
Reviewed 2015-02-12: Added filename globbing.\\
Reviewed 2015-02-02: Passed {\tt hlint}.\\
Reviewed 2014-05-29: Made all the {\tt -Wall}s go away.\\
Reviewed 2013-11-06.\\
Reviewed 2012-11-24: Moved into {\tt ABR.Util}. \\
Reviewed 2012-11-01: Keep this module but make it use the 
data types in the CHC libs instead of my own data types -- done.
   
\submodule{Dependencies} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


{\tt ABR.Util.Args} uses the {\tt Glob} package. Install
that on your system with {\tt cabal}:

\begin{Verbatim}
cabal install Glob
\end{Verbatim}

\noindent To compile where {\tt Glob} is not available use
the {\tt ghc} compiler option \verb"-DNO_GLOB".


\submodule{Language tweaks} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
undefined' :: String -> a
undefined' label = error $
   "Intentional undefined at module ABR.Util.Args, " ++
   " with label \"" ++ label ++ "\"."
\end{code}


\submodule{Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

An \highlighttt{OptSpec} is used to specify the
types of option expected. 

\begin{code}
data OptSpec = 
   FlagS String | ParamS String | QueueS String
   deriving (Eq, Show)
\end{code}

\noindent An option is one of:

\begin{itemize}
   \item a flag to be set or unset. Specify with 
      \highlighttt{FlagS}~$\mathit{name}$. Users
      set or unset with \verb"+"$\mathit{name}$ or
      \verb"-"$\mathit{name}$ respectively.
   \item a parameter with a value. Specify with
      \highlighttt{ParamS}~$\mathit{name}$. Users
      provide values with
      \verb"-"$\mathit{name}~\mathit{value}$.
   \item a parameter that can have multiple values.
      The order of the multiple values might be
      significant. In this case a queue of strings
      should be returned. Specify with
      \highlighttt{QueueS}~$\mathit{name}$. Users
      provide values with
      \verb"-"$\mathit{name}~\mathit{value}_{1}$
      \verb"-"$\mathit{name}~\mathit{value}_{2}~\ldots$.
\end{itemize}   

\noindent An \highlighttt{OptVal} is used to
indicate presence of a command line option. Flags
might be \highlighttt{FlagPlus} or
\highlighttt{FlagMinus}. Parameters will either
return the
\highlighttt{ParamValue}~$\mathit{value}$ or an
indication that the value was missing,
\highlighttt{ParamMissingValue}. Queue parameters
return \highlighttt{ParamQueue}~$\mathit{queue}$.
Missing values for queue parameters might yield an
empty queue.

\begin{code}
data OptVal = 
      FlagPlus | FlagMinus | 
      ParamValue String | ParamMissingValue |
      ParamQueue (S.Seq String)
   deriving Show
\end{code}

\noindent An \highlighttt{Options} is used to map
from an option name to its value(s).

\begin{code}
type Options = M.Map String OptVal
\end{code}


\submodule{Empty options} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{emptyOptions} is an empty {\tt Options}.

\begin{code}
emptyOptions :: Options

emptyOptions = M.empty

\end{code}


\submodule{Option detection} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{findOpts}~$\mathit{optSpecs}~\mathit{
args}$ returns $(\mathit{options},
\mathit{leftovers})$, where:\\
$\mathit{optSpecs}$ is
a list of option specifications; $\mathit{args}$ is
a list of command line arguments;
$\mathit{options}$ is a mapping from the option
names to the values found; and $\mathit{leftovers}$
is a list of any unconsumed arguments, typically
file names.
      
\begin{code}
findOpts :: [OptSpec] -> [String] -> (Options, [String])

findOpts specs = fo
   where
   rep, enq :: Options -> String -> OptVal -> Options
   rep t cs v = M.insertWith const cs v t
   enq t cs v = M.insertWith enq' cs v t
   enq' :: OptVal -> OptVal -> OptVal
   enq' (ParamQueue q) (ParamQueue q') = 
      ParamQueue (q' S.>< q)
   enq' _ _ = undefined' "enq'"
   fo :: [String] -> (Options, [String])
   fo args = case args of
      [] -> (M.empty, [])
      ((c:cs):css) 
         | c == '+' && FlagS cs `elem` specs ->
            let (t, css') = fo css
            in (rep t cs FlagPlus, css')
         | c == '-' && FlagS cs `elem` specs ->
            let (t, css') = fo css
            in (rep t cs FlagMinus, css')
         | c == '-' && ParamS cs `elem` specs ->
            case css of
               [] ->
                  (rep M.empty cs ParamMissingValue, [])
               (cs':css') ->
                  let (t, css'') = fo css'
                  in (rep t cs (ParamValue cs'), css'')
         | c == '-' && QueueS cs `elem` specs ->
            case css of
               [] ->
                  (enq M.empty cs 
                     (ParamQueue S.empty), [])
               (cs':css') ->
                  let (t, css'') = fo css'
                  in (enq t cs (ParamQueue
                        (S.singleton cs')), css'')
         | otherwise ->
            let (t, css') = fo css
            in (t, (c:cs):css')
      ([]:_) -> undefined' "fo"

\end{code}

\submodule{Adding and deleting options} %%%%%%%%%%%%%%%%%%%

Flags can be asserted positive or negative or deleted, with
\highlighttt{assertFlagPlus}, \highlighttt{assertFlagMinus},
and \highlighttt{deleteFlag}, respectively.

\begin{code}
assertFlagPlus, assertFlagMinus, deleteFlag ::
   String -> Options -> Options

assertFlagPlus name  = M.insert name FlagPlus 
assertFlagMinus name = M.insert name FlagMinus 
deleteFlag           = M.delete  

\end{code}

\noindent Params can be inserted or deleted, with
\highlighttt{insertParam} and \highlighttt{deleteParam},
respectively.

\begin{code}
insertParam :: String -> String -> Options -> Options

insertParam name value = M.insert name (ParamValue value)

\end{code}

\begin{code}
deleteParam :: String -> Options -> Options

deleteParam = M.delete 

\end{code}

\submodule{Looking up options} %%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{lookupFlag}~$\mathit{name}~\mathit{options}~\mathit{def}$
returns the value ({\tt FlagPlus} means {\tt True}) stored
for the $\mathit{name}$ed flag in $\mathit{options}$ or
$\mathit{def}$ if it has not been properly specified.
      
\begin{code}
lookupFlag :: String -> Options -> Bool -> Bool

lookupFlag name options def = 
   case M.lookup name options of
      Just FlagPlus  -> True
      Just FlagMinus -> False
      _              -> def

\end{code}

\noindent
\highlighttt{lookupParam}~$\mathit{name}~\mathit{options}~\mathit{def}$
returns the value stored for the $\mathit{name}$ed
parameter in $\mathit{options}$ or $\mathit{def}$ if it
has not been properly specified.
      
\begin{code}
lookupParam :: String -> Options -> String -> String

lookupParam name options def = 
   case M.lookup name options of
      Just (ParamValue v) -> v
      _                   -> def

\end{code}

\noindent
\highlighttt{lookupQueue}~$\mathit{name}~\mathit{options}$
returns the list stored for the $\mathit{name}$ed
queue parameter in $\mathit{options}$ or {\tt []} if it
has not been properly specified.
      
\begin{code}
lookupQueue :: String -> Options -> [String]

lookupQueue name options = 
   case M.lookup name options of
      Just (ParamQueue q) -> F.toList q
      _                   -> []

\end{code}

\submodule{Filename globbing} %%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{glob}~$\mathit{paths}$ returns the really existing
file names that match the $\mathit{paths}$ which may contain
wildcards.

\begin{code}
# ifdef NO_GLOB
-- legacy for hobbit
# else 
glob :: [FilePath] -> IO [FilePath]

glob paths = do
   pathss <- G.globDir (map G.compile paths) []
   return $ L.nub $ concat pathss

# endif
\end{code}

