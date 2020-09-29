% Lock.#ext
% This file was produced from Lock.lit

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

\module{File.Lock} %%%%%%%%%%%%%%%%%%%%%%%%%%

The \highlighttt{ABR.File.Lock} module provides a facility to lock a 
file so that multiple concurrent processes don't
destructively interfere.

\begin{code}
{-# language ScopedTypeVariables #-}
\end{code}

\begin{code}
module ABR.File.Lock (
      lockFile, unlockFile, isLockedFile, areAnyLocked,
      lockFiles, unlockFiles, lockGuard, blockGuard
   ) where
\end{code}

\begin{code}
import System.Directory
import qualified Control.Exception as E
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2014-06-06: Made all the {\tt -Wall}s go away.\\
Reviewed 2013-11-24.\\
Reviewed 2012-11-24: Moved to {\tt ABR.File.Lock} from {\tt ABR.LockFile}.
   

\submodule{Basic lock operations} %%%%%%%%

\noindent \highlighttt{lockFile}~$\mathit{path}$ locks the file at
$\mathit{path}$, returning {\tt True} iff the file was not already
locked and was successfully locked.
\highlighttt{unlockFile}~$\mathit{path}$ unlocks the file at
$\mathit{path}$, returning {\tt True} iff the file was locked and
was successfully unlocked.
\highlighttt{isLockedFile}~$\mathit{path}$ returns {\tt True} iff
the file at $\mathit{path}$ is locked.

\begin{code}
lockFile, unlockFile, isLockedFile :: String -> IO Bool
lockFile filename = do
   islocked <- isLockedFile filename
   if islocked
      then return False
      else do writeFile (extend filename) "LOCKED\n"
              return True
unlockFile filename = do
   islocked <- isLockedFile filename
   if islocked 
      then do removeFile (extend filename)
              return True
      else return False
isLockedFile filename = do 
   f <- E.catch (readFile (extend filename)) 
      (\(_ :: E.IOException) -> return "")
   return $ not $ null f
\end{code}

\begin{code}
extend :: String -> String
extend filename = filename ++ ".LOCK"
\end{code}

\submodule{Multiple file operations} %%%%%%%%

\noindent \highlighttt{areAnyLocked}~$\mathit{fs}$ returns {\tt
True} iff at least one of the files named in $\mathit{fs}$ is
locked.

\begin{code}
areAnyLocked :: [String] -> IO Bool
areAnyLocked []     = return False
areAnyLocked (f:fs) = do
   l <- isLockedFile f
   if l then return True
        else areAnyLocked fs
\end{code}

\noindent \highlighttt{lockFiles}~$\mathit{fs}$ locks all files
named in $\mathit{fs}$. \highlighttt{unlockFiles}~$\mathit{fs}$ unlocks all
files named in $\mathit{fs}$.

\begin{code}
lockFiles, unlockFiles :: [String] -> IO ()
lockFiles   = mapM_ lockFile  
unlockFiles = mapM_ unlockFile
\end{code}

\submodule{Guards} %%%%%%%%

\noindent \highlighttt{lockGuard}~$\mathit{directory}~\mathit{fs}~\mathit{handler}~\mathit{process}$
checks whether\\
any of the files $\mathit{fs}$ in
$\mathit{directory}$ are locked. If any \\
one is, $\mathit{handler}$ is
executed, otherwise $\mathit{process}$ is executed.\\
\highlighttt{blockGuard}~$\mathit{directory}~\mathit{fs}~\mathit{handler}~\mathit{process}$
checks whether any of the files $\mathit{fs}$ in
$\mathit{directory}$ are locked. If any one is, $\mathit{handler}$ is
executed, otherwise the files are locked, $\mathit{process}$ is
executed, then the files are unlocked again.

\begin{code}
lockGuard, blockGuard ::
   String -> [String] -> IO () -> IO () -> IO ()
lockGuard directory fs handler process = do
   anyLocked <- areAnyLocked $ map (directory ++) fs
   if anyLocked then handler
                else process
blockGuard directory fs handler process = do
   let fs' = map (directory ++) fs
   anyLocked <- areAnyLocked fs'
   if anyLocked 
      then handler
      else do lockFiles fs'
              process
              unlockFiles fs'
\end{code}

