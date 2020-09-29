% Versions.#ext
% This file was produced from Versions.lit

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

\begin{code}
{-# language ScopedTypeVariables #-}
\end{code}

\module{File.Versions} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The \highlighttt{ABR.Fie.Versions} module provides replacements for \\
{\tt Prelude.readFile} and {\tt Prelude.writeFile} that
read the most recent and write the next version of
a file. Each version of a file is distinguished by
a {\tt .}\emph{number} extension appended to the
root name of the file. It also provides some IO
utilities.

\begin{code}
module ABR.File.Versions (
      readLatest, writeNew, writeNew', writeNew'',
      purgeVersions, getNames, readFile', writeFile',
      removeR, createDirectory', removeVersions,
      latestDate
   ) where
\end{code}

\begin{code}
import Control.Monad
import Data.Char
import Data.List
--import Data.Maybe
import System.Directory
import System.Posix hiding (createDirectory, removeDirectory)
import qualified Control.Exception as E
import System.IO
\end{code}

\begin{code}
import ABR.Text.String
import ABR.Util.Time
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2017-01-17: Made all the {\tt -Wall}s go away.\\
Reviewed 2014-11-10: Fixed binary submission in Virgil. \\
Reviewed 2014-06-06: Made all the {\tt -Wall}s go away.\\
Reviewed 2013-11-24.\\
Reviewed 2012-11-24: Moved to {\tt ABR.File}.
   

\submodule{Read the latest version} %%%%%%%%%%%%%%%%%%%%%%
 
\noindent \highlighttt{readLatest}~$\mathit{dir}~\mathit{root}$ reads
the contents of the latest version of the file with
the given $\mathit{root}$ file name in directory $\mathit{dir}$.
Either {\tt Just} the contents are returned, or
{\tt Nothing} if no version of the file could be read.

\begin{code}
readLatest :: FilePath -> FilePath -> IO (Maybe String)
readLatest dir root = do
   mn <- latestName dir root
   case mn of
      Nothing -> return Nothing
      Just name -> readFile' (dir ++/++ name)
\end{code}


\submodule{Date of the latest version} %%%%%%%%%%%%%%%%%%%%%%
 
\noindent \highlighttt{latestDate}~$\mathit{dir}~\mathit{root}$ returns
a {\tt String} containing the modification date of the latest version,
if one exists.

\begin{code}
latestDate :: FilePath -> FilePath -> IO (Maybe String)
latestDate dir root = do
   mn <- latestName dir root
   case mn of
      Nothing   -> 
         return Nothing
      Just name -> do
         t <- fileModTime (dir ++/++ name)
         return $ Just t 
\end{code}


\submodule{Write the next version} %%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{writeNew}~$\mathit{dir}~\mathit{root}~\mathit{content}~\mathit{binary}$ writes
$\mathit{content}$ to a new version of the file with the given
$\mathit{root}$ file name in directory $\mathit{dir}$. The new file is
assigned the access permissions of $\mathit{dir}$ (masked by
\verb+-rw-rw-rw-+) and the same userID and groupID as 
$\mathit{dir}$. Iff $\mathit{binary}$ no text encoding is used.

\begin{code}
writeNew :: FilePath -> FilePath -> String -> Bool -> IO ()
writeNew dir root content binary = do
   name <- nextName' dir root
   writeFile' dir name content binary
\end{code}

\noindent \highlighttt{writeNew'}~$\mathit{dir}~\mathit{root}~\mathit{content}~\mathit{mode}~\mathit{binary}$
writes $\mathit{content}$ to a new version of the file with the given
$\mathit{root}$ file name in directory $\mathit{dir}$. The new file has 
the given access $\mathit{mode}$ and the same userID and groupID as 
$\mathit{dir}$. Iff $\mathit{binary}$ no text encoding is used.

\begin{code}
writeNew' ::
   FilePath -> FilePath -> String -> FileMode -> Bool -> IO ()
writeNew' dir root content mode binary = do
   name <- nextName' dir root
   writeFile' dir name content binary
   setFileMode (dir ++/++ name) mode
\end{code}

\noindent \highlighttt{writeNew''}~$\mathit{dir}~\mathit{root}~\mathit{content}~\mathit{binary}$
writes $\mathit{content}$ to a new version of the file with the
given $\mathit{root}$ file name in directory $\mathit{dir}$. The new
file has the access mode \verb+-rw-------+ and the
same userID and groupID as $\mathit{dir}$. Iff $\mathit{binary}$ no text encoding is used.

\begin{code}
writeNew'' :: FilePath -> FilePath -> String -> Bool -> IO ()
writeNew'' dir root content binary = do
   name <- nextName' dir root
   writeFile' dir name content binary
   setFileMode (dir ++/++ name) $
      unionFileModes ownerReadMode ownerWriteMode
\end{code}


\submodule{Purge old versions} %%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{purgeVersions}~$\mathit{dir}~\mathit{root}$ deletes all
old versions of the file with the given $\mathit{root}$ name
in directory $\mathit{dir}$.

\begin{code}
purgeVersions :: FilePath -> FilePath -> IO ()
purgeVersions dir root = do
   names <- getNames dir root
   let names' = names \\ [latest root names]
   mapM_ (removeFile . (dir ++/++)) names'
\end{code}


\submodule{Remove all versions} %%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{removeVersions}~$\mathit{dir}~\mathit{root}$ deletes all
versions of the file with the given $\mathit{root}$ name in 
directory $\mathit{dir}$.

\begin{code}
removeVersions :: FilePath -> FilePath -> IO ()
removeVersions dir root = do
   names <- getNames dir root
   mapM_ (removeFile . (dir ++/++)) names
\end{code}


\submodule{Get all versions} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{getNames}~$\mathit{dir}~\mathit{root}$ returns the list of 
filenames in directory $\mathit{dir}$ that contain the given
$\mathit{root}$ file name and a version number.

\begin{code}
getNames :: FilePath -> FilePath -> IO [String]
getNames dir root = do
      ls <- E.catch (getDirectoryContents dir)
         (\(_ :: E.IOException) -> 
            error $ "Error: Directory " ++ dir
               ++ " can not be read.")
      return $ filter (match root) ls
   where
   match :: String -> String -> Bool
   match xs ys =
      let n = length xs
      in xs == take n ys && extOnly (drop n ys)
   extOnly :: String -> Bool
   extOnly ('.':xs)
      = not (null xs) && all isDigit xs
   extOnly _
      = False
\end{code}


\submodule{Read and write file bottlenecks} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{readFile'}~$\mathit{path}$ provides a safe
way to read a file without raising an exception if
the file does not exist. It returns {\tt Nothing} if the
file could not be read, {\tt Just}~$\mathit{contents}$ otherwise.

\begin{code}
readFile' :: FilePath -> IO (Maybe String)
readFile' filename = do
   f <- E.catch (readFile filename)
           (\(_ :: E.IOException) -> return "\0")
   if f == "\0"
      then return Nothing
      else return (f `seq` Just f)
\end{code}

\noindent \highlighttt{writeFile'}~$\mathit{dir}~\mathit{file}~\mathit{content}~\mathit{binary}$ writes
$\mathit{content}$ to $\mathit{dir}$\verb+/+$\mathit{file}$ The new file is
assigned the access permissions of $\mathit{dir}$ (masked by
\verb+-rw-rw-rw-+) and the same userID and groupID as 
$\mathit{dir}$. Iff $\mathit{binary}$ no text encoding is used.

\begin{code}
writeFile' :: FilePath -> FilePath -> String -> Bool -> IO ()
writeFile' dir name content binary = do
   let path = dir ++/++ name
   if binary then do h <- openFile path WriteMode
                     hSetBinaryMode h binary
                     hPutStr h content
                     hClose h
             else writeFile path content
   stat <- getFileStatus dir
   let dirMode = fileMode stat
       dirOwner = fileOwner stat
       dirGroup = fileGroup stat
       mask = foldl1 unionFileModes [
          ownerReadMode, ownerWriteMode,
          groupReadMode, groupWriteMode,
          otherReadMode, otherWriteMode]
       newMode = intersectFileModes 
          mask dirMode
   E.catch (do
      setFileMode path newMode
      setOwnerAndGroup path dirOwner dirGroup
     ) (\(_ :: E.IOException) ->
      return ()
     )
\end{code}


\submodule{Creating and removing directories} %%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{removeR}~$\mathit{path}$ removes a file
or directory, $\mathit{path}$, first, recursively removing
all its contents if it is a directory. If $\mathit{path}$
does not exists, nothing is done.

\begin{code}
removeR :: FilePath -> IO ()
removeR path = do
   existsAsF <- doesFileExist path
   if existsAsF 
      then removeFile path
      else do
         existsAsD <- doesDirectoryExist path
         when existsAsD $ do
            contents <- getDirectoryContents path
            mapM_ (removeR . (path ++/++)) $ 
               filter (any (/= '.')) contents
            removeDirectory path
\end{code}

\noindent \highlighttt{createDirectory'}~$\mathit{parentDir}~\mathit{newDir}$
creates a directory, $\mathit{newDir}$ inside $\mathit{parentDir}$.
If $\mathit{newDir}$ already exists (as a file or directory)
it is removed (along with its contents) first. $\mathit{newDir}$
is assigned the same userID groupID and access 
permissions as $\mathit{parentDir}$.

\begin{code}
createDirectory' :: FilePath -> FilePath -> IO ()
createDirectory' parentDir newDir = do
   let path = parentDir ++/++ newDir
   existsAsF <- doesFileExist path 
   existsAsD <- doesDirectoryExist path
   when (existsAsF || existsAsD) $
      removeR path
   createDirectory path
   stat <- getFileStatus parentDir
   let parentMode = fileMode stat
       parentOwner = fileOwner stat
       parentGroup = fileGroup stat
   E.catch (do
      setFileMode path parentMode
      setOwnerAndGroup path parentOwner parentGroup
     ) (\(_ :: E.IOException) ->
      return ()
     )
\end{code}


\submodule{Private routines} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{code}
latest :: FilePath -> [FilePath] -> FilePath
latest fName
   = foldl1 later
     where
     later :: String -> String -> String
     later xs ys
        | nxs < nys = ys
        | otherwise = xs
        where
        nxs, nys :: Int
        nxs = read $ drop (length fName + 1) xs
        nys = read $ drop (length fName + 1) ys
\end{code}

\begin{code}
latestName :: FilePath -> FilePath -> IO (Maybe FilePath)
latestName dirPath fName = do
   ls <- getNames dirPath fName
   case ls of
      [] -> return Nothing
      _  -> return $ Just $ latest fName ls
\end{code}

\begin{haddock}
nextName :: FilePath -> FilePath -> IO (Maybe FilePath)
nextName dirPath fName = do
   oldVer <- latestName dirPath fName
   case oldVer of
      Nothing -> return Nothing
      Just oldVer' -> do
         let n = length fName
             v :: Int = read $ drop (n+1) oldVer'
         return $ Just $ fName ++ "." ++ show (v + 1)
\end{haddock}

\begin{code}
nextName' :: FilePath -> FilePath -> IO FilePath
nextName' dirPath fName = do
   oldVer <- latestName dirPath fName
   case oldVer of
      Nothing -> return $ fName ++.++ "0"
      Just oldVer' -> do
         let n = length fName
             v :: Int = read $ drop (n+1) oldVer'
         return $ fName ++ "." ++ show (v + 1)
\end{code}

