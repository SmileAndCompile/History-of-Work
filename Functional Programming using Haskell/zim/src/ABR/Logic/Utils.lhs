% Utils.#ext
% This file was produced from Utils.lit

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

\module{Utils} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlightModule{Utils} implements some shared stuff for CDL.

\begin{code}
module CDL.Utils (
      GetName(..), fatalError'
   ) where
\end{code}

\begin{code}
import System.IO
import System.Exit
import System.Environment
import System.FilePath
import Data.Char
\end{code}

\begin{code}
import ABR.Parser.Pos
import ABR.Parser
import ABR.Showing
\end{code}

\submodule{Overloaded accessors} %%%%%%%%%%%%%%%%%%%%%%%%%%

Most of the data types defined below have attitibutes like 
a position and a name. These overloaded accessor functions
permit access without remembering the specific field name.

Class \highlighttt{ABR.Parser.HasPos} overloads \highlighttt{getPos}.

\noindent Class \highlighttt{GetName} overloads \highlighttt{getName}.

\begin{code}
class GetName a where
   getName :: a -> String
\end{code}
   
\submodule{Errors and messages} %%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{fatalError}~$\mathit{message}$
prints an error $\mathit{message}$ and 
exits with a failure status.

\begin{code}
fatalError :: Msg -> IO a
fatalError msg = do
   tool <- getProgName
   hPutStrLn stderr $ 
      dropExtension tool ++ " error: " ++ msg
   exitFailure
   return $ error "THIS SHOULD NOT HAPPEN"
\end{code}

\noindent \highlighttt{warning}~$\mathit{message}$
prints a warning $\mathit{message}$.

\begin{code}
warning :: Msg -> IO ()
warning msg = do
   tool <- getProgName
   hPutStrLn stderr $ 
      dropExtension tool ++ " warning: " ++ msg
\end{code}

\noindent \highlighttt{fatalError'}~$\mathit{pos}~\mathit{message}~\mathit{source}$
prints an error $\mathit{message}$ and 
exits with a failure status.

\begin{code}
fatalError' :: Pos -> Msg -> String -> IO a
fatalError' pos msg source = do
   tool <- getProgName
   hPutStrLn stderr $ 
      dropExtension tool ++ " error" ++ 
      drop 5 (errMsg pos msg source)
   exitFailure
   return $ error "THIS SHOULD NOT HAPPEN"
\end{code}

\noindent \highlighttt{warning'}~$\mathit{pos}~\mathit{message}~\mathit{source}$
prints an error $\mathit{message}$ and 
exits with a failure status.

\begin{code}
warning' :: Pos -> Msg -> String -> IO ()
warning' pos msg source = do
   tool <- getProgName
   hPutStrLn stderr $ 
      dropExtension tool  ++ " warning" ++ 
      drop 5 (errMsg pos msg source)
\end{code}

\noindent \highlighttt{logicError}~$\mathit{message}$
prints an error $\mathit{message}$ and 
exits with a failure status. This is for catching 
errors that should not happen.

\begin{code}
logicError :: Msg -> IO a
logicError msg = do
   tool <- getProgName
   hPutStrLn stderr $ 
      dropExtension tool ++ " ERROR THAT SHOULD NOT \
         \HAPPEN: " ++ msg
   exitFailure
   return $ error "THIS SHOULD NOT HAPPEN"
\end{code}
