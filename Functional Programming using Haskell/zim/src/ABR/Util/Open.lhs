% Open.#ext
% This file was produced from Open.lit

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

\module{Util.Open} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Util.Open} allows access to system facilities to open
things like URLs.

\begin{code}
{-# LANGUAGE CPP #-}
\end{code}

\begin{code}
module ABR.Util.Open (
      openURL
   ) where
\end{code}

\begin{code}
import System.Exit
import System.Process
\end{code}

\begin{code}
import ABR.Data.List
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

% Reviewed 2015-02-02. Passed {\tt hlint}.\\
% Reviewed 2014-05-29: Made all the {\tt -Wall}s go away.\\
New 2015-04-02.
   
\submodule{System specifics} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{openTool} is the system specific comand line tool
for opening a URL in the default browser. \highlighttt{delimiter}
is the character(s) that should delimit the URL.\highlighttt{ampersand}
is the character(s) should replace any ampersands in the URL.

\begin{code}
openTool, delimiter, ampersand :: String
openTool =
#if defined darwin_HOST_OS
   "open"
#elif defined linux_BUILD_OS
   "xdg-open"
#elif defined mingw32_BUILD_OS
   "start"
#else 
   "NO OPEN COMMAND DEFINED"
#endif
delimiter =
#if defined mingw32_BUILD_OS
   ""
#else
   "\""
#endif
ampersand =
#if defined mingw32_BUILD_OS
   "^&"
#else
   "&"
#endif
\end{code}

\submodule{Tests} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{haddock}
testOpenURL :: IO ExitCode
testOpenURL = openURL 
   "http://www.ict.griffith.edu.au/arock/itp/"
\end{haddock}

\submodule{Opening a URL} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{openURL}~$\mathit{url}$ 
opens the $\mathit{url}$ in the default browser.

\begin{code}
openURL :: String -> IO ExitCode
openURL url = system $
   openTool ++ " " ++ delimiter ++
   replaceAll "&" ampersand url ++ delimiter
\end{code}

