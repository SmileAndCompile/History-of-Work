% Errors.#ext
% This file was produced from Errors.lit

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

\module{Util.Errors} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Util.Errors} provides a framework
for collecting warnings and errors, displaying them, and exiting
on fatal errors.

\begin{code}
module ABR.Util.Errors (
     WEMessage(..), WEResult(..),
     returnPass, returnFail, returnWarn
   ) where
\end{code}

\begin{code}
import ABR.Util.Pos
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2015-02-02. Passed {\tt hlint}.\\
Reviewed 2014-06-10. Added {\tt Functor} and {\tt Applicative}
instances for WEResult, written by George Wilson.\\
Reviewed 2014-05-29. Made all the {\tt -Wall}s go away.\\
Reviewed 2013-11-06.\\
Reviewed 2012-11-23: Adapted and generalized from {\tt MaSH.Errors}.

\submodule{Warning and error data type} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent A \highlighttt{WEMessage} always has:

\begin{itemize}
   \item a category (\highlighttt{weCat});
   \item the text of the message (\highlighttt{weMsg});
\end{itemize}

\noindent and may have:

\begin{itemize}
   \item a position \highlighttt{wePos}.
\end{itemize}

\noindent The category is a type provided by the
application.
   
\begin{code}
data WEMessage cat = WEMessage {
      weCat :: cat,
      wePos :: Maybe Pos,
      weMsg :: String
   }
\end{code}

\submodule{Warning and error category type class} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent Class \highlighttt{ErrorCategory} includes all
types that categorise errors or warnings.

\begin{code}
class ErrorCategory cat where
\end{code}

\noindent \highlighttt{isFatal}~$x$ returns true iff $x$ denotes
a fatal, unrecoverable error.

\begin{code}
   isFatal :: cat -> Bool
   isFatal _ = True
\end{code}

\submodule{Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent A phase of a compiler will typically return a 
list of {\tt WEMessage}s, along with the result of the phase
if there is one. A \highlighttt{WEResult} distinguishes the
cases that have passed (for example with only warnings)
from those that have failed (with a fatal error). {\tt a}
is the phase's result. 

\begin{code}
data WEResult cat a =   WEPass [WEMessage cat] a
                      | WEFail [WEMessage cat]
\end{code}

\submodule{Returning successfully, failing or warning} %%%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{returnPass}~$x$ returns $x$ as
a passing value.

\begin{code}
returnPass :: a -> WEResult cat a
returnPass = return
\end{code}

\noindent \highlighttt{returnFail}~$\mathit{wem}$ returns a
failing message, $\mathit{wem}$.

\begin{code}
returnFail :: WEMessage cat -> WEResult cat ()
returnFail wem = WEFail [wem]
\end{code}

\noindent \highlighttt{returnWarn}~$\mathit{wem}$ returns a
warning message, $\mathit{wem}$.

\begin{code}
returnWarn :: WEMessage cat -> a -> WEResult cat a
returnWarn wem = WEPass [wem]
\end{code}

\submodule{Instances} %%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Functor} %%%%%%

\begin{code}
instance Functor (WEResult cat) where
   fmap f r = case r of
      WEPass wems x -> WEPass wems (f x)
      WEFail wems   -> WEFail wems
\end{code}

\subsubmodule{Applicative} %%%%%%

\begin{code}
instance Applicative (WEResult cat) where
   pure = WEPass []
   r <*> s = case r of
      WEPass wems f -> case s of
         WEPass wems' x -> WEPass (wems ++ wems') (f x)
         WEFail wems'   -> WEFail (wems ++ wems')
      WEFail wems   -> WEFail wems
\end{code}

\subsubmodule{Monad} %%%%%%

\noindent Making a {\tt WEResult} a monad makes possible
the silent accumulation messages as the computation
proceeds, and stopping when it's failed.

\begin{code}
instance Monad (WEResult cat) where
   return = WEPass []
   r >>= f = case r of
      WEPass wems x -> case f x of
         WEPass wems' x' -> WEPass (wems ++ wems') x'
         WEFail wems'    -> WEFail (wems ++ wems')
      WEFail wems      -> WEFail wems
\end{code}


