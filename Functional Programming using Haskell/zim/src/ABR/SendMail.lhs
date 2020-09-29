% SendMail.#ext
% This file was produced from SendMail.lit

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

\module{SendMail} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.SendMail} lets a Haskell program send an email.

\begin{code}
module ABR.SendMail (sendMail) where
\end{code}

\begin{code}
import System.Process
import System.IO
import Control.Monad
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Requires review.\\
Reviewed 2015-02-10. Passed {\tt hlint}.
   

\submodule{Function} %%%%%%%%%%%%%%%%%%%%%%

\noindent \highlighttt{sendMail}~$\mathit{whoTo}~\mathit{subject}~\mathit{content}$
sends an email to $\mathit{whoTo}$ about $\mathit{subject}$ containing $\mathit{content}$.

\begin{code}
sendMail :: String -> String -> String -> IO ()
sendMail whoTo subject content = do
   (inp, out, err, process) <- runInteractiveProcess 
       "/usr/lib/sendmail"
       ["-i", "-t"]
       Nothing
       Nothing
   hPutStrLn inp $ "To: " ++ whoTo
   hPutStrLn inp $ "Subject: " ++ subject
   hPutStrLn inp ""
   hPutStrLn inp content
   hClose inp
   output <- hGetContents out
   unless (null output) $ putStrLn output
   errors <- hGetContents err
   unless (null errors) $ putStrLn errors
   exit <- waitForProcess process
   exit `seq` return ()
\end{code}
