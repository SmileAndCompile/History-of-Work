% MySQL.#ext
% This file was produced from MySQL.lit

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

\module{(Experimental) MySQL Haskell API} %%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.MySQL} is a Haskell interface to MySQL.
This interface presents only Haskell data types, and
restricts or hides many options provided by the C API. 

\begin{code}
module ABR.MySQL(
      MySQL, myConnect, myClose, myQuery, myFetch
   ) where
\end{code}

\begin{code}
import Foreign
import Foreign.C
import Control.Monad
\end{code}

\begin{code}
import ABR.MySQLCBinding
import ABR.Check
import ABR.DeepSeq
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2012-11-24: Moved into {\tt ABR.Database}.
   

\submodule{Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Connections}

A \highlighttt{MySQL} is our handle on a MySQL
connection.

\begin{code}
type MySQL = 
   Ptr MYSQL
\end{code}

\submodule{Functions} %%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Establishing a connection}  %%%%%%

\noindent
\highlighttt{myConnect}~$\mathit{host~user~passwd~db}$
returns {\tt CheckPass}~$\mathit{mysql}$ if a
connection could be established to the MySQL server
running on $\mathit{host}$ ({\tt ""} = {\tt
"localhost"}), as $\mathit{user}$ ({\tt ""} = {\tt the
current user}), with password $\mathit{passwd}$ ({\tt
""} = no password), using database $\mathit{db}$ ({\tt
""} = no database selected). If an error occurs, {\tt
CheckFail}~$(\mathit{errNum},\mathit{errMsg})$ is
returned.

\begin{code}
myConnect :: String -> String -> String -> String 
   -> IO (CheckResult MySQL (Integer, String))
myConnect host user passwd db = do
   mysql <- mysql_init nullPtr
   mysql' <- withCStringOrNull host (\host ->
      withCStringOrNull user (\user ->
         withCStringOrNull passwd (\passwd ->
            withCStringOrNull db (\db ->
                  mysql_real_connect mysql host user
                     passwd db 0 nullPtr 0
              )
           )
        )
     )
   if mysql' == nullPtr
      then do
         e <- mysql_errno mysql
         m <- mysql_error mysql
         let e' = toInteger e
         m' <- peekCString m
         (e',m') `deepSeq` mysql_close mysql
         return (CheckFail (e',m'))
      else return (CheckPass mysql')
\end{code}

\subsubmodule{Closing a connection} %%%%%%%

\noindent \highlighttt{myClose}~$\mathit{mysql}$
closes the connection and frees the memory it uses.

\begin{code}
myClose :: MySQL -> IO ()
myClose = mysql_close
\end{code}

\subsubmodule{Issuing a query} %%%%%%%

\noindent \highlighttt{myQuery}~$\mathit{mysql~query}$
executes the SQL $\mathit{query}$, returning\\
{\tt
CheckPass}~$(\mathit{fields},\mathit{rows})$ if
successful, where $\mathit{fields}$ is the number
of fields that would be in any result set fetched
after this query and $\mathit{rows}$ is the number
of rows affected by this query or $-1$ if there is
a result to be fetched, or {\tt
CheckFail}~$(\mathit{errNum},\mathit{errMsg})$ if
not. 

\begin{code}
myQuery :: MySQL -> String 
   -> IO (CheckResult (Int,Integer) (Integer, String))
myQuery mysql query = do
   i <- withCStringLen query (\(query,len) ->
      mysql_real_query mysql query (fromIntegral len)
     )
   if i == 0 
      then do 
         rows <- mysql_affected_rows mysql
         fields <- mysql_field_count mysql
         return (CheckPass (fromIntegral fields,
                            toInteger rows))
      else do
         e <- mysql_errno mysql
         m <- mysql_error mysql
         let e' = toInteger e
         m' <- peekCString m
         (e',m') `deepSeq` mysql_close mysql
         return (CheckFail (e',m'))
\end{code}

\subsubmodule{Fetching query results} %%%%%%%

\noindent \highlighttt{myFetch}~$\mathit{mysql}~\mathit{fields}$
fetches the results set for the last query. $\mathit{fields}$ is
the number of fields that wil be returned, as reported by the last
call to {\tt myQuery}.
It returns {\tt
CheckPass}~$(\mathit{rows},\mathit{lss},\mathit{
csss})$, where $\mathit{rows}$ is the number of
rows in the data set, $\mathit{lss}$ is the list of
lengths of each field for each row, and
$\mathit{csss}$ is the list of rows of columns. In
the case of an error, {\tt
CheckFail}~$(\mathit{errNum},\mathit{errMsg})$ is
returned instead.

\begin{code}
myFetch :: MySQL -> Int
   -> IO (CheckResult (Integer, [[Int]], 
      [[String]]) (Integer, String))
myFetch mysql fields = do
   results <- mysql_store_result mysql
   if results /= nullPtr
      then do
         rows <- mysql_num_rows results 
         let rows' = toInteger rows
         (lss,csss) <- getRows results rows' fields
         (lss,csss) `deepSeq` mysql_free_result results
         return (CheckPass (rows', lss, csss))
      else do
         e <- mysql_errno mysql
         m <- mysql_error mysql
         let e' = toInteger e
         m' <- peekCString m
         return (CheckFail (e',m'))
   where
   getRows :: Ptr MYSQL_RES -> Integer -> Int
      -> IO ([[Int]], [[String]])
   getRows results rows fields = gr rows
      where
      gr :: Integer -> IO ([[Int]], [[String]])
      gr r | r <= 0 =
         return ([], [])
           | otherwise = do
         row <- mysql_fetch_row results
         ls <- mysql_fetch_lengths results
         ls' <- peekArray fields ls
         let ls'' = map fromIntegral ls'
         row' <- peekArray fields row
         css <- mapM (\(l,p) -> do 
            cs <- peekArray l p
            return $ map (toEnum . fromIntegral) cs
           ) (zip ls'' row')
         (lss, csss) <- gr (r - 1)
         return (ls'' : lss, css : csss)
\end{code}

\noindent This has not been tested the case of
NULL field values, where the row contains a 
null pointer.

\submodule{Utilities} %%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent {\tt withCStringOrNull}~$\mathit{cs~f}$
extends {\tt CString.withCString} by passing a null
pointer to $f$ if $\mathit{cs}$ is empty, instead of
making an empty C string.

\begin{code}
withCStringOrNull :: String -> (CString -> IO a) -> IO a
withCStringOrNull cs f = if null cs
   then f nullPtr 
   else withCString cs f
\end{code}

