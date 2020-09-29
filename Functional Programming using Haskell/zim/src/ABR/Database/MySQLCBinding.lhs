% MySQLCBinding.#ext
% This file was produced from MySQLCBinding.lit

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

\module{(Experimental) MySQL C API Binding} %%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.MySQLCBinding} is an interface
to the MySQL C API. It uses C types for all
arguments and results, without any attempt to make
if Haskell friendly. Module {\tt MySQL}, which is
built on top of this module, provides an interface
using Haskell types.

The descriptions have been adapted from the MySQL
Reference Manual, omitting much detail, and introducing
new errors. I'd have that close at hand while using
this module.

\begin{code}
module ABR.MySQLCBinding (
      My_bool, My_ulonglong, MYSQL, MYSQL_RES,
      MYSQL_ROW, MYSQL_ROW_OFFSET, MYSQL_FIELD,
      MYSQL_FIELD_OFFSET, Enum_mysql_option,
      mysql_affected_rows, mysql_change_user,
      mysql_character_set_name, mysql_close,
      mysql_data_seek, mysql_errno, mysql_error,
      mysql_fetch_field, mysql_fetch_fields,
      mysql_fetch_field_direct, mysql_fetch_lengths,
      mysql_fetch_row, mysql_field_count,
      mysql_field_seek, mysql_field_tell,
      mysql_free_result, mysql_get_client_info,
      mysql_get_host_info, mysql_get_proto_info,
      mysql_get_server_info, mysql_info, mysql_init,
      mysql_insert_id, mysql_kill, mysql_list_dbs,
      mysql_list_fields, mysql_list_processes,
      mysql_list_tables, mysql_num_fields,
      mysql_num_rows, mysql_options, mysql_ping,
      mysql_query, mysql_real_connect,
      mysql_real_escape_string, mysql_real_query,
      mysql_row_seek, mysql_row_tell, mysql_select_db,
      mysql_shutdown, mysql_stat, mysql_store_result,
      mysql_thread_id, mysql_use_result
   ) where
\end{code}

\begin{code}
import Foreign
import Foreign.C
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2012-11-24: Moved into {\tt ABR.Database}.
   

\submodule{API Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Basics} %%%%%

\noindent A \highlighttt{My\_bool} is a C {\tt char}.

\begin{code}
type My_bool = CChar
\end{code}

\noindent A \highlighttt{My\_ulonglong} is
supposedly a 64 bit, unsigned integer, but the way
is used implies signed is a more useful choice.

\begin{code}
type My_ulonglong = CLLong
\end{code}

\subsubmodule{Connections} %%%%%%%%%

\noindent A \highlighttt{MYSQL} is some opaque C object. A pointer to
this structure is our handle on a connection.

\begin{code}
type MYSQL =
   ()
\end{code}

\subsubmodule{Results} %%%%%%%%%

\noindent A \highlighttt{MYSQL\_RES} is some opaque C object.
A pointer to this structure is our handle on a result to a query.

\begin{code}
type MYSQL_RES =
   ()
\end{code}

\subsubmodule{Rows} %%%%%%%%%

\noindent A \highlighttt{MYSQL\_ROW} is an array of
strings. The strings are \emph{not} terminated with
\verb"\0"s, as they could be binary data.

\begin{code}
type MYSQL_ROW = Ptr CString
\end{code}

\noindent A \highlighttt{MYSQL\_ROW\_OFFSET} is 
a pointer to a {\tt MYSQL\_ROWS}.

\begin{code}
type MYSQL_ROW_OFFSET = 
   Ptr ()
\end{code}

\subsubmodule{Fields} %%%%%%%%%

\noindent A \highlighttt{MYSQL\_FIELD} is a C structure.

\begin{code}
type MYSQL_FIELD = 
   ()
\end{code}

\noindent A \highlighttt{MYSQL\_FIELD\_OFFSET} is an offset
into a MySQL field list.

\begin{code}
type MYSQL_FIELD_OFFSET = CUInt 
\end{code}

\subsubmodule{Options} %%%%%%%%%

\noindent An \highlighttt{Enum\_mysql\_option} is a
C enumeration, used by function {\tt mysql\_options}.

\begin{code}
type Enum_mysql_option = CUInt 
\end{code}

\submodule{API Functions} %%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent
\highlighttt{mysql\_affected\_rows}~$\mathit{mysql}$
returns the number of rows changed by the last {\tt
UPDATE}, deleted by the last {\tt DELETE} or inserted
by the last {\tt INSERT} statement. May be called
immediately after {\tt mysql\_query} for {\tt UPDATE},
{\tt DELETE}, or {\tt INSERT} statements. For {\tt
SELECT} statements, {\tt mysql\_affected\_rows} works
like {\tt mysql\_num\_rows}. Returns an integer greater
than zero to indicate the number of rows affected or
retrieved. Zero indicates that no records where updated
for an {\tt UPDATE} statement, no rows matched the {\tt
WHERE} clause in the query or that no query has yet
been executed. $-1$ indicates that the query returned
an error or that, for a {\tt SELECT} query, {\tt
mysql\_affected\_rows} was called prior to calling {\tt
mysql\_store\_result}.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_affected_rows :: Ptr MYSQL -> IO My_ulonglong
\end{code}

\noindent
\highlighttt{mysql\_change\_user}~$\mathit{mysql~user~passwd~db}$
changes the user to $user$ with $passwd$ and causes the
database specified by $db$ to become the default
(current) database on the connection specified by
$mysql$. In subsequent queries, this database is the
default for table references that do not include an
explicit database specifier. Returns zero for success,
non-zero if an error occurred.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_change_user :: Ptr MYSQL -> CString -> CString 
       -> CString -> IO My_bool
\end{code}

\noindent
\highlighttt{mysql\_character\_set\_name}~$\mathit{mysql}$
returns the default character set for the current
connection.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_character_set_name :: Ptr MYSQL -> IO CString
\end{code}

\noindent \highlighttt{mysql\_close}~$\mathit{mysql}$
closes and deallocates the connection $\mathit{mysql}$.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_close :: Ptr MYSQL -> IO ()
\end{code}

\noindent
\highlighttt{mysql\_data\_seek}~$\mathit{result~offset}$
seeks to an arbitrary row in a query result set. This
requires that the result set structure contains the
entire result of the query, so {\tt mysql\_data\_seek}
may be used in conjunction only with {\tt
mysql\_store\_result}, not with {\tt mysql\_use\_result}.
The offset should be a value in the range from 0 to
{\tt mysql\_num\_rows~result}~$-1$.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_data_seek :: Ptr MYSQL_RES -> CULLong -> IO ()
\end{code}

\noindent \highlighttt{mysql\_errno}~$\mathit{mysql}$
returns the error code returned by the last MySQL API
function. 0 indicates no error.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_errno :: Ptr MYSQL -> IO CUInt
\end{code}

\noindent \highlighttt{mysql\_error}~$\mathit{mysql}$
returns the error message returned by the last MySQL API
function. An empty string indicates no error.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_error :: Ptr MYSQL -> IO CString
\end{code}

\noindent
\highlighttt{mysql\_fetch\_field}~$\mathit{result}$
returns the definition of one column of a result set as
a {\tt MYSQL\_FIELD} structure. Call this function
repeatedly to retrieve information about all columns in
the result set. Returns {\tt NULL} when no more  fields
are left. {\tt mysql\_fetch\_field} is reset to return
information about the first  field each time you
execute a new {\tt SELECT} query. The field returned is
also affected by calls to {\tt mysql\_field\_seek}.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_fetch_field :: Ptr MYSQL_RES 
       -> IO (Ptr MYSQL_FIELD)
\end{code}

\noindent
\highlighttt{mysql\_fetch\_fields}~$\mathit{result}$
returns an array of all {\tt MYSQL\_FIELD} structures
for a result set. Each structure provides the field
definition for one column of the result set.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_fetch_fields :: Ptr MYSQL_RES 
       -> IO (Ptr MYSQL_FIELD)
\end{code}

\noindent
\highlighttt{mysql\_fetch\_field\_direct}~$\mathit{result~fieldnr}$
returns column $\mathit{fieldnr}$'s field definition as
a {\tt MYSQL\_FIELD} structure. The value of
$\mathit{fieldnr}$ should be in the range from 0 to
{\tt mysql\_num\_fields~result}~$-1$.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_fetch_field_direct :: Ptr MYSQL_RES -> CUInt
       -> IO (Ptr MYSQL_FIELD)
\end{code}

\noindent
\highlighttt{mysql\_fetch\_lengths}~$\mathit{result}$
returns an array of the lengths of the columns of the
current row within a {\tt result} set, or {\tt NULL} in
the case of an error.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_fetch_lengths :: Ptr MYSQL_RES -> IO (Ptr CULong)
\end{code}

\noindent
\highlighttt{mysql\_fetch\_row}~$\mathit{result}$
retrieves the next row of a result set. Returns
NULL when there are no more rows to retrieve.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_fetch_row :: Ptr MYSQL_RES -> IO (MYSQL_ROW)
\end{code}

\noindent
\highlighttt{mysql\_field\_count}~$\mathit{mysql}$
returns the number of columns for the most recent
query on the connection.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_field_count :: Ptr MYSQL -> IO CUInt
\end{code}

\noindent
\highlighttt{mysql\_field\_seek}~$\mathit{result~offset}$
sets the field cursor to the given offset. The next
call to {\tt mysql\_fetch\_field} will retrieve the field
definition of the column associated with that offset.
To seek to the beginning of a row, pass an offset value
of zero. Returns the previous value of the field cursor.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_field_seek :: Ptr MYSQL_RES 
       -> MYSQL_FIELD_OFFSET -> IO MYSQL_FIELD_OFFSET
\end{code}

\noindent
\highlighttt{mysql\_field\_tell}~$\mathit{result}$
returns the position of the field cursor used for the
last {\tt mysql\_fetch\_field}. This value can be used
as an argument to {\tt mysql\_field\_seek}.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_field_tell :: Ptr MYSQL_RES
       -> IO MYSQL_FIELD_OFFSET
\end{code}

\noindent
\highlighttt{mysql\_free\_result}~$\mathit{result}$
frees the memory allocated for a result set by {\tt
mysql\_store\_result}, {\tt mysql\_use\_result}, {\tt
mysql\_list\_dbs}, etc. When you are done with a result
set, you must free the memory it uses by calling this
function.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_free_result :: Ptr MYSQL_RES -> IO ()
\end{code}

\noindent \highlighttt{mysql\_get\_client\_info}
returns a string that represents the client library
version.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_get_client_info :: IO CString
\end{code}

\noindent
\highlighttt{mysql\_get\_host\_info}~$\mathit{mysql}$
returns a string describing the type of connection in
use, including the server host name.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_get_host_info :: Ptr MYSQL -> IO CString
\end{code}

\noindent
\highlighttt{mysql\_get\_proto\_info}~$\mathit{mysql}$
returns the protocol version used by current connection.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_get_proto_info :: Ptr MYSQL -> IO CUInt
\end{code}

\noindent
\highlighttt{mysql\_get\_server\_info}~$\mathit{mysql}$
returns a string describing the type of connection in
use, including the server host name.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_get_server_info :: Ptr MYSQL -> IO CString
\end{code}

\noindent
\highlighttt{mysql\_info}~$\mathit{mysql}$
retrieves a string providing information about the most
recently executed query, but only for some statements.
For other statements, {\tt mysql\_info} returns NULL.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_info :: Ptr MYSQL -> IO CString
\end{code}

\noindent \highlighttt{mysql\_init}~$\mathit{mysql}$
allocates or initializes a {\tt MYSQL} object suitable
for {\tt mysql\_real\_connect}. If $\mathit{mysql}$ is a
null pointer (use {\tt Foreign.nullPtr}), the function
allocates, initializes and returns a new object.
Otherwise the object is initialized and the address
returned.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_init :: Ptr MYSQL -> IO (Ptr MYSQL)
\end{code}

\noindent
\highlighttt{mysql\_insert\_id}~$\mathit{mysql}$
returns the ID generated for an \\
{\tt AUTO\_INCREMENT}
column by the previous query. Use this function after
you have performed an {\tt INSERT} query into a table
that contains an {\tt AUTO\_INCREMENT} field.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_insert_id :: Ptr MYSQL -> IO My_ulonglong
\end{code}

\noindent
\highlighttt{mysql\_kill}~$\mathit{mysql~pid}$
asks the server to kill the thread specified by
$\mathit{pid}$.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_kill :: Ptr MYSQL -> CULong -> IO CInt
\end{code}

\noindent
\highlighttt{mysql\_list\_dbs}~$\mathit{mysql~wild}$
returns a result set consisting of database names on
the server that match the simple regular expression
specified by the $\mathit{wild}$ parameter.
$\mathit{wild}$ may contain the wild-card characters
`\verb"%"' or `\verb"_"', or may be a NULL pointer to
match all databases. Returns NULL if an error occurred.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_list_dbs :: Ptr MYSQL -> CString 
       -> IO (Ptr MYSQL_RES)
\end{code}

\noindent
\highlighttt{mysql\_list\_fields}~$\mathit{mysql~table~wild}$
returns a result set consisting of field names in the
given table that match the simple regular expression
specified by the wild parameter. $\mathit{wild}$ may
contain the wild-card characters `\verb"%"' or
`\verb"_"', or may be a NULL pointer to match all
fields. Returns NULL if an error occurred.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_list_fields :: Ptr MYSQL -> CString -> CString
       -> IO (Ptr MYSQL_RES)
\end{code}

\noindent
\highlighttt{mysql\_list\_processes}~$\mathit{mysql}$
returns a result set describing the current server threads.
Returns NULL if an error occurred.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_list_processes :: Ptr MYSQL -> IO (Ptr MYSQL_RES)
\end{code}

\noindent
\highlighttt{mysql\_list\_tables}~$\mathit{mysql~wild}$
returns a result set consisting of table names in the
current database that match the simple regular
expression specified by the $\mathit{wild}$ parameter.
$\mathit{wild}$ may contain the wild-card characters
`\verb"%"' or `\verb"_"', or may be a NULL pointer to
match all databases. Returns NULL if an error occurred.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_list_tables :: Ptr MYSQL -> CString 
       -> IO (Ptr MYSQL_RES)
\end{code}

\noindent
\highlighttt{mysql\_num\_fields}~$\mathit{result}$
returns the number of columns in a result set.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_num_fields :: Ptr MYSQL_RES -> IO CUInt
\end{code}

\noindent
\highlighttt{mysql\_num\_rows}~$\mathit{result}$
returns the number of rows in the result set.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_num_rows :: Ptr MYSQL_RES -> IO My_ulonglong
\end{code}

\noindent
\highlighttt{mysql\_options}~$\mathit{mysql~option~arg}$
Can be used to set extra connect options and affect
behavior for a connection.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_options :: Ptr MYSQL -> Enum_mysql_option 
       -> CString -> IO CInt
\end{code}

\noindent
\highlighttt{mysql\_ping}~$\mathit{mysql}$
checks whether or not the connection to the server is
working. If it has gone down, an automatic reconnection
is attempted. This function can be used by clients that
remain idle for a long while, to check whether or not
the server has closed the connection and reconnect if
necessary. Returns zero if the server is alive,
non-zero otherwise.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_ping :: Ptr MYSQL -> IO CInt
\end{code}

\noindent
\highlighttt{mysql\_query}~$\mathit{mysql~query}$ 
executes the SQL query pointed to by the
null-terminated string $\mathit{query}$. The query must
consist of a single SQL statement. You should not add a
terminating semicolon (`\verb";"') or \verb"\g" to the statement.
{\tt mysql\_query} cannot be used for queries that contain
binary data; you should use {\tt mysql\_real\_query}
instead. (Binary data may contain the `\verb"\0"' character.)

\begin{code}
foreign import ccall "mysql.h" 
    mysql_query :: Ptr MYSQL -> CString -> IO CInt
\end{code}

\noindent
\highlighttt{mysql\_real\_connect}~$\mathit{mysql~host~user~passwd~db~port}$\\
$\mathit{unix\_socket~client\_flag}$ attempts to establish a
connection to a MySQL database engine running on
$\mathit{host}$. {\tt mysql\_real\_connect} must complete
successfully before you can execute nearly all of the
other API functions. The function returns
$\mathit{mysql}$ if successful, otherwise null. The
parameters are specified as follows:

\begin{description}
   \item[$\mathit{mysql}$] is a pointer to an
      existing {\tt MYSQL} structure, initialized
      by {\tt mysql\_init}.
   \item[$\mathit{host}$] may be either a hostname
      or an IP address. If null or {\tt
      "localhost"} the local host is assumed.
   \item[$\mathit{user}$] is the MySQL login ID. If
      null, the current user is assumed.
   \item[$\mathit{passwd}$] is the password for
      $\mathit{user}$ (unencrypted). If null, only
      users that have empty passwords are checked.
   \item[$\mathit{db}$] is the database name. If
      not null, the connection will set the default
      database to this value.
   \item[$\mathit{port}$] If not 0, sets the port
      number to use.
   \item[$\mathit{unix\_socket}$] If not null, sets
      the socket or named pipe to use.
   \item[$\mathit{client\_flag}$] usually 0, but
      can be used to cope with some special
      circumstances.
\end{description}

\begin{code}
foreign import ccall "mysql.h" 
    mysql_real_connect :: Ptr MYSQL -> CString -> CString
       -> CString -> CString -> CUInt -> CString -> CUInt 
       -> IO (Ptr MYSQL)
\end{code}

\noindent
\highlighttt{mysql\_real\_escape\_string}~$\mathit{mysql~to~from~length}$ 
creates a legal SQL string that you can use in a SQL
statement. The string in $\mathit{from}$ is encoded to
an escaped SQL string, taking into account the current
character set of the connection. The result is placed
in $\mathit{to}$ and a terminating null byte is
appended. Characters encoded are \verb"NUL" (ASCII 0),
`\verb"\n"', `\verb"\r"', `\verb"\"', `\verb"'"',
`\verb"""', and Control-Z.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_real_escape_string :: Ptr MYSQL -> CString 
       -> CString -> CUInt -> IO (CInt)
\end{code}

\noindent
\highlighttt{mysql\_real\_query}~$\mathit{mysql~query~length}$ 
executes the SQL query pointed to by $query$, which
should be a string $length$ bytes long. The $query$
must consist of a single SQL statement. You should
not add a terminating semicolon (`\verb";"') or
\verb"\g" to the statement. You must use {\tt
mysql\_real\_query} rather than {\tt mysql\_query}
for queries that contain binary data, because
binary data may contain the `\verb"\0"' character.
In addition, {\tt mysql\_real\_query} is faster
than {\tt mysql\_query} because it does not call
{\tt strlen()} on the $query$ string. If you want
to know if the query should return a result set or
not, you can use {\tt mysql\_field\_count} to check
for this. Returns zero if the query was successful,
or non-zero if an error occurred.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_real_query :: Ptr MYSQL -> CString -> CUInt 
       -> IO (CInt)
\end{code}

\noindent
\highlighttt{mysql\_row\_seek}~$\mathit{result~offset}$ 
sets the row cursor to an arbitrary row in a query
result set. This requires that the result set structure
contains the entire result of the query, so {\tt
mysql\_row\_seek} may be used in conjunction only with
{\tt mysql\_store\_result}, not with {\tt
mysql\_use\_result}. The offset should be a value
returned from a call to {\tt mysql\_row\_tell} or to
{\tt mysql\_row\_seek}. This value is not simply a row
number; if you want to seek to a row within a result
set using a row number, use {\tt mysql\_data\_seek}
instead. Returns the previous value of the row cursor.
This value may be passed to a subsequent call to {\tt
mysql\_row\_seek}.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_row_seek :: Ptr MYSQL_RES -> MYSQL_ROW_OFFSET
       -> IO MYSQL_ROW_OFFSET
\end{code}

\noindent
\highlighttt{mysql\_row\_tell}~$\mathit{result}$ 
returns the current position of the row cursor for the
last {\tt mysql\_fetch\_row}. This value can be used as
an argument to {\tt mysql\_row\_seek}. You should use
{\tt mysql\_row\_tell} only after {\tt
mysql\_store\_result}, not after {\tt
mysql\_use\_result}.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_row_tell :: Ptr MYSQL_RES -> IO MYSQL_ROW_OFFSET
\end{code}

\noindent
\highlighttt{mysql\_select\_db}~$\mathit{mysql~db}$ 
causes the database specified by $\mathit{db}$ to
become the default (current) database on the connection
specified by $\mathit{mysql}$. Returns zero for success
or non-zero otherwise.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_select_db :: Ptr MYSQL -> CString -> IO CInt
\end{code}

\noindent
\highlighttt{mysql\_shutdown}~$\mathit{mysql}~\mathit{level}$ 
asks the database server to shut down. The connected
user must have shutdown privileges.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_shutdown :: Ptr MYSQL -> CInt -> IO CInt
\end{code}

\noindent
\highlighttt{mysql\_stat}~$\mathit{mysql}$ 
returns a character string containing information
similar to that provided by the {\tt mysqladmin} status
command. This includes uptime in seconds and the number
of running threads, questions, reloads, and open
tables, or NULL if an error occurred.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_stat :: Ptr MYSQL -> IO CString
\end{code}

\noindent
\highlighttt{mysql\_store\_result}~$\mathit{mysql}$
reads and returns a pointer to the entire result
for the last query, or NULL is there has been an
error.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_store_result :: Ptr MYSQL 
       -> IO (Ptr MYSQL_RES)
\end{code}

\noindent
\highlighttt{mysql\_thread\_id}~$\mathit{mysql}$
returns the thread ID of the current connection. This
value can be used as an argument to {\tt mysql\_kill}
to kill the thread. If the connection is lost and you
reconnect with {\tt mysql\_ping}, the thread ID will
change. This means you should not get the thread ID and
store it for later. You should get it when you need it.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_thread_id :: Ptr MYSQL -> IO CULong
\end{code}

\noindent
\highlighttt{mysql\_use\_result}~$\mathit{mysql}$
initiates a result set retrieval but does not actually
read the result set into the client like {\tt
mysql\_store\_result} does. Instead, each row must be
retrieved individually by making calls to {\tt
mysql\_fetch\_row}.

\begin{code}
foreign import ccall "mysql.h" 
    mysql_use_result :: Ptr MYSQL -> IO (Ptr MYSQL_RES)
\end{code}

