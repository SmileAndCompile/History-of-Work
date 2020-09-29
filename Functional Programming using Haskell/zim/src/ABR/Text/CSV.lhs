% CSV.#ext
% This file was produced from CSV.lit

% ABRHLibs -- a personal library of Haskell modules
% Copyright (C) 2014  Andrew Rock
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

\module{Text.CSV} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The \highlighttt{ABR.Text.CSV} library provides functions to
parse, construct, and interrogate CSV data.


\begin{code}
module ABR.Text.CSV where
\end{code}



\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

New 2014-12-29: Started to support marks entry via Blackboard 
Marks centre.
   

\submodule{Basic data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent A CSV file is one big string.

\begin{code}
type CSV = String
\end{code}

\noindent A {\tt Field} is one cell.

\begin{code}
type Field = String
\end{code}

\noindent A {\tt Row} is one list of {\tt Field}s.

\begin{code}
type Row = [Field]
\end{code}

\noindent A {\tt Table} is a just list of {\tt Row}s.

\begin{code}
type Table = [Row]
\end{code}

\submodule{Parser} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Don't really need a full-on parser.

\submodule{Interrogation} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\submodule{Instances} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{Showing} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\noindent The {\tt Show} instances reconstruct valid CSV syntax.


\subsubmodule{Interrogation} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


