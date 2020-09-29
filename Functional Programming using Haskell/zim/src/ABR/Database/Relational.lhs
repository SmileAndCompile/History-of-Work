% Relational.#ext
% This file was produced from Relational.lit

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

\module{(Experimental) Database.Relational} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Database.Relational} is \emph{UNDER CONSTRUCTION}.

\begin{code}
module ABR.Database.Relational where
\end{code}

\begin{code}
import Data.List
import Data.Maybe
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2012-11-24: Moved to {\tt ABR.Database.Relational} from {\tt ABR.RelationalDB}
   

\submodule{Definitions and data types} %%%%%%%%%%%%%%%%%%%

A \emph{database} is a collection of tables. 
Each \emph{table} represents a relation.
Each row of a table represents one tuple, an element
of the relation. Each column of a table, is an
\emph{attribute} of that table. The terms table and 
relation are synonymous, as are row and tuple.

Any given database, populated with data, is an instance
of a \emph{database schema}, a specification of the
names and types of data in each table attribute.

A name is just a string. Names are used to identify
particular attributes tables and databases.

\begin{code}
type Name = String
\end{code}

\noindent An attribute specification consists of
a name and a type. 

\begin{code}
data Attribute =   AString  Name
                 | AInteger Name
                 | ADouble  Name
                 deriving (Eq, Ord, Show)
\end{code}

\noindent A \emph{schema} consists of a name, which may 
be empty, and a list of specifications. A \emph{table
schema} contains a list of attribute specifications,
and specifies the information to be stored in each column
of a table. The relation schema that relation $r$ is
an instance of is denoted $\alpha(r)$. A database schema
contains a list of table schemas.
            
\begin{code}
data Schema a = Schema Name [a]
\end{code}
            
\begin{code}
type TableSchema = Schema Attribute
\end{code}
            
\begin{code}
type DatabaseSchema = Schema TableSchema
\end{code}

\noindent Each element of a row is one \emph{datum}.
The following union type allows each datum to be of one
of a range of types. The types correspond to the possible 
types of attributes, with one extra to represent the
absence of a datum.

\begin{code}
data Datum =   DNull
             | DString  String
             | DInteger Integer
             | DDouble  Double
             deriving (Eq, Ord, Show)
\end{code}

\noindent Within each row, the order of the data is
significant.

\begin{code}
type Row = [Datum]
\end{code}

\noindent Each table has a schema that identifies the
contents of each attribute.

\begin{code}
data Table = Table TableSchema [Row]
\end{code}


\submodule{Relational algebra} %%%%%%%%%%%%%%%%%%%

\subsubmodule{Projection of a tuple on a relation schema} 

The projection $t[X]$ of a tuple $t$ on a relation
schema $X$ is computed by discarding the
attributes in $t$ that do not appear in $X$. This
projection operation will be applied to many rows and
is worth computing just once. A \emph{projector}
is a function that performs a projection on one
tuple.

\begin{code}
type Projector = Row -> Row
\end{code}

\noindent A projector can be computed from the new and old
table schemas. {\tt makeProjector}~$Y$~$X$ returns either:
\begin{itemize}
   \item {\tt Nothing}, if the new schema $Y$ contains
      an attribute not in the old schema $X$; or
   \item {\tt Just~(}$Y'${\tt,~}$p${\tt )}, where table
      schema $Y'$ is the same as the requested new table
      schema $Y$ with the exception that the attributes
      are in the same order as in the old table 
      schema $X$, and $p$ is the projector that can be
      applied to tuple that conforms to $X$ to produce
      a tuple the conforms to $Y'$.
\end{itemize}

\begin{code}
makeProjector :: TableSchema -> TableSchema 
   -> Maybe (TableSchema, Projector)
makeProjector (Schema _ ys) (Schema _ xs) =
   let ys' = filter (`elem` ys) xs
       ks = map (`elem` ys') xs
       p ds = [d | (k,d) <- zip ks ds, k]
   in if null (ys \\ xs) 
      then Just (Schema "" ys', p)
      else Nothing
\end{code}

\subsubmodule{Projection of a relation on a relation
              schema} %%%%%%

The projection $\pi_{X}(r)$ of a relation $r$ on a
relation schema $X$, is defined by:
\begin{enumerate}
   \item $X \subseteq \alpha(r)$ and
      $\alpha(\pi_{X}(r)) = X$
   \item $\pi_{X}(r) = \{ t[X] : t \in r \}$
\end{enumerate}

\noindent {\tt projectTable (}$X${\tt , }$p{\tt )}$~$r$ returns 
$\pi_{X}(r)$ using $p$ to project all the tuples in $r$.
              
\begin{code}
projectTable :: (TableSchema, Projector) -> Table -> Table
projectTable (x, p) (Table _ rows) = Table x (map p rows)
\end{code}

\noindent {\tt proj}~$X$~$r$ returns $\pi_{X}(r)$.
If the schema restrictions are not met then the program
will terminate with an error.

\begin{code}
proj :: TableSchema -> Table -> Table
proj x r@(Table x' _) =  case makeProjector x x' of
   Nothing -> error "Can not perform projection, \
                    \incompatible schemas."
   Just xp -> projectTable xp r
\end{code}


\subsubmodule{Natural join} %%%%%%%%%

The \emph{natural join} $r_{1} \Join r_{2}$ of relations
$r_{1}$ and $r_{2}$ is defined by:

\begin{enumerate}
   \item $\alpha(r_{1} \Join r_{2}) =
      \alpha(r_{1}) \cup \alpha(r_{2})$
   \item $r_{1} \Join r_{2} = \{t \in 
      (r_{1} \times r_{2})[\alpha(r_{1} \Join r_{2})] : 
      t[\alpha(r_{1})] \in r_{1} \wedge
      t[\alpha(r_{2})] \in r_{2}\}$
\end{enumerate}

\noindent Each tuple of the joined relation contains
the combined attributes from two tuples, one from each
of the original relations. A joined tuple is only formed
if the overlapping attributes were equal. 

A \emph{joiner} is a function that joins two tuples
if the overlapping attributes are equal.

\begin{code}
type Joiner = Row -> Row -> Maybe Row
\end{code}

\noindent {\tt makeJoiner}~$X$~$X'$ returns 
{\tt (}$X \cup X'${\tt ,}$j${\tt )}, where $j$ is a 
joiner that joins a tuple that conforms to $X$ to
a tuple that conforms to $X'$, producing a tuple conforming
to $X \cup X'$ if the overlapping attributes are equal.

\begin{code}
makeJoiner :: TableSchema -> TableSchema
   -> (TableSchema, Joiner)
makeJoiner (Schema _ xs) (Schema _ xs') =
   let xsxs' = xs ++ xs'
       ixs = nubBy (\ (_,x) (_,x') -> x == x') $
                zip [0..] xsxs'
       is = map fst ixs
       ks = map (`elem` is) [0..length xsxs'-1]
       xs'' = map snd ixs
       js = [(i,i') | (i,x) <- zip [0..] xs,
                      (i',x') <- zip [0..] xs', x == x'] 
       j ds ds' = if and [ds!!i == ds'!!i' | (i,i') <- js]
          then Just [d | (k,d) <- zip ks (ds ++ ds'), k]
          else Nothing 
   in (Schema "" xs'', j)
\end{code}

\noindent {\tt joinTables (}$X${\tt, }$j${\tt )}~$r_{1}$~$r_{2}$
returns $r_{1} \Join r_{2}$, provided $X = \alpha(r_{1}
\Join r_{2})$. $j$ is used to join individual tuples.

\begin{code}
joinTables :: (TableSchema, Joiner) -> Table -> Table
   -> Table
joinTables (x,j) (Table _ ts) (Table _ ts') =
   Table x (catMaybes [j t t' | t <- ts, t' <- ts'])
\end{code}

\noindent $r_{1}$~\verb"|><|"~$r_{1}$ returns $r_{1} \Join r_{2}$.

\begin{code}
infixl 5 |><|
\end{code}

\begin{code}
(|><|) :: Table -> Table -> Table
r1@(Table x _) |><| r2@(Table x' _) = 
   joinTables (makeJoiner x x') r1 r2
\end{code}


\subsubmodule{Union} %%%%%%%%%

The \emph{union} $r_{1} \cup r_{2}$ of relations
$r_{1}$ and $r_{2}$ is defined by:

\begin{enumerate}
   \item $\alpha(r_{1}) = \alpha(r_{2})$ and
      $\alpha(r_{1} \cup r_{2}) = \alpha(r_{1})$
   \item $r_{1} \cup r_{2} = 
      \{t : t \in r_{1} \vee t \in r_{2} \}$
\end{enumerate}

\noindent $r_{1}$\verb" `u` "$r_{2}$ returns
$r_{1} \cup r_{2}$, provided $\alpha(r_{1}) =
\alpha(r_{2})$. No check is performed to ensure this
precondition.

\begin{code}
u :: Table -> Table -> Table
(Table x ts) `u` (Table _ ts') = Table x (ts ++ ts')
\end{code}


\subsubmodule{Difference} %%%%%%%%%

The \emph{difference} $r_{1} - r_{2}$ of relations
$r_{1}$ and $r_{2}$ is defined by:

\begin{enumerate}
   \item $\alpha(r_{1}) = \alpha(r_{2})$ and
      $\alpha(r_{1} \cup r_{2}) = \alpha(r_{1})$
   \item $r_{1} \cup r_{2} = 
      \{t : t \in r_{1} \vee t \notin r_{2} \}$
\end{enumerate}

\noindent $r_{1}$\verb" `dif` "$r_{2}$ returns
$r_{1} - r_{2}$, provided $\alpha(r_{1}) =
\alpha(r_{2})$. No check is performed to ensure this
precondition.

\begin{code}
dif :: Table -> Table -> Table
(Table x ts) `dif` (Table _ ts') = Table x (ts \\ ts')
\end{code}


\subsubmodule{Selection} %%%%%%%%%

The \emph{selection} $\sigma_{p}(r)$ of relation 
$r$ by predicate $p$ is defined by:

\begin{enumerate}
   \item $\alpha(\sigma_{p}(r)) = \alpha(r)$
   \item $\sigma_{p}(r) = \{t \in r : p(r) \}$
\end{enumerate}

\noindent {\tt select}~$p$~$r$ returns $\sigma_{p}(r)$.

\begin{code}
select :: (Row -> Bool) -> Table -> Table
select p (Table x ts) = Table x (filter p ts)
\end{code}

\subsubmodule{Renaming} %%%%%%%%%

The \emph{renaming} $\rho_{B|A}(r)$ in relation 
$r$ of attribute $A$ to attribute $B$ is defined by:

\begin{enumerate}
   \item $A \in \alpha(r)$, $B \notin \alpha(r)$ and
      $\alpha(\rho_{B|A}(r)) = 
      (\alpha(r) - \{A\}) \cup \{B\}$
   \item $\rho_{B|A}(r) = \{ t : t' \in r \wedge
      t'[B] = t[A] \wedge 
      \forall C \in (\alpha(r) - \{A\}) \cdot 
      t[C] = t'[C]\}$
\end{enumerate}

\noindent {\tt renameTable}~$B$~$A$~$r$ returns
$\rho_{B|A}(r)$.

\begin{code}
renameTable :: Attribute -> Attribute -> Table -> Table
renameTable b a (Table x ts) = 
   Table (renameTableSchema b a x) ts
\end{code}

\noindent {\tt renameTableSchema}~$B$~$A$~$X$ returns
$(X - \{A\}) \cup \{B\}$.

\begin{code}
renameTableSchema :: Attribute -> Attribute ->
   TableSchema -> TableSchema
renameTableSchema b a (Schema _ as) =
   Schema "" [if a' == a then a else b | a' <- as]
\end{code}


\submodule{Datum operations} %%%%%%%%%%%%%%%%%%%


\begin{code}
addDatum :: Datum -> Datum -> Datum
addDatum DNull y = y
addDatum x DNull = x
addDatum (DString cs) y = DString (cs ++ (case y of
      DString cs' -> cs'
      DInteger i  -> show i
      DDouble d   -> show d
   ))
addDatum x (DString cs) = DString ((case x of
      DInteger i  -> show i
      DDouble d   -> show d
   ) ++ cs)
addDatum (DDouble d) y = DDouble (d + (case y of
      DDouble d' -> d'
      DInteger i -> fromIntegral i
   ))
addDatum x (DDouble d) = DDouble ((case x of
      DDouble d' -> d'
      DInteger i -> fromIntegral i
   ) + d)
addDatum (DInteger i) (DInteger i') = DInteger (i + i')
\end{code}

