% Time.#ext
% This file was produced from Time.lit

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

\module{Util.Time} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module \highlighttt{ABR.Util.Time} collects time-related functions. 
The new time libraries are complicated. So are the old ones. The
transition is not smooth. GHC 7.0.x (hobbit) is not compatible with
the current versions, hence all the legacy stuff implemented with 
conditional compilation.

\begin{code}
{-# LANGUAGE CPP #-}
\end{code}

\begin{code}
module ABR.Util.Time (
      LegacyTime,
      C.UTCTime, LT.ZonedTime, LT.LocalTime,
      utcToZonedTime, utcToLocalTime,
      getCurrentUTCTime, getCurrentLocalTime,
         getCurrentZonedTime, 
      TimeFormat, dateThenTime1, dateThenTime2,
      formatTime, formatUTCTime,
      currentTime, fileModTime,
      LegacyTimes (getCurrentLegacyTime,
         diffSec, diffMin, diffHour, diffDay,
         formatLegacyTime)
   ) where
\end{code}

\begin{code}
import qualified Data.Time.Clock as C
import qualified Data.Time.Format as F
import qualified Data.Time.LocalTime as LT
import qualified System.Directory as D
# if __GLASGOW_HASKELL__ < 705
-- legacy for hobbit
import qualified System.Locale as L
import qualified System.Time as ST
# else 
# endif
\end{code}

\submodule{Maintenance notes} %%%%%%%%%%%%%%%%%%%%%%

Reviewed 2016-05-30. Updated for changes to dependencies.\\
Reviewed 2015-02-02. Passed {\tt hlint}.\\
Reviewed 2014-05-29: Made all the {\tt -Wall}s go away.\\
New 2013-11-27.
   
\submodule{Data types} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{LegacyTime} is a type synonym for
either {\tt ClockTime} or {\tt UTCTime}, whichever
is returned by {\tt getModificationTime}.

\begin{code}
type LegacyTime = 
# if __GLASGOW_HASKELL__ < 705
   -- legacy for hobbit
   ST.ClockTime
# else 
   C.UTCTime
# endif
\end{code}

\submodule{Time conversions} %%%%%%%%%%%%%%%%%%%%

\highlighttt{utcToZonedTime} converts a UTC time to a {\tt ZonedTime}.

\begin{code}
utcToZonedTime :: C.UTCTime -> IO LT.ZonedTime
utcToZonedTime = LT.utcToLocalZonedTime
\end{code}
   
\noindent\highlighttt{utcToLocalTime} converts a UTC time to a {\tt LocalTime}.

\begin{code}
utcToLocalTime :: C.UTCTime -> IO LT.LocalTime
utcToLocalTime utc = do
   tz <- LT.getCurrentTimeZone
   return $ LT.utcToLocalTime tz utc
\end{code}

\submodule{Getting the current time} %%%%%%%%%%%%%%%%%%%%

\highlighttt{getCurrentUTCTime} gets the current time in UTC.

\begin{code}
getCurrentUTCTime :: IO C.UTCTime
getCurrentUTCTime = C.getCurrentTime
\end{code}
   
\noindent\highlighttt{getCurrentZonedTime} gets the current 
local time and time zone.

\begin{code}
getCurrentZonedTime :: IO LT.ZonedTime
getCurrentZonedTime = do
   utc <- C.getCurrentTime
   utcToZonedTime utc
\end{code}

\noindent\highlighttt{getCurrentLocalTime} gets the current 
local time. Local times can not display the time zone on formatting.

\begin{code}
getCurrentLocalTime :: IO LT.LocalTime
getCurrentLocalTime = do
   utc <- C.getCurrentTime
   utcToLocalTime utc
\end{code}

\submodule{Format strings} %%%%%%%%%%%%%%%%%%%%

A \highlighttt{TimeFormat} is a time format string.

\begin{code}
type TimeFormat = String
\end{code}

\noindent These are strings to use with the time {\tt formatTime}.

\begin{tabbing}
dateThenTime1mmmm\=\kill
\emph{format}\>\emph{produces}\\
\highlighttt{dateThenTime1}\>{\tt Wed 27 Nov 2013 10:16:14 EST}\\
\highlighttt{dateThenTime2}\>{\tt Wed 27 Nov 2013 10:16:14}
\end{tabbing}

\begin{code}
dateThenTime1, dateThenTime2 :: TimeFormat
dateThenTime1 = "%a %d %b %Y %H:%M:%S %Z"
dateThenTime2 = "%a %d %b %Y %H:%M:%S"
\end{code}

\submodule{Formatting times} %%%%%%%%%%%%%%%%%%%%

\highlighttt{formatTime} formats a time using the default
locale.

\begin{code}
formatTime :: F.FormatTime t => TimeFormat -> t -> String
formatTime = F.formatTime F.defaultTimeLocale
\end{code}

\highlighttt{formatUTCTime} returns a formatted  UTC time 
in local time showing the time zone.

\begin{code}
formatUTCTime :: C.UTCTime -> IO String
formatUTCTime utc = do
   zt <- utcToZonedTime utc
   return $ formatTime dateThenTime1 zt
\end{code}

\submodule{Formated current time} %%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{currentTime} returns a the formatted current time 
in local time showing the time zone.

\begin{code}
currentTime :: IO String
currentTime = do
   zt <- getCurrentZonedTime
   return $ formatTime dateThenTime1 zt
\end{code}

\submodule{Formated file modification time} %%%%%%%%%%%%%%%%%%%%%%%%

\highlighttt{fileModTime} returns the formatted modification time of a file.

\begin{code}
fileModTime :: FilePath -> IO String
fileModTime path = do
   legacyT <- D.getModificationTime path
   formatLegacyTime legacyT
\end{code}

\submodule{Overloading for old/new time systems} %%%%%%%%%%%%%%%%%%%%%%%%

Class \highlighttt{LegacyTimes} overloads time operations with 
old and new time types.

\begin{code}
class LegacyTimes t where
\end{code}

\noindent \highlighttt{getCurrentLegacyTime} gets the current time.

\begin{code}
   getCurrentLegacyTime :: IO t
\end{code}

\noindent $t_1$\`{}~\highlighttt{diffSec}\`{}~$t_2$returns $t_1 - t_2$ rounded down
(floor) to whole seconds.

\begin{code}
   diffSec :: t -> t -> Int
\end{code}

\noindent $t_1$\`{}~\highlighttt{diffMin}\`{}~$t_2$returns $t_1 - t_2$ rounded
down to whole minutes.

\begin{code}
   diffMin :: t -> t -> Int
   t1 `diffMin` t2 = (t1 `diffSec` t2) `div` 60
\end{code}

\noindent $t_1$\`{}~\highlighttt{diffHour}\`{}~$t_2$returns $t_1 - t_2$ rounded
down to whole hours.

\begin{code}
   diffHour :: t -> t -> Int
   t1 `diffHour` t2 = (t1 `diffMin` t2) `div` 60
\end{code}

\noindent $t_1$\`{}~\highlighttt{diffDay}\`{}~$t_2$returns $t_1 - t_2$ rounded
down to whole days.

\begin{code}
   diffDay :: t -> t -> Int
   t1 `diffDay` t2 = (t1 `diffHour` t2) `div` 24
\end{code}

\noindent \highlighttt{formatLegacyTime}returns a formatted  legacy time 
in local time showing the time zone.

\begin{code}
   formatLegacyTime :: t -> IO String
\end{code}

\submodule{Instances} %%%%%%%%%%%%%%%%%%%%%%%%

\subsubmodule{LegacyTimes} %%%%%%%%%%

\begin{code}
instance LegacyTimes C.UTCTime where
\end{code}

\begin{code}
   getCurrentLegacyTime = getCurrentUTCTime
\end{code}

\begin{code}
   t1 `diffSec` t2 = floor (t1 `C.diffUTCTime` t2)
\end{code}

\begin{code}
   formatLegacyTime = formatUTCTime
\end{code}

\begin{code}
# if __GLASGOW_HASKELL__ < 705
instance LegacyTimes ST.ClockTime where
\end{code}

\begin{code}
   getCurrentLegacyTime = ST.getClockTime
\end{code}

\begin{code}
   t1 `diffSec` t2 = 
      let diff = t1 `ST.diffClockTimes` t2
      in ((((ST.tdYear diff * 12 + 
             ST.tdMonth diff) * 31 + 
             ST.tdDay diff) * 24 + 
             ST.tdHour diff) * 60 +
             ST.tdMin diff) * 60 + 
             ST.tdSec diff
\end{code}

\begin{code}
   formatLegacyTime clockT = do
      calT <- ST.toCalendarTime clockT
      return $ ST.formatCalendarTime 
         L.defaultTimeLocale dateThenTime1 calT
# else 
# endif
\end{code}

