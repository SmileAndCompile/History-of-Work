\module{Zim}

{\tt Zim} is the main module for the project, which provides the main function and is responsible for integrating the various modules of the project with the user input data.
The goals of the Zim module are to:
\begin{enumerate}
	\item Define and detect the command line options that will be used when comparing the collection of files (such as the programming language to be used).
	\item Compare a collection of files provided as command line arguments and return a summary of the results as an HTML report.
\end{enumerate}

\begin{code}
module Main (main) where
\end{code}

\begin{code}
import System.Environment
import System.Exit
import System.IO
import Control.Monad
import Data.Tuple
import Data.Typeable
import Data.List as L
import Data.Char
import Data.Map.Strict as M
import Data.Maybe
import Data.Set as S
\end{code}

\begin{code}
import ABR.Util.Pos
import ABR.Util.Args as A
import ABR.Control.Check
import ABR.Parser
import ABR.Parser.Checks
import ABR.Text.Configs as C
import ABR.Text.String
import ABR.Text.Markup
\end{code}

\begin{code}
import Zimm.Lexer
import Zimm.Token
import Zimm.Matching
\end{code}

\noindent {\tt Source} is a type alias for a String, representing the source code for a file.

\begin{code}
type Source = String
\end{code}

\noindent {\tt readLex path} reads the file given by the {\tt path}, lexs it, then returns the (path, source, tlps) -- where tlps are a list of tagged lexemes and their respective positions within the lexed file, with the tag being used to identify the different types of lexemes.

\begin{code}
readLex :: FilePath -> IO (FilePath, Source, TLPs)
readLex path = do
   putStrLn $ "File: " ++ path
   source <- readFile path
   case checkLex lexerL source of
      CheckPass tlps -> return (path, source, tlps)
      CheckFail msg  -> do hPutStr stderr msg
                           exitWith $ ExitFailure $ -1
\end{code}

\noindent {\tt snippet startPos endPos source} returns the lines of 
{\tt source} between the {\tt startPos} and {\tt endPos} (inclusive).

\begin{code}
snippet :: Pos -> Pos -> Source -> Source
snippet (l1, _) (l2, _) source = unlines $
   L.take (l2 - l1 + 1) $
   L.drop l1 $
   lines source
\end{code}

\noindent {\tt snippetTokens p1 p2 tlps} returns the lines of {\tt tlps} between {\tt p1} and {\tt p2} (inclusive).

\begin{code}
snippetTokens :: Pos -> Pos -> TLPs -> Source
snippetTokens p1 p2 tlps = f (fst (snd (head tlps))) $
   L.takeWhile (\((_, _), p) -> p <= p2) $
   L.dropWhile (\((_, _), p) -> p < p1) tlps
   where 
   f :: Int -> TLPs -> Source
   f line tlps = case tlps of
      [] -> ""
      ((_, lexeme), (l, c)) : tlps' 
         | l == line -> ' ' : makeHTMLSafe lexeme ++ f line tlps'
         | otherwise -> '\n' : makeHTMLSafe lexeme ++ f l tlps'
\end{code}

\noindent {\tt findCrossTotals runs} returns the total length of the matching 
runs found between each pair of compared files where at least 1 matching run occured.

\begin{code}
findCrossTotals :: [(FilePath, FilePath, [Run])] -> [(FilePath, FilePath, Int)]
findCrossTotals runs = L.sortBy (\(_,_,tc1) (_,_,tc2) -> compare tc2 tc1) 
                       $ L.map (\(f1, f2, rs) -> (f1, f2, sum 
                       $ L.map (len) rs)) runs 
\end{code}


\noindent {\tt findClusters sets} takes a list of Sets and returns a list of the
merged Sets, where a pair of Sets are merged whenever their intersection is not the empty list (i.e they share atleast one element) -- where the function will continue to process the sets until no further merges can be achieved.

\begin{code}
findClusters :: Ord a => [Set a] -> [Set a]
findClusters sets | L.length sets < 2 = sets
                  | otherwise         = findClusters' (head sets) (tail sets)
    where checkSets :: Ord a => Set a -> Set a -> Bool
          checkSets set1 set2 = S.null (S.intersection set1 set2)
          findClusters' :: Ord a => Set a -> [Set a] -> [Set a]
          findClusters' set' sets' | L.null sets' = [set']
                                   | otherwise    =
            let (p1, p2) = L.partition (checkSets set') sets'
            in case p2 of
                [] -> [set'] ++ findClusters' (head p1) (tail p1)
                _  -> if (L.null p1) 
                      then [S.unions ([set'] ++ p2)]
                      else findClusters' (S.unions ([set'] ++ p2)) p1
\end{code}

\begin{code}
main :: IO ()
main = do
   -- check command line arguments. If any invalid command line options are
   -- provided, exit the program displaying the invalid options
   args <- getArgs
   let (options, fileNames) = findOpts
          [ParamS "lang", ParamS "run", FlagS "list", FlagS "desc"] args
       bads = L.filter (\(c:_) -> c `elem` "+-") fileNames
       lang = L.map toLower $ trim $
          (A.lookupParam "lang" options "JavaScript")
   unless (L.null bads) (do
      putStrLn $ "bad options: " ++ show bads
      exitWith $ ExitFailure $ -1
     )
   unless (lang == "JavaScript") (do
      putStrLn $ "language type not supported: " ++ show lang
      exitWith $ ExitFailure $ -1
     )
   let minRunLen :: Int
       minRunLen = read $ A.lookupParam "run" options "20"
       listSources = A.lookupFlag "list" options False
       listOrder = A.lookupFlag "desc" options True
   -- read in the files that were provided and lex them, and the template
   pathSourceTlpss <- mapM readLex fileNames
   template <- readFile "template.html"
   let
      indexPathSourceTlpss = L.zip [1..] pathSourceTlpss
      sourceMap :: M.Map FilePath Source
      sourceMap = M.fromList $ L.map (\(path, source, _) -> (path, source)) 
         pathSourceTlpss
      tlpsMap :: M.Map FilePath TLPs
      tlpsMap = M.fromList $ L.map (\(path, _, tlps) -> (path, tlps)) 
         pathSourceTlpss
      filePathMap :: M.Map FilePath Int
      filePathMap = M.fromList $ L.zip (L.map (\(path,_,_)
            -> path) pathSourceTlpss) [1..]
      keyMap = makeMapsKey $ keywords ++ separators ++ operators
      tlpss = L.map (\(_, _, tlps) -> tlps) pathSourceTlpss
      keyLitMap = makeMapsLiteral keyMap tlpss 
      pathTokenss = [(path, tokens) | path <- fileNames,
                     let tlps = fromJust $ M.lookup path tlpsMap,
                     let keyLitIdMap = makeMapsId keyLitMap tlps,
                     let tokens = tokenizeFile keyLitIdMap tlps]
      allRuns :: [(FilePath, FilePath, [Run])] 
      allRuns = L.concat [[(path1, path2, runs) 
                       | (path2, tokens2) <- pts, 
                          let runs = filterRuns $ findRuns minRunLen tokens1 tokens2,
                          not (L.null runs)] 
                       | (path1, tokens1) : pts <- L.tails pathTokenss]
      matches = if listOrder 
                then L.sortBy (\(_,_,r1) (_,_,r2) -> compare (len r2) (len r1))
                     $ [(path1, path2, run) 
                       | (path1, path2, runs) <- allRuns, run <- runs] 
                else [(path1, path2, run) 
                       | (path1, path2, runs) <- allRuns, run <- runs]
      crossTotals = findCrossTotals allRuns
      indexMatches = L.zip [1..] matches
      clusters = L.map (S.toList) $ findClusters (L.map (\(p1,p2,_) 
                 -> S.fromList [(filePathMap ! p1), (filePathMap ! p2)]) allRuns)
      listFlag = if listSources then [CParam "listFlag" "true"] else []
      reportData = listFlag ++ [
         CList "files"
            [[CParam "number" (show n), 
              CParam "path" path, 
              CParam "tokensNum" (show (L.length tlps)),
              CParam "fullSource" (makeHTMLSafe source)]
            | (n, (path, source, tlps)) <- indexPathSourceTlpss],
         CList "matches" 
            [[CParam "file1" (show (filePathMap ! path1)), 
              CParam "file2" (show (filePathMap ! path2)),
              CParam "pos1" (show (startPos1 run) ++ " - " ++ show (endPos1 run)), 
              CParam "pos2" (show (startPos2 run) ++ " - " ++ show (endPos2 run)), 
              CParam "matchingTokens" (show (len run)), 
              CParam "matchNum" (show n),
              CParam "source1" (makeHTMLSafe (snippet (startPos1 run) 
                     (endPos1 run) (sourceMap ! path1))), 
              CParam "source2" (makeHTMLSafe (snippet (startPos2 run) 
                     (endPos2 run) (sourceMap ! path2))),
              CParam "tokens1" (snippetTokens (startPos1 run) (endPos1 run) 
                     (tlpsMap ! path1)), 
              CParam "tokens2" (snippetTokens (startPos2 run) (endPos2 run) 
                     (tlpsMap ! path2))]
            | (n, (path1, path2, run)) <- indexMatches],
        CList "crossTotals"
           [[CParam "file1" (show (filePathMap ! path1)),
             CParam "file2" (show (filePathMap ! path2)),
             CParam "tokens" (show tokens)]
            | (path1,path2,tokens) <- crossTotals],
        CList "clusters"
           [[CParam "cluster" (show cluster)]
            | cluster <- clusters]
        ]
      report = popTemplate reportData template
   writeFile "report.html" report
\end{code}

