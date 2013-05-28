import System.Exit
import System.Process
import System.Environment (getArgs,getProgName)
import Control.Monad (filterM)
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies

-- data types for duplicates and commit pairs

showUsage :: IO [String]
showUsage = do
          progname <- getProgName
          putStrLn $ "Usage: " ++ progname ++ " <date> <fork branch> <base branch>"
          putStrLn ""
          putStrLn "Compares the commits from the history of two different git branches,"
          putStrLn "and returns a list of commits which are found only in the base branch,"
          putStrLn "but not in the fork branch.  Commits with the same commit ID are "
          putStrLn "considered equivalent.  Commits with different IDs but the same one-line"
          putStrLn "log message have their diffs compared, and are considered equivalent"
          putStrLn "if the patches differ only by line offsets."
          putStrLn ""
          putStrLn $ "<date>       \t\t-- Compare git commits since this date (YYYY-MM-DD)"
          putStrLn $ "<fork branch>\t\t-- Find commits that are NOT on this branch"
          putStrLn $ "<base branch>\t\t-- From commits that ARE on this branch"
          putStrLn ""
          exitWith ExitSuccess

main :: IO()
main = do
     (date:forkBranch:baseBranch:_) <- (getArgs >>= \x -> if (length x == 3) then return x else showUsage)
     (statusFork, outFork, _) <- readProcessWithExitCode "git"
                  ["log", "--pretty=oneline", "--since", date, forkBranch]
                  ""
     (statusBase, outBase, _) <- readProcessWithExitCode "git"
                  ["log", "--pretty=oneline", "--since", date, baseBranch]
                  ""
     let baseTuples = map commitAndMessageTuple $ lines outBase
     let forkTuples = map commitAndMessageTuple $ lines outFork
     let missingCommits = parFilter commitsNotMatching baseTuples forkTuples
     let missingMessages = parFilter messagesNotMatching missingCommits forkTuples
     let maybeMissing = filter (\x -> not $ x `elem` missingMessages) missingCommits
     let duplicates = [(cfork, cbase, fmsg) | (cfork,fmsg) <- forkTuples, (cbase, bmsg) <- missingCommits,
                       fmsg == bmsg]
     -- Show entries in base that aren't in fork
     putStrLn $ "Testing " ++ (show $ (length baseTuples)*(length forkTuples)) ++ " options."
     putStrLn ""
     putStrLn $ "*** Definitely missing: ***"
     prettyPrintList missingMessages
     putStrLn ""
     putStrLn $ "*** Commit IDs changed: ***"
     mapM_ (\(c1,c2,m) -> putStrLn $ (take 10 c1) ++ ", " ++ (take 10 c2) ++ ", " ++ m) duplicates
     putStrLn ""
     putStrLn $ "*** Probably missing (patches differ): ***"
     prettyPrintDuplicates duplicates
     putStrLn ""

prettyPrintDuplicates :: [(String,String,String)] -> IO ()
prettyPrintDuplicates dupes = do
                      nonmatching <- filterM (\x -> duplicateValid x >>= \valid -> return (not valid)) dupes
                      mapM_ (\dupl@(c1,c2,m) -> (duplicateValid dupl)
                            >>= \r -> putStrLn $ (take 10 c1) ++ ", " ++ (take 10 c2) ++ m) nonmatching

duplicateValid :: (String,String,String) -> IO Bool
duplicateValid dupl = do
               (p1,p2) <- patchesFromDuplicate dupl
               return (patchesMatch p1 p2)

patchesMatch :: String -> String -> Bool
patchesMatch p1 p2 = (scrubbedPatch p1) == (scrubbedPatch p2)

scrubbedPatch :: String -> String
scrubbedPatch patch = patchWithoutIndex . patchWithoutRange $ patch

patchWithoutIndex :: String -> String
patchWithoutIndex patch = unlines (patchWithoutIndexRecurse (lines patch) [] False)
patchWithoutIndexRecurse :: [String] -> [String] -> Bool -> [String]
patchWithoutIndexRecurse [] accum _ = accum
patchWithoutIndexRecurse (l:ls) accum lastWasDiff
                         | lastWasDiff && (take 6 l) == "index " = patchWithoutIndexRecurse ls accum False
                         | (take 5 l) == "diff " = patchWithoutIndexRecurse ls (accum++[l]) True
                         | otherwise = patchWithoutIndexRecurse ls (accum++[l]) False


patchWithoutRange :: String -> String
patchWithoutRange commit = patchWithoutRangeRecurse commit "" 0 False
patchWithoutRangeRecurse :: String -> String -> Int -> Bool -> String
patchWithoutRangeRecurse [] accum _ _ = reverse accum
patchWithoutRangeRecurse (x:xs) accum atCount inPatch
                         | x == '@' && not inPatch =
                           patchWithoutRangeRecurse xs ('@':accum) (atCount+1) (atCount == 1)
                         | x == '@' && inPatch =
                           patchWithoutRangeRecurse xs accum (atCount-1) (atCount /= 1)
                         | inPatch = patchWithoutRangeRecurse xs accum atCount True
                         | otherwise = patchWithoutRangeRecurse xs (x:accum) atCount False


patchesFromDuplicate :: (String,String,String) -> IO (String, String)
patchesFromDuplicate (c1,c2,msg) = do
                     p1 <- patchForCommit c1
                     p2 <- patchForCommit c2
                     return (p1,p2)

patchForCommit :: String -> IO String
patchForCommit commit = do
               (status, out, _) <- readProcessWithExitCode "git" ["diff", commit ++ "~1.." ++ commit] ""
               return out

prettyPrintList :: [(String,String)] -> IO()
prettyPrintList xs = mapM_ (\(c,m) -> putStrLn $ (take 10 c) ++ ", " ++ m) xs

-- run a filter fn in parallel by repeatedly splitting base in half and comparing
-- against all of the fork items.
parFilter fn base fork
            | length base <= 50 = fn base fork
            | otherwise = commits1 `par` (commits2 `pseq` (commits1 ++ commits2))
            where (base1,base2) = splitAt ((length base) `div` 2) base
                  commits1 = parFilter fn base1 fork
                  commits2 = parFilter fn base2 fork

-- Split a line into a tuple containing the git commit and git message
commitAndMessageTuple :: String -> (String, String)
commitAndMessageTuple line = let (w:ws) = words line in (w, unwords ws)

-- Return whether commit string compares to any of the tuples using tupleFn
stringInTuples :: String -> [(String,String)] -> ((String,String) -> String) -> Bool
stringInTuples commit tuples tupleFn = (>0) $ length $ filter (\x -> commit == tupleFn x) tuples

-- git log lines in baseTuples as (commit,msg) tuples, filtered by whether it matches
-- forkTuples using the helper functions.  tupleFn is fst or snd, and cmpFn is == or /=.
linesMatchingRules :: [(String,String)] -> [(String,String)] -> ((String,String) -> String) -> (Bool -> Bool -> Bool) -> [(String,String)]
linesMatchingRules baseTuples forkTuples tupleFn cmpFn =
          filter (\x -> (cmpFn True) $ (stringInTuples (tupleFn x) forkTuples tupleFn)) baseTuples

-- Commits in base that aren't in fork
commitsNotMatching :: [(String,String)] -> [(String,String)] -> [(String,String)]
commitsNotMatching baseTuples forkTuples = linesMatchingRules baseTuples forkTuples fst (/=)

-- Message logs in base that aren't in fork
messagesNotMatching :: [(String,String)] -> [(String,String)] -> [(String,String)]
messagesNotMatching baseTuples forkTuples = linesMatchingRules baseTuples forkTuples snd (/=)
