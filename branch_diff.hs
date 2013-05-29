import System.Exit
import System.Process
import System.Environment (getArgs,getProgName)
import Control.Monad (filterM)
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies

-- data types for duplicates and commit pairs
type GitDuplicate = (String,String,String)
type GitCommitEntry = (String,String)
type GitCommitID = String
type GitCommitMsg = String
type GitPatch = String

-- Print command line usage
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
     (statusBase, outBase, _) <- readProcessWithExitCode "git"
                  ["log", "--pretty=oneline", "--since", date, baseBranch]
                  ""
     (statusFork, outFork, _) <- readProcessWithExitCode "git"
                  ["log", "--pretty=oneline", "--since", date, forkBranch]
                  ""
     -- Get (commit ID, log msg) tuples for each branch
     let baseTuples = map commitAndMessageTuple $ lines outBase
     let forkTuples = map commitAndMessageTuple $ lines outFork
     -- Find tuples with no matching git commit IDs in fork branch
     let unmatchedCommitTuples = parFilter commitsNotMatching baseTuples forkTuples
     -- Of those, find tuples with no matching log message in fork branch
     let missingTuples = parFilter messagesNotMatching unmatchedCommitTuples forkTuples
     -- Find tuples with mismatched commit ID, but matching log msg
     let logMsgDuplicates = [(cbase, cfork, fmsg)
                          | (cfork,fmsg) <- forkTuples, (cbase, bmsg) <- unmatchedCommitTuples,
                          fmsg == bmsg]
     -- Of those, find tuples with mismatched diff patches
     unmatchedDiffTuples <- filterM (\x -> duplicateValid x >>= \valid -> return (not valid)) logMsgDuplicates

     putStrLn $ "Testing " ++ (show $ (length baseTuples)*(length forkTuples)) ++ " options."
     putStrLn ""
     putStrLn $ "*** Definitely missing: ***"
     prettyPrintList missingTuples
     putStrLn ""
     putStrLn $ "*** Commit IDs changed: ***"
     prettyPrintDuplicates logMsgDuplicates
     putStrLn ""
     putStrLn $ "*** Probably missing (patches differ): ***"
     prettyPrintDuplicates unmatchedDiffTuples
     putStrLn ""


-- Return true of patchsets from both commits in a duplicate tuple match
duplicateValid :: GitDuplicate -> IO Bool
duplicateValid dupl = do
               (p1,p2) <- patchesFromDuplicate dupl
               return (patchesMatch p1 p2)

-- Return true if both given patchsets, when cleaned up, match
patchesMatch :: GitPatch -> GitPatch -> Bool
patchesMatch p1 p2 = (scrubbedPatch p1) == (scrubbedPatch p2)

-- Remove index line and line range sections from given patch
scrubbedPatch :: GitPatch -> GitPatch
scrubbedPatch patch = patchWithoutIndex . patchWithoutRange $ patch

-- Strip 'index' line from patch
patchWithoutIndex :: GitPatch -> GitPatch
patchWithoutIndex patch = unlines (patchWithoutIndexRecurse (lines patch) [] False)
patchWithoutIndexRecurse :: [String] -> [String] -> Bool -> [String]
patchWithoutIndexRecurse [] accum _ = accum
patchWithoutIndexRecurse (l:ls) accum lastWasDiff
                         | lastWasDiff && (take 6 l) == "index " = patchWithoutIndexRecurse ls accum False
                         | (take 5 l) == "diff " = patchWithoutIndexRecurse ls (accum++[l]) True
                         | otherwise = patchWithoutIndexRecurse ls (accum++[l]) False

-- String line offset 'range' from patch
patchWithoutRange :: GitPatch -> GitPatch
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

-- Given a duplicate tuple, return tuple containing full text of each commit's patch
patchesFromDuplicate :: GitDuplicate -> IO (GitPatch, GitPatch)
patchesFromDuplicate (c1,c2,msg) = do
                     p1 <- patchForCommit c1
                     p2 <- patchForCommit c2
                     return (p1,p2)

-- Return the 'git diff' patch for a given commit ID
patchForCommit :: GitCommitID -> IO GitPatch
patchForCommit commit = do
               (status, out, _) <- readProcessWithExitCode "git" ["diff", commit ++ "~1.." ++ commit] ""
               return out

-- Print a (commit ID, msg) tuple
prettyPrintList :: [GitCommitEntry] -> IO()
prettyPrintList xs = mapM_ (\(c,m) -> putStrLn $ (take 10 c) ++ ", " ++ m) xs

-- Print a (commit ID, commit ID, msg) tuple
prettyPrintDuplicates :: [GitDuplicate] -> IO ()
prettyPrintDuplicates xs = mapM_ (\(c1,c2,m) ->
                      putStrLn $ (take 10 c1) ++ ", " ++ (take 10 c2) ++ m) xs

-- run a filter fn in parallel by repeatedly splitting base in half and comparing
-- against all of the fork items. Stop splitting when list is below some size.
parFilter :: ([GitCommitEntry] -> [GitCommitEntry] -> [GitCommitEntry]) -> [GitCommitEntry] -> [GitCommitEntry] -> [GitCommitEntry]
parFilter fn base fork
            | length base <= 50 = fn base fork
            | otherwise = commits1 `par` (commits2 `pseq` (commits1 ++ commits2))
            where (base1,base2) = splitAt ((length base) `div` 2) base
                  commits1 = parFilter fn base1 fork
                  commits2 = parFilter fn base2 fork

-- Return whether commit string compares to any of the tuples using tupleFn
stringInTuples :: GitCommitID -> [GitCommitEntry] -> (GitCommitEntry -> String) -> Bool
stringInTuples commit tuples tupleFn = (>0) $ length $ filter (\x -> commit == tupleFn x) tuples

-- git log lines in baseTuples as (commit,msg) tuples, filtered by whether it matches
-- forkTuples using the helper functions.  tupleFn is fst or snd, and cmpFn is == or /=.
linesMatchingRules :: [GitCommitEntry] -> [GitCommitEntry] -> (GitCommitEntry -> String) -> (Bool -> Bool -> Bool) -> [GitCommitEntry]
linesMatchingRules baseTuples forkTuples tupleFn cmpFn =
          filter (\x -> (cmpFn True) $ (stringInTuples (tupleFn x) forkTuples tupleFn)) baseTuples

-- Commits in base that aren't in fork
commitsNotMatching :: [GitCommitEntry] -> [GitCommitEntry] -> [GitCommitEntry]
commitsNotMatching baseTuples forkTuples = linesMatchingRules baseTuples forkTuples fst (/=)

-- Message logs in base that aren't in fork
messagesNotMatching :: [GitCommitEntry] -> [GitCommitEntry] -> [GitCommitEntry]
messagesNotMatching baseTuples forkTuples = linesMatchingRules baseTuples forkTuples snd (/=)

-- Split a line into a tuple containing the git commit and git message
commitAndMessageTuple :: String -> GitCommitEntry
commitAndMessageTuple line = let (w:ws) = words line in (w, unwords ws)
