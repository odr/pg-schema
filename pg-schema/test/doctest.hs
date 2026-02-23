-- Run doctest on HListTag when GHC_PACKAGE_PATH is set (e.g. by some CI);
-- otherwise run the same checks as unit tests (see HListTagDoctest).
--
-- Manual doctest from package dir:
--   cabal exec doctest -- ./src/Database/PostgreSQL/HListTag.hs -package pg-schema -XDataKinds
import System.Environment (getEnvironment)
import Test.DocTest (doctest)

import HListTagDoctest

main :: IO ()
main = do
  env <- getEnvironment
  let
    pkgDbs = case lookup "GHC_PACKAGE_PATH" env of
      Just path -> filter (not . null) (splitPath path)
      Nothing   -> []
    splitPath = splitOn ':'
    splitOn c s = case break (== c) s of
      (chunk, "") -> [chunk | not $ null chunk]
      (chunk, _ : rest) -> if null chunk then splitOn c rest else chunk : splitOn c rest
  if not (null pkgDbs)
    then doctest (concatMap (\db -> ["-package-db", db]) pkgDbs ++ ["-package", "pg-schema", "-XDataKinds", "src/Database/PostgreSQL/HListTag.hs"])
    else HListTagDoctest.runTests
