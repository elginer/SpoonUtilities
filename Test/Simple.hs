{-
This file is part of Spoon's Utilities.

    Spoon's Utilities is free software: you can 
    redistribute it and/or modify it under the terms of the GNU 
    General Public License as published by the Free Software Foundation, 
    either version 3 of the License, or any later version.

    Spoon's Utilities is distributed in the hope that it 
    will be useful, but WITHOUT ANY WARRANTY; without even the implied 
    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
    See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Spoon's Utilities.  
    If not, see <http://www.gnu.org/licenses/>
-}

-- | Spoon's test utility!  Simple testing for spoons.
module Test.Simple
   (test_all
   ,test
   ,test_all_IO
   ,test_IO)
   where

import Error.Report

import Prelude hiding (catch)

import System.FilePath

import System.Directory

import Control.Exception

-- | Test all source files in a stage of the testing process.
test_all :: (ErrorReport a, Show b)
         => String -- ^ The name of the testing process
         -> (FilePath -> String -> Either a b) -- ^ The test
         -> IO ()
test_all process t =
   test_all_abstract process t test

-- | Test all source files in a stage of the testing process.
test_all_IO :: (ErrorReport a, Show b)
            => String -- ^ The name of the testing process
            -> (FilePath -> String -> IO (Either a b)) -- ^ The test
            -> IO ()
test_all_IO process t =
   test_all_abstract process t test_IO


-- | Test a source file.  
test_IO :: (ErrorReport a, Show b)
        => String -- ^ The name of the testing process
        -> (FilePath -> String -> IO (Either a b)) -- ^ The test
        -> Bool              -- ^ Whether the test is to succeed or to fail.
        -> FilePath -- ^ The file we are to test.  The file must be in the current directory/"name of testing process"
        -> IO () -- ^ Run the test
test_IO process test success fp =
   readFile fp >>= (\src -> do
      putStrLn $ '\n' : replicate 50 '*'
      putStrLn $ "Testing " ++ fp ++ ":"
      catch (test fp src >>= approp success)
            with_err)
   where
   with_err :: SomeException -> IO ()
   with_err e = 
      if success
         then throw e
         else putStrLn $ show e

test_all_abstract :: String -- ^ The name of the testing process
                  -> (FilePath -> String -> a) -- ^ The test
                  -> (String -> (FilePath -> String -> a) -> Bool -> FilePath -> IO ()) -- ^ The tester
                  -> IO () -- ^ Run the test
test_all_abstract process t test =
   catch (do
      success >> failure
      putStrLn "\n******AUTOMATIC TESTING PASSED******") 
         (\e -> do
      print (e :: SomeException)
      error "\n******TESTING FAILED!******")
   where
   test_dir n = joinPath ["tests", process, n]
   success_files :: IO [FilePath]
   success_files = test_files $ test_dir "success"
   failure_files :: IO [FilePath]
   failure_files = test_files $ test_dir "failure"
   success = success_files >>= run_tests True
   failure = failure_files >>= run_tests False
   run_tests b =
      mapM (test process t b)
   test_files :: FilePath -> IO [FilePath]
   test_files dir = do
      putStrLn $ "\nIn directory: " ++ dir
      conts <- getDirectoryContents dir
      putStrLn $ "Found files: " ++ show conts
      let chosen = map (combine dir) $ filter (\f -> not $ head f == '.') conts
      putStrLn $ "Choosing files: " ++ show chosen
      return chosen

-- | Test a source file.  
test :: (ErrorReport a, Show b)
     => String -- ^ The name of the testing process
     -> (FilePath -> String -> Either a b) -- ^ The test
     -> Bool              -- ^ Whether the test is to succeed or to fail.
     -> FilePath -- ^ The file we are to test.  The file must be in the current directory/"name of testing process"
     -> IO () -- ^ Run the test
test process test success fp =
   readFile fp >>= (\src -> do
      putStrLn $ '\n' : replicate 50 '*'
      putStrLn $ "Testing " ++ fp ++ ":"
      approp success $ test fp src)
   
-- | Perform an appropriate action to the test's results
approp :: (ErrorReport a, Show b) => Bool -> Either a b -> IO ()
approp b test_res = do
   catch (without_error test_res) with_error
   where
   with_error :: SomeException -> IO ()
   with_error e =
      if b
         then throw e
         else putStrLn $ show e
   without_error = 
      uncurry either $ 
         if b
            then (error . pretty . report, putStrLn . nl . show)
            else (putStrLn . pretty . report, error . nl . ("ERROR:" ++ ) . nl . show)
   nl = ('\n' :)
