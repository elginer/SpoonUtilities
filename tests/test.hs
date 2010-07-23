-- Test the tester

import Prelude hiding (catch)

import Test.Simple
import Control.Exception


main = do
   test_all "success_success" test_tester
   fail_test "success_failure"
   fail_test "failure_failure"

fail_test nm =
   catch (test_all nm test_tester) (\ e -> do
      print (e :: SomeException)
      putStrLn "\n******AUTOMATIC TESTING PASSED******")

test_tester fp src =
   case src of
      (x:_) ->
         if x == 'f'
            then Left "Failure!"
            else Right "Success!"
      _     ->
         Left "Empty file!  Testing utility of testing utility is broken!"
