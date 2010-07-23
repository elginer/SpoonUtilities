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

{-# OPTIONS
    -XExistentialQuantification
    -XTypeSynonymInstances
#-}
-- | Nice looking errors.
module Error.Report
   (module Text.Pretty
   ,Error (..)
   ,ErrorReport (..)
   ,new_error
   ,empty_error
   ,error_line
   ,error_lines
   ,error_section
   ,error_join
   ,broken)
   where

import Text.Pretty

import Data.List

import Text.Parsec

-- | A nice looking error
data Error =
     -- | Error text
     forall p . Pretty p => PrettyError p
   | ErrorMessage [Error]
   | NoRaise [Error]

instance Pretty Error where
   pretty' ce i =
      case ce of
         PrettyError p -> pretty' p i
         ErrorMessage ces ->
            foldr (.) id $ (map (flip pretty' (i + 1))) ces
         NoRaise ces ->
            foldr (.) id $ (map (flip pretty' i)) ces

-- | A class for things which can be turned into error reports
class ErrorReport err where
   report :: err -> Error

-- Strings can be turned to errors
instance ErrorReport String where
   report = new_error 

instance ErrorReport ParseError where
   report pe = error_lines (lines $ show pe) empty_error

-- | Combine multiple errors into one report
error_join :: [Error] -> Error
error_join = NoRaise . intersperse (new_error $ "\n" ++ replicate 50 '-' ++ "\n" )

-- | An empty error
empty_error :: Error
empty_error = ErrorMessage []

-- | Create a new error
new_error :: Pretty p => p -> Error
new_error p = PrettyError p

-- | Add a number of lines to the start of the error
error_lines :: Pretty p => [p] -> Error -> Error
error_lines ps ce = 
   foldr error_line ce ps

-- | Add a line to the start of the error (This appears further up the error report)
error_line :: Pretty p => p -> Error -> Error
error_line p ce =
   case ce of
      PrettyError _ -> NoRaise [PrettyError p, ce]
      ErrorMessage ers -> ErrorMessage $ PrettyError p : ers
      NoRaise ers -> NoRaise $ PrettyError p : ers

-- | Create a new error section (This appears further up, and with fewer tabs than the rest of the report)
error_section :: Error -> Error
error_section ce =
   ErrorMessage [ErrorMessage [ce]]

-- | There was an error because the software was broken
broken :: String -- ^ Your email address
       -> String -- ^ Your name
       -> String -- ^ The name of the software
       -> [String] -- ^ An error message spread over multiple lines
       -> Error -- ^ A composable error to join to.
       -> a     -- ^ Bottom _|_
broken email name soft msg =
   error . pretty . error_lines ["BUG ALERT!"
                                , soft ++ " has broken because of a programming error."
                                ,"Report the following as a bug to " ++ name ++ " at "] 
                  . error_section . error_lines msg
