{-

Copyright 2010 John Morrice

This source file is part of Spoon's Utilities and is distributed under the terms of the GNU General Public License

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

{-#
   OPTIONS
   -XTypeSynonymInstances
#-}
-- | Pretty printing for spoons.
module Text.Pretty where

-- | Pretty printer
class Pretty p where
   pretty' :: p     -- ^ To be pretty printed 
           -> Int   -- ^ Tab depth
           -> ShowS -- ^ Shown, waiting for a string to be joined to the end.

instance Pretty String where
   pretty' s i = pspace i . (s ++)

-- | Space before a pretty printed line
pspace :: Int -> ShowS 
pspace i = (('\n' : replicate (i * 3) ' ') ++)

-- | Pretty print!
pretty :: Pretty p => p -> String
pretty p = pretty' p (-1) ""

-- | Pretty print a list, putting two newlines between each element. (Paragraph layout)
pretty_list_nl' :: Pretty p => [p] -> Int -> ShowS
pretty_list_nl' ps i = foldr jn id (map (flip pretty' i) first_pretties) . last_pretty
   where
   (first_pretties, last_pretty) =
      case ps of
         (p:[]) -> ([], pretty' p i)
         (p:rs) -> (take (length ps - 1) ps, pretty' (last ps) i)
         _      -> ([], id)
   jn g h = g . nl . nl . h

-- | Pretty print a list, putting spaces between each element.
pretty_list_sp' :: Pretty p => [p] -> Int -> ShowS
pretty_list_sp' ps i = foldr jn id (map (flip pretty' i) ps)
   where
   jn g h = g . (' ' :) . h



-- | Pretty print a list
pretty_list' :: Pretty p => [p] -> Int -> ShowS
pretty_list' ps i = foldr (.) id (map (flip pretty' i) ps)


nl :: ShowS
nl = ('\n' :)
