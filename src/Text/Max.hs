{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Text.Max where
import Type
import qualified Data.Sequence as DS
import qualified Foreign.C.Types as FCT

find_max::DS.Seq Row->FCT.CInt->FCT.CInt->Int
find_max DS.Empty _ _=0
find_max (seq_row DS.:|> row) up down=case row of
    Row _ y height->if down<up+height then error "find_max: error 1" else DS.length seq_row-find_max_a seq_row up (y+height-down) 0
    Row_blank y height->if down<up+height then error "find_max: error 2" else DS.length seq_row-find_max_a seq_row up (y+height-down) 0

find_max_a::DS.Seq Row->FCT.CInt->FCT.CInt->Int->Int
find_max_a DS.Empty _ _ number=number
find_max_a (seq_row DS.:|> row) up delta_height number=case row of
    Row _ y _->if y<up+delta_height then number else find_max_a seq_row up delta_height (number+1)
    Row_blank y _->if y<up+delta_height then number else find_max_a seq_row up delta_height (number+1)