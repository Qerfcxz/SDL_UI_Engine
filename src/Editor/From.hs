{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Editor.From where
import Other.Error
import qualified Data.Char as DC
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified SDL.Raw.Types as SRT

from_seq_seq_char::DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->Int->FCT.CInt->DS.Seq (DS.Seq Char)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)
from_seq_seq_char _ _ _ DS.Empty seq_seq_char=seq_seq_char
from_seq_seq_char intmap_texture block_number block_width (seq_char DS.:<| other_seq_text) seq_seq_char=from_seq_seq_char intmap_texture block_number block_width other_seq_text (from_seq_seq_char_a intmap_texture 0 0 block_number block_width seq_char DS.empty seq_seq_char)

from_seq_seq_char_a::DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->Int->Int->Int->FCT.CInt->DS.Seq Char->DS.Seq (Char,Int,FCT.CInt)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)
from_seq_seq_char_a _ number char_number _ _ DS.Empty seq_char seq_seq_char=seq_seq_char DS.|> (seq_char,number,char_number,True)
from_seq_seq_char_a intmap_texture number char_number block_number block_width (char DS.:<| other_text) seq_char seq_seq_char=let char_ord=DC.ord char in case error_lookup "from_seq_seq_char_a: error 1" char_ord intmap_texture of
    (_,intmap_int,_,_,_,_,_)->case error_lookup "from_seq_seq_char_a: error 2" (fromIntegral block_width) intmap_int of
        (block,delta_x)->let new_number=number+block in if new_number<=block_number then from_seq_seq_char_a intmap_texture new_number (char_number+1) block_number block_width other_text (seq_char DS.:|> (char,block,delta_x)) seq_seq_char else if block_number<block then error "from_seq_seq_char_a: error 3" else from_seq_seq_char_a intmap_texture block 1 block_number block_width other_text (DS.singleton (char,block,delta_x)) (seq_seq_char DS.|> (seq_char,number,char_number,False))