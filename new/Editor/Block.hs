{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Editor.Block where
import Other.Get
import Type
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Types as SRT

find_block_font_equal::DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->DS.Seq Int->Int->Int->(Int,Int,FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))
find_block_font_equal widget design_window_size window_size seq_id start_id size=case get_widget_widget seq_id start_id widget of
    Leaf_widget _ (Block_font window_id _ _ _ _ font)->let new_size=div (size*fromIntegral window_size) (fromIntegral design_window_size) in case DIS.lookup new_size font of
        Nothing->error "find_block_font_equal: error 1"
        Just (this_font,font_height,intmap_texture)->(window_id,new_size,this_font,font_height,intmap_texture)
    _->error "find_block_font_equal: error 2"

find_block_font_near::DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->DS.Seq Int->Int->Int->(Int,Int,FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))
find_block_font_near widget design_window_size window_size seq_id start_id size=case get_widget_widget seq_id start_id widget of
    Leaf_widget _ (Block_font window_id _ _ _ _ font)->let new_size=div (size*fromIntegral window_size) (fromIntegral design_window_size) in case DIS.lookupLE new_size font of
        Nothing->case DIS.lookupGE new_size font of
            Nothing->error "find_block_font_near: error 1"
            Just (great_size,(great_font,great_height,great_intmap_texture))->(window_id,great_size,great_font,great_height,great_intmap_texture)
        Just (small_size,(small_font,small_height,small_intmap_texture))->case DIS.lookupGE new_size font of
            Nothing->(window_id,small_size,small_font,small_height,small_intmap_texture)
            Just (great_size,(great_font,great_height,great_intmap_texture))->if 2*new_size<small_size+great_size then (window_id,small_size,small_font,small_height,small_intmap_texture) else (window_id,great_size,great_font,great_height,great_intmap_texture)
    _->error "find_block_font_near: error 2"

find_block_font::Block_find->(DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->DS.Seq Int->Int->Int->(Int,Int,FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)))
find_block_font Block_near=find_block_font_equal
find_block_font Block_equal=find_block_font_near

get_block_font::Block_find->Int->DIS.IntMap (FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))->(Int,FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))
get_block_font Block_equal size font=case DIS.lookup size font of
    Nothing->error "get_block_font: error 1"
    Just (this_font,font_height,intmap_texture)->(size,this_font,font_height,intmap_texture)
get_block_font Block_near size font=case DIS.lookupLE size font of
    Nothing->case DIS.lookupGE size font of
        Nothing->error "get_block_font: error 2"
        Just (great_size,(great_font,great_height,great_intmap_texture))->(great_size,great_font,great_height,great_intmap_texture)
    Just (small_size,(small_font,small_height,small_intmap_texture))->case DIS.lookupGE size font of
        Nothing->(small_size,small_font,small_height,small_intmap_texture)
        Just (great_size,(great_font,great_height,great_intmap_texture))->if 2*size<small_size+great_size then (small_size,small_font,small_height,small_intmap_texture) else (great_size,great_font,great_height,great_intmap_texture)