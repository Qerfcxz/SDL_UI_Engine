{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Editor where
import Other
import Type
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Types as SRT

find_texture_font_equal::DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->Int->Int->DS.Seq Int->(FP.Ptr SRF.Font,FCT.CInt,FCT.CInt,DIS.IntMap (SRT.Texture,Int,FCT.CInt,FCT.CInt))
find_texture_font_equal widget design_window_size window_size start_id size seq_id=case get_widget_widget seq_id start_id widget of
    Leaf_widget _ (Texture_font _ font)->case DIS.lookup (div (size*fromIntegral window_size) (fromIntegral design_window_size)) font of
        Nothing->error "find_font_equal: no such font size"
        Just new_font->new_font
    _->error "find_font_equal: not a font widget"

find_texture_font_near::DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->Int->Int->DS.Seq Int->(FP.Ptr SRF.Font,FCT.CInt,FCT.CInt,DIS.IntMap (SRT.Texture,Int,FCT.CInt,FCT.CInt))
find_texture_font_near widget design_window_size window_size start_id size seq_id=case get_widget_widget seq_id start_id widget of
    Leaf_widget _ (Texture_font _ font)->let new_size=div (size*fromIntegral window_size) (fromIntegral design_window_size) in case DIS.lookupLE new_size font of
        Nothing->case DIS.lookupGE new_size font of
            Nothing->error "find_font_near: empty font widget"
            Just (_,great_font)->great_font
        Just (small_size,small_font)->case DIS.lookupGE new_size font of
            Nothing->small_font
            Just (great_size,great_font)->if 2*new_size<great_size+small_size then small_font else great_font
    _->error "find_font_near: not a font widget"

find_texture_font::Texture_find->(DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->Int->Int->DS.Seq Int->(FP.Ptr SRF.Font,FCT.CInt,FCT.CInt,DIS.IntMap (SRT.Texture,Int,FCT.CInt,FCT.CInt)))
find_texture_font Texture_equal=find_texture_font_equal
find_texture_font Texture_near=find_texture_font_near
