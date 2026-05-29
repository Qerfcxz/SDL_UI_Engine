{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Text.Font where
import Other.Error
import Other.Get
import Type
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified GHC.Stack as GS
import qualified SDL.Raw.Font as SRF

find_font::GS.HasCallStack=>Find->DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->Int->Int->DS.Seq Int->FP.Ptr SRF.Font
find_font Equal widget design_window_size window_size start_id size seq_id=case get_widget_widget seq_id start_id widget of
    Leaf_widget _ (Font font)->error_lookup "find_font: error 1" (div (size*fromIntegral window_size) (fromIntegral design_window_size)) font
    _->error "find_font: error 2"
find_font Near widget design_window_size window_size start_id size seq_id=case get_widget_widget seq_id start_id widget of
    Leaf_widget _ (Font font)->let new_size=div (size*fromIntegral window_size) (fromIntegral design_window_size) in case DIS.lookupLE new_size font of
        Nothing->case error_lookup_great "find_font: error 3" new_size font of
            (_,great_font)->great_font
        Just (small_size,small_font)->case DIS.lookupGE new_size font of
            Nothing->small_font
            Just (great_size,great_font)->if 2*new_size<great_size+small_size then small_font else great_font
    _->error "find_font: error 4"