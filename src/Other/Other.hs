{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Other.Other where
import Other.Get
import Type
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified SDL.Raw.Types as SRT

maybe_get::Maybe a->a->a
maybe_get Nothing value=value
maybe_get (Just value) _=value

maybe_set::a->Maybe a->Maybe a
maybe_set value Nothing=Just value
maybe_set _ (Just value)=Just value

adaptive_window::FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->(FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)
adaptive_window design_x design_y x y=let new_x=design_y*x in let new_y=design_x*y in if new_x<new_y then let common=gcd design_x x in (0,div (new_y-new_x) (2*design_x),div design_x common,div x common) else let common=gcd design_y y in (div (new_x-new_y) (2*design_y),0,div design_y common,div y common)

check_render::DS.Seq Int->Engine a->Bool
check_render seq_id engnie=case get_widget seq_id engnie of
    Leaf_widget _ (Text _ _ _ render _ _ _ _ _ _ _ _ _ _ _ _ _ _)->render
    Leaf_widget _ (Editor _ _ _ _ _ _ render _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)->render
    _->error "check_render: error 1"

color::DW.Word8->DW.Word8->DW.Word8->DW.Word8->Color
color=SRT.Color