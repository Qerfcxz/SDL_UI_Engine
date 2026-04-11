{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Other.Other where
import Type
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Enum as SRE
import qualified SDL.Raw.Types as SRT

maybe_set::a->Maybe a->Maybe a
maybe_set value Nothing=Just value
maybe_set _ (Just value)=Just value

adaptive_window::FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->(FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)
adaptive_window design_x design_y x y=let new_x=design_y*x in let new_y=design_x*y in if new_x<new_y then let common=gcd design_x x in (0,div (new_y-new_x) (2*design_x),div design_x common,div x common) else let common=gcd design_y y in (div (new_x-new_y) (2*design_y),0,div design_y common,div y common)

from_flip::Flip->SRE.RendererFlip
from_flip Flip_none=SRE.SDL_FLIP_NONE
from_flip Flip_horizontal=SRE.SDL_FLIP_HORIZONTAL
from_flip Flip_vertical=SRE.SDL_FLIP_VERTICAL

double_poke_element::(FS.Storable a,FS.Storable b)=>(a,b)->FP.Ptr a->FP.Ptr b->Int->IO Int
double_poke_element (first_element,second_element) first_ptr second_ptr index=do
    FS.pokeElemOff first_ptr index first_element
    FS.pokeElemOff second_ptr index second_element
    return (index+1)

double_poke_element_with_transform::(FS.Storable c,FS.Storable d)=>(a,b)->(a->c)->(b->d)->FP.Ptr c->FP.Ptr d->Int->IO Int
double_poke_element_with_transform (first_element,second_element) first_transform second_transform first_ptr second_ptr index=do
    FS.pokeElemOff first_ptr index (first_transform first_element)
    FS.pokeElemOff second_ptr index (second_transform second_element)
    return (index+1)

simple_request::Raw_request a->Request a
simple_request request=Request request DS.empty

color::DW.Word8->DW.Word8->DW.Word8->DW.Word8->Color
color=SRT.Color