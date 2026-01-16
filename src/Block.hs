{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Block where
import Other
import Type
import qualified Data.Char as DC
import qualified Data.Foldable as DF
import qualified Data.IntMap.Strict as DIS
import qualified Data.Set as DS
import qualified Data.Text as DT
import qualified Data.Text.Foreign as DTF
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Font as SRF

update_block_font::DIS.IntMap Window->Int->FCT.CInt->DS.Set Char->Combined_widget a->IO (Combined_widget a)
update_block_font window size block_width seq_char (Leaf_widget next_id (Block_font window_id red green blue alpha font))=do
    new_font<-DIS.alterF (update_block_font_a (get_renderer_window window_id window) red green blue alpha block_width seq_char) size font
    return (Leaf_widget next_id (Block_font window_id red green blue alpha new_font))
update_block_font _ _ _ _ _=error "update_block_font: error 1"

update_block_font_a::SRT.Renderer->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->DS.Set Char->Maybe (FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt))->IO (Maybe (FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt)))
update_block_font_a _ _ _ _ _ _ _ Nothing=error "update_block_font_a: error 1"
update_block_font_a renderer red green blue alpha block_width seq_char (Just (font,font_height,intmap_texture))=FMA.alloca $ \font_color->do
    FS.poke font_color (color red green blue alpha)
    new_intmap_texture<-DF.foldlM (flip (update_block_font_b renderer font_color red green blue alpha font block_width)) intmap_texture seq_char
    return (Just (font,font_height,new_intmap_texture))

update_block_font_b::SRT.Renderer->FP.Ptr Color->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FP.Ptr SRF.Font->FCT.CInt->Char->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt)->IO (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt))
update_block_font_b renderer font_color red green blue alpha font block_width char intmap_texture=let char_ord=DC.ord char in case DIS.lookup char_ord intmap_texture of
    Nothing->do
        (texture,width)<-DTF.withCString (DT.singleton char) (to_texture_with_width renderer font_color font)
        let width_mod=mod width block_width
        let block=if width_mod==0 then div width block_width else div width block_width+1
        return (DIS.insert char_ord (texture,DIS.singleton (fromIntegral block_width) (fromIntegral block,if width_mod==0 then 0 else div (block_width-width_mod) 2),width) intmap_texture)
    Just (texture,intmap_int,width)->let this_block_width=fromIntegral block_width in if DIS.member this_block_width intmap_int then update_block_font_b renderer font_color red green blue alpha font block_width char intmap_texture else do
        let width_mod=mod width block_width
        let block=if width_mod==0 then div width block_width else div width block_width+1
        return (DIS.insert char_ord (texture,DIS.insert this_block_width (fromIntegral block,if width_mod==0 then 0 else div (block_width-width_mod) 2) intmap_int,width) intmap_texture)