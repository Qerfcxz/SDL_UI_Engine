{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Other.Text where
import Other.Error
import Type
import qualified Control.Monad as CM
import qualified Data.ByteString as DB
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified Foreign.Marshal.Alloc as FMA
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

get_width::FP.Ptr SRF.Font->DT.Text->IO FCT.CInt
get_width font text=FMA.alloca $ \width->FMA.alloca $ \height->DB.useAsCString (DTE.encodeUtf8 text) $ \new_text->do
    catch_error "get_width: error 1" 0 (SRF.sizeUTF8 font new_text width height)
    FS.peek width

cut_text::FCT.CInt->FP.Ptr SRF.Font->DT.Text->IO (Int,Int,FCT.CInt,DT.Text,DT.Text)
cut_text width font text=let text_length=DT.length text in do
    (left_length,last_width,left_text,right_text)<-cut_text_a 0 text_length width width font text
    return (left_length,text_length-left_length,last_width,left_text,right_text)

cut_text_a::Int->Int->FCT.CInt->FCT.CInt->FP.Ptr SRF.Font->DT.Text->IO (Int,FCT.CInt,DT.Text,DT.Text)
cut_text_a left right last_width width font text=if left==right then let (left_text,right_text)=DT.splitAt left text in return (left,last_width,left_text,right_text) else let middle=div (left+right+1) 2 in let left_text=DT.take middle text in do
    new_width<-get_width font left_text
    let new_new_width=width-new_width in if new_new_width==0 then return (middle,0,left_text,DT.drop middle text) else if 0<new_new_width then cut_text_a middle right new_new_width width font text else cut_text_a left (middle-1) last_width width font text

to_texture::SRT.Renderer->FP.Ptr Color->FP.Ptr SRF.Font->FCS.CString->IO SRT.Texture
to_texture renderer text_color font text=do
    surface<-SRF.renderUTF8_Blended font text text_color
    CM.when (surface==FP.nullPtr) $ error "to_texture: error 1"
    texture<-SRV.createTextureFromSurface renderer surface
    SRV.freeSurface surface
    CM.when (texture==FP.nullPtr) $ error "to_texture: error 2"
    return texture

to_texture_with_width::SRT.Renderer->FP.Ptr Color->FP.Ptr SRF.Font->FCS.CString->IO (SRT.Texture,FCT.CInt)
to_texture_with_width renderer text_color font text=do
    surface<-SRF.renderUTF8_Blended font text text_color
    CM.when (surface==FP.nullPtr) $ error "to_texture_with_width: error 1"
    this_surface<-FS.peek surface
    let width=SRT.surfaceW this_surface
    texture<-SRV.createTextureFromSurface renderer surface
    SRV.freeSurface surface
    CM.when (texture==FP.nullPtr) $ error "to_texture_with_width: error 2"
    return (texture,width)