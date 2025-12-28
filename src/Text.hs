{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Text where
import Other
import Type
import qualified Control.Monad as CM
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Text.Foreign as DTF
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

get_width::FP.Ptr SRF.Font->DT.Text->IO FCT.CInt
get_width font text=FMA.alloca $ \width->FMA.alloca $ \height->DTF.withCString text $ \new_text->do
    catch_error "get_width: SDL.Raw.Font.sizeText returns error" 0 (SRF.sizeUTF8 font new_text width height)
    FS.peek width

cut_text::FCT.CInt->FP.Ptr SRF.Font->DT.Text->IO (FCT.CInt,DT.Text,DT.Text)
cut_text width font text=do
    (left,last_width,left_text,right_text)<-cut_text_a 0 (DT.length text) width width font text
    if left==0 then error "cut_text: too large font or empty text" else return (last_width,left_text,right_text)

cut_text_a::Int->Int->FCT.CInt->FCT.CInt->FP.Ptr SRF.Font->DT.Text->IO (Int,FCT.CInt,DT.Text,DT.Text)
cut_text_a left right last_width width font text=if left==right then let (left_text,right_text)=DT.splitAt left text in return (left,last_width,left_text,right_text) else let middle=div (left+right+1) 2 in let left_text=DT.take middle text in do
    new_width<-get_width font left_text
    let new_new_width=width-new_width in if new_new_width==0 then return (middle,0,left_text,DT.drop middle text) else if 0<new_new_width then cut_text_a middle right new_new_width width font text else cut_text_a left (middle-1) last_width width font text

to_texture::SRT.Renderer->FP.Ptr SRT.Color->FP.Ptr SRF.Font->FCS.CString->IO SRT.Texture
to_texture renderer color font text=do
    surface<-SRF.renderUTF8_Blended font text color
    CM.when (surface==FP.nullPtr) $ error "to_texture: SDL.Raw.Font.renderUTF8_Blended returns error"
    texture<-SRV.createTextureFromSurface renderer surface
    SRV.freeSurface surface
    CM.when (texture==FP.nullPtr) $ error "to_texture: SDL.Raw.Video.createTextureFromSurface returns error"
    return texture

from_paragraph::DIS.IntMap (DIS.IntMap (Combined_widget a))->SRT.Renderer->(DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->Int->Int->DS.Seq Int->FP.Ptr SRF.Font)->Int->Int->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq Paragraph->DS.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)->IO (DS.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt))
from_paragraph _ _ _ _ _ _ _ _ _ _ _ DS.Empty seq_texture=return seq_texture
from_paragraph widget renderer find window_id start_id design_window_size window_size left up width delta_height (seq_paragraph DS.:<| other_seq_paragraph) seq_texture=let new_find=find widget design_window_size window_size start_id in case seq_paragraph of
    Blank seq_int size->do
        let font=new_find size seq_int
        ascent<-SRF.fontAscent font
        descent<-SRF.fontDescent font
        from_paragraph widget renderer find window_id start_id design_window_size window_size left (up+ascent-descent+delta_height) width delta_height other_seq_paragraph seq_texture
    Paragraph_left seq_text->do
        (new_seq_texture,new_up)<-by_left renderer new_find left up width width delta_height 0 0 seq_text DS.Empty seq_texture
        from_paragraph widget renderer find window_id start_id design_window_size window_size left new_up width delta_height other_seq_paragraph new_seq_texture
    _->error "unfinished"


by_left::SRT.Renderer->(Int->DS.Seq Int->FP.Ptr SRF.Font)->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DT.Text,SRT.Color,DS.Seq Int,Int)->DS.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)->DS.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)->IO (DS.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt),FCT.CInt)
by_left _ _ _ up _ _ delta_height max_ascent min_descent DS.Empty this_seq_texture seq_texture=let base_up=up+max_ascent in return (seq_texture DS.>< fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,this_x,base_up-this_ascent,this_width,this_height)) this_seq_texture,base_up-min_descent+delta_height)
by_left renderer find left up width last_width delta_height max_ascent min_descent ((text,color,seq_id,size) DS.:<| seq_text) this_seq_texture seq_texture=let font=find size seq_id in FMA.alloca $ \this_color->do
    ascent<-SRF.fontAscent font
    descent<-SRF.fontDescent font
    (new_last_width,left_text,right_text)<-cut_text last_width font text
    FS.poke this_color color
    texture<-DTF.withCString left_text (to_texture renderer this_color font)
    let new_ascent=max max_ascent ascent
    let new_descent=min min_descent descent
    let new_height=ascent-descent
    if DT.null right_text then by_left renderer find left up width new_last_width delta_height new_ascent new_descent seq_text (this_seq_texture DS.|> (texture,left+width-last_width,ascent,last_width-new_last_width,new_height)) seq_texture else do
        (new_seq_texture,new_texture,new_new_last_width,new_up)<-let base_up=up+new_ascent in by_left_b renderer left (base_up-new_descent+delta_height) width new_height delta_height this_color font right_text (seq_texture DS.>< (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,this_x,base_up-this_ascent,this_width,this_height)) this_seq_texture DS.|> (texture,left+width-last_width,base_up-ascent,last_width-new_last_width,new_height)))
        by_left renderer find left new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,left,ascent,width-new_new_last_width,new_height)) new_seq_texture



by_left_b::SRT.Renderer->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FP.Ptr SRT.Color->FP.Ptr SRF.Font->DT.Text->DS.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)->IO (DS.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt),SRT.Texture,FCT.CInt,FCT.CInt)
by_left_b renderer left up width this_height delta_height color font text seq_texture=do
    (last_width,left_text,right_text)<-cut_text width font text
    texture<-DTF.withCString left_text (to_texture renderer color font)
    if DT.null right_text then return (seq_texture,texture,last_width,up) else by_left_b renderer left (up+this_height+delta_height) width this_height delta_height color font right_text (seq_texture DS.|> (texture,left,up,width-last_width,this_height))


find_font_equal::DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->Int->Int->DS.Seq Int->FP.Ptr SRF.Font
find_font_equal widget window_size design_window_size start_id size seq_id=case get_combined_widget start_id seq_id widget of
    Leaf_widget _ (Font font)->case DIS.lookup (div (size*fromIntegral window_size) (fromIntegral design_window_size)) font of
        Nothing->error "find_font_equal: no such font size"
        Just new_font->new_font
    _->error "find_font_equal: it's not a font widget"

find_font_near::DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->Int->Int->DS.Seq Int->FP.Ptr SRF.Font
find_font_near widget window_size design_window_size start_id size seq_id=case get_combined_widget start_id seq_id widget of
    Leaf_widget _ (Font font)->let new_size=div (size*fromIntegral window_size) (fromIntegral design_window_size) in case DIS.lookupLE new_size font of
        Nothing->case DIS.lookupGE new_size font of
            Nothing->error "find_font_near: empty font widget"
            Just (_,great_font)->great_font
        Just (small_size,small_font)->case DIS.lookupGE new_size font of
            Nothing->small_font
            Just (great_size,great_font)->if 2*new_size<great_size+small_size then small_font else great_font
    _->error "find_font_equal: it's not a font widget"