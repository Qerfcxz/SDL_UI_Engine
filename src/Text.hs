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
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

get_width::FP.Ptr SRF.Font->DT.Text->IO FCT.CInt
get_width font text=FMA.alloca $ \width->FMA.alloca $ \height->DTF.withCString text $ \new_text->do
    catch_error "get_width: SDL.Raw.Font.sizeUTF8 returns error" 0 (SRF.sizeUTF8 font new_text width height)
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
to_texture renderer this_color font text=do
    surface<-SRF.renderUTF8_Blended font text this_color
    CM.when (surface==FP.nullPtr) $ error "to_texture: SDL.Raw.Font.renderUTF8_Blended returns error"
    texture<-SRV.createTextureFromSurface renderer surface
    SRV.freeSurface surface
    CM.when (texture==FP.nullPtr) $ error "to_texture: SDL.Raw.Video.createTextureFromSurface returns error"
    return texture

from_paragraph::DIS.IntMap (DIS.IntMap (Combined_widget a))->SRT.Renderer->(DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->Int->Int->DS.Seq Int->FP.Ptr SRF.Font)->Int->Int->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq Paragraph->DS.Seq Row->IO (DS.Seq Row)
from_paragraph _ _ _ _ _ _ _ _ _ _ DS.Empty seq_texture=return seq_texture
from_paragraph widget renderer find window_id start_id design_window_size window_size up width delta_height (seq_paragraph DS.:<| other_seq_paragraph) seq_texture=let new_find=find widget design_window_size window_size start_id in case seq_paragraph of
    Paragraph seq_text typesetting->case typesetting of
        Typesetting_left->do
            (new_seq_texture,new_up)<-by_left renderer new_find up width width delta_height 0 0 seq_text DS.empty seq_texture
            from_paragraph widget renderer find window_id start_id design_window_size window_size new_up width delta_height other_seq_paragraph new_seq_texture
        Typesetting_right->do
            (new_seq_texture,new_up)<-by_right renderer new_find up width width delta_height 0 0 seq_text DS.empty seq_texture
            from_paragraph widget renderer find window_id start_id design_window_size window_size new_up width delta_height other_seq_paragraph new_seq_texture
        Typesetting_center->do
            (new_seq_texture,new_up)<-by_center renderer new_find up width width delta_height 0 0 seq_text DS.empty seq_texture
            from_paragraph widget renderer find window_id start_id design_window_size window_size new_up width delta_height other_seq_paragraph new_seq_texture
    Paragraph_blank seq_id size->do
        let font=new_find size seq_id
        ascent<-SRF.fontAscent font
        descent<-SRF.fontDescent font
        let new_height=ascent-descent
        from_paragraph widget renderer find window_id start_id design_window_size window_size (up+new_height+delta_height) width delta_height other_seq_paragraph (seq_texture DS.|> Row_blank up new_height)

by_left::SRT.Renderer->(Int->DS.Seq Int->FP.Ptr SRF.Font)->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DT.Text,Color,DS.Seq Int,Int)->DS.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)->DS.Seq Row->IO (DS.Seq Row,FCT.CInt)
by_left _ _ up _ _ delta_height max_ascent min_descent DS.Empty seq_texture seq_row=return (seq_row DS.|> Row (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,this_x,up+max_ascent-this_ascent,this_width,this_height)) seq_texture) up (max_ascent-min_descent),up+max_ascent-min_descent+delta_height)
by_left renderer find up width last_width delta_height max_ascent min_descent ((text,this_color,seq_id,size) DS.:<| seq_text) seq_texture seq_row=if DT.null text then by_left renderer find up width last_width delta_height max_ascent min_descent seq_text seq_texture seq_row else let font=find size seq_id in FMA.alloca $ \new_color->do
    (left_length,right_length,new_last_width,left_text,right_text)<-cut_text last_width font text
    if DS.null seq_texture
        then if left_length==0 then error "by_left: too large font" else do
            ascent<-SRF.fontAscent font
            descent<-SRF.fontDescent font
            FS.poke new_color this_color
            texture<-DTF.withCString left_text (to_texture renderer new_color font)
            let new_height=ascent-descent
            if right_length==0 then let new_ascent=max ascent max_ascent in let new_descent=min descent min_descent in by_left renderer find up width new_last_width delta_height new_ascent new_descent seq_text (DS.singleton (texture,width-last_width,ascent,last_width-new_last_width,new_height)) seq_row else do
                (new_seq_row,new_texture,new_new_last_width,new_up)<-by_left_a renderer (up+new_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (DS.singleton (texture,0,up,last_width-new_last_width,new_height)) up new_height)
                by_left renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row
        else do
            ascent<-SRF.fontAscent font
            descent<-SRF.fontDescent font
            FS.poke new_color this_color
            let new_height=ascent-descent in if left_length==0
                then let row_height=max_ascent-min_descent in do
                    (new_seq_row,new_texture,new_new_last_width,new_up)<-by_left_a renderer (up+row_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,this_x,up+max_ascent-this_ascent,this_width,this_height)) seq_texture) up row_height)
                    by_left renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row
                else let new_ascent=max ascent max_ascent in let new_descent=min descent min_descent in let row_height=new_ascent-new_descent in do
                    texture<-DTF.withCString left_text (to_texture renderer new_color font)
                    if right_length==0 then by_left renderer find up width new_last_width delta_height new_ascent new_descent seq_text (seq_texture DS.|> (texture,width-last_width,ascent,last_width-new_last_width,new_height)) seq_row else do
                        (new_seq_row,new_texture,new_new_last_width,new_up)<-let base_height=up+new_ascent in by_left_a renderer (up+row_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,this_x,base_height-this_ascent,this_width,this_height)) seq_texture DS.|> (texture,width-last_width,base_height-ascent,last_width-new_last_width,new_height)) up row_height)
                        by_left renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row

by_left_a::SRT.Renderer->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FP.Ptr Color->FP.Ptr SRF.Font->DT.Text->DS.Seq Row->IO (DS.Seq Row,SRT.Texture,FCT.CInt,FCT.CInt)
by_left_a renderer up width this_height delta_height this_color font text seq_row=do
    (left_length,right_length,last_width,left_text,right_text)<-cut_text width font text
    if left_length==0 then error "by_left_a: too large font" else do
        texture<-DTF.withCString left_text (to_texture renderer this_color font)
        if right_length==0 then return (seq_row,texture,last_width,up) else by_left_a renderer (up+this_height+delta_height) width this_height delta_height this_color font right_text (seq_row DS.|> Row (DS.singleton (texture,0,up,width-last_width,this_height)) up this_height)

by_right::SRT.Renderer->(Int->DS.Seq Int->FP.Ptr SRF.Font)->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DT.Text,Color,DS.Seq Int,Int)->DS.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)->DS.Seq Row->IO (DS.Seq Row,FCT.CInt)
by_right _ _ up _ last_width delta_height max_ascent min_descent DS.Empty seq_texture seq_row=return (seq_row DS.|> Row (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,last_width+this_x,up+max_ascent-this_ascent,this_width,this_height)) seq_texture) up (max_ascent-min_descent),up+max_ascent-min_descent+delta_height)
by_right renderer find up width last_width delta_height max_ascent min_descent ((text,this_color,seq_id,size) DS.:<| seq_text) seq_texture seq_row=if DT.null text then by_right renderer find up width last_width delta_height max_ascent min_descent seq_text seq_texture seq_row else let font=find size seq_id in FMA.alloca $ \new_color->do
    (left_length,right_length,new_last_width,left_text,right_text)<-cut_text last_width font text
    if DS.null seq_texture
        then if left_length==0 then error "by_right: too large font" else do
            ascent<-SRF.fontAscent font
            descent<-SRF.fontDescent font
            FS.poke new_color this_color
            texture<-DTF.withCString left_text (to_texture renderer new_color font)
            let new_height=ascent-descent
            if right_length==0 then let new_ascent=max ascent max_ascent in let new_descent=min descent min_descent in by_right renderer find up width new_last_width delta_height new_ascent new_descent seq_text (DS.singleton (texture,width-last_width,ascent,last_width-new_last_width,new_height)) seq_row else do
                (new_seq_row,new_texture,new_new_last_width,new_up)<-by_right_a renderer (up+new_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (DS.singleton (texture,new_last_width,up,last_width-new_last_width,new_height)) up new_height)
                by_right renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row
        else do
            ascent<-SRF.fontAscent font
            descent<-SRF.fontDescent font
            FS.poke new_color this_color
            let new_height=ascent-descent in if left_length==0
                then let row_height=max_ascent-min_descent in do
                    (new_seq_row,new_texture,new_new_last_width,new_up)<-by_right_a renderer (up+row_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,new_last_width+this_x,up+max_ascent-this_ascent,this_width,this_height)) seq_texture) up row_height)
                    by_right renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row
                else let new_ascent=max ascent max_ascent in let new_descent=min descent min_descent in let row_height=new_ascent-new_descent in do
                    texture<-DTF.withCString left_text (to_texture renderer new_color font)
                    if right_length==0 then by_right renderer find up width new_last_width delta_height new_ascent new_descent seq_text (seq_texture DS.|> (texture,width-last_width,ascent,last_width-new_last_width,new_height)) seq_row else do
                        (new_seq_row,new_texture,new_new_last_width,new_up)<-let base_height=up+new_ascent in by_right_a renderer (up+row_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,new_last_width+this_x,base_height-this_ascent,this_width,this_height)) seq_texture DS.|> (texture,new_last_width+width-last_width,base_height-ascent,last_width-new_last_width,new_height)) up row_height)
                        by_right renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row

by_right_a::SRT.Renderer->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FP.Ptr Color->FP.Ptr SRF.Font->DT.Text->DS.Seq Row->IO (DS.Seq Row,SRT.Texture,FCT.CInt,FCT.CInt)
by_right_a renderer up width this_height delta_height this_color font text seq_row=do
    (left_length,right_length,last_width,left_text,right_text)<-cut_text width font text
    if left_length==0 then error "by_right_a: too large font" else do
        texture<-DTF.withCString left_text (to_texture renderer this_color font)
        if right_length==0 then return (seq_row,texture,last_width,up) else by_right_a renderer (up+this_height+delta_height) width this_height delta_height this_color font right_text (seq_row DS.|> Row (DS.singleton (texture,last_width,up,width-last_width,this_height)) up this_height)

by_center::SRT.Renderer->(Int->DS.Seq Int->FP.Ptr SRF.Font)->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DT.Text,Color,DS.Seq Int,Int)->DS.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)->DS.Seq Row->IO (DS.Seq Row,FCT.CInt)
by_center _ _ up _ last_width delta_height max_ascent min_descent DS.Empty seq_texture seq_row=return (seq_row DS.|> Row (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,div last_width 2+this_x,up+max_ascent-this_ascent,this_width,this_height)) seq_texture) up (max_ascent-min_descent),up+max_ascent-min_descent+delta_height)
by_center renderer find up width last_width delta_height max_ascent min_descent ((text,this_color,seq_id,size) DS.:<| seq_text) seq_texture seq_row=if DT.null text then by_center renderer find up width last_width delta_height max_ascent min_descent seq_text seq_texture seq_row else let font=find size seq_id in FMA.alloca $ \new_color->do
    (left_length,right_length,new_last_width,left_text,right_text)<-cut_text last_width font text
    if DS.null seq_texture
        then if left_length==0 then error "by_center: too large font" else do
            ascent<-SRF.fontAscent font
            descent<-SRF.fontDescent font
            FS.poke new_color this_color
            texture<-DTF.withCString left_text (to_texture renderer new_color font)
            let new_height=ascent-descent
            if right_length==0 then let new_ascent=max ascent max_ascent in let new_descent=min descent min_descent in by_center renderer find up width new_last_width delta_height new_ascent new_descent seq_text (DS.singleton (texture,width-last_width,ascent,last_width-new_last_width,new_height)) seq_row else do
                (new_seq_row,new_texture,new_new_last_width,new_up)<-by_center_a renderer (up+new_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (DS.singleton (texture,div new_last_width 2,up,last_width-new_last_width,new_height)) up new_height)
                by_center renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row
        else do
            ascent<-SRF.fontAscent font
            descent<-SRF.fontDescent font
            FS.poke new_color this_color
            let new_height=ascent-descent in if left_length==0
                then let row_height=max_ascent-min_descent in do
                    (new_seq_row,new_texture,new_new_last_width,new_up)<-by_center_a renderer (up+row_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,div new_last_width 2+this_x,up+max_ascent-this_ascent,this_width,this_height)) seq_texture) up row_height)
                    by_center renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row
                else let new_ascent=max ascent max_ascent in let new_descent=min descent min_descent in let row_height=new_ascent-new_descent in do
                    texture<-DTF.withCString left_text (to_texture renderer new_color font)
                    if right_length==0 then by_center renderer find up width new_last_width delta_height new_ascent new_descent seq_text (seq_texture DS.|> (texture,width-last_width,ascent,last_width-new_last_width,new_height)) seq_row else do
                        (new_seq_row,new_texture,new_new_last_width,new_up)<-let base_height=up+new_ascent in by_center_a renderer (up+row_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,div new_last_width 2+this_x,base_height-this_ascent,this_width,this_height)) seq_texture DS.|> (texture,div new_last_width 2+width-last_width,base_height-ascent,last_width-new_last_width,new_height)) up row_height)
                        by_center renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row

by_center_a::SRT.Renderer->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FP.Ptr Color->FP.Ptr SRF.Font->DT.Text->DS.Seq Row->IO (DS.Seq Row,SRT.Texture,FCT.CInt,FCT.CInt)
by_center_a renderer up width this_height delta_height this_color font text seq_row=do
    (left_length,right_length,last_width,left_text,right_text)<-cut_text width font text
    if left_length==0 then error "by_center_a: too large font" else do
        texture<-DTF.withCString left_text (to_texture renderer this_color font)
        if right_length==0 then return (seq_row,texture,last_width,up) else by_center_a renderer (up+this_height+delta_height) width this_height delta_height this_color font right_text (seq_row DS.|> Row (DS.singleton (texture,div last_width 2,up,width-last_width,this_height)) up this_height)

find_font_equal::DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->Int->Int->DS.Seq Int->FP.Ptr SRF.Font
find_font_equal widget design_window_size window_size start_id size seq_id=case get_widget_widget seq_id start_id widget of
    Leaf_widget _ (Font font)->case DIS.lookup (div (size*fromIntegral window_size) (fromIntegral design_window_size)) font of
        Nothing->error "find_font_equal: no such font size"
        Just new_font->new_font
    _->error "find_font_equal: not a font widget"

find_font_near::DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->Int->Int->DS.Seq Int->FP.Ptr SRF.Font
find_font_near widget design_window_size window_size start_id size seq_id=case get_widget_widget seq_id start_id widget of
    Leaf_widget _ (Font font)->let new_size=div (size*fromIntegral window_size) (fromIntegral design_window_size) in case DIS.lookupLE new_size font of
        Nothing->case DIS.lookupGE new_size font of
            Nothing->error "find_font_near: empty font widget"
            Just (_,great_font)->great_font
        Just (small_size,small_font)->case DIS.lookupGE new_size font of
            Nothing->small_font
            Just (great_size,great_font)->if 2*new_size<great_size+small_size then small_font else great_font
    _->error "find_font_near: not a font widget"

find_font::Find->(DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->Int->Int->DS.Seq Int->FP.Ptr SRF.Font)
find_font Equal=find_font_equal
find_font Near=find_font_near

find_max::DS.Seq Row->FCT.CInt->FCT.CInt->Int
find_max DS.Empty _ _=0
find_max (seq_row DS.:|> row) up down=case row of
    Row _ y height->if down<up+height then error "find_max: too large font" else DS.length seq_row-find_max_a seq_row up (y+height-down) 0
    Row_blank y height->if down<up+height then error "find_max: too large font" else DS.length seq_row-find_max_a seq_row up (y+height-down) 0

find_max_a::DS.Seq Row->FCT.CInt->FCT.CInt->Int->Int
find_max_a DS.Empty _ _ number=number
find_max_a (seq_row DS.:|> row) up delta_height number=case row of
    Row _ y _->if y<up+delta_height then number else find_max_a seq_row up delta_height (number+1)
    Row_blank y _->if y<up+delta_height then number else find_max_a seq_row up delta_height (number+1)

render_seq_texture::FCT.CInt->FCT.CInt->FCT.CInt->SRT.Renderer->DS.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)->IO ()
render_seq_texture _ _ _ _ DS.Empty=return ()
render_seq_texture left up down renderer ((texture,x,y,width,height) DS.:<| seq_texture)=do
    catch_error "render_seq_texture: SDL.Raw.Video.renderCopy returns error" 0 (FMU.with (SRT.Rect (left+x) (up+y) width height) (SRV.renderCopy renderer texture FP.nullPtr))
    render_seq_texture left up down renderer seq_texture

render_seq_row::FCT.CInt->FCT.CInt->FCT.CInt->SRT.Renderer->DS.Seq Row->IO ()
render_seq_row _ _ _ _ DS.Empty=return ()
render_seq_row left up down renderer (row DS.:<| seq_row)=case row of
    Row seq_texture y row_height->if down<up+y+row_height then return () else do
        render_seq_texture left up down renderer seq_texture
        render_seq_row left up down renderer seq_row
    Row_blank y row_height->if down<up+y+row_height then return () else render_seq_row left up down renderer seq_row
