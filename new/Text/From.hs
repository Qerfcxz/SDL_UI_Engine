{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Text.From where
import Other.Text
import Type
import qualified Data.ByteString as DB
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Types as SRT

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
by_left renderer find up width last_width delta_height max_ascent min_descent ((text,text_color,seq_id,size) DS.:<| seq_text) seq_texture seq_row=if DT.null text then by_left renderer find up width last_width delta_height max_ascent min_descent seq_text seq_texture seq_row else let font=find size seq_id in FMA.alloca $ \new_color->do
    (left_length,right_length,new_last_width,left_text,right_text)<-cut_text last_width font text
    if DS.null seq_texture
        then if left_length==0 then error "by_left: error 1" else do
            ascent<-SRF.fontAscent font
            descent<-SRF.fontDescent font
            FS.poke new_color text_color
            texture<-DB.useAsCString (DTE.encodeUtf8 left_text) (to_texture renderer new_color font)
            let new_height=ascent-descent
            if right_length==0 then let new_ascent=max ascent max_ascent in let new_descent=min descent min_descent in by_left renderer find up width new_last_width delta_height new_ascent new_descent seq_text (DS.singleton (texture,width-last_width,ascent,last_width-new_last_width,new_height)) seq_row else do
                (new_seq_row,new_texture,new_new_last_width,new_up)<-by_left_a renderer (up+new_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (DS.singleton (texture,0,up,last_width-new_last_width,new_height)) up new_height)
                by_left renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row
        else do
            ascent<-SRF.fontAscent font
            descent<-SRF.fontDescent font
            FS.poke new_color text_color
            let new_height=ascent-descent in if left_length==0
                then let font_height=max_ascent-min_descent in do
                    (new_seq_row,new_texture,new_new_last_width,new_up)<-by_left_a renderer (up+font_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,this_x,up+max_ascent-this_ascent,this_width,this_height)) seq_texture) up font_height)
                    by_left renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row
                else let new_ascent=max ascent max_ascent in let new_descent=min descent min_descent in let font_height=new_ascent-new_descent in do
                    texture<-DB.useAsCString (DTE.encodeUtf8 left_text) (to_texture renderer new_color font)
                    if right_length==0 then by_left renderer find up width new_last_width delta_height new_ascent new_descent seq_text (seq_texture DS.|> (texture,width-last_width,ascent,last_width-new_last_width,new_height)) seq_row else do
                        (new_seq_row,new_texture,new_new_last_width,new_up)<-let base_height=up+new_ascent in by_left_a renderer (up+font_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,this_x,base_height-this_ascent,this_width,this_height)) seq_texture DS.|> (texture,width-last_width,base_height-ascent,last_width-new_last_width,new_height)) up font_height)
                        by_left renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row

by_left_a::SRT.Renderer->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FP.Ptr Color->FP.Ptr SRF.Font->DT.Text->DS.Seq Row->IO (DS.Seq Row,SRT.Texture,FCT.CInt,FCT.CInt)
by_left_a renderer up width this_height delta_height text_color font text seq_row=do
    (left_length,right_length,last_width,left_text,right_text)<-cut_text width font text
    if left_length==0 then error "by_left_a: error 1" else do
        texture<-DB.useAsCString (DTE.encodeUtf8 left_text) (to_texture renderer text_color font)
        if right_length==0 then return (seq_row,texture,last_width,up) else by_left_a renderer (up+this_height+delta_height) width this_height delta_height text_color font right_text (seq_row DS.|> Row (DS.singleton (texture,0,up,width-last_width,this_height)) up this_height)

by_right::SRT.Renderer->(Int->DS.Seq Int->FP.Ptr SRF.Font)->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DT.Text,Color,DS.Seq Int,Int)->DS.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)->DS.Seq Row->IO (DS.Seq Row,FCT.CInt)
by_right _ _ up _ last_width delta_height max_ascent min_descent DS.Empty seq_texture seq_row=return (seq_row DS.|> Row (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,last_width+this_x,up+max_ascent-this_ascent,this_width,this_height)) seq_texture) up (max_ascent-min_descent),up+max_ascent-min_descent+delta_height)
by_right renderer find up width last_width delta_height max_ascent min_descent ((text,text_color,seq_id,size) DS.:<| seq_text) seq_texture seq_row=if DT.null text then by_right renderer find up width last_width delta_height max_ascent min_descent seq_text seq_texture seq_row else let font=find size seq_id in FMA.alloca $ \new_color->do
    (left_length,right_length,new_last_width,left_text,right_text)<-cut_text last_width font text
    if DS.null seq_texture
        then if left_length==0 then error "by_right: error 1" else do
            ascent<-SRF.fontAscent font
            descent<-SRF.fontDescent font
            FS.poke new_color text_color
            texture<-DB.useAsCString (DTE.encodeUtf8 left_text) (to_texture renderer new_color font)
            let new_height=ascent-descent
            if right_length==0 then let new_ascent=max ascent max_ascent in let new_descent=min descent min_descent in by_right renderer find up width new_last_width delta_height new_ascent new_descent seq_text (DS.singleton (texture,width-last_width,ascent,last_width-new_last_width,new_height)) seq_row else do
                (new_seq_row,new_texture,new_new_last_width,new_up)<-by_right_a renderer (up+new_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (DS.singleton (texture,new_last_width,up,last_width-new_last_width,new_height)) up new_height)
                by_right renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row
        else do
            ascent<-SRF.fontAscent font
            descent<-SRF.fontDescent font
            FS.poke new_color text_color
            let new_height=ascent-descent in if left_length==0
                then let font_height=max_ascent-min_descent in do
                    (new_seq_row,new_texture,new_new_last_width,new_up)<-by_right_a renderer (up+font_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,new_last_width+this_x,up+max_ascent-this_ascent,this_width,this_height)) seq_texture) up font_height)
                    by_right renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row
                else let new_ascent=max ascent max_ascent in let new_descent=min descent min_descent in let font_height=new_ascent-new_descent in do
                    texture<-DB.useAsCString (DTE.encodeUtf8 left_text) (to_texture renderer new_color font)
                    if right_length==0 then by_right renderer find up width new_last_width delta_height new_ascent new_descent seq_text (seq_texture DS.|> (texture,width-last_width,ascent,last_width-new_last_width,new_height)) seq_row else do
                        (new_seq_row,new_texture,new_new_last_width,new_up)<-let base_height=up+new_ascent in by_right_a renderer (up+font_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,new_last_width+this_x,base_height-this_ascent,this_width,this_height)) seq_texture DS.|> (texture,new_last_width+width-last_width,base_height-ascent,last_width-new_last_width,new_height)) up font_height)
                        by_right renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row

by_right_a::SRT.Renderer->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FP.Ptr Color->FP.Ptr SRF.Font->DT.Text->DS.Seq Row->IO (DS.Seq Row,SRT.Texture,FCT.CInt,FCT.CInt)
by_right_a renderer up width this_height delta_height text_color font text seq_row=do
    (left_length,right_length,last_width,left_text,right_text)<-cut_text width font text
    if left_length==0 then error "by_right_a: error 1" else do
        texture<-DB.useAsCString (DTE.encodeUtf8 left_text) (to_texture renderer text_color font)
        if right_length==0 then return (seq_row,texture,last_width,up) else by_right_a renderer (up+this_height+delta_height) width this_height delta_height text_color font right_text (seq_row DS.|> Row (DS.singleton (texture,last_width,up,width-last_width,this_height)) up this_height)

by_center::SRT.Renderer->(Int->DS.Seq Int->FP.Ptr SRF.Font)->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DT.Text,Color,DS.Seq Int,Int)->DS.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)->DS.Seq Row->IO (DS.Seq Row,FCT.CInt)
by_center _ _ up _ last_width delta_height max_ascent min_descent DS.Empty seq_texture seq_row=return (seq_row DS.|> Row (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,div last_width 2+this_x,up+max_ascent-this_ascent,this_width,this_height)) seq_texture) up (max_ascent-min_descent),up+max_ascent-min_descent+delta_height)
by_center renderer find up width last_width delta_height max_ascent min_descent ((text,text_color,seq_id,size) DS.:<| seq_text) seq_texture seq_row=if DT.null text then by_center renderer find up width last_width delta_height max_ascent min_descent seq_text seq_texture seq_row else let font=find size seq_id in FMA.alloca $ \new_color->do
    (left_length,right_length,new_last_width,left_text,right_text)<-cut_text last_width font text
    if DS.null seq_texture
        then if left_length==0 then error "by_center: error 1" else do
            ascent<-SRF.fontAscent font
            descent<-SRF.fontDescent font
            FS.poke new_color text_color
            texture<-DB.useAsCString (DTE.encodeUtf8 left_text) (to_texture renderer new_color font)
            let new_height=ascent-descent
            if right_length==0 then let new_ascent=max ascent max_ascent in let new_descent=min descent min_descent in by_center renderer find up width new_last_width delta_height new_ascent new_descent seq_text (DS.singleton (texture,width-last_width,ascent,last_width-new_last_width,new_height)) seq_row else do
                (new_seq_row,new_texture,new_new_last_width,new_up)<-by_center_a renderer (up+new_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (DS.singleton (texture,div new_last_width 2,up,last_width-new_last_width,new_height)) up new_height)
                by_center renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row
        else do
            ascent<-SRF.fontAscent font
            descent<-SRF.fontDescent font
            FS.poke new_color text_color
            let new_height=ascent-descent in if left_length==0
                then let font_height=max_ascent-min_descent in do
                    (new_seq_row,new_texture,new_new_last_width,new_up)<-by_center_a renderer (up+font_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,div new_last_width 2+this_x,up+max_ascent-this_ascent,this_width,this_height)) seq_texture) up font_height)
                    by_center renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row
                else let new_ascent=max ascent max_ascent in let new_descent=min descent min_descent in let font_height=new_ascent-new_descent in do
                    texture<-DB.useAsCString (DTE.encodeUtf8 left_text) (to_texture renderer new_color font)
                    if right_length==0 then by_center renderer find up width new_last_width delta_height new_ascent new_descent seq_text (seq_texture DS.|> (texture,width-last_width,ascent,last_width-new_last_width,new_height)) seq_row else do
                        (new_seq_row,new_texture,new_new_last_width,new_up)<-let base_height=up+new_ascent in by_center_a renderer (up+font_height+delta_height) width new_height delta_height new_color font right_text (seq_row DS.|> Row (fmap (\(this_texture,this_x,this_ascent,this_width,this_height)->(this_texture,div new_last_width 2+this_x,base_height-this_ascent,this_width,this_height)) seq_texture DS.|> (texture,div new_last_width 2+width-last_width,base_height-ascent,last_width-new_last_width,new_height)) up font_height)
                        by_center renderer find new_up width new_new_last_width delta_height ascent descent seq_text (DS.singleton (new_texture,0,ascent,width-new_new_last_width,new_height)) new_seq_row

by_center_a::SRT.Renderer->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FP.Ptr Color->FP.Ptr SRF.Font->DT.Text->DS.Seq Row->IO (DS.Seq Row,SRT.Texture,FCT.CInt,FCT.CInt)
by_center_a renderer up width this_height delta_height text_color font text seq_row=do
    (left_length,right_length,last_width,left_text,right_text)<-cut_text width font text
    if left_length==0 then error "by_center_a: error 1" else do
        texture<-DB.useAsCString (DTE.encodeUtf8 left_text) (to_texture renderer text_color font)
        if right_length==0 then return (seq_row,texture,last_width,up) else by_center_a renderer (up+this_height+delta_height) width this_height delta_height text_color font right_text (seq_row DS.|> Row (DS.singleton (texture,div last_width 2,up,width-last_width,this_height)) up this_height)