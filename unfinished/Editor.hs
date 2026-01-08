{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Editor where
import Other
import Text
import Type
import qualified Control.Monad as CM
import qualified Data.Foldable as DF
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Text.Foreign as DTF
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

typesetting_adjust::Typesetting->FCT.CInt->FCT.CInt
typesetting_adjust Typesetting_left _=0
typesetting_adjust Typesetting_right x=x
typesetting_adjust Typesetting_center x=div x 2

editor_text::SRT.Renderer->Int->Int->Int->Int->Int->Int->Int->DS.Seq Int->FP.Ptr SRF.Font->Find->Typesetting->FP.Ptr Color->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq DT.Text->IO (Single_widget a)
editor_text renderer window_id origin_paragraph_id origin_row_id paragraph_id row_id row font_size path font find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha x y design_size size delta_height extra_width extra_height left right up down text=do
        ascent<-SRF.fontAscent font
        descent<-SRF.fontDescent font
        (new_paragraph_id,new_row_id,intmap_text,seq_seq_texture,seq_map)<-editor_text_a renderer paragraph_id row_id font typesetting text_color (div ((right-left)*size) design_size) text DIS.empty DIS.empty DS.empty
        let font_height=ascent-descent in let new_delta_height=div (delta_height*size) design_size in return (Editor window_id origin_paragraph_id origin_row_id new_paragraph_id new_row_id row (fromIntegral (div (div ((down+delta_height-up)*size) design_size)  (font_height+new_delta_height))) (DS.length seq_map) font_size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height delta_height extra_width extra_height left right up down new_delta_height (div (extra_width*size) design_size) (div (extra_height*size) design_size) (x+div (left*size) design_size) (x+div (right*size) design_size) (y+div (up*size) design_size) (y+div (down*size) design_size) Cursor_none intmap_text seq_seq_texture seq_map)

editor_text_a::SRT.Renderer->Int->Int->FP.Ptr SRF.Font->Typesetting->FP.Ptr Color->FCT.CInt->DS.Seq DT.Text->DIS.IntMap DT.Text->DIS.IntMap (DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int))->DS.Seq (Int,Int)->IO (Int,Int,DIS.IntMap DT.Text,DIS.IntMap (DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int)),DS.Seq (Int,Int))
editor_text_a _ paragraph_id row_id _ _ _ _ DS.Empty intmap_text intmap_intmap_texture seq_map=return (paragraph_id,row_id,intmap_text,intmap_intmap_texture,seq_map)
editor_text_a renderer paragraph_id row_id font typesetting text_color width (text DS.:<| seq_text) intmap_text intmap_intmap_texture seq_map=do
    (new_row_id,intmap_texture,new_seq_map)<-editor_text_b True renderer row_id paragraph_id 0 font typesetting text_color width text DIS.empty seq_map
    editor_text_a renderer (paragraph_id+1) new_row_id font typesetting text_color width seq_text (DIS.insert paragraph_id text intmap_text) (DIS.insert paragraph_id intmap_texture intmap_intmap_texture) new_seq_map

editor_text_b::Bool->SRT.Renderer->Int->Int->Int->FP.Ptr SRF.Font->Typesetting->FP.Ptr Color->FCT.CInt->DT.Text->DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int)->DS.Seq (Int,Int)->IO (Int,DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int),DS.Seq (Int,Int))
editor_text_b bool renderer row_id paragraph_id text_number font typesetting text_color width text intmap_texture seq_map=if bool&&DT.null text then return (row_id+1,DIS.singleton row_id (FP.nullPtr,typesetting_adjust typesetting width,0,0,0),seq_map DS.|> (paragraph_id,row_id)) else do
    (left_length,right_length,last_width,left_text,right_text)<-cut_text width font text
    if left_length==0 then error "editor_text_b: too large font" else do
        texture<-DTF.withCString left_text (to_texture renderer text_color font)
        if right_length==0 then return (row_id+1,DIS.insert row_id (texture,typesetting_adjust typesetting last_width,width-last_width,left_length,text_number) intmap_texture,seq_map DS.|> (paragraph_id,row_id)) else editor_text_b False renderer (row_id+1) paragraph_id (text_number+left_length) font typesetting text_color width right_text (DIS.insert row_id (texture,typesetting_adjust typesetting last_width,width-last_width,left_length,text_number) intmap_texture) (seq_map DS.|> (paragraph_id,row_id))

update_paragraph::SRT.Renderer->Either Int Int->Int->Int->Int->Int->Int->Int->Int->FP.Ptr SRF.Font->Typesetting->FP.Ptr SRT.Color->FCT.CInt->DT.Text->DT.Text->DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int)->DS.Seq (Int,Int)->IO (Int,Int,DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int),DS.Seq (Int,Int))
update_paragraph renderer cursor row first_number origin_paragraph_id origin_row_id paragraph_id row_id this_row_id font typesetting this_color width this_text text intmap_texture seq_map=case DS.lookup (row+1) seq_map of
    Nothing->if DT.null this_text
        then if DS.length seq_map==1 then return (0,origin_row_id,DIS.singleton origin_row_id (FP.nullPtr,typesetting_adjust typesetting width,0,0,0),DS.singleton (origin_paragraph_id,origin_row_id)) else do
            new_intmap_texture<-delete_io "update_paragraph: you changed something without proper design" this_row_id (\(texture,_,_,_,_)->CM.when (texture/=FP.nullPtr) (SRV.destroyTexture texture)) intmap_texture
            return (update_paragraph_a cursor row,row_id,new_intmap_texture,DS.deleteAt row seq_map)
        else do
            (left_length,right_length,last_width,left_text,right_text)<-cut_text width font this_text
            texture<-DTF.withCString left_text (to_texture renderer this_color font)
            new_intmap_texture<-alter_io "update_paragraph: you changed something without proper design" this_row_id (texture,typesetting_adjust typesetting last_width,width-last_width,left_length,first_number) (\(this_texture,_,_,_,_)->CM.unless (this_texture==FP.nullPtr) (SRV.destroyTexture this_texture)) intmap_texture
            if right_length==0 then return (update_paragraph_a cursor row,row_id,new_intmap_texture,seq_map) else let new_first_number=first_number+left_length in update_paragraph_b renderer (update_paragraph_c row new_first_number cursor) (row+1) new_first_number paragraph_id row_id font typesetting this_color width right_text new_intmap_texture seq_map
    Just (new_paragraph_id,new_row_id)->if paragraph_id==new_paragraph_id
        then case DIS.lookup new_row_id intmap_texture of
            Nothing->error "update_paragraph: you changed something without proper design"
            Just (_,_,_,text_number,this_first_number)->let (left_text,right_text)=DT.splitAt text_number text in do
                (left_length,right_length,last_width,new_left_text,new_right_text)<-cut_text width font (DT.append this_text left_text)
                if right_length==text_number
                    then do
                        texture<-DTF.withCString new_left_text (to_texture renderer this_color font)
                        new_intmap_texture<-alter_io "update_paragraph: you changed something without proper design" this_row_id (texture,typesetting_adjust typesetting last_width,width-last_width,left_length,first_number) (\(this_texture,_,_,_,_)->CM.unless (this_texture==FP.nullPtr) (SRV.destroyTexture this_texture)) intmap_texture
                        return (update_paragraph_a cursor row,row_id,let number=first_number+left_length-this_first_number in if number==0 then new_intmap_texture else let seq_row_id=snd <$> DS.takeWhileL (\(this_paragraph_id,_)->paragraph_id==this_paragraph_id) (DS.drop (row+1) seq_map) in DF.foldl' (flip (DIS.alter (error_update_a "update_paragraph: you changed something without proper design" (\(this_texture,x,this_width,this_text_number,this_this_first_number)->(this_texture,x,this_width,this_text_number,this_this_first_number+number))))) new_intmap_texture seq_row_id,seq_map)
                    else if right_length==0
                        then if DT.null right_text
                            then do
                                (new_intmap_texture,new_seq_map)<-update_paragraph_d (row+1) paragraph_id intmap_texture seq_map
                                texture<-DTF.withCString new_left_text (to_texture renderer this_color font)
                                new_new_intmap_texture<-alter_io "update_paragraph: you changed something without proper design" this_row_id (texture,typesetting_adjust typesetting last_width,width-last_width,left_length,first_number) (\(this_texture,_,_,_,_)->CM.unless (this_texture==FP.nullPtr) (SRV.destroyTexture this_texture)) new_intmap_texture
                                return (update_paragraph_a cursor row,row_id,new_new_intmap_texture,new_seq_map)
                            else let new_first_number=first_number+left_length in update_paragraph renderer (update_paragraph_c row new_first_number cursor) (row+1) new_first_number origin_paragraph_id origin_row_id paragraph_id row_id new_row_id font typesetting this_color width DT.empty right_text intmap_texture seq_map
                        else if left_length==0 then error "update_paragraph: too large font" else do
                            texture<-DTF.withCString new_left_text (to_texture renderer this_color font)
                            new_intmap_texture<-alter_io "update_paragraph: you changed something without proper design" this_row_id (texture,typesetting_adjust typesetting last_width,width-last_width,left_length,first_number) (\(this_texture,_,_,_,_)->CM.unless (this_texture==FP.nullPtr) (SRV.destroyTexture this_texture)) intmap_texture
                            let new_first_number=first_number+left_length in update_paragraph renderer (update_paragraph_c row new_first_number cursor) (row+1) new_first_number origin_paragraph_id origin_row_id paragraph_id row_id new_row_id font typesetting this_color width new_right_text right_text new_intmap_texture seq_map
        else if DT.null this_text
            then do
                new_intmap_texture<-delete_io "update_paragraph: you changed something without proper design" this_row_id (\(texture,_,_,_,_)->CM.when (texture/=FP.nullPtr) (SRV.destroyTexture texture)) intmap_texture
                return (update_paragraph_a cursor row,row_id,new_intmap_texture,DS.deleteAt row seq_map)
            else do
                (left_length,right_length,last_width,left_text,right_text)<-cut_text width font this_text
                texture<-DTF.withCString left_text (to_texture renderer this_color font)
                new_intmap_texture<-alter_io "update_paragraph: you changed something without proper design" this_row_id (texture,typesetting_adjust typesetting last_width,width-last_width,left_length,first_number) (\(this_texture,_,_,_,_)->CM.unless (this_texture==FP.nullPtr) (SRV.destroyTexture this_texture)) intmap_texture
                if right_length==0 then return (update_paragraph_a cursor row,row_id,new_intmap_texture,seq_map) else let new_first_number=first_number+left_length in update_paragraph_b renderer (update_paragraph_c row new_first_number cursor) (row+1) new_first_number paragraph_id row_id font typesetting this_color width right_text new_intmap_texture seq_map

update_paragraph_a::Either Int Int->Int->Int
update_paragraph_a (Left _) number=number
update_paragraph_a (Right number) _=number

update_paragraph_b::SRT.Renderer->Either Int Int->Int->Int->Int->Int->FP.Ptr SRF.Font->Typesetting->FP.Ptr SRT.Color->FCT.CInt->DT.Text->DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int)->DS.Seq (Int,Int)->IO (Int,Int,DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int),DS.Seq (Int,Int))
update_paragraph_b renderer cursor row first_number paragraph_id row_id font typesetting this_color width text intmap_texture seq_map=if DT.null text then return (update_paragraph_a cursor row,row_id,intmap_texture,seq_map) else do
    (left_length,right_length,last_width,left_text,right_text)<-cut_text width font text
    texture<-DTF.withCString left_text (to_texture renderer this_color font)
    if right_length==0 then return (update_paragraph_a cursor row,row_id+1,DIS.insert row_id (texture,typesetting_adjust typesetting last_width,width-last_width,left_length,first_number) intmap_texture,DS.insertAt row (paragraph_id,row_id) seq_map) else let new_first_number=first_number+left_length in update_paragraph_b renderer (update_paragraph_c row new_first_number cursor) (row+1) new_first_number paragraph_id (row_id+1) font typesetting this_color width right_text (DIS.insert row_id (texture,typesetting_adjust typesetting last_width,width-last_width,left_length,first_number) intmap_texture) (DS.insertAt row (paragraph_id,row_id) seq_map)

update_paragraph_c::Int->Int->Either Int Int->Either Int Int
update_paragraph_c row first_number (Left number)=if number<first_number then Right row else Left number
update_paragraph_c _ _ cursor=cursor

update_paragraph_d::Int->Int->DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int)->DS.Seq (Int,Int)->IO (DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int),DS.Seq (Int,Int))
update_paragraph_d row paragraph_id intmap_texture seq_map=case DS.lookup row seq_map of
    Nothing->return (intmap_texture,seq_map)
    Just (this_paragraph_id,row_id)->if paragraph_id==this_paragraph_id
        then do
            new_intmap_texture<-delete_io "update_paragraph_d: you changed something without proper design" row_id (\(texture,_,_,_,_)->CM.when (texture/=FP.nullPtr) (SRV.destroyTexture texture)) intmap_texture
            update_paragraph_d row paragraph_id new_intmap_texture (DS.deleteAt row seq_map)
        else return (intmap_texture,seq_map)

left_cursor::FP.Ptr SRF.Font->DIS.IntMap DT.Text->DIS.IntMap (DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int))->DS.Seq (Int,Int)->Cursor->IO (Cursor,Bool)
left_cursor _ _ _ _ Cursor_none=return (Cursor_none,False)
left_cursor font intmap_text intmap_intmap_texture seq_map cursor@(Cursor_single cursor_number_row cursor_paragraph_id cursor_row_id cursor_text_number _ _)=case DIS.lookup cursor_paragraph_id intmap_intmap_texture of
    Nothing->error "left_cursor: you changed something without proper design"
    Just intmap_texture->case DIS.lookup cursor_row_id intmap_texture of
        Nothing->error "left_cursor: you changed something without proper design"
        Just (_,x,_,_,first_number)->if first_number<cursor_text_number
            then case DIS.lookup cursor_paragraph_id intmap_text of
                Nothing->error "left_cursor: you changed something without proper design"
                Just text->do
                    width<-get_width font (DT.take (cursor_text_number-1-first_number) (DT.drop first_number text))
                    return (Cursor_single cursor_number_row cursor_paragraph_id cursor_row_id (cursor_text_number-1) (x+width) (x+width),True)
            else let new_cursor_number_row=cursor_number_row-1 in case DS.lookup new_cursor_number_row seq_map of
                Nothing->return (cursor,False)
                Just (paragraph_id,row_id)->case DIS.lookup paragraph_id intmap_intmap_texture of
                    Nothing->error "left_cursor: you changed something without proper design"
                    Just new_intmap_texture->case DIS.lookup row_id new_intmap_texture of
                        Nothing->error "left_cursor: you changed something without proper design"
                        Just (_,new_x,width,new_text_number,new_first_number)->let new_new_x=new_x+width in return (Cursor_single new_cursor_number_row paragraph_id row_id (new_text_number+new_first_number) new_new_x new_new_x,True)
left_cursor _ _ _ _ (Cursor_double _ cursor_number_row cursor_paragraph_id cursor_row_id cursor_text_number _ _ _ _ x_render x_click _ _)=return (Cursor_single cursor_number_row cursor_paragraph_id cursor_row_id cursor_text_number x_render x_click,True)

right_cursor::FP.Ptr SRF.Font->DIS.IntMap DT.Text->DIS.IntMap (DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int))->DS.Seq (Int,Int)->Cursor->IO (Cursor,Bool)
right_cursor _ _ _ _ Cursor_none=return (Cursor_none,False)
right_cursor font intmap_text intmap_intmap_texture seq_map cursor@(Cursor_single cursor_number_row cursor_paragraph_id cursor_row_id cursor_text_number _ _)=case DIS.lookup cursor_paragraph_id intmap_intmap_texture of
    Nothing->error "right_cursor: you changed something without proper design"
    Just intmap_texture->case DIS.lookup cursor_row_id intmap_texture of
        Nothing->error "right_cursor: you changed something without proper design"
        Just (_,x,_,text_number,first_number)->if cursor_text_number<first_number+text_number
            then case DIS.lookup cursor_paragraph_id intmap_text of
                Nothing->error "right_cursor: you changed something without proper design"
                Just text->do
                    width<-get_width font (DT.take (cursor_text_number+1-first_number) (DT.drop first_number text))
                    return (Cursor_single cursor_number_row cursor_paragraph_id cursor_row_id (cursor_text_number+1) (x+width) (x+width),True)
            else let new_cursor_number_row=cursor_number_row+1 in case DS.lookup new_cursor_number_row seq_map of
                Nothing->return (cursor,False)
                Just (paragraph_id,row_id)->case DIS.lookup paragraph_id intmap_intmap_texture of
                    Nothing->error "right_cursor: you changed something without proper design"
                    Just new_intmap_texture->case DIS.lookup row_id new_intmap_texture of
                        Nothing->error "right_cursor: you changed something without proper design"
                        Just (_,new_x,_,_,new_first_number)->return (Cursor_single new_cursor_number_row paragraph_id row_id new_first_number new_x new_x,True)
right_cursor _ _ _ _ (Cursor_double _ _ _ _ _ cursor_number_row cursor_paragraph_id cursor_row_id cursor_text_number _ _ x_render x_click)=return (Cursor_single cursor_number_row cursor_paragraph_id cursor_row_id cursor_text_number x_render x_click,True)

up_cursor::FP.Ptr SRF.Font->DIS.IntMap DT.Text->DIS.IntMap (DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int))->DS.Seq (Int,Int)->Cursor->IO (Cursor,Bool)
up_cursor _ _ _ _ Cursor_none=return (Cursor_none,False)
up_cursor font intmap_text intmap_intmap_texture seq_map cursor@(Cursor_single cursor_number_row cursor_paragraph_id cursor_row_id cursor_text_number _ cursor_x_click)=case DS.lookup (cursor_number_row-1) seq_map of
    Nothing->case DIS.lookup cursor_paragraph_id intmap_intmap_texture of
        Nothing->error "up_cursor: you changed something without proper design"
        Just intmap_texture->case DIS.lookup cursor_row_id intmap_texture of
            Nothing->error "up_cursor: you changed something without proper design"
            Just (_,x,_,_,first_number)->if cursor_text_number==first_number then return (cursor,False) else return (Cursor_single cursor_number_row cursor_paragraph_id cursor_row_id first_number x x,True)
    Just (paragraph_id,row_id)->case DIS.lookup paragraph_id intmap_intmap_texture of
        Nothing->error "up_cursor: you changed something without proper design"
        Just intmap_texture->case DIS.lookup row_id intmap_texture of
            Nothing->error "up_cursor: you changed something without proper design"
            Just (_,x,_,text_number,first_number)->case DIS.lookup paragraph_id intmap_text of
                Nothing->error"up_cursor: you changed something without proper design"
                Just text->do
                    (left_length,right_length,last_width,_,right_text)<-cut_text (cursor_x_click-x) font (DT.take text_number (DT.drop first_number text))
                    if right_length==0 then return (Cursor_single (cursor_number_row-1) paragraph_id row_id (first_number+left_length) (cursor_x_click-last_width) cursor_x_click,True) else do
                        new_width<-get_width font (DT.take 1 right_text)
                        if div new_width 2<last_width then return (Cursor_single (cursor_number_row-1) paragraph_id row_id (first_number+left_length+1) (cursor_x_click+new_width-last_width) cursor_x_click,True) else return (Cursor_single (cursor_number_row-1) paragraph_id row_id (first_number+left_length) (cursor_x_click-last_width) cursor_x_click,True)
up_cursor font intmap_text intmap_intmap_texture seq_map (Cursor_double bool cursor_number_row_start cursor_paragraph_id_start cursor_row_id_start cursor_text_number_start cursor_number_row_end cursor_paragraph_id_end cursor_row_id_end cursor_text_number_end cursor_x_render_start cursor_x_click_start cursor_x_render_end cursor_x_click_end)=let (cursor_number_row,cursor_paragraph_id,cursor_row_id,_,_,cursor_x_click)=if bool then (cursor_number_row_start,cursor_paragraph_id_start,cursor_row_id_start,cursor_text_number_start,cursor_x_render_start,cursor_x_click_start) else (cursor_number_row_end,cursor_paragraph_id_end,cursor_row_id_end,cursor_text_number_end,cursor_x_render_end,cursor_x_click_end) in case DS.lookup (cursor_number_row-1) seq_map of
    Nothing->case DIS.lookup cursor_paragraph_id intmap_intmap_texture of
        Nothing->error "up_cursor: you changed something without proper design"
        Just intmap_texture->case DIS.lookup cursor_row_id intmap_texture of
            Nothing->error "up_cursor: you changed something without proper design"
            Just (_,x,_,_,first_number)->return (Cursor_single cursor_number_row cursor_paragraph_id cursor_row_id first_number x x,True)
    Just (paragraph_id,row_id)->case DIS.lookup paragraph_id intmap_intmap_texture of
        Nothing->error "up_cursor: you changed something without proper design"
        Just intmap_texture->case DIS.lookup row_id intmap_texture of
            Nothing->error "up_cursor: you changed something without proper design"
            Just (_,x,_,text_number,first_number)->case DIS.lookup paragraph_id intmap_text of
                Nothing->error"up_cursor: you changed something without proper design"
                Just text->do
                    (left_length,right_length,last_width,_,right_text)<-cut_text (cursor_x_click-x) font (DT.take text_number (DT.drop first_number text))
                    if right_length==0 then return (Cursor_single (cursor_number_row-1) paragraph_id row_id (first_number+left_length) (cursor_x_click-last_width) cursor_x_click,True) else do
                        new_width<-get_width font (DT.take 1 right_text)
                        if div new_width 2<last_width then return (Cursor_single (cursor_number_row-1) paragraph_id row_id (first_number+left_length+1) (cursor_x_click+new_width-last_width) cursor_x_click,True) else return (Cursor_single (cursor_number_row-1) paragraph_id row_id (first_number+left_length) (cursor_x_click-last_width) cursor_x_click,True)

down_cursor::FP.Ptr SRF.Font->DIS.IntMap DT.Text->DIS.IntMap (DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int))->DS.Seq (Int,Int)->Cursor->IO (Cursor,Bool)
down_cursor _ _ _ _ Cursor_none=return (Cursor_none,False)
down_cursor font intmap_text intmap_intmap_texture seq_map cursor@(Cursor_single cursor_number_row cursor_paragraph_id cursor_row_id cursor_text_number _ cursor_x_click)=case DS.lookup (cursor_number_row+1) seq_map of
    Nothing->case DIS.lookup cursor_paragraph_id intmap_intmap_texture of
        Nothing->error "down_cursor: you changed something without proper design"
        Just intmap_texture->case DIS.lookup cursor_row_id intmap_texture of
            Nothing->error "down_cursor: you changed something without proper design"
            Just (_,x,width,text_number,first_number)->if cursor_text_number==first_number+text_number then return (cursor,False) else return (Cursor_single cursor_number_row cursor_paragraph_id cursor_row_id (first_number+text_number) (x+width) (x+width),True)
    Just (paragraph_id,row_id)->case DIS.lookup paragraph_id intmap_intmap_texture of
        Nothing->error "down_cursor: you changed something without proper design"
        Just intmap_texture->case DIS.lookup row_id intmap_texture of
            Nothing->error "down_cursor: you changed something without proper design"
            Just (_,x,_,text_number,first_number)->case DIS.lookup paragraph_id intmap_text of
                Nothing->error"down_cursor: you changed something without proper design"
                Just text->do
                    (left_length,right_length,last_width,_,right_text)<-cut_text (cursor_x_click-x) font (DT.take text_number (DT.drop first_number text))
                    if right_length==0 then return (Cursor_single (cursor_number_row+1) paragraph_id row_id (first_number+left_length) (cursor_x_click-last_width) cursor_x_click,True) else do
                        new_width<-get_width font (DT.take 1 right_text)
                        if div new_width 2<last_width then return (Cursor_single (cursor_number_row+1) paragraph_id row_id (first_number+left_length+1) (cursor_x_click+new_width-last_width) cursor_x_click,True) else return (Cursor_single (cursor_number_row+1) paragraph_id row_id (first_number+left_length) (cursor_x_click-last_width) cursor_x_click,True)
down_cursor font intmap_text intmap_intmap_texture seq_map (Cursor_double bool cursor_number_row_start cursor_paragraph_id_start cursor_row_id_start cursor_text_number_start cursor_number_row_end cursor_paragraph_id_end cursor_row_id_end cursor_text_number_end cursor_x_render_start cursor_x_click_start cursor_x_render_end cursor_x_click_end)=let (cursor_number_row,cursor_paragraph_id,cursor_row_id,_,_,cursor_x_click)=if bool then (cursor_number_row_start,cursor_paragraph_id_start,cursor_row_id_start,cursor_text_number_start,cursor_x_render_start,cursor_x_click_start) else (cursor_number_row_end,cursor_paragraph_id_end,cursor_row_id_end,cursor_text_number_end,cursor_x_render_end,cursor_x_click_end) in case DS.lookup (cursor_number_row+1) seq_map of
    Nothing->case DIS.lookup cursor_paragraph_id intmap_intmap_texture of
        Nothing->error "down_cursor: you changed something without proper design"
        Just intmap_texture->case DIS.lookup cursor_row_id intmap_texture of
            Nothing->error "down_cursor: you changed something without proper design"
            Just (_,x,width,text_number,first_number)->return (Cursor_single cursor_number_row cursor_paragraph_id cursor_row_id (first_number+text_number) (x+width) (x+width),True)
    Just (paragraph_id,row_id)->case DIS.lookup paragraph_id intmap_intmap_texture of
        Nothing->error "down_cursor: you changed something without proper design"
        Just intmap_texture->case DIS.lookup row_id intmap_texture of
            Nothing->error "down_cursor: you changed something without proper design"
            Just (_,x,_,text_number,first_number)->case DIS.lookup paragraph_id intmap_text of
                Nothing->error"down_cursor: you changed something without proper design"
                Just text->do
                    (left_length,right_length,last_width,_,right_text)<-cut_text (cursor_x_click-x) font (DT.take text_number (DT.drop first_number text))
                    if right_length==0 then return (Cursor_single (cursor_number_row+1) paragraph_id row_id (first_number+left_length) (cursor_x_click-last_width) cursor_x_click,True) else do
                        new_width<-get_width font (DT.take 1 right_text)
                        if div new_width 2<last_width then return (Cursor_single (cursor_number_row+1) paragraph_id row_id (first_number+left_length+1) (cursor_x_click+new_width-last_width) cursor_x_click,True) else return (Cursor_single (cursor_number_row+1) paragraph_id row_id (first_number+left_length) (cursor_x_click-last_width) cursor_x_click,True)

min_cursor::Cursor->DIS.IntMap (DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int))->DS.Seq (Int,Int)->(Cursor,Bool)
min_cursor Cursor_none _ _=(Cursor_none,False)
min_cursor cursor@(Cursor_single cursor_number_row _ _ cursor_text_number _ _) intmap_intmap_texture seq_map=if cursor_number_row==0&&cursor_text_number==0 then (cursor,False) else case seq_map of
    DS.Empty->error "min_cursor: you changed something without proper design"
    (paragraph_id,row_id) DS.:<| _->case DIS.lookup paragraph_id intmap_intmap_texture of
        Nothing->error"min_cursor: you changed something without proper design"
        Just intmap_texture->case DIS.lookup row_id intmap_texture of
            Nothing->error "min_cursor: you changed something without proper design"
            Just (_,x,_,_,_)->(Cursor_single 0 paragraph_id row_id 0 x x,True)
min_cursor (Cursor_double {}) intmap_intmap_texture seq_map=case seq_map of
    DS.Empty->error "min_cursor: you changed something without proper design"
    (paragraph_id,row_id) DS.:<| _->case DIS.lookup paragraph_id intmap_intmap_texture of
        Nothing->error"min_cursor: you changed something without proper design"
        Just intmap_texture->case DIS.lookup row_id intmap_texture of
            Nothing->error "min_cursor: you changed something without proper design"
            Just (_,x,_,_,_)->(Cursor_single 0 paragraph_id row_id 0 x x,True)

max_cursor::Cursor->DIS.IntMap (DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int))->DS.Seq (Int,Int)->(Cursor,Bool)
max_cursor Cursor_none _ _=(Cursor_none,False)
max_cursor cursor@(Cursor_single cursor_number_row _ _ cursor_text_number _ _) intmap_intmap_texture seq_map=case seq_map of
    DS.Empty->error "max_cursor: you changed something without proper design"
    other_seq_map DS.:|> (paragraph_id,row_id) ->case DIS.lookup paragraph_id intmap_intmap_texture of
        Nothing->error"max_cursor: you changed something without proper design"
        Just intmap_texture->case DIS.lookup row_id intmap_texture of
            Nothing->error "max_cursor: you changed something without proper design"
            Just (_,x,width,text_number,first_number)->let new_row=DS.length other_seq_map in let new_first_number=text_number+first_number in let new_width=x+width in if cursor_number_row==new_row&&cursor_text_number==new_first_number then (cursor,False) else (Cursor_single new_row paragraph_id row_id new_first_number new_width new_width,True)
max_cursor (Cursor_double {}) intmap_intmap_texture seq_map=case seq_map of
    DS.Empty->error "max_cursor: you changed something without proper design"
    other_seq_map DS.:|> (paragraph_id,row_id) ->case DIS.lookup paragraph_id intmap_intmap_texture of
        Nothing->error"max_cursor: you changed something without proper design"
        Just intmap_texture->case DIS.lookup row_id intmap_texture of
            Nothing->error "max_cursor: you changed something without proper design"
            Just (_,x,width,text_number,firt_number)->let new_width=x+width in (Cursor_single (DS.length other_seq_map) paragraph_id row_id (text_number+firt_number) new_width new_width,True)


to_cursor::DIS.IntMap DT.Text->DIS.IntMap (DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int))->DS.Seq (Int,Int)->FP.Ptr SRF.Font->Int->Int->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->IO (Int,Int,Int,Int,FCT.CInt,FCT.CInt)
to_cursor intmap_text intmap_intmap_texture seq_map font row max_row font_height delta_height left up x y=let new_row=min (max_row-1) (row+max 0 (fromIntegral (div (y+div delta_height 2-up) (font_height+delta_height)))) in case DS.lookup new_row seq_map of
    Nothing->error "to_cursor: you changed something without proper design"
    Just (paragraph_id,row_id)->case DIS.lookup paragraph_id intmap_intmap_texture of
        Nothing->error "to_cursor: you changed something without proper design"
        Just intmap_texture->case DIS.lookup row_id intmap_texture of
            Nothing->error "to_cursor: you changed something without proper design"
            Just (_,this_x,_,text_number,first_number)->case DIS.lookup paragraph_id intmap_text of
                Nothing->error "to_cursor: you changed something without proper design"
                Just text->do
                    (left_length,right_length,last_width,_,right_text)<-cut_text (x-left-this_x) font (DT.take text_number (DT.drop first_number text))
                    if right_length==0 then return (new_row,paragraph_id,row_id,first_number+left_length,x-last_width-left,x-left) else do
                        new_width<-get_width font (DT.take 1 right_text)
                        if div new_width 2<last_width then return (new_row,paragraph_id,row_id,first_number+left_length+1,x+new_width-last_width-left,x-left) else return (new_row,paragraph_id,row_id,first_number+left_length,x-last_width-left,x-left)
