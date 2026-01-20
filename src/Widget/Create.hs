{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Widget.Create where
import Editor.Block
import Editor.From
import Other.Error
import Other.Get
import Other.Other
import Text.Font
import Text.From
import Text.Max
import Type
import qualified Control.Monad as CM
import qualified Data.ByteString as DB
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Text.Encoding as DTE
import qualified Data.Word as DW
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

create_single_widget::Int->DIS.IntMap Window->Single_widget_request a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Single_widget a)
create_single_widget _ _ (Data_request content) _=return (Data content)
create_single_widget _ _ (Trigger_request handle) _=return (Trigger handle)
create_single_widget _ _ (Io_trigger_request handle) _=return (Io_trigger handle)
create_single_widget _ _ (Font_request path size) _=do
    font<-DB.useAsCString (DTE.encodeUtf8 path) (`create_font` size)
    return (Font font)
create_single_widget _ _ (Block_font_request window_id red green blue alpha path size) _=do
    font<-DB.useAsCString (DTE.encodeUtf8 path) (`create_block_font` size)
    return (Block_font window_id red green blue alpha font)
create_single_widget _ window (Rectangle_request window_id red green blue alpha left right up down) _=case DIS.lookup window_id window of
    Nothing->error "create_single_widget: error 1"
    Just (Window _ _ _ _ _ x y design_size size)->return (Rectangle window_id red green blue alpha left right up down (x+div (left*size) design_size) (y+div (up*size) design_size) (div ((right-left)*size) design_size) (div ((down-up)*size) design_size))
create_single_widget _ window (Picture_request window_id path x y width_multiply width_divide height_multiply height_divide) _=case DIS.lookup window_id window of
    Nothing->error "create_single_widget: error 2"
    Just (Window _ _ renderer _ _ window_x window_y design_size size)->do
        surface<-DB.useAsCString (DTE.encodeUtf8 path) SRV.loadBMP
        CM.when (surface==FP.nullPtr) $ error "create_single_widget: error 3"
        (SRT.Surface _ width height _ _ _ _)<-FS.peek surface
        texture<-SRV.createTextureFromSurface renderer surface
        SRV.freeSurface surface
        CM.when (texture==FP.nullPtr) $ error "create_single_widget: error 4"
        let new_width=div (width*width_multiply) width_divide in let new_height=div (height*height_multiply) height_divide in return (Picture window_id texture x y width_multiply width_divide height_multiply height_divide width height (window_x+div ((x-div new_width 2)*size) design_size) (window_y+div ((y-div new_height 2)*size) design_size) (div (new_width*size) design_size) (div (new_height*size) design_size))
create_single_widget start_id window (Text_request window_id row find delta_height left right up down seq_paragraph) widget=case DIS.lookup window_id window of
    Nothing->error "create_single_widget: error 5"
    Just (Window _ _ renderer _ _ x y design_size size)->let new_delta_height=div (delta_height*size) design_size in do
        seq_row<-from_paragraph widget renderer (find_font find) window_id start_id design_size size 0 (div ((right-left)*size) design_size) new_delta_height seq_paragraph DS.empty
        let new_up=y+div (up*size) design_size in let new_down=y+div (down*size) design_size in let max_row=find_max seq_row new_up new_down in return (Text window_id (max 0 (min row max_row)) max_row False False find delta_height left right up down new_delta_height (x+div (left*size) design_size) (x+div (right*size) design_size) new_up new_down seq_paragraph seq_row)
create_single_widget start_id window (Editor_request window_id block_number font_size path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha block_width height delta_height x y extra_width extra_height seq_text) widget=let (window_x,window_y,design_size,size)=get_transform_window window_id window in let (this_window_id,new_font_size,_,font_height,intmap_texture)=find_block_font find widget design_size size path start_id font_size in if window_id==this_window_id
    then FMA.alloca $ \text_color->do
        FS.poke text_color (color text_red text_green text_blue text_alpha)
        let new_block_width=div (block_width*size) design_size
        let new_delta_height=div (delta_height*size) design_size
        let new_height=div ((height+delta_height)*size) design_size
        let half_width=div (fromIntegral block_number*new_block_width) 2
        let half_height=div (div (height*size) design_size) 2
        return (Editor window_id block_number (fromIntegral (div new_height (font_height+new_delta_height))) 0 font_size new_font_size False path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height block_width delta_height x y extra_width extra_height font_height new_block_width new_delta_height (window_x+div (x*size) design_size-div (fromIntegral block_number*new_block_width) 2) (window_y+div (y*size) design_size-div new_height 2) (window_x+div ((x-extra_width)*size) design_size-half_width) (window_x+div ((x+extra_width)*size) design_size+half_width) (window_y+div ((y-extra_height)*size) design_size-half_height) (window_y+div ((y+extra_height)*size) design_size+half_height) Cursor_none (from_seq_seq_char intmap_texture block_number new_block_width seq_text DS.empty))
    else error "create_single_widget: error 6"

create_font::FCS.CString->DS.Seq Int->IO (DIS.IntMap (FP.Ptr SRF.Font))
create_font _ DS.Empty=return DIS.empty
create_font path (size DS.:<| other_size)=do
    font<-create_font path other_size
    new_font<-SRF.openFont path (fromIntegral size)
    CM.when (new_font==FP.nullPtr) $ error "create_font: error 1"
    return (DIS.insert size new_font font)

create_block_font::FCS.CString->DS.Seq Int->IO (DIS.IntMap (FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)))
create_block_font _ DS.Empty=return DIS.empty
create_block_font path (size DS.:<| other_size)=do
    font<-create_block_font path other_size
    new_font<-SRF.openFont path (fromIntegral size)
    CM.when (new_font==FP.nullPtr) $ error "create_block_font: error 1"
    ascent<-SRF.fontAscent new_font
    descent<-SRF.fontDescent new_font
    return (DIS.insert size (new_font,ascent-descent,DIS.empty) font)

create_widget::DS.Seq Int->Combined_widget_request a->Engine a->IO (Engine a)
create_widget seq_single_id combined_widget_request (Engine widget window window_map request count_id start_id main_id)=case seq_single_id of
    DS.Empty->error "create_widget: error 1"
    (single_id DS.:<| other_seq_single_id)->do
        (new_count_id,new_widget)<-create_widget_a other_seq_single_id count_id start_id single_id start_id window combined_widget_request widget
        return (Engine new_widget window window_map request new_count_id start_id main_id)

create_widget_a::DS.Seq Int->Int->Int->Int->Int->DIS.IntMap Window->Combined_widget_request a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))
create_widget_a seq_single_id count_id combined_id single_id start_id window combined_widget_request widget=case seq_single_id of
    DS.Empty->create_widget_top count_id combined_id single_id start_id window combined_widget_request widget
    (new_single_id DS.:<| other_seq_single_id)->case DIS.lookup combined_id widget of
        Nothing->error "create_widget_a: error 1"
        Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
            Nothing->error "create_widget_a: error 2"
            Just (Leaf_widget _ _)->error "create_widget_a: error 3"
            Just (Node_widget _ _ new_combined_id)->create_widget_a other_seq_single_id count_id new_combined_id new_single_id start_id window combined_widget_request widget

create_widget_top::Int->Int->Int->Int->DIS.IntMap Window->Combined_widget_request a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))
create_widget_top count_id combined_id single_id start_id window (Leaf_widget_request next_id single_widget_request) widget=do
    new_single_widget<-create_single_widget start_id window single_widget_request widget
    return (count_id,error_insert_insert "create_widget_top: error 1" "create_widget_top: error 2" combined_id single_id (Leaf_widget next_id new_single_widget) widget)
create_widget_top count_id combined_id single_id start_id window (Node_widget_request next_id main_single_id intmap_combined_widget_request) widget=DIS.foldlWithKey (\io this_single_id->create_widget_top_a count_id this_single_id start_id window io)  (return (count_id+1,error_insert "create_widget_top: you changed something without proper design" count_id DIS.empty (error_insert_insert "create_widget_top: error 3" "create_widget_top: error 4" combined_id single_id (Node_widget next_id main_single_id count_id) widget))) intmap_combined_widget_request

create_widget_top_a::Int->Int->Int->DIS.IntMap Window->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))->Combined_widget_request a->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))
create_widget_top_a combined_id single_id start_id window io combined_widget_request=do
    (count_id,widget)<-io
    create_widget_top count_id combined_id single_id start_id window combined_widget_request widget