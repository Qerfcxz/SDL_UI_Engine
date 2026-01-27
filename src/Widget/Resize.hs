{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Widget.Resize where
import Editor.Block
import Other.Error
import Other.Get
import Other.Other
import Other.Text
import Text.Font
import Text.From
import Text.Max
import Widget.Create
import Widget.Remove
import Type
import qualified Control.Monad as CM
import qualified Data.ByteString as DB
import qualified Data.Char as DC
import qualified Data.Foldable as DF
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Types as SRT

create_window_trigger::(Engine a->Event->Id)->DS.Seq Int->DS.Seq Int->Engine a->IO (Engine a)
create_window_trigger next_id seq_id seq_window_id=create_widget seq_id (Leaf_widget_request next_id (Trigger_request (create_window_trigger_a seq_window_id)))

create_window_trigger_a::DS.Seq Int->Event->Engine a->Engine a
create_window_trigger_a seq_window_id event engine@(Engine widget window window_map request key main_id start_id count_id time)=case event of
    Resize window_id width height->case DS.elemIndexL window_id seq_window_id of
        Nothing->engine
        Just _->Engine widget (error_update "create_window_trigger_a: error 1" window_id (\(Window this_window_id this_window renderer design_width design_height _ _ _ _)->let (x,y,design_size,size)=adaptive_window design_width design_height width height in Window this_window_id this_window renderer design_width design_height x y design_size size) window) window_map request key main_id start_id count_id time
    _->engine

create_widget_trigger::(Engine a->Event->Id)->DS.Seq Int->DIS.IntMap (DS.Seq (DS.Seq Int))->Engine a->IO (Engine a)
create_widget_trigger next_id seq_id id_map=create_widget seq_id (Leaf_widget_request next_id (Io_trigger_request (create_widget_trigger_a id_map)))

create_widget_trigger_a::DIS.IntMap (DS.Seq (DS.Seq Int))->Event->Engine a->IO (Engine a)
create_widget_trigger_a id_map event engine=case event of
    Resize window_id _ _->case DIS.lookup window_id id_map of
        Nothing->return engine
        Just seq_seq_id->CM.foldM (flip update_widget) engine seq_seq_id
    _->return engine

update_widget::DS.Seq Int->Engine a->IO (Engine a)
update_widget seq_id (Engine widget window window_map request key main_id start_id count_id time)=do
    new_widget<-update_widget_a seq_id start_id window widget
    return (Engine new_widget window window_map request key main_id start_id count_id time)

update_widget_a::DS.Seq Int->Int->DIS.IntMap Window->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))
update_widget_a seq_id start_id window widget=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in case DIS.lookup combined_id widget of
    Nothing->error "update_widget_a: error 1"
    Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
        Nothing->error "update_widget_a: error 2"
        Just combined_widget->case combined_widget of
            Leaf_widget next_id (Rectangle window_id red green blue alpha left right up down _ _ _ _)->let (x,y,design_size,size)=get_transform_window window_id window in return (error_replace_replace "update_widget_a: error 3" "update_widget_a: error 4" combined_id single_id (Leaf_widget next_id (Rectangle window_id red green blue alpha left right up down (x+div (left*size) design_size) (y+div (up*size) design_size) (div ((right-left)*size) design_size) (div ((down-up)*size) design_size))) widget)
            Leaf_widget next_id (Picture window_id texture render_flip angle x y width_multiply width_divide height_multiply height_divide width height _ _ _ _)->let (window_x,window_y,design_size,size)=get_transform_window window_id window in let new_width=div (width*width_multiply) width_divide in let new_height=div (height*height_multiply) height_divide in return (error_replace_replace "update_widget_a: error 5" "update_widget_a: error 6" combined_id single_id (Leaf_widget next_id (Picture window_id texture render_flip angle x y width_multiply width_divide height_multiply height_divide width height (window_x+div ((x-div new_width 2)*size) design_size) (window_y+div ((y-div new_height 2)*size) design_size) (div (new_width*size) design_size) (div (new_height*size) design_size))) widget)
            Leaf_widget next_id (Text window_id row _ _ select find delta_height left right up down _ _ _ _ _ seq_paragraph seq_row)->do
                DF.mapM_ clean_row seq_row
                let (renderer,x,y,design_size,size)=get_renderer_with_transform_window window_id window
                let new_delta_height=div (delta_height*size) design_size
                new_seq_row<-from_paragraph widget renderer (find_font find) window_id start_id design_size size 0 (div ((right-left)*size) design_size) new_delta_height seq_paragraph DS.empty
                let new_up=(y+div (up*size) design_size) in let new_down=(y+div (down*size) design_size) in let max_row=find_max new_seq_row new_up new_down in return (error_replace_replace "update_widget_a: error 7" "update_widget_a: error 8" combined_id single_id (Leaf_widget next_id (Text window_id (max 0 (min row max_row)) max_row True select find delta_height left right up down new_delta_height (x+div (left*size) design_size) (x+div (right*size) design_size) new_up new_down seq_paragraph new_seq_row)) widget)
            Leaf_widget next_id (Editor window_id block_number _ row font_size _ _ path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height block_width delta_height x y extra_width extra_height _ _ _ _ _ _ _ _ _ _ seq_seq_char)->let (renderer,window_x,window_y,design_size,size)=get_renderer_with_transform_window window_id window in let new_block_width=div (block_width*size) design_size in let (font_next_id,font_combined_id,font_single_id,new_font_size,font_window_id,font_red,font_green,font_blue,font_alpha,font,this_font,_,intmap_texture)=find_block_font find widget block_width new_block_width design_size size path start_id font_size in FMA.alloca $ \text_color->do
                FS.poke text_color (color text_red text_green text_blue text_alpha)
                case seq_seq_char of
                    DS.Empty->error "update_widget_a: error 9"
                    ((seq_char,_,_,end) DS.:<| other_seq_seq_char)->do
                        (new_intmap_texture,new_seq_seq_char)<-update_editor end renderer 0 0 block_number text_color text_red text_green text_blue text_alpha this_font new_block_width intmap_texture DS.empty seq_char DS.empty other_seq_seq_char
                        ascent<-SRF.fontAscent this_font
                        descent<-SRF.fontDescent this_font
                        let new_height=div ((height+delta_height)*size) design_size
                        let new_font_height=ascent-descent
                        let new_delta_height=div (delta_height*size) design_size
                        let new_row_number=fromIntegral (div new_height (new_font_height+new_delta_height))
                        let half_width=div (fromIntegral block_number*new_block_width) 2
                        let half_height=div (div (height*size) design_size) 2
                        return (error_replace_replace "update_widget_a: error 10" "update_widget_a: error 11" combined_id single_id (Leaf_widget next_id (Editor window_id block_number new_row_number (min (max 0 (DS.length new_seq_seq_char-new_row_number)) row) font_size new_font_size True path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height block_width delta_height x y extra_width extra_height new_font_height new_block_width new_delta_height (window_x+div (x*size) design_size-div (fromIntegral block_number*new_block_width) 2) (window_y+div (y*size) design_size-div new_height 2) (window_x+div ((x-extra_width)*size) design_size-half_width) (window_x+div ((x+extra_width)*size) design_size+half_width) (window_y+div ((y-extra_height)*size) design_size-half_height) (window_y+div ((y+extra_height)*size) design_size+half_height) Cursor_none new_seq_seq_char)) (error_replace_replace "update_widget_a: error 12" "update_widget_a: error 13" font_combined_id font_single_id (Leaf_widget font_next_id (Block_font font_window_id font_red font_green font_blue font_alpha (DIS.insert new_font_size (this_font,new_font_height,new_intmap_texture) font))) widget))
            _->error "update_widget_a: error 14"

update_editor::Bool->SRT.Renderer->Int->Int->Int->FP.Ptr Color->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FP.Ptr SRF.Font->FCT.CInt->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->DS.Seq (Char,Int,FCT.CInt)->DS.Seq (Char,Int,FCT.CInt)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->IO (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8),DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool))
update_editor _ _ number_block number_char _ _ _ _ _ _ _ _ intmap_texture this_seq_char DS.Empty this_seq_seq_char DS.Empty=return (intmap_texture,this_seq_seq_char DS.|> (this_seq_char,number_block,number_char,True))
update_editor end renderer number_block number_char block_number text_color text_red text_green text_blue text_alpha font block_width intmap_texture this_seq_char DS.Empty this_seq_seq_char ((seq_char,_,_,new_end) DS.:<| seq_seq_char)=if end then update_editor new_end renderer 0 0 block_number text_color text_red text_green text_blue text_alpha font block_width intmap_texture DS.empty seq_char (this_seq_seq_char DS.|> (this_seq_char,number_block,number_char,end)) seq_seq_char else update_editor new_end renderer number_block number_char block_number text_color text_red text_green text_blue text_alpha font block_width intmap_texture this_seq_char seq_char this_seq_seq_char seq_seq_char 
update_editor end renderer number_block number_char block_number text_color text_red text_green text_blue text_alpha font block_width intmap_texture this_seq_char ((char,_,_) DS.:<| seq_char) this_seq_seq_char seq_seq_char=let char_ord=DC.ord char in case DIS.lookup char_ord intmap_texture of
    Nothing->do
        (texture,width)<-DB.useAsCString (DTE.encodeUtf8 (DT.singleton char)) (to_texture_with_width renderer text_color font)
        let width_mod=mod width block_width
        let block=if width_mod==0 then fromIntegral (div width block_width) else fromIntegral (div width block_width+1)
        let delta_x=if width_mod==0 then 0 else div (block_width-width_mod) 2
        let new_number_block=number_block+block in if block_number<new_number_block then if block_number<block then error "update_editor: error 1" else update_editor end renderer block 1 block_number text_color text_red text_green text_blue text_alpha font block_width (DIS.insert char_ord (texture,DIS.singleton (fromIntegral block_width) (block,delta_x),width,text_red,text_green,text_blue,text_alpha) intmap_texture) (DS.singleton (char,block,delta_x)) seq_char (this_seq_seq_char DS.|> (this_seq_char,number_block,number_char,False)) seq_seq_char else update_editor end renderer new_number_block (number_char+1) block_number text_color text_red text_green text_blue text_alpha font block_width (DIS.insert char_ord (texture,DIS.singleton (fromIntegral block_width) (block,delta_x),width,text_red,text_green,text_blue,text_alpha) intmap_texture) (this_seq_char DS.|> (char,block,delta_x)) seq_char this_seq_seq_char seq_seq_char
    Just (texture,intmap_int,width,red,green,blue,alpha)->let this_block_width=fromIntegral block_width in case DIS.lookup this_block_width intmap_int of
        Nothing->do
            let width_mod=mod width block_width
            let block=if width_mod==0 then fromIntegral (div width block_width) else fromIntegral (div width block_width+1)
            let delta_x=if width_mod==0 then 0 else div (block_width-width_mod) 2
            let new_number_block=number_block+block in if block_number<new_number_block then if block_number<block then error "update_editor: error 2" else update_editor end renderer block 1 block_number text_color text_red text_green text_blue text_alpha font block_width (DIS.insert char_ord (texture,DIS.insert this_block_width (block,delta_x) intmap_int,width,red,green,blue,alpha) intmap_texture) (DS.singleton (char,block,delta_x)) seq_char (this_seq_seq_char DS.|> (this_seq_char,number_block,number_char,False)) seq_seq_char else update_editor end renderer new_number_block (number_char+1) block_number text_color text_red text_green text_blue text_alpha font block_width (DIS.insert char_ord (texture,DIS.insert this_block_width (block,delta_x) intmap_int,width,red,green,blue,alpha) intmap_texture) (this_seq_char DS.|> (char,block,delta_x)) seq_char this_seq_seq_char seq_seq_char
        Just (block,delta_x)->let new_number_block=number_block+block in if block_number<new_number_block then if block_number<block then error "update_editor: error 3" else update_editor end renderer block 1 block_number text_color text_red text_green text_blue text_alpha font block_width intmap_texture (DS.singleton (char,block,delta_x)) seq_char (this_seq_seq_char DS.|> (this_seq_char,number_block,number_char,False)) seq_seq_char else update_editor end renderer new_number_block (number_char+1) block_number text_color text_red text_green text_blue text_alpha font block_width intmap_texture (this_seq_char DS.|> (char,block,delta_x)) seq_char this_seq_seq_char seq_seq_char