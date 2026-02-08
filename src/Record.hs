{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
module Record where
import Type
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet
import qualified Data.Text as DT
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Font as SRF

class Convert a b where
    convert::a->b

data Engine_record a=Engine_record {widget::DIS.IntMap (DIS.IntMap (Combined_widget a)),window::DIS.IntMap Window,window_map::DIS.IntMap Int,request::DSeq.Seq (Request a),key::DSet.Set Key,main_id::Engine a->Event->Int,start_id::Int,count_id::Int,time::Maybe DW.Word32}

to_engine_record::Engine a->Engine_record a
to_engine_record (Engine widget window window_map request key main_id start_id count_id time)=Engine_record widget window window_map request key main_id start_id count_id time

from_engine_record::Engine_record a->Engine a
from_engine_record (Engine_record widget window window_map request key main_id start_id count_id time)=Engine widget window window_map request key main_id start_id count_id time

instance Convert (Engine a) (Engine_record a) where
    convert=to_engine_record

instance Convert (Engine_record a) (Engine a) where
    convert=from_engine_record

data Single_widget_record a=Label_data_record {label::Label}|Bool_data_record {bool::Bool}|Int_data_record {int::Int}|Char_data_record {char::Char}|List_char_data_record {list_char::List_char}|Data_record {content::a}|Trigger_record {trigger::Event->Engine a->Engine a}|Io_trigger_record {io_trigger::Event->Engine a->IO (Engine a)}|Collector_record {request::DIS.IntMap (Request a)}|Font_record {font::DIS.IntMap (FP.Ptr SRF.Font)}|Block_font_record {window_id::Int,red::DW.Word8,green::DW.Word8,blue::DW.Word8,alpha::DW.Word8,block_font::DIS.IntMap (FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))}|Rectangle_record {window_id::Int,red::DW.Word8,green::DW.Word8,blue::DW.Word8,alpha::DW.Word8,left::FCT.CInt,right::FCT.CInt,up::FCT.CInt,down::FCT.CInt,x::FCT.CInt,y::FCT.CInt,width::FCT.CInt,height::FCT.CInt}|Picture_record {window_id::Int,texture::SRT.Texture,render_flip::Flip,angle::FCT.CDouble,design_x::FCT.CInt,design_y::FCT.CInt,width_multiply::FCT.CInt,width_divide::FCT.CInt,height_multiply::FCT.CInt,height_divide::FCT.CInt,original_width::FCT.CInt,original_height::FCT.CInt,x::FCT.CInt,y::FCT.CInt,width::FCT.CInt,height::FCT.CInt}|Text_record {window_id::Int,row::Int,max_row::Int,render::Bool,select::Bool,find::Find,design_delta_height::FCT.CInt,design_left::FCT.CInt,design_right::FCT.CInt,design_up::FCT.CInt,design_down::FCT.CInt,delta_height::FCT.CInt,left::FCT.CInt,right::FCT.CInt,up::FCT.CInt,down::FCT.CInt,seq_paragraph::DSeq.Seq Paragraph,seq_row::DSeq.Seq Row,text_binding::Text_binding}|Editor_record {window_id::Int,block_number::Int,row_number::Int,row::Int,design_font_size::Int,font_size::Int,render::Bool,font_path::DSeq.Seq Int,block_find::Block_find,typesetting::Typesetting,text_red::DW.Word8,text_green::DW.Word8,text_blue::DW.Word8,text_alpha::DW.Word8,cursor_red::DW.Word8,cursor_green::DW.Word8,cursor_blue::DW.Word8,cursor_alpha::DW.Word8,select_red::DW.Word8,select_green::DW.Word8,select_blue::DW.Word8,select_alpha::DW.Word8,design_height::FCT.CInt,design_block_width::FCT.CInt,design_delta_height::FCT.CInt,design_x::FCT.CInt,design_y::FCT.CInt,design_extra_width::FCT.CInt,design_extra_height::FCT.CInt,design_ime_left::FCT.CInt,design_ime_right::FCT.CInt,design_ime_up::FCT.CInt,design_ime_down::FCT.CInt,font_height::FCT.CInt,block_width::FCT.CInt,delta_height::FCT.CInt,x::FCT.CInt,y::FCT.CInt,left::FCT.CInt,right::FCT.CInt,up::FCT.CInt,down::FCT.CInt,ime_left::FCT.CInt,ime_right::FCT.CInt,ime_up::FCT.CInt,ime_down::FCT.CInt,cursor::Cursor,seq_seq_char::DSeq.Seq (DSeq.Seq (Char,Int,FCT.CInt),Int,Int,Bool),editor_binding::Editor_binding}

to_single_widget_record::Single_widget a->Single_widget_record a
to_single_widget_record (Label_data label)=Label_data_record label
to_single_widget_record (Bool_data bool)=Bool_data_record bool
to_single_widget_record (Int_data int)=Int_data_record int
to_single_widget_record (Char_data char)=Char_data_record char
to_single_widget_record (List_char_data list_char)=List_char_data_record list_char
to_single_widget_record (Data content)=Data_record content
to_single_widget_record (Trigger trigger)=Trigger_record trigger
to_single_widget_record (Io_trigger io_trigger)=Io_trigger_record io_trigger
to_single_widget_record (Collector request)=Collector_record request
to_single_widget_record (Font font)=Font_record font
to_single_widget_record (Block_font window_id red green blue alpha block_font)=Block_font_record window_id red green blue alpha block_font
to_single_widget_record (Rectangle window_id red green blue alpha left right up down x y width height)=Rectangle_record window_id red green blue alpha left right up down x y width height
to_single_widget_record (Picture window_id texture render_flip angle design_x design_y width_multiply width_divide height_multiply height_divide original_width original_height x y width height)=Picture_record window_id texture render_flip angle design_x design_y width_multiply width_divide height_multiply height_divide original_width original_height x y width height
to_single_widget_record (Text window_id row max_row render select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row text_binding)=Text_record window_id row max_row render select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row text_binding
to_single_widget_record (Editor window_id block_number row_number row design_font_size font_size render font_path block_find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha design_height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height design_ime_left design_ime_right design_ime_up design_ime_down font_height block_width delta_height x y left right up down ime_left ime_right ime_up ime_down cursor seq_seq_char editor_binding)=Editor_record window_id block_number row_number row design_font_size font_size render font_path block_find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha design_height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height design_ime_left design_ime_right design_ime_up design_ime_down font_height block_width delta_height x y left right up down ime_left ime_right ime_up ime_down cursor seq_seq_char editor_binding

from_single_widget_record::Single_widget_record a->Single_widget a
from_single_widget_record (Label_data_record label)=Label_data label
from_single_widget_record (Bool_data_record bool)=Bool_data bool
from_single_widget_record (Int_data_record int)=Int_data int
from_single_widget_record (Char_data_record char)=Char_data char
from_single_widget_record (List_char_data_record list_char)=List_char_data list_char
from_single_widget_record (Data_record content)=Data content
from_single_widget_record (Trigger_record trigger)=Trigger trigger
from_single_widget_record (Io_trigger_record io_trigger)=Io_trigger io_trigger
from_single_widget_record (Collector_record request)=Collector request
from_single_widget_record (Font_record font)=Font font
from_single_widget_record (Block_font_record window_id red green blue alpha block_font)=Block_font window_id red green blue alpha block_font
from_single_widget_record (Rectangle_record window_id red green blue alpha left right up down x y width height)=Rectangle window_id red green blue alpha left right up down x y width height
from_single_widget_record (Picture_record window_id texture render_flip angle design_x design_y width_multiply width_divide height_multiply height_divide original_width original_height x y width height)=Picture window_id texture render_flip angle design_x design_y width_multiply width_divide height_multiply height_divide original_width original_height x y width height
from_single_widget_record (Text_record window_id row max_row render select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row text_binding)=Text window_id row max_row render select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row text_binding
from_single_widget_record (Editor_record window_id block_number row_number row design_font_size font_size render font_path block_find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha design_height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height design_ime_left design_ime_right design_ime_up design_ime_down font_height block_width delta_height x y left right up down ime_left ime_right ime_up ime_down cursor seq_seq_char editor_binding)=Editor window_id block_number row_number row design_font_size font_size render font_path block_find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha design_height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height design_ime_left design_ime_right design_ime_up design_ime_down font_height block_width delta_height x y left right up down ime_left ime_right ime_up ime_down cursor seq_seq_char editor_binding

instance Convert (Single_widget a) (Single_widget_record a) where
    convert=to_single_widget_record

instance Convert (Single_widget_record a) (Single_widget a) where
    convert=from_single_widget_record

data Raw_request_record a=Create_widget_record {request::Combined_widget_request a,seq_id::DSeq.Seq Int}|Remove_widget_record {transmit::Bool,simple::Bool,seq_id::DSeq.Seq Int}|Replace_widget_record {transmit::Bool,request::Combined_widget_request a,seq_id::DSeq.Seq Int}|Alter_widget_record {transmit::Bool,request::Combined_widget_request a,seq_id::DSeq.Seq Int}|Create_window_record {window_id::Int,text::DT.Text,left::FCT.CInt,right::FCT.CInt,up::FCT.CInt,down::FCT.CInt}|Remove_window_record {window_id::Int}|Present_window_record {window_id::Int}|Clear_window_record {window_id::Int,red::DW.Word8,green::DW.Word8,blue::DW.Word8,alpha::DW.Word8}|Resize_window_record {window_id::Int,left::FCT.CInt,right::FCT.CInt,up::FCT.CInt,down::FCT.CInt}|Min_size_window_record {window_id::Int,width::FCT.CInt,height::FCT.CInt}|Max_size_window_record {window_id::Int,width::FCT.CInt,height::FCT.CInt}|Whether_bordered_window_record {window_id::Int,whether::Bool}|Io_record {io::Engine a->IO (Engine a)}|Render_rectangle_record {window_id::Int,red::DW.Word8,green::DW.Word8,blue::DW.Word8,alpha::DW.Word8,left::FCT.CInt,right::FCT.CInt,up::FCT.CInt,down::FCT.CInt}|Render_picture_record {window_id::Int,path::DT.Text,render_flip::Flip,angle::FCT.CDouble,x::FCT.CInt,y::FCT.CInt,width_multiply::FCT.CInt,width_divide::FCT.CInt,height_multiply::FCT.CInt,height_divide::FCT.CInt}|Render_rectangle_widget_record {transmit::Bool,seq_id::DSeq.Seq Int}|Render_picture_widget_record {transmit::Bool,seq_id::DSeq.Seq Int}|Render_text_widget_record {transmit::Bool,seq_id::DSeq.Seq Int}|Render_editor_widget_record {transmit::Bool,seq_id::DSeq.Seq Int}|Update_block_font_widget_record {transmit::Bool,size::Int,block_width::FCT.CInt,set_char::DSet.Set Char,seq_id::DSeq.Seq Int}

to_raw_request_record::Raw_request a->Raw_request_record a
to_raw_request_record (Create_widget request seq_id)=Create_widget_record request seq_id
to_raw_request_record (Remove_widget transmit simple seq_id)=Remove_widget_record transmit simple seq_id
to_raw_request_record (Replace_widget transmit request seq_id)=Replace_widget_record transmit request seq_id
to_raw_request_record (Alter_widget transmit request seq_id)=Alter_widget_record transmit request seq_id
to_raw_request_record (Create_window window_id text left right up down)=Create_window_record window_id text left right up down
to_raw_request_record (Remove_window window_id)=Remove_window_record window_id
to_raw_request_record (Present_window window_id)=Present_window_record window_id
to_raw_request_record (Clear_window window_id red green blue alpha)=Clear_window_record window_id red green blue alpha
to_raw_request_record (Resize_window window_id left right up down)=Resize_window_record window_id left right up down
to_raw_request_record (Min_size_window window_id width height)=Min_size_window_record window_id width height
to_raw_request_record (Max_size_window window_id width height)=Max_size_window_record window_id width height
to_raw_request_record (Whether_bordered_window window_id whether)=Whether_bordered_window_record window_id whether
to_raw_request_record (Io io)=Io_record io
to_raw_request_record (Render_rectangle window_id red green blue alpha left right up down)=Render_rectangle_record window_id red green blue alpha left right up down
to_raw_request_record (Render_picture window_id path render_flip angle x y width_multiply width_divide height_multiply height_divide)=Render_picture_record window_id path render_flip angle x y width_multiply width_divide height_multiply height_divide
to_raw_request_record (Render_rectangle_widget transmit seq_id)=Render_rectangle_widget_record transmit seq_id
to_raw_request_record (Render_picture_widget transmit seq_id)=Render_picture_widget_record transmit seq_id
to_raw_request_record (Render_text_widget transmit seq_id)=Render_text_widget_record transmit seq_id
to_raw_request_record (Render_editor_widget transmit seq_id)=Render_editor_widget_record transmit seq_id
to_raw_request_record (Update_block_font_widget transmit size block_width set_char seq_id)=Update_block_font_widget_record transmit size block_width set_char seq_id

from_raw_request_record::Raw_request_record a->Raw_request a
from_raw_request_record (Create_widget_record request seq_id)=Create_widget request seq_id
from_raw_request_record (Remove_widget_record transmit simple seq_id)=Remove_widget transmit simple seq_id
from_raw_request_record (Replace_widget_record transmit request seq_id)=Replace_widget transmit request seq_id
from_raw_request_record (Alter_widget_record transmit request seq_id)=Alter_widget transmit request seq_id
from_raw_request_record (Create_window_record window_id text left right up down)=Create_window window_id text left right up down
from_raw_request_record (Remove_window_record window_id)=Remove_window window_id
from_raw_request_record (Present_window_record window_id)=Present_window window_id
from_raw_request_record (Clear_window_record window_id red green blue alpha)=Clear_window window_id red green blue alpha
from_raw_request_record (Resize_window_record window_id left right up down)=Resize_window window_id left right up down
from_raw_request_record (Min_size_window_record window_id width height)=Min_size_window window_id width height
from_raw_request_record (Max_size_window_record window_id width height)=Max_size_window window_id width height
from_raw_request_record (Whether_bordered_window_record window_id whether)=Whether_bordered_window window_id whether
from_raw_request_record (Io_record io)=Io io
from_raw_request_record (Render_rectangle_record window_id red green blue alpha left right up down)=Render_rectangle window_id red green blue alpha left right up down
from_raw_request_record (Render_picture_record window_id path render_flip angle x y width_multiply width_divide height_multiply height_divide)=Render_picture window_id path render_flip angle x y width_multiply width_divide height_multiply height_divide
from_raw_request_record (Render_rectangle_widget_record transmit seq_id)=Render_rectangle_widget transmit seq_id
from_raw_request_record (Render_picture_widget_record transmit seq_id)=Render_picture_widget transmit seq_id
from_raw_request_record (Render_text_widget_record transmit seq_id)=Render_text_widget transmit seq_id
from_raw_request_record (Render_editor_widget_record transmit seq_id)=Render_editor_widget transmit seq_id
from_raw_request_record (Update_block_font_widget_record transmit size block_width set_char seq_id)=Update_block_font_widget transmit size block_width set_char seq_id

instance Convert (Raw_request a) (Raw_request_record a) where
    convert=to_raw_request_record

instance Convert (Raw_request_record a) (Raw_request a) where
    convert=from_raw_request_record

data Single_widget_request_record a=Label_data_request_record {label::Label}|Bool_data_request_record {bool::Bool}|Int_data_request_record {int::Int}|Char_data_request_record {char::Char}|List_char_data_request_record {list_char::List_char}|Data_request_record {content::a}|Trigger_request_record {trigger::Event->Engine a->Engine a}|Io_trigger_request_record {io_trigger::Event->Engine a->IO (Engine a)}|Collector_request_record {request::DIS.IntMap (Request a)}|Font_request_record {path::DT.Text,size::DSeq.Seq Int}|Block_font_request_record {window_id::Int,red::DW.Word8,green::DW.Word8,blue::DW.Word8,alpha::DW.Word8,path::DT.Text,size::DSeq.Seq Int}|Rectangle_request_record {window_id::Int,red::DW.Word8,green::DW.Word8,blue::DW.Word8,alpha::DW.Word8,left::FCT.CInt,right::FCT.CInt,up::FCT.CInt,down::FCT.CInt}|Picture_request_record {window_id::Int,path::DT.Text,render_flip::Flip,angle::FCT.CDouble,x::FCT.CInt,y::FCT.CInt,width_multiply::FCT.CInt,width_divide::FCT.CInt,height_multiply::FCT.CInt,height_divide::FCT.CInt}|Text_request_record {window_id::Int,find::Find,delta_height::FCT.CInt,left::FCT.CInt,right::FCT.CInt,up::FCT.CInt,down::FCT.CInt,seq_paragraph::DSeq.Seq Paragraph,text_binding::Text_binding}|Editor_request_record {window_id::Int,block_number::Int,font_size::Int,font_path::DSeq.Seq Int,block_find::Block_find,typesetting::Typesetting,text_red::DW.Word8,text_green::DW.Word8,text_blue::DW.Word8,text_alpha::DW.Word8,cursor_red::DW.Word8,cursor_green::DW.Word8,cursor_blue::DW.Word8,cursor_alpha::DW.Word8,select_red::DW.Word8,select_green::DW.Word8,select_blue::DW.Word8,select_alpha::DW.Word8,block_width::FCT.CInt,height::FCT.CInt,delta_height::FCT.CInt,x::FCT.CInt,y::FCT.CInt,extra_width::FCT.CInt,extra_height::FCT.CInt,ime_left::FCT.CInt,ime_right::FCT.CInt,ime_up::FCT.CInt,ime_down::FCT.CInt,seq_seq_char::DSeq.Seq (DSeq.Seq Char),editor_binding::Editor_binding}

to_single_widget_request_record::Single_widget_request a->Single_widget_request_record a
to_single_widget_request_record (Label_data_request label)=Label_data_request_record label
to_single_widget_request_record (Bool_data_request bool)=Bool_data_request_record bool
to_single_widget_request_record (Int_data_request int)=Int_data_request_record int
to_single_widget_request_record (Char_data_request char)=Char_data_request_record char
to_single_widget_request_record (List_char_data_request list_char)=List_char_data_request_record list_char
to_single_widget_request_record (Data_request content)=Data_request_record content
to_single_widget_request_record (Trigger_request trigger)=Trigger_request_record trigger
to_single_widget_request_record (Io_trigger_request io_trigger)=Io_trigger_request_record io_trigger
to_single_widget_request_record (Collector_request request)=Collector_request_record request
to_single_widget_request_record (Font_request path size)=Font_request_record path size
to_single_widget_request_record (Block_font_request window_id red green blue alpha path size)=Block_font_request_record window_id red green blue alpha path size
to_single_widget_request_record (Rectangle_request window_id red green blue alpha left right up down)=Rectangle_request_record window_id red green blue alpha left right up down
to_single_widget_request_record (Picture_request window_id path render_flip angle x y width_multiply width_divide height_multiply height_divide)=Picture_request_record window_id path render_flip angle x y width_multiply width_divide height_multiply height_divide
to_single_widget_request_record (Text_request window_id find delta_height left right up down seq_paragraph text_binding)=Text_request_record window_id find delta_height left right up down seq_paragraph text_binding
to_single_widget_request_record (Editor_request window_id block_number font_size font_path block_find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha block_width height delta_height x y extra_width extra_height ime_left ime_right ime_up ime_down seq_seq_char editor_binding)=Editor_request_record window_id block_number font_size font_path block_find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha block_width height delta_height x y extra_width extra_height ime_left ime_right ime_up ime_down seq_seq_char editor_binding

from_single_widget_request_record::Single_widget_request_record a->Single_widget_request a
from_single_widget_request_record (Label_data_request_record label)=Label_data_request label
from_single_widget_request_record (Bool_data_request_record bool)=Bool_data_request bool
from_single_widget_request_record (Int_data_request_record int)=Int_data_request int
from_single_widget_request_record (Char_data_request_record char)=Char_data_request char
from_single_widget_request_record (List_char_data_request_record list_char)=List_char_data_request list_char
from_single_widget_request_record (Data_request_record content)=Data_request content
from_single_widget_request_record (Trigger_request_record trigger)=Trigger_request trigger
from_single_widget_request_record (Io_trigger_request_record io_trigger)=Io_trigger_request io_trigger
from_single_widget_request_record (Collector_request_record request)=Collector_request request
from_single_widget_request_record (Font_request_record path size)=Font_request path size
from_single_widget_request_record (Block_font_request_record window_id red green blue alpha path size)=Block_font_request window_id red green blue alpha path size
from_single_widget_request_record (Rectangle_request_record window_id red green blue alpha left right up down)=Rectangle_request window_id red green blue alpha left right up down
from_single_widget_request_record (Picture_request_record window_id path render_flip angle x y width_multiply width_divide height_multiply height_divide)=Picture_request window_id path render_flip angle x y width_multiply width_divide height_multiply height_divide
from_single_widget_request_record (Text_request_record window_id find delta_height left right up down seq_paragraph text_binding)=Text_request window_id find delta_height left right up down seq_paragraph text_binding
from_single_widget_request_record (Editor_request_record window_id block_number font_size font_path block_find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha block_width height delta_height x y extra_width extra_height ime_left ime_right ime_up ime_down seq_seq_char editor_binding)=Editor_request window_id block_number font_size font_path block_find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha block_width height delta_height x y extra_width extra_height ime_left ime_right ime_up ime_down seq_seq_char editor_binding

instance Convert (Single_widget_request a) (Single_widget_request_record a) where
    convert=to_single_widget_request_record

instance Convert (Single_widget_request_record a) (Single_widget_request a) where
    convert=from_single_widget_request_record

data Text_binding_record=Empty_text_binding_record|Text_binding_record {wheel::Bool,up_press::DSet.Set (Press,DSet.Set Key),down_press::DSet.Set (Press,DSet.Set Key),min_press::DSet.Set (Press,DSet.Set Key),max_press::DSet.Set (Press,DSet.Set Key),down_click::DSet.Set (Click,Mouse)}

to_text_binding_record::Text_binding->Text_binding_record
to_text_binding_record Empty_text_binding=Empty_text_binding_record
to_text_binding_record (Text_binding wheel up_press down_press min_press max_press down_click)=Text_binding_record wheel up_press down_press min_press max_press down_click

from_text_binding_record::Text_binding_record->Text_binding
from_text_binding_record Empty_text_binding_record=Empty_text_binding
from_text_binding_record (Text_binding_record wheel up_press down_press min_press max_press down_click)=Text_binding wheel up_press down_press min_press max_press down_click

instance Convert Text_binding Text_binding_record where
    convert=to_text_binding_record

instance Convert Text_binding_record Text_binding where
    convert=from_text_binding_record

data Editor_binding_record=Empty_editor_binding_record|Editor_binding_record {trace::Bool,wheel::Bool,up_press::DSet.Set (Press,DSet.Set Key),down_press::DSet.Set (Press,DSet.Set Key),start_press::DSet.Set (Press,DSet.Set Key),end_press::DSet.Set (Press,DSet.Set Key),min_trace_press::DSet.Set (Press,DSet.Set Key),max_trace_press::DSet.Set (Press,DSet.Set Key),cursor_left_press::DSet.Set (Press,DSet.Set Key),cursor_right_press::DSet.Set (Press,DSet.Set Key),cursor_up_press::DSet.Set (Press,DSet.Set Key),cursor_down_press::DSet.Set (Press,DSet.Set Key),cursor_start_press::DSet.Set (Press,DSet.Set Key),cursor_end_press::DSet.Set (Press,DSet.Set Key),cursor_paragraph_start_press::DSet.Set (Press,DSet.Set Key),cursor_paragraph_end_press::DSet.Set (Press,DSet.Set Key),cursor_row_start_press::DSet.Set (Press,DSet.Set Key),cursor_row_end_press::DSet.Set (Press,DSet.Set Key),copy_press::DSet.Set (Press,DSet.Set Key),min_paste_press::DSet.Set (Press,DSet.Set Key),max_paste_press::DSet.Set (Press,DSet.Set Key),min_cut_press::DSet.Set (Press,DSet.Set Key),max_cut_press::DSet.Set (Press,DSet.Set Key),min_select_all_press::DSet.Set (Press,DSet.Set Key),max_select_all_press::DSet.Set (Press,DSet.Set Key),select_start_press::DSet.Set (Press,DSet.Set Key),select_end_press::DSet.Set (Press,DSet.Set Key),min_select_paragraph_all_press::DSet.Set (Press,DSet.Set Key),max_select_paragraph_all_press::DSet.Set (Press,DSet.Set Key),select_paragraph_start_press::DSet.Set (Press,DSet.Set Key),select_paragraph_end_press::DSet.Set (Press,DSet.Set Key),min_select_row_all_press::DSet.Set (Press,DSet.Set Key),max_select_row_all_press::DSet.Set (Press,DSet.Set Key),select_row_start_press::DSet.Set (Press,DSet.Set Key),select_row_end_press::DSet.Set (Press,DSet.Set Key),select_left_press::DSet.Set (Press,DSet.Set Key),select_right_press::DSet.Set (Press,DSet.Set Key),select_up_press::DSet.Set (Press,DSet.Set Key),select_down_press::DSet.Set (Press,DSet.Set Key),min_select_swap_press::DSet.Set (Press,DSet.Set Key),max_select_swap_press::DSet.Set (Press,DSet.Set Key),min_backspace_press::DSet.Set (Press,DSet.Set Key),max_backspace_press::DSet.Set (Press,DSet.Set Key),min_delete_press::DSet.Set (Press,DSet.Set Key),max_delete_press::DSet.Set (Press,DSet.Set Key),min_enter_press::DSet.Set (Press,DSet.Set Key),max_enter_press::DSet.Set (Press,DSet.Set Key),exit_press::DSet.Set (Press,DSet.Set Key),up_click::DSet.Set (Click,Mouse),down_click::DSet.Set (Click,Mouse)}

to_editor_binding_record::Editor_binding->Editor_binding_record
to_editor_binding_record Empty_editor_binding=Empty_editor_binding_record
to_editor_binding_record (Editor_binding trace wheel up_press down_press start_press end_press min_trace_press max_trace_press cursor_left_press cursor_right_press cursor_up_press cursor_down_press cursor_start_press cursor_end_press cursor_paragraph_start_press cursor_paragraph_end_press cursor_row_start_press cursor_row_end_press copy_press min_paste_press max_paste_press min_cut_press max_cut_press min_select_all_press max_select_all_press select_start_press select_end_press min_select_paragraph_all_press max_select_paragraph_all_press select_paragraph_start_press select_paragraph_end_press min_select_row_all_press max_select_row_all_press select_row_start_press select_row_end_press select_left_press select_right_press select_up_press select_down_press min_select_swap_press max_select_swap_press min_backspace_press max_backspace_press min_delete_press max_delete_press min_enter_press max_enter_press exit_press up_click down_click)=Editor_binding_record trace wheel up_press down_press start_press end_press min_trace_press max_trace_press cursor_left_press cursor_right_press cursor_up_press cursor_down_press cursor_start_press cursor_end_press cursor_paragraph_start_press cursor_paragraph_end_press cursor_row_start_press cursor_row_end_press copy_press min_paste_press max_paste_press min_cut_press max_cut_press min_select_all_press max_select_all_press select_start_press select_end_press min_select_paragraph_all_press max_select_paragraph_all_press select_paragraph_start_press select_paragraph_end_press min_select_row_all_press max_select_row_all_press select_row_start_press select_row_end_press select_left_press select_right_press select_up_press select_down_press min_select_swap_press max_select_swap_press min_backspace_press max_backspace_press min_delete_press max_delete_press min_enter_press max_enter_press exit_press up_click down_click

from_editor_binding_record::Editor_binding_record->Editor_binding
from_editor_binding_record Empty_editor_binding_record=Empty_editor_binding
from_editor_binding_record (Editor_binding_record trace wheel up_press down_press start_press end_press min_trace_press max_trace_press cursor_left_press cursor_right_press cursor_up_press cursor_down_press cursor_start_press cursor_end_press cursor_paragraph_start_press cursor_paragraph_end_press cursor_row_start_press cursor_row_end_press copy_press min_paste_press max_paste_press min_cut_press max_cut_press min_select_all_press max_select_all_press select_start_press select_end_press min_select_paragraph_all_press max_select_paragraph_all_press select_paragraph_start_press select_paragraph_end_press min_select_row_all_press max_select_row_all_press select_row_start_press select_row_end_press select_left_press select_right_press select_up_press select_down_press min_select_swap_press max_select_swap_press min_backspace_press max_backspace_press min_delete_press max_delete_press min_enter_press max_enter_press exit_press up_click down_click)=Editor_binding trace wheel up_press down_press start_press end_press min_trace_press max_trace_press cursor_left_press cursor_right_press cursor_up_press cursor_down_press cursor_start_press cursor_end_press cursor_paragraph_start_press cursor_paragraph_end_press cursor_row_start_press cursor_row_end_press copy_press min_paste_press max_paste_press min_cut_press max_cut_press min_select_all_press max_select_all_press select_start_press select_end_press min_select_paragraph_all_press max_select_paragraph_all_press select_paragraph_start_press select_paragraph_end_press min_select_row_all_press max_select_row_all_press select_row_start_press select_row_end_press select_left_press select_right_press select_up_press select_down_press min_select_swap_press max_select_swap_press min_backspace_press max_backspace_press min_delete_press max_delete_press min_enter_press max_enter_press exit_press up_click down_click

instance Convert Editor_binding Editor_binding_record where
    convert=to_editor_binding_record

instance Convert Editor_binding_record Editor_binding where
    convert=from_editor_binding_record