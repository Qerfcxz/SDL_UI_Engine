{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Type where
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet
import qualified Data.Text as DTe
import qualified Data.Tuple as DTu
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Font as SRF

data Engine a=Engine (DIS.IntMap (DIS.IntMap (Combined_widget a))) (DIS.IntMap Window) (DIS.IntMap Int) (DSeq.Seq (Request a)) (DSet.Set Key) Int Int Int
--所有控件，所有窗口，窗口id映射（SDL到引擎），所有请求，组合控件预分配id（每创建一个组合控件就加一），整个引擎的组合控件id，整个引擎的主控件id

data Combined_widget a=Leaf_widget (Event->Engine a->Id) (Single_widget a)|Node_widget (Event->Engine a->Id) Int Int
--Node_widget:后继函数，该组合控件的主控件id，跳转id（到它的组合控件id）

data Single_widget a=Label_data Label|Bool_data Bool|Int_data Int|Char_data Char|Data a|Trigger (Event->Engine a->Engine a)|Io_trigger (Event->Engine a->IO (Engine a))|Font (DIS.IntMap (FP.Ptr SRF.Font))|Block_font Int DW.Word8 DW.Word8 DW.Word8 DW.Word8 (DIS.IntMap (FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)))|Rectangle Int DW.Word8 DW.Word8 DW.Word8 DW.Word8 FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Picture Int SRT.Texture FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Text Int Int Int Bool Bool Find FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt (DSeq.Seq Paragraph) (DSeq.Seq Row)|Editor Int Int Int Int Int Int Bool (DSeq.Seq Int) Block_find Typesetting DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt Cursor (DSeq.Seq (DSeq.Seq (Char,Int,FCT.CInt),Int,Int,Bool))

data Label=Any_label|Bool_label|Int_label|Char_label|Solo_label Label|Tuple_label Label Label|List_label Label Int

data Cursor=Cursor_none|Cursor_single Int Int Int Int|Cursor_double Bool Int Int Int Int Int Int Int Int

data Window=Window Int SRT.Window SRT.Renderer FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt

data Id=End|Goto Int|Back Int

data Request a=Create_widget (DSeq.Seq Int) (Combined_widget_request a)|Remove_widget (DSeq.Seq Int)|Replace_widget (DSeq.Seq Int) (Combined_widget_request a)|Alter_widget (DSeq.Seq Int) (Combined_widget_request a)|Create_window Int DTe.Text FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Remove_window Int|Present_window Int|Clear_window Int DW.Word8 DW.Word8 DW.Word8 DW.Word8|Resize_window Int FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Io_request (Engine a->IO (Engine a))|Render_rectangle Int DW.Word8 DW.Word8 DW.Word8 DW.Word8 FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Render_picture Int DTe.Text FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Render_rectangle_widget (DSeq.Seq Int)|Render_picture_widget (DSeq.Seq Int)|Render_text_widget (DSeq.Seq Int)|Render_editor_widget (DSeq.Seq Int)|Update_block_font_widget (DSeq.Seq Int) Int FCT.CInt (DSet.Set Char)

data Combined_widget_request a=Leaf_widget_request (Event->Engine a->Id) (Single_widget_request a)|Node_widget_request (Event->Engine a->Id) Int (DIS.IntMap (Combined_widget_request a))
--Node_widget_requestInt：其主控件id::Int

data Single_widget_request a=Label_data_request Label|Bool_data_request Bool|Int_data_request Int|Char_data_request Char|Data_request a|Trigger_request (Event->Engine a->Engine a)|Io_trigger_request (Event->Engine a->IO (Engine a))|Font_request DTe.Text (DSeq.Seq Int)|Block_font_request Int DW.Word8 DW.Word8 DW.Word8 DW.Word8 DTe.Text (DSeq.Seq Int)|Rectangle_request Int DW.Word8 DW.Word8 DW.Word8 DW.Word8 FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Picture_request Int DTe.Text FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Text_request Int Int Find FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt (DSeq.Seq Paragraph)|Editor_request Int Int Int (DSeq.Seq Int) Block_find Typesetting DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt (DSeq.Seq (DSeq.Seq Char))

data Paragraph=Paragraph (DSeq.Seq (DTe.Text,Color,DSeq.Seq Int,Int)) Typesetting|Paragraph_blank (DSeq.Seq Int) Int

data Typesetting=Typesetting_left|Typesetting_right|Typesetting_center

data Row=Row (DSeq.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)) FCT.CInt FCT.CInt|Row_blank FCT.CInt FCT.CInt
--Row_blank的参数为纵坐标和高度

data Find=Equal|Near

data Block_find=Block_equal|Block_near|Block_small|Block_great

data Event=Unknown|Quit|Time|At Int Action|Resize Int FCT.CInt FCT.CInt

data Action=Close|Input DTe.Text|Wheel Int|Press Press Key (DSet.Set Key)|Click Click Mouse FCT.CInt FCT.CInt

data Press=Press_up|Press_down deriving (Eq,Ord)

data Click=Click_up|Click_down deriving (Eq,Ord)

data Mouse=Mouse_left|Mouse_right deriving (Eq,Ord)

data Key=Key_a|Key_b|Key_c|Key_d|Key_e|Key_f|Key_g|Key_h|Key_i|Key_j|Key_k|Key_l|Key_m|Key_n|Key_o|Key_p|Key_q|Key_r|Key_s|Key_t|Key_u|Key_v|Key_w|Key_x|Key_y|Key_z|Key_left|Key_right|Key_up|Key_down|Key_backspace|Key_delete|Key_enter|Key_esc|Key_tab|Key_left_shift|Key_right_shift|Key_left_ctrl|Key_right_ctrl|Key_numpad_0|Key_numpad_1|Key_numpad_2|Key_numpad_3|Key_numpad_4|Key_numpad_5|Key_numpad_6|Key_numpad_7|Key_numpad_8|Key_numpad_9|Key_f1|Key_f2|Key_f3|Key_f4|Key_f5|Key_f6|Key_f7|Key_f8|Key_f9|Key_f10|Key_f11|Key_f12|Key_unknown deriving (Eq,Ord)

type Color=SRT.Color

class Data a where
    clean_data::a->IO ()

class Predefined_data a where
    label_data::a->Label
    write_data::a->Combined_widget_request b
    read_data::DIS.IntMap (DIS.IntMap (Combined_widget b))->Combined_widget b->a

instance Predefined_data Bool where
    label_data=label_bool_data
    write_data=write_bool_data
    read_data=read_bool_data

label_bool_data::Bool->Label
label_bool_data _=Bool_label

write_bool_data::Bool->Combined_widget_request b
write_bool_data bool=Leaf_widget_request (\_ _->End) (Bool_data_request bool)

read_bool_data::DIS.IntMap (DIS.IntMap (Combined_widget a))->Combined_widget a->Bool
read_bool_data _ (Leaf_widget _ (Bool_data bool))=bool
read_bool_data _ _=error "read_bool_data: error 1"

instance Predefined_data Int where
    label_data=label_int_data
    write_data=write_int_data
    read_data=read_int_data

label_int_data::Int->Label
label_int_data _=Int_label

write_int_data::Int->Combined_widget_request b
write_int_data int=Leaf_widget_request (\_ _->End) (Int_data_request int)

read_int_data::DIS.IntMap (DIS.IntMap (Combined_widget a))->Combined_widget a->Int
read_int_data _ (Leaf_widget _ (Int_data int))=int
read_int_data _ _=error "read_int_data: error 1"

instance Predefined_data Char where
    label_data=label_char_data
    write_data=write_char_data
    read_data=read_char_data

label_char_data::Char->Label
label_char_data _=Char_label

write_char_data::Char->Combined_widget_request b
write_char_data char=Leaf_widget_request (\_ _->End) (Char_data_request char)

read_char_data::DIS.IntMap (DIS.IntMap (Combined_widget a))->Combined_widget a->Char
read_char_data _ (Leaf_widget _ (Char_data char))=char
read_char_data _ _=error "read_char_data: error 1"

instance Predefined_data a=>Predefined_data (DTu.Solo a) where
    label_data=label_solo_data
    write_data=write_solo_data
    read_data=read_solo_data

label_solo_data::Predefined_data a=>DTu.Solo a->Label
label_solo_data (DTu.MkSolo solo)=label_data solo

write_solo_data::Predefined_data a=>DTu.Solo a->Combined_widget_request b
write_solo_data (DTu.MkSolo solo)=Node_widget_request (\_ _->End) 0 (DIS.insert 1 (write_data solo) (DIS.singleton 0 (Leaf_widget_request (\_ _->End) (Label_data_request (Solo_label (label_data solo))))))

read_solo_data::Predefined_data a=>DIS.IntMap (DIS.IntMap (Combined_widget c))->Combined_widget c->DTu.Solo a
read_solo_data widget (Node_widget _ _ combined_id)=case DIS.lookup combined_id widget of
    Nothing->error "read_solo_data: error 1"
    Just intmap_widget->case DIS.lookup 1 intmap_widget of
        Nothing->error "read_solo_data: error 2"
        Just combined_widget->DTu.MkSolo (read_data widget combined_widget)
read_solo_data _ _=error "read_solo_data: error 3"

instance (Predefined_data a,Predefined_data b)=>Predefined_data (a,b) where
    label_data=label_tuple_data
    write_data=write_tuple_data
    read_data=read_tuple_data

label_tuple_data::(Predefined_data a,Predefined_data b)=>(a,b)->Label
label_tuple_data (first,second)=Tuple_label (label_data first) (label_data second)

write_tuple_data::(Predefined_data a,Predefined_data b)=>(a,b)->Combined_widget_request c
write_tuple_data (first,second)=Node_widget_request (\_ _->End) 0 (DIS.insert 2 (write_data second) (DIS.insert 1 (write_data first) (DIS.singleton 0 (Leaf_widget_request (\_ _->End) (Label_data_request (Tuple_label (label_data first) (label_data second)))))))

read_tuple_data::(Predefined_data a,Predefined_data b)=>DIS.IntMap (DIS.IntMap (Combined_widget c))->Combined_widget c->(a,b)
read_tuple_data widget (Node_widget _ _ combined_id)=case DIS.lookup combined_id widget of
    Nothing->error "read_tuple_data: error 1"
    Just intmap_widget->case DIS.lookup 1 intmap_widget of
        Nothing->error "read_tuple_data: error 2"
        Just first_combined_widget->case DIS.lookup 2 intmap_widget of
            Nothing->error "read_tuple_data: error 3"
            Just second_combined_widget->(read_data widget first_combined_widget,read_data widget second_combined_widget)
read_tuple_data _ _=error "read_tuple_data: error 4"

instance Predefined_data a=>Predefined_data [a] where
    label_data=label_list_data
    write_data=write_list_data
    read_data=read_list_data

label_list_data::Predefined_data a=>[a]->Label
label_list_data []=Any_label
label_list_data (first:_)=label_data first

write_list_data::Predefined_data a=>[a]->Combined_widget_request b
write_list_data list=let list_length=length list in let request=write_list_data_a list list_length DIS.empty in Node_widget_request (\_ _->End) 0 (DIS.insert 0 (Leaf_widget_request (\_ _->End) (Label_data_request (List_label (label_list_data list) list_length))) request)

write_list_data_a::Predefined_data a=>[a]->Int->DIS.IntMap (Combined_widget_request b)->DIS.IntMap (Combined_widget_request b)
write_list_data_a [] _ request=request
write_list_data_a (value:other_value) number request=write_list_data_a other_value (number-1) (DIS.insert number (write_data value) request)

read_list_data::Predefined_data a=>DIS.IntMap (DIS.IntMap (Combined_widget b))->Combined_widget b->[a]
read_list_data widget (Node_widget _ _ combined_id)=case DIS.lookup combined_id widget of
        Nothing->error "read_list_data: error 1"
        Just intmap_widget->read_list_data_a widget intmap_widget 1 []
read_list_data _ _=error "read_list_data: error 2"

read_list_data_a::Predefined_data a=>DIS.IntMap (DIS.IntMap (Combined_widget b))->DIS.IntMap (Combined_widget b)->Int->[a]->[a]
read_list_data_a widget intmap_widget single_id list=case DIS.lookup single_id intmap_widget of
    Nothing->list
    Just combined_widget->read_list_data_a widget intmap_widget (single_id+1) (read_data widget combined_widget:list)