{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE InstanceSigs #-}
module Type where
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Set as DS
import qualified Data.Text as DT
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Font as SRF

data Engine a=Engine (DIS.IntMap (DIS.IntMap (Combined_widget a))) (DIS.IntMap Window) (DIS.IntMap Int) (DS.Seq (Request a)) Int Int Int
--所有控件，所有窗口，窗口id映射（SDL到引擎），所有请求，组合控件预分配id（每创建一个组合控件就加一），整个引擎的组合控件id，整个引擎的主控件id

data Combined_widget a=Leaf_widget (Event->Engine a->Id) (Single_widget a)|Node_widget (Event->Engine a->Id) Int Int
--Node_widget:后继函数，该组合控件的主控件id，跳转id（到它的组合控件id）

data Single_widget a=Data a|Trigger (Event->Engine a->Engine a)|Io_trigger (Event->Engine a->IO (Engine a))|Font (DIS.IntMap (FP.Ptr SRF.Font))|Block_font Int DW.Word8 DW.Word8 DW.Word8 DW.Word8 (DIS.IntMap (FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)))|Rectangle Int DW.Word8 DW.Word8 DW.Word8 DW.Word8 FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Picture Int SRT.Texture FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Text Int Int Int Bool Bool Find FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt (DS.Seq Paragraph) (DS.Seq Row)|Editor Int Int Int Int Int Int Bool (DS.Seq Int) Block_find Typesetting DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt Cursor (DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Bool))

data Cursor=Cursor_none|Cursor_single Int Int Int FCT.CInt|Cursor_double Bool Int Int Int Int Int Int FCT.CInt FCT.CInt

data Window=Window Int SRT.Window SRT.Renderer FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt

data Id=End|Goto Int|Back Int

data Request a=Create_widget (DS.Seq Int) (Combined_widget_request a)|Remove_widget (DS.Seq Int)|Replace_widget (DS.Seq Int) (Combined_widget_request a)|Alter_widget (DS.Seq Int) (Combined_widget_request a)|Create_window Int DT.Text FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Remove_window Int|Present_window Int|Clear_window Int DW.Word8 DW.Word8 DW.Word8 DW.Word8|Resize_window Int FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Io_request (Engine a->IO (Engine a))|Render_rectangle Int DW.Word8 DW.Word8 DW.Word8 DW.Word8 FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Render_picture Int DT.Text FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Render_rectangle_widget (DS.Seq Int)|Render_picture_widget (DS.Seq Int)|Render_text_widget (DS.Seq Int)|Render_editor_widget (DS.Seq Int)|Update_block_font_widget (DS.Seq Int) Int FCT.CInt (DS.Set Char)

data Combined_widget_request a=Leaf_widget_request (Event->Engine a->Id) (Single_widget_request a)|Node_widget_request (Event->Engine a->Id) Int (DIS.IntMap (Combined_widget_request a))
--Node_widget_requestInt：其主控件id::Int

data Single_widget_request a=Data_request a|Trigger_request (Event->Engine a->Engine a)|Io_trigger_request (Event->Engine a->IO (Engine a))|Font_request DT.Text (DS.Seq Int)|Block_font_request Int DW.Word8 DW.Word8 DW.Word8 DW.Word8 DT.Text (DS.Seq Int)|Rectangle_request Int DW.Word8 DW.Word8 DW.Word8 DW.Word8 FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Picture_request Int DT.Text FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Text_request Int Int Find FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt (DS.Seq Paragraph)|Editor_request Int Int Int (DS.Seq Int) Block_find Typesetting DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 DW.Word8 FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt (DS.Seq (DS.Seq Char))

data Paragraph=Paragraph (DS.Seq (DT.Text,Color,DS.Seq Int,Int)) Typesetting|Paragraph_blank (DS.Seq Int) Int

data Typesetting=Typesetting_left|Typesetting_right|Typesetting_center

data Row=Row (DS.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)) FCT.CInt FCT.CInt|Row_blank FCT.CInt FCT.CInt
--Row_blank的参数为纵坐标和高度

data Find=Equal|Near

data Block_find=Block_equal|Block_near

data Event=Unknown|Quit|Time|At Int Action|Resize Int FCT.CInt FCT.CInt

data Action=Close|Wheel Int|Press Press Key|Click Click Mouse FCT.CInt FCT.CInt|Input DT.Text

data Press=Press_up|Press_down

data Click=Click_up|Click_down

data Mouse=Mouse_left|Mouse_right

data Key=Key_a|Key_b|Key_c|Key_d|Key_e|Key_f|Key_g|Key_h|Key_i|Key_j|Key_k|Key_l|Key_m|Key_n|Key_o|Key_p|Key_q|Key_r|Key_s|Key_t|Key_u|Key_v|Key_w|Key_x|Key_y|Key_z|Key_left|Key_right|Key_up|Key_down|Key_backspace|Key_delete|Key_enter|Key_escape|Key_tab|Key_left_shift|Key_right_shift|Key_left_ctrl|Key_right_ctrl|Key_numpad_0|Key_numpad_1|Key_numpad_2|Key_numpad_3|Key_numpad_4|Key_numpad_5|Key_numpad_6|Key_numpad_7|Key_numpad_8|Key_numpad_9|Key_f1|Key_f2|Key_f3|Key_f4|Key_f5|Key_f6|Key_f7|Key_f8|Key_f9|Key_f10|Key_f11|Key_f12|Key_unknown

type Color=SRT.Color

class Data a where
    clean_data::a->IO ()

instance Eq Press where
    (==)::Press->Press->Bool
    Press_up==Press_up=True
    Press_down==Press_down=True
    _==_=False

instance Eq Click where
    (==)::Click->Click->Bool
    Click_up==Click_up=True
    Click_down==Click_down=True
    _==_=False

instance Eq Mouse where
    (==)::Mouse->Mouse->Bool
    Mouse_left==Mouse_left=True
    Mouse_right==Mouse_right=True
    _==_=False

instance Eq Key where
    (==)::Key->Key->Bool
    Key_a==Key_a=True
    Key_b==Key_b=True
    Key_c==Key_c=True
    Key_d==Key_d=True
    Key_e==Key_e=True
    Key_f==Key_f=True
    Key_g==Key_g=True
    Key_h==Key_h=True
    Key_i==Key_i=True
    Key_j==Key_j=True
    Key_k==Key_k=True
    Key_l==Key_l=True
    Key_m==Key_m=True
    Key_n==Key_n=True
    Key_o==Key_o=True
    Key_p==Key_p=True
    Key_q==Key_q=True
    Key_r==Key_r=True
    Key_s==Key_s=True
    Key_t==Key_t=True
    Key_u==Key_u=True
    Key_v==Key_v=True
    Key_w==Key_w=True
    Key_x==Key_x=True
    Key_y==Key_y=True
    Key_z==Key_z=True
    Key_left==Key_left=True
    Key_right==Key_right=True
    Key_up==Key_up=True
    Key_down==Key_down=True
    Key_backspace==Key_backspace=True
    Key_delete==Key_delete=True
    Key_enter==Key_enter=True
    Key_escape==Key_escape=True
    Key_tab==Key_tab=True
    Key_left_shift==Key_left_shift=True
    Key_right_shift==Key_right_shift=True
    Key_left_ctrl==Key_left_ctrl=True
    Key_right_ctrl==Key_right_ctrl=True
    Key_numpad_0==Key_numpad_0=True
    Key_numpad_1==Key_numpad_1=True
    Key_numpad_2==Key_numpad_2=True
    Key_numpad_3==Key_numpad_3=True
    Key_numpad_4==Key_numpad_4=True
    Key_numpad_5==Key_numpad_5=True
    Key_numpad_6==Key_numpad_6=True
    Key_numpad_7==Key_numpad_7=True
    Key_numpad_8==Key_numpad_8=True
    Key_numpad_9==Key_numpad_9=True
    Key_f1==Key_f1=True
    Key_f2==Key_f2=True
    Key_f3==Key_f3=True
    Key_f4==Key_f4=True
    Key_f5==Key_f5=True
    Key_f6==Key_f6=True
    Key_f7==Key_f7=True
    Key_f8==Key_f8=True
    Key_f9==Key_f9=True
    Key_f10==Key_f10=True
    Key_f11==Key_f11=True
    Key_f12==Key_f12=True
    _==_=False