{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE InstanceSigs #-}
module Type where
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
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

data Single_widget a=Data a|Trigger (Event->Engine a->Engine a)|Io_trigger (Event->Engine a->IO (Engine a))|Font (DIS.IntMap (FP.Ptr SRF.Font))|Rectangle Int DW.Word8 DW.Word8 DW.Word8 DW.Word8 FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Picture Int SRT.Texture FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Text Int Int Int Bool Find FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt (DS.Seq Paragraph) (DS.Seq Row)
--Text的参数分别为窗口id，起始行，字体查找函数，额外行间距，设计的左右上下坐标，现在实际的左右上下坐标，设计的初始文本，已渲染储存的文本；Picture的参数分别是窗口id，渲染好的图片，设计的中心坐标，横向缩放比值（两个整数相比），纵向缩放比值（两个整数相比），图片的尺寸，实际的坐标及大小

data Window=Window Int SRT.Window SRT.Renderer FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt
--CInt分别为设计大小，平移和缩放

data Id=End|Goto Int|Back Int

data Request a=Create_widget (DS.Seq Int) (Combined_widget_request a)|Remove_widget (DS.Seq Int)|Replace_widget (DS.Seq Int) (Combined_widget_request a)|Alter_widget (DS.Seq Int) (Combined_widget_request a)|Create_window Int DT.Text FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Remove_window Int|Present_window Int|Clear_window Int DW.Word8 DW.Word8 DW.Word8 DW.Word8|Resize_window Int FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Io_request (Engine a->IO (Engine a))|Render_rectangle Int DW.Word8 DW.Word8 DW.Word8 DW.Word8 FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Render_picture Int DT.Text FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Render_rectangle_widget (DS.Seq Int)|Render_picture_widget (DS.Seq Int)|Render_text_widget (DS.Seq Int)

data Combined_widget_request a=Leaf_widget_request (Event->Engine a->Id) (Single_widget_request a)|Node_widget_request (Event->Engine a->Id) Int (DIS.IntMap (Combined_widget_request a))
--Node_widget_requestInt：其主控件id::Int

data Single_widget_request a=Data_request a|Trigger_request (Event->Engine a->Engine a)|Io_trigger_request (Event->Engine a->IO (Engine a))|Font_request DT.Text (DS.Seq Int)|Text_request Int Int Find FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt (DS.Seq Paragraph)|Picture_request Int DT.Text FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Rectangle_request Int DW.Word8 DW.Word8 DW.Word8 DW.Word8 FCT.CInt FCT.CInt FCT.CInt FCT.CInt

data Paragraph=Paragraph (DS.Seq (DT.Text,Color,DS.Seq Int,Int)) Typesetting|Paragraph_blank (DS.Seq Int) Int

data Typesetting=Typesetting_left|Typesetting_right|Typesetting_center

data Row=Row (DS.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)) FCT.CInt FCT.CInt|Row_blank FCT.CInt FCT.CInt
--Row_blank的参数为纵坐标和高度

data Find=Equal|Near

data Event=Unknown|Quit|Time|At Int Action|Resize Int FCT.CInt FCT.CInt

data Action=Close|Press Press Key|Click Click Mouse FCT.CInt FCT.CInt|Input DT.Text

data Press=Press_up|Press_down

data Click=Click_up|Click_down

data Mouse=Mouse_left|Mouse_right

data Key=Key_a|Key_b|Key_c|Key_d|Key_e|Key_f|Key_g|Key_h|Key_i|Key_j|Key_k|Key_l|Key_m|Key_n|Key_o|Key_p|Key_q|Key_r|Key_s|Key_t|Key_u|Key_v|Key_w|Key_x|Key_y|Key_z|Key_left|Key_right|Key_up|Key_down|Key_unknown

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
    _==_=False