{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Type where
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified SDL.Raw.Types as SRT


data Engine a=Engine (DIS.IntMap (DIS.IntMap (Combined_widget a))) (DIS.IntMap Window) (DIS.IntMap Int) (DS.Seq (Request a)) Int Int Int
--所有控件，所有窗口，所有请求，窗口id映射（SDL到引擎），组合控件预分配id（每创建一个组合控件就加一），整个引擎的组合控件id，整个引擎的主控件id

data Combined_widget a=Leaf_widget (Engine a->Id) (Single_widget a)|Node_widget (Engine a->Id) Int Int
--Node_widget:后继函数，该组合控件的主控件id，跳转id（到它的组合控件id）

data Single_widget a=Data a|Trigger (Event->Engine a->Engine a)

data Window=Window Int SRT.Window SRT.Renderer

data Id=End|Goto Int|Back Int

data Request a=Create_widget (DS.Seq Int) (Combined_widget_request a)|Remove_widget (DS.Seq Int)|Replace_widget (DS.Seq Int) (Combined_widget_request a)|Create_window Int [Char] FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Remove_window Int|Present_window Int|Clear_window Int DW.Word8 DW.Word8 DW.Word8 DW.Word8|Resize_window Int FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Draw_Rectangle Int FCT.CInt FCT.CInt FCT.CInt FCT.CInt DW.Word8 DW.Word8 DW.Word8 DW.Word8

data Combined_widget_request a=Leaf_widget_request (Engine a->Id) (Single_widget_request a)|Node_widget_request (Engine a->Id) Int (DIS.IntMap (Combined_widget_request a))
--Node_widget_requestInt：其主控件id::Int

data Single_widget_request a=Data_request a|Trigger_request (Event->Engine a->Engine a)

data Event=Unknown|Quit|Time|At Int Action

data Action=Close|Press_down Key|Press_up Key|Click_down FCT.CInt FCT.CInt|Click_up FCT.CInt FCT.CInt|Input [FCT.CChar]

data Key=Key_a|Key_b|Key_c|Key_d|Key_e|Key_f|Key_g|Key_h|Key_i|Key_j|Key_k|Key_l|Key_m|Key_n|Key_o|Key_p|Key_q|Key_r|Key_s|Key_t|Key_u|Key_v|Key_w|Key_x|Key_y|Key_z|Key_left|Key_right|Key_up|Key_down|Key_unknown

class Data a where
    remove_data::a->IO ()
