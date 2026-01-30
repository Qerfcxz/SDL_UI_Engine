{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Instruction where
import Type
import Underlying
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet
import qualified Data.Text as DT
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT

create_widget_instruction::Instruction->Engine a->(Combined_widget_request a,DSeq.Seq Int)->IO (Combined_widget_request a,DSeq.Seq Int)
create_widget_instruction _ _=return

remove_widget_instruction::Instruction->Engine a->(Bool,Combined_widget a)->IO (Bool,Combined_widget a)
remove_widget_instruction _ _=return

replace_widget_instruction::Instruction->Engine a->(Combined_widget_request a,Combined_widget a)->IO (Combined_widget_request a,Combined_widget a)
replace_widget_instruction _ _=return

alter_widget_instruction::Instruction->Engine a->(Combined_widget_request a,Combined_widget a)->IO (Combined_widget_request a,Combined_widget a)
alter_widget_instruction _ _=return

create_window_instruction::Instruction->Engine a->(Int,DT.Text,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)->IO (Int,DT.Text,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)
create_window_instruction _ _ (window_id,window_name,left,right,up,down)=return (window_id,window_name,left,right,up,down)

remove_window_instruction::Instruction->Engine a->Int->IO Int
remove_window_instruction _ _=return

present_window_instruction::Instruction->Engine a->Int->IO Int
present_window_instruction _ _=return

clear_window_instruction::Instruction->Engine a->(Int,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (Int,DW.Word8,DW.Word8,DW.Word8,DW.Word8)
clear_window_instruction _ _=return

resize_window_instruction::Instruction->Engine a->(Int,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)->IO (Int,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)
resize_window_instruction _ _=return

io_instruction::Instruction->Engine a->(Engine a->IO (Engine a))->IO (Engine a->IO (Engine a))
io_instruction _ _=return

render_rectangle_instruction::Instruction->Engine a->(Int,DW.Word8,DW.Word8,DW.Word8,DW.Word8,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)->IO (Int,DW.Word8,DW.Word8,DW.Word8,DW.Word8,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)
render_rectangle_instruction _ _=return

render_picture_instruction::Instruction->Engine a->(Int,DT.Text,Flip,FCT.CDouble,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)->IO (Int,DT.Text,Flip,FCT.CDouble,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)
render_picture_instruction _ _=return

move_render_rectangle::Instruction->Engine a->(Combined_widget a->IO (Combined_widget a))
move_render_rectangle (Move_widget move_x move_y) engine=return.move_combined_widget_combined_widget move_x move_y engine
move_render_rectangle (Move_rectangle move_x move_y) engine=return.move_combined_widget_combined_widget move_x move_y engine
move_render_rectangle (Resize_widget resize_width resize_height) engine=return.resize_combined_widget_combined_widget resize_width resize_height engine
move_render_rectangle (Resize_rectangle resize_width resize_height) engine=return.resize_combined_widget_combined_widget resize_width resize_height engine
move_render_rectangle _ _=return

move_render_picture::Instruction->Engine a->(Combined_widget a->IO (Combined_widget a))
move_render_picture (Move_widget move_x move_y) engine=return.move_combined_widget_combined_widget move_x move_y engine
move_render_picture (Move_picture move_x move_y) engine=return.move_combined_widget_combined_widget move_x move_y engine
move_render_picture (Resize_widget resize_width resize_height) engine=return.resize_combined_widget_combined_widget resize_width resize_height engine
move_render_picture (Resize_picture resize_width resize_height) engine=return.resize_combined_widget_combined_widget resize_width resize_height engine
move_render_picture _ _=return

move_render_text::Instruction->Engine a->(Combined_widget a->IO (Combined_widget a))
move_render_text (Move_widget move_x move_y) engine=return.move_combined_widget_combined_widget move_x move_y engine
move_render_text (Move_text move_x move_y) engine=return.move_combined_widget_combined_widget move_x move_y engine
move_render_text _ _=return

move_render_editor::Instruction->Engine a->(Combined_widget a->IO (Combined_widget a))
move_render_editor (Move_widget move_x move_y) engine=return.move_combined_widget_combined_widget move_x move_y engine
move_render_editor (Move_editor move_x move_y) engine=return.move_combined_widget_combined_widget move_x move_y engine
move_render_editor _ _=return

update_block_font_instruction::Instruction->Engine a->(Int,FCT.CInt,DSet.Set Char,Combined_widget a)->IO (Int,FCT.CInt,DSet.Set Char,Combined_widget a)
update_block_font_instruction _ _=return