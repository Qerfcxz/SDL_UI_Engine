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
create_window_instruction _ _=return

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

render_rectangle_widget_instruction::Instruction->Engine a->(Combined_widget a->IO (Combined_widget a))
render_rectangle_widget_instruction (Move_widget move_x move_y) engine=return.move_rectangle_combined_widget move_x move_y engine
render_rectangle_widget_instruction (Move_rectangle move_x move_y) engine=return.move_rectangle_combined_widget move_x move_y engine
render_rectangle_widget_instruction _ _=return

render_picture_widget_instruction::Instruction->Engine a->(Combined_widget a->IO (Combined_widget a))
render_picture_widget_instruction (Move_widget move_x move_y) engine=return.move_picture_combined_widget move_x move_y engine
render_picture_widget_instruction (Move_picture move_x move_y) engine=return.move_picture_combined_widget move_x move_y engine
render_picture_widget_instruction (Scale_picture width_multiply width_divide height_multiply height_divide) _=return.scale_picture_combined_widget width_multiply width_divide height_multiply height_divide
render_picture_widget_instruction (Flip_picture render_flip) _=return.flip_picture_combined_widget render_flip
render_picture_widget_instruction _ _=return

render_text_widget_instruction::Instruction->Engine a->(Combined_widget a->IO (Combined_widget a))
render_text_widget_instruction (Move_widget move_x move_y) engine=return.move_text_combined_widget move_x move_y engine
render_text_widget_instruction (Move_text move_x move_y) engine=return.move_text_combined_widget move_x move_y engine
render_text_widget_instruction _ _=return

render_editor_widget_instruction::Instruction->Engine a->(Combined_widget a->IO (Combined_widget a))
render_editor_widget_instruction (Move_widget move_x move_y) engine=return.move_editor_combined_widget move_x move_y engine
render_editor_widget_instruction (Move_editor move_x move_y) engine=return.move_editor_combined_widget move_x move_y engine
render_editor_widget_instruction (Text_color_editor red green blue alpha) _=return.text_color_editor_combined_widget red green blue alpha
render_editor_widget_instruction (Cursor_color_editor red green blue alpha) _=return.cursor_color_editor_combined_widget red green blue alpha
render_editor_widget_instruction (Select_color_editor red green blue alpha) _=return.select_color_editor_combined_widget red green blue alpha
render_editor_widget_instruction _ _=return

update_block_font_instruction::Instruction->Engine a->(Int,FCT.CInt,DSet.Set Char,Combined_widget a)->IO (Int,FCT.CInt,DSet.Set Char,Combined_widget a)
update_block_font_instruction _ _=return