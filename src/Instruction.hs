{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Instruction where
import Type
import qualified Foreign.C.Types as FCT

instruction_render_rectangle::Engine a->Instruction_render_rectangle->(Combined_widget a->Combined_widget a)
instruction_render_rectangle engine (Move_render_rectangle delta_x delta_y)=move_rectangle delta_x delta_y engine

instruction_render_picture::Engine a->Instruction_render_picture->(Combined_widget a->Combined_widget a)
instruction_render_picture engine (Move_render_picture delta_x delta_y)=move_picture delta_x delta_y engine

instruction_render_text::Engine a->Instruction_render_text->(Combined_widget a->Combined_widget a)
instruction_render_text engine (Move_render_text delta_x delta_y)=move_text delta_x delta_y engine

instruction_render_editor::Engine a->Instruction_render_editor->(Combined_widget a->Combined_widget a)
instruction_render_editor engine (Move_render_editor delta_x delta_y)=move_editor delta_x delta_y engine

move_rectangle::FCT.CInt->FCT.CInt->Engine a->Combined_widget a->Combined_widget a
move_rectangle delta_x delta_y _ (Leaf_widget next_id (Rectangle window_id red green blue alpha left right up down x y width height))=Leaf_widget next_id (Rectangle window_id red green blue alpha (left+delta_x) (right+delta_x) (up+delta_y) (down+delta_y) (x+delta_x) (y+delta_y) width height)
move_rectangle _ _ _ _=error "move_rectangle: error 1"

move_picture::FCT.CInt->FCT.CInt->Engine a->Combined_widget a->Combined_widget a
move_picture delta_x delta_y _ (Leaf_widget next_id (Picture window_id texture render_flip angle design_x design_y width_multiply width_divide height_multiply height_divide origin_width origin_height x y width height))=Leaf_widget next_id (Picture window_id texture render_flip angle (design_x+delta_x) (design_y+delta_y) width_multiply width_divide height_multiply height_divide origin_width origin_height (x+delta_x) (y+delta_y) width height)
move_picture _ _ _ _=error "move_picture: error 1"

move_text::FCT.CInt->FCT.CInt->Engine a->Combined_widget a->Combined_widget a
move_text delta_x delta_y _ (Leaf_widget next_id (Text window_id row max_row render select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row))=Leaf_widget next_id (Text window_id row max_row render select find design_delta_height (design_left+delta_x) (design_right+delta_x) (design_up+delta_y) (design_down+delta_y) delta_height (left+delta_x) (right+delta_x) (up+delta_y) (down+delta_y) seq_paragraph seq_row)
move_text _ _ _ _=error "move_text: error 1"

move_editor::FCT.CInt->FCT.CInt->Engine a->Combined_widget a->Combined_widget a
move_editor delta_x delta_y _ (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size render path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down cursor seq_seq_char))=Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size render path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height (design_x+delta_x) (design_y+delta_y) design_extra_width design_extra_height font_height block_width delta_height (x+delta_x) (y+delta_y) (left+delta_x) (right+delta_x) (up+delta_y) (down+delta_y) cursor seq_seq_char)
move_editor _ _ _ _=error "move_editor: error 1"