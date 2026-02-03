{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Underlying where
import Other.Get
import Other.Update
import Type
import Data.Sequence as DS
import qualified Foreign.C.Types as FCT

move_rectangle::FCT.CInt->FCT.CInt->DS.Seq Int->Engine a->Engine a
move_rectangle move_x move_y seq_id engine=update_combined_widget seq_id (move_rectangle_combined_widget move_x move_y engine) engine

move_picture::FCT.CInt->FCT.CInt->DS.Seq Int->Engine a->Engine a
move_picture move_x move_y seq_id engine=update_combined_widget seq_id (move_picture_combined_widget move_x move_y engine) engine

move_text::FCT.CInt->FCT.CInt->DS.Seq Int->Engine a->Engine a
move_text move_x move_y seq_id engine=update_combined_widget seq_id (move_text_combined_widget move_x move_y engine) engine

move_editor::FCT.CInt->FCT.CInt->DS.Seq Int->Engine a->Engine a
move_editor move_x move_y seq_id engine=update_combined_widget seq_id (move_editor_combined_widget move_x move_y engine) engine

move_rectangle_combined_widget::FCT.CInt->FCT.CInt->Engine a->Combined_widget a->Combined_widget a
move_rectangle_combined_widget move_x move_y engine (Leaf_widget next_id (Rectangle window_id red green blue alpha design_left design_right design_up design_down _ _ width height))=let (window_x,window_y,design_size,size)=get_adaptive window_id engine in let new_left=design_left+move_x in let new_right=design_right+move_x in let new_up=design_up+move_y in let new_down=design_down+move_y in Leaf_widget next_id (Rectangle window_id red green blue alpha new_left new_right new_up new_down (window_x+div (new_left*size) design_size) (window_y+div (new_up*size) design_size) width height)
move_rectangle_combined_widget _ _ _ _=error "move_rectangle_combined_widget: error 1"

move_picture_combined_widget::FCT.CInt->FCT.CInt->Engine a->Combined_widget a->Combined_widget a
move_picture_combined_widget move_x move_y engine (Leaf_widget next_id (Picture window_id texture render_flip angle design_x design_y width_multiply width_divide height_multiply height_divide original_width original_height _ _ width height))=let (window_x,window_y,design_size,size)=get_adaptive window_id engine in let new_x=design_x+move_x in let new_y=design_y+move_y in Leaf_widget next_id (Picture window_id texture render_flip angle new_x new_y width_multiply width_divide height_multiply height_divide original_width original_height (window_x+div ((new_x-div (div (width*width_multiply) width_divide) 2)*size) design_size) (window_y+div ((new_y-div (div (height*height_multiply) height_divide) 2)*size) design_size) width height)
move_picture_combined_widget _ _ _ _=error "move_picture_combined_widget: error 1"

move_text_combined_widget::FCT.CInt->FCT.CInt->Engine a->Combined_widget a->Combined_widget a
move_text_combined_widget move_x move_y engine (Leaf_widget next_id (Text window_id row max_row render select find design_delta_height design_left design_right design_up design_down delta_height _ _ _ _ seq_paragraph seq_row))=let (window_x,window_y,design_size,size)=get_adaptive window_id engine in let new_left=design_left+move_x in let new_right=design_right+move_x in let new_up=design_up+move_y in let new_down=design_down+move_y in Leaf_widget next_id (Text window_id row max_row render select find design_delta_height new_left new_right new_up new_down delta_height (window_x+div (new_left*size) design_size) (window_x+div (new_right*size) design_size) (window_y+div (new_up*size) design_size) (window_y+div (new_down*size) design_size) seq_paragraph seq_row)
move_text_combined_widget _ _ _ _=error "move_text_combined_widget: error 1"

move_editor_combined_widget::FCT.CInt->FCT.CInt->Engine a->Combined_widget a->Combined_widget a
move_editor_combined_widget move_x move_y engine (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size render path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height _ _ _ _ _ _ cursor seq_seq_char))=let (window_x,window_y,design_size,size)=get_adaptive window_id engine in let new_x=design_x+move_x in let new_y=design_y+move_y in let half_width=div (fromIntegral block_number*block_width) 2 in let half_height=div (div (height*size) design_size) 2 in Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size render path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height new_x new_y design_extra_width design_extra_height font_height block_width delta_height (window_x+div (new_x*size) design_size-half_width) (window_y+div (new_y*size) design_size-half_height) (window_x+div ((new_x-design_extra_width)*size) design_size-half_width) (window_x+div ((new_x+design_extra_width)*size) design_size+half_width) (window_y+div ((new_y-design_extra_height)*size) design_size-half_height) (window_y+div ((new_y+design_extra_height)*size) design_size+half_height) cursor seq_seq_char)
move_editor_combined_widget _ _ _ _=error "move_editor_combined_widget: error 1"