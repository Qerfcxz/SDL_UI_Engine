{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Underlying where
import Other.Get
import Other.Update
import Type
import Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified GHC.Stack as GS

move_rectangle::FCT.CInt->FCT.CInt->DS.Seq Int->Engine a->Engine a
move_rectangle move_x move_y seq_id engine=update_combined_widget seq_id (move_rectangle_combined_widget move_x move_y engine) engine

move_rectangle_combined_widget::GS.HasCallStack=>FCT.CInt->FCT.CInt->Engine a->Combined_widget a->Combined_widget a
move_rectangle_combined_widget move_x move_y engine (Leaf_widget next_id (Rectangle window_id red green blue alpha design_left design_right design_up design_down _ _ width height))=let (window_x,window_y,design_size,size)=get_adaptive window_id engine in let new_left=design_left+move_x in let new_right=design_right+move_x in let new_up=design_up+move_y in let new_down=design_down+move_y in Leaf_widget next_id (Rectangle window_id red green blue alpha new_left new_right new_up new_down (window_x+div (new_left*size) design_size) (window_y+div (new_up*size) design_size) width height)
move_rectangle_combined_widget _ _ _ _=error "move_rectangle_combined_widget: error 1"

move_picture::FCT.CInt->FCT.CInt->DS.Seq Int->Engine a->Engine a
move_picture move_x move_y seq_id engine=update_combined_widget seq_id (move_picture_combined_widget move_x move_y engine) engine

move_picture_combined_widget::GS.HasCallStack=>FCT.CInt->FCT.CInt->Engine a->Combined_widget a->Combined_widget a
move_picture_combined_widget move_x move_y engine (Leaf_widget next_id (Picture window_id texture render_flip angle design_x design_y width_multiply width_divide height_multiply height_divide original_width original_height _ _ width height))=let (window_x,window_y,design_size,size)=get_adaptive window_id engine in let new_x=design_x+move_x in let new_y=design_y+move_y in Leaf_widget next_id (Picture window_id texture render_flip angle new_x new_y width_multiply width_divide height_multiply height_divide original_width original_height (window_x+div ((new_x-div (div (width*width_multiply) width_divide) 2)*size) design_size) (window_y+div ((new_y-div (div (height*height_multiply) height_divide) 2)*size) design_size) width height)
move_picture_combined_widget _ _ _ _=error "move_picture_combined_widget: error 1"

move_text::FCT.CInt->FCT.CInt->DS.Seq Int->Engine a->Engine a
move_text move_x move_y seq_id engine=update_combined_widget seq_id (move_text_combined_widget move_x move_y engine) engine

move_text_combined_widget::GS.HasCallStack=>FCT.CInt->FCT.CInt->Engine a->Combined_widget a->Combined_widget a
move_text_combined_widget move_x move_y engine (Leaf_widget next_id (Text window_id row max_row render select find design_delta_height design_left design_right design_up design_down delta_height _ _ _ _ seq_paragraph seq_row text_binding))=let (window_x,window_y,design_size,size)=get_adaptive window_id engine in let new_left=design_left+move_x in let new_right=design_right+move_x in let new_up=design_up+move_y in let new_down=design_down+move_y in Leaf_widget next_id (Text window_id row max_row render select find design_delta_height new_left new_right new_up new_down delta_height (window_x+div (new_left*size) design_size) (window_x+div (new_right*size) design_size) (window_y+div (new_up*size) design_size) (window_y+div (new_down*size) design_size) seq_paragraph seq_row text_binding)
move_text_combined_widget _ _ _ _=error "move_text_combined_widget: error 1"

move_editor::FCT.CInt->FCT.CInt->DS.Seq Int->Engine a->Engine a
move_editor move_x move_y seq_id engine=update_combined_widget seq_id (move_editor_combined_widget move_x move_y engine) engine

move_editor_combined_widget::GS.HasCallStack=>FCT.CInt->FCT.CInt->Engine a->Combined_widget a->Combined_widget a
move_editor_combined_widget move_x move_y engine (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size render path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height design_ime_left design_ime_right design_ime_up design_ime_down font_height block_width delta_height _ _ _ _ _ _ _ _ _ _ cursor seq_seq_char editor_binding))=let (window_x,window_y,design_size,size)=get_adaptive window_id engine in let new_x=design_x+move_x in let new_y=design_y+move_y in let half_width=div (fromIntegral block_number*block_width) 2 in let half_height=div (div (height*size) design_size) 2 in Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size render path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height new_x new_y design_extra_width design_extra_height design_ime_left design_ime_right design_ime_up design_ime_down font_height block_width delta_height (window_x+div (new_x*size) design_size-half_width) (window_y+div (new_y*size) design_size-half_height) (window_x+div ((new_x-design_extra_width)*size) design_size-half_width) (window_x+div ((new_x+design_extra_width)*size) design_size+half_width) (window_y+div ((new_y-design_extra_height)*size) design_size-half_height) (window_y+div ((new_y+design_extra_height)*size) design_size+half_height) (window_x+div ((new_x+design_ime_left)*size) design_size) (window_x+div ((new_x+design_ime_right)*size) design_size) (window_y+div ((new_y+design_ime_up)*size) design_size) (window_y+div ((new_y+design_ime_down)*size) design_size) cursor seq_seq_char editor_binding)
move_editor_combined_widget _ _ _ _=error "move_editor_combined_widget: error 1"

scale_picture::FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq Int->Engine a->Engine a
scale_picture width_multiply width_divide height_multiply height_divide seq_id engine=update_combined_widget seq_id (scale_picture_combined_widget width_multiply width_divide height_multiply height_divide engine) engine

scale_picture_combined_widget::GS.HasCallStack=>FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->Engine a->Combined_widget a->Combined_widget a
scale_picture_combined_widget width_multiply width_divide height_multiply height_divide engine (Leaf_widget next_id (Picture window_id texture render_flip angle design_x design_y _ _ _ _ original_width original_height _ _ _ _))=let (window_x,window_y,design_size,size)=get_adaptive window_id engine in let new_width=div (original_width*width_multiply) width_divide in let new_height=div (original_height*height_multiply) height_divide in Leaf_widget next_id (Picture window_id texture render_flip angle design_x design_y width_multiply width_divide height_multiply height_divide original_width original_height (window_x+div ((design_x-div new_width 2)*size) design_size) (window_y+div ((design_y-div new_height 2)*size) design_size) (div (new_width*size) design_size) (div (new_height*size) design_size))
scale_picture_combined_widget _ _ _ _ _ _=error "scale_picture_combined_widget: error 1"

flip_picture::Flip->DS.Seq Int->Engine a->Engine a
flip_picture render_flip seq_id=update_combined_widget seq_id (flip_picture_combined_widget render_flip)

flip_picture_combined_widget::GS.HasCallStack=>Flip->Combined_widget a->Combined_widget a
flip_picture_combined_widget render_flip (Leaf_widget next_id (Picture window_id texture _ angle design_x design_y width_multiply width_divide height_multiply height_divide original_width original_height x y width height))=Leaf_widget next_id (Picture window_id texture render_flip angle design_x design_y width_multiply width_divide height_multiply height_divide original_width original_height x y width height)
flip_picture_combined_widget _ _=error "flip_picture_combined_widget: error 1"

move_animation::FCT.CInt->FCT.CInt->DS.Seq Int->Engine a->Engine a
move_animation move_x move_y seq_id engine=update_combined_widget seq_id (move_animation_combined_widget move_x move_y engine) engine

move_animation_combined_widget::GS.HasCallStack=>FCT.CInt->FCT.CInt->Engine a->Combined_widget a->Combined_widget a
move_animation_combined_widget move_x move_y engine (Leaf_widget next_id (Animation window_id count frame_index seq_frame render_flip angle design_x design_y width_multiply width_divide height_multiply height_divide))=let (window_x,window_y,design_size,size)=get_adaptive window_id engine in let new_x=design_x+move_x in let new_y=design_y+move_y in Leaf_widget next_id (Animation window_id count frame_index (fmap (\(Frame texture original_width original_height _ _ width height)->let new_width=div (original_width*width_multiply) width_divide in let new_height=div (original_height*height_multiply) height_divide in Frame texture original_width original_height (window_x+div ((new_x-div new_width 2)*size) design_size) (window_y+div ((new_y-div new_height 2)*size) design_size) width height) seq_frame) render_flip angle new_x new_y width_multiply width_divide height_multiply height_divide)
move_animation_combined_widget _ _ _ _=error "move_animation_combined_widget: error 1"

scale_animation::FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq Int->Engine a->Engine a
scale_animation width_multiply width_divide height_multiply height_divide seq_id engine=update_combined_widget seq_id (scale_animation_combined_widget width_multiply width_divide height_multiply height_divide engine) engine

scale_animation_combined_widget::GS.HasCallStack=>FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->Engine a->Combined_widget a->Combined_widget a
scale_animation_combined_widget width_multiply width_divide height_multiply height_divide engine (Leaf_widget next_id (Animation window_id count frame_index seq_frame render_flip angle design_x design_y _ _ _ _))=let (window_x,window_y,design_size,size)=get_adaptive window_id engine in Leaf_widget next_id (Animation window_id count frame_index (fmap (\(Frame texture original_width original_height _ _ _ _)->let new_width=div (original_width*width_multiply) width_divide in let new_height=div (original_height*height_multiply) height_divide in Frame texture original_width original_height (window_x+div ((design_x-div new_width 2)*size) design_size) (window_y+div ((design_y-div new_height 2)*size) design_size) (div (new_width*size) design_size) (div (new_height*size) design_size)) seq_frame) render_flip angle design_x design_y width_multiply width_divide height_multiply height_divide)
scale_animation_combined_widget _ _ _ _ _ _=error "scale_animation_combined_widget: error 1"

flip_animation::Flip->DS.Seq Int->Engine a->Engine a
flip_animation render_flip seq_id=update_combined_widget seq_id (flip_animation_combined_widget render_flip)

flip_animation_combined_widget::GS.HasCallStack=>Flip->Combined_widget a->Combined_widget a
flip_animation_combined_widget render_flip (Leaf_widget next_id (Animation window_id count frame_index seq_frame _ angle design_x design_y width_multiply width_divide height_multiply height_divide))=Leaf_widget next_id (Animation window_id count frame_index seq_frame render_flip angle design_x design_y width_multiply width_divide height_multiply height_divide)
flip_animation_combined_widget _ _=error "flip_animation_combined_widget: error 1"

bind_text::Text_binding->DS.Seq Int->Engine a->Engine a
bind_text text_binding seq_id=update_combined_widget seq_id (bind_text_combined_widget text_binding)

bind_text_combined_widget::GS.HasCallStack=>Text_binding->Combined_widget a->Combined_widget a
bind_text_combined_widget text_binding (Leaf_widget next_id (Text window_id row max_row render select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row _))=Leaf_widget next_id (Text window_id row max_row render select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row text_binding)
bind_text_combined_widget _ _=error "bind_text_combined_widget: error 1"

bind_editor::Editor_binding->DS.Seq Int->Engine a->Engine a
bind_editor editor_binding seq_id=update_combined_widget seq_id (bind_editor_combined_widget editor_binding)

bind_editor_combined_widget::GS.HasCallStack=>Editor_binding->Combined_widget a->Combined_widget a
bind_editor_combined_widget editor_binding (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size render path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height design_ime_left design_ime_right design_ime_up design_ime_down font_height block_width delta_height x y left right up down ime_left ime_right ime_up ime_down cursor seq_seq_char _))=Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size render path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height design_ime_left design_ime_right design_ime_up design_ime_down font_height block_width delta_height x y left right up down ime_left ime_right ime_up ime_down cursor seq_seq_char editor_binding)
bind_editor_combined_widget _ _=error "bind_editor_combined_widget: error 1"

text_color_editor::DW.Word8->DW.Word8->DW.Word8->DW.Word8->DS.Seq Int->Engine a->Engine a
text_color_editor red green blue alpha seq_id=update_combined_widget seq_id (text_color_editor_combined_widget red green blue alpha)

text_color_editor_combined_widget::GS.HasCallStack=>DW.Word8->DW.Word8->DW.Word8->DW.Word8->Combined_widget a->Combined_widget a
text_color_editor_combined_widget red green blue alpha (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size render path find typesetting _ _ _ _ cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height design_ime_left design_ime_right design_ime_up design_ime_down font_height block_width delta_height x y left right up down ime_left ime_right ime_up ime_down cursor seq_seq_char editor_binding))=Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size render path find typesetting red green blue alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height design_ime_left design_ime_right design_ime_up design_ime_down font_height block_width delta_height x y left right up down ime_left ime_right ime_up ime_down cursor seq_seq_char editor_binding)
text_color_editor_combined_widget _ _ _ _ _=error "text_color_editor_combined_widget: error 1"

cursor_color_editor::DW.Word8->DW.Word8->DW.Word8->DW.Word8->DS.Seq Int->Engine a->Engine a
cursor_color_editor red green blue alpha seq_id=update_combined_widget seq_id (cursor_color_editor_combined_widget red green blue alpha)

cursor_color_editor_combined_widget::GS.HasCallStack=>DW.Word8->DW.Word8->DW.Word8->DW.Word8->Combined_widget a->Combined_widget a
cursor_color_editor_combined_widget red green blue alpha (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size render path find typesetting text_red text_green text_blue text_alpha _ _ _ _ select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height design_ime_left design_ime_right design_ime_up design_ime_down font_height block_width delta_height x y left right up down ime_left ime_right ime_up ime_down cursor seq_seq_char editor_binding))=Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size render path find typesetting text_red text_green text_blue text_alpha red green blue alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height design_ime_left design_ime_right design_ime_up design_ime_down font_height block_width delta_height x y left right up down ime_left ime_right ime_up ime_down cursor seq_seq_char editor_binding)
cursor_color_editor_combined_widget _ _ _ _ _=error "cursor_color_editor_combined_widget: error 1"

select_color_editor::DW.Word8->DW.Word8->DW.Word8->DW.Word8->DS.Seq Int->Engine a->Engine a
select_color_editor red green blue alpha seq_id=update_combined_widget seq_id (select_color_editor_combined_widget red green blue alpha)

select_color_editor_combined_widget::GS.HasCallStack=>DW.Word8->DW.Word8->DW.Word8->DW.Word8->Combined_widget a->Combined_widget a
select_color_editor_combined_widget red green blue alpha (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size render path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha _ _ _ _ height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height design_ime_left design_ime_right design_ime_up design_ime_down font_height block_width delta_height x y left right up down ime_left ime_right ime_up ime_down cursor seq_seq_char editor_binding))=Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size render path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha red green blue alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height design_ime_left design_ime_right design_ime_up design_ime_down font_height block_width delta_height x y left right up down ime_left ime_right ime_up ime_down cursor seq_seq_char editor_binding)
select_color_editor_combined_widget _ _ _ _ _=error "select_color_editor_combined_widget: error 1"

goto_animation_combined_widget::GS.HasCallStack=>(Int->Int->Int)->Combined_widget a->Combined_widget a
goto_animation_combined_widget goto (Leaf_widget next_id (Animation window_id count frame_index seq_frame render_flip angle design_x design_y width_multiply width_divide height_multiply height_divide))=Leaf_widget next_id (Animation window_id count (goto count frame_index) seq_frame render_flip angle design_x design_y width_multiply width_divide height_multiply height_divide)
goto_animation_combined_widget _ _=error "goto_animation_combined_widget: error 1"