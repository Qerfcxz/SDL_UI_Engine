{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Instruction where
import Type
import Underlying

from_widget_instruction::Instruction->Engine a->(Combined_widget a->IO (Combined_widget a))
from_widget_instruction (Move_render_rectangle delta_x delta_y) engine=return.move_rectangle delta_x delta_y engine
from_widget_instruction (Move_render_picture delta_x delta_y) engine=return.move_picture delta_x delta_y engine
from_widget_instruction (Move_render_text delta_x delta_y) engine=return.move_text delta_x delta_y engine
from_widget_instruction (Move_render_editor delta_x delta_y) engine=return.move_editor delta_x delta_y engine

create_widget_instruction::Instruction->Engine a->Combined_widget_request a->IO (Combined_widget_request a)
create_widget_instruction _ _=return

create_window_instruction::Instruction->Engine a->Mix_create_window->IO Mix_create_window
create_window_instruction _ _ (Mix_create_window window_id window_name left right up down)=return (Mix_create_window window_id window_name left right up down)

remove_window_instruction::Instruction->Int->IO Int
remove_window_instruction _=return