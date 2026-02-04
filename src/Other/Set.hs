{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Other.Set where
import Type
import qualified Data.IntMap.Strict as DIS
import qualified Data.Set as DSet
import qualified GHC.Stack as GS

set_render_combined_widget::GS.HasCallStack=>Bool->Combined_widget a->Combined_widget a
set_render_combined_widget render (Leaf_widget next_id (Text window_id row max_row _ select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row))=Leaf_widget next_id (Text window_id row max_row render select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row)
set_render_combined_widget render (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size _ path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down cursor seq_seq_char))=Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size render path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down cursor seq_seq_char)
set_render_combined_widget _ _=error "set_render_combined_widget: error 1"

set_engine_key::DSet.Set Key->Engine a->Engine a
set_engine_key key (Engine widget window window_map request _ main_id start_id count_id time)=Engine widget window window_map request key main_id start_id count_id time

set_engine_widget::DIS.IntMap (DIS.IntMap (Combined_widget a))->Engine a->Engine a
set_engine_widget widget (Engine _ window window_map request key main_id start_id count_id time)=Engine widget window window_map request key main_id start_id count_id time

set_engine_window::DIS.IntMap Window->Engine a->Engine a
set_engine_window window (Engine widget _ window_map request key main_id start_id count_id time)=Engine widget window window_map request key main_id start_id count_id time