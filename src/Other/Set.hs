{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Other.Set where
import Other.Error
import Other.Get
import Type
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet

set_key::DSet.Set Key->Engine a->Engine a
set_key key (Engine widget window window_map request _ count_id start_id main_id)=Engine widget window window_map request key count_id start_id main_id

set_engine_main_id::Int->Engine a->Engine a
set_engine_main_id main_id (Engine widget window window_map request key count_id start_id _)=Engine widget window window_map request key count_id start_id main_id

set_main_id::Int->DSeq.Seq Int->Engine a->Engine a
set_main_id this_main_id seq_id (Engine widget window window_map request key count_id start_id main_id)=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in Engine (error_update_update "set_main_id: error 1" "set_main_id: error 2" combined_id single_id (set_main_id_a this_main_id) widget) window window_map request key count_id start_id main_id

set_main_id_a::Int->Combined_widget a->Combined_widget a
set_main_id_a main_id (Node_widget next_id _ combined_id)=Node_widget next_id main_id combined_id
set_main_id_a _ _=error "set_main_id_a: error 1"

set_next_id::(Event->Engine a->Id)->DSeq.Seq Int->Engine a->Engine a
set_next_id next_id seq_id (Engine widget window window_map request key count_id start_id main_id)=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in Engine (error_update_update "set_next_id: error 1" "set_next_id: error 2" combined_id single_id (set_next_id_a next_id) widget) window window_map request key count_id start_id main_id

set_next_id_a::(Event->Engine a->Id)->Combined_widget a->Combined_widget a
set_next_id_a next_id (Leaf_widget _ single_widget)=Leaf_widget next_id single_widget
set_next_id_a next_id (Node_widget _ main_id combined_id)=Node_widget next_id main_id combined_id