{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Data where
import Other.Error
import Other.Get
import Type
import qualified Data.Sequence as DS

get_data::Data a=>DS.Seq Int->Engine a->a
get_data seq_id (Engine widget _ _ _ _ start_id _)=case get_widget_widget seq_id start_id widget of
    Leaf_widget _ (Data content)->content
    _->error "get_data: error 1"

set_data::Data a=>a->DS.Seq Int->Engine a->Engine a
set_data content seq_id (Engine widget window window_map request count_id start_id main_id)=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in Engine (error_update_update "set_data: error 1" "set_data: error 2" combined_id single_id (set_data_a content) widget) window window_map request count_id start_id main_id

set_data_a::a->Combined_widget a->Combined_widget a
set_data_a content (Leaf_widget next_id (Data _))=Leaf_widget next_id (Data content)
set_data_a _ _=error "set_data_a: error 1"

update_data::Data a=>(a->a)->DS.Seq Int->Engine a->Engine a
update_data update seq_id (Engine widget window window_map request count_id start_id main_id)=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in Engine (error_update_update "set_data: error 1" "set_data: error 2" combined_id single_id (update_data_a update) widget) window window_map request count_id start_id main_id

update_data_a::(a->a)->Combined_widget a->Combined_widget a
update_data_a update (Leaf_widget next_id (Data content))=Leaf_widget next_id (Data (update content))
update_data_a _ _=error "set_data_a: error 1"