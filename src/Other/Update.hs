{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Other.Update where
import Other.Error
import Other.Get
import Type
import qualified Data.Sequence as DS

update_engine_main_id::((Engine a->Event->Int)->Engine a->Event->Int)->Engine a->Engine a
update_engine_main_id update (Engine widget window window_map request key main_id start_id count_id time)=Engine widget window window_map request key (update main_id) start_id count_id time

update_main_id::((Engine a->Event->Int)->Engine a->Event->Int)->DS.Seq Int->Engine a->Engine a
update_main_id update seq_id (Engine widget window window_map request key main_id start_id count_id time)=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in Engine (error_update_update "update_main_id: error 1" "update_main_id: error 2" combined_id single_id (update_main_id_a update) widget) window window_map request key main_id start_id count_id time

update_main_id_a::((Engine a->Event->Int)->Engine a->Event->Int)->Combined_widget a->Combined_widget a
update_main_id_a update (Node_widget next_id main_id event_transform request_transform combined_id)=Node_widget next_id (update main_id) event_transform request_transform combined_id
update_main_id_a _ _=error ""

update_next_id::((Engine a->Event->Id)->Engine a->Event->Id)->DS.Seq Int->Engine a->Engine a
update_next_id update seq_id (Engine widget window window_map request key main_id start_id count_id time)=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in Engine (error_update_update "update_next_id: error 1" "update_next_id: error 2" combined_id single_id (update_next_id_a update) widget) window window_map request key main_id start_id count_id time

update_next_id_a::((Engine a->Event->Id)->Engine a->Event->Id)->Combined_widget a->Combined_widget a
update_next_id_a update (Leaf_widget next_id single_widget)=Leaf_widget (update next_id) single_widget
update_next_id_a update (Node_widget next_id main_id event_transform request_transform combined_id)=Node_widget (update next_id) main_id event_transform request_transform combined_id