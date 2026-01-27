{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Collector where
import Other.Error
import Other.Get
import Type
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS

collect::DS.Seq Int->Int->Request a->Engine a->Engine a
collect seq_id index intmap_request (Engine widget window window_map request key main_id start_id count_id time)=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in Engine (error_update_update "collect: error 1" "collect: error 2" combined_id single_id (collect_a index intmap_request) widget) window window_map request key main_id start_id count_id time

collect_a::Int->Request a->Combined_widget a->Combined_widget a
collect_a index intmap_request (Leaf_widget next_id (Collector request))=Leaf_widget next_id (Collector (error_insert "collect_a: error 1" index intmap_request request))
collect_a _ _ _=error "collect_a: error 1"

deliver::DS.Seq Int->(Int->Request a->Bool)->Engine a->Engine a
deliver seq_id judge (Engine widget window window_map request key main_id start_id count_id time)=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in let (seq_request,new_widget)=DIS.alterF (deliver_a single_id judge) combined_id widget in Engine new_widget window window_map (request DS.>< seq_request) key main_id start_id count_id time

deliver_a::Int->(Int->Request a->Bool)->Maybe (DIS.IntMap (Combined_widget a))->(DS.Seq (Request a),Maybe (DIS.IntMap (Combined_widget a)))
deliver_a _ _ Nothing=error "deliver_a: error 1"
deliver_a single_id judge (Just intmap_combined_widget)=let (seq_request,new_intmap_combined_widget)=DIS.alterF (deliver_b judge) single_id intmap_combined_widget in (seq_request,Just new_intmap_combined_widget)

deliver_b::(Int->Request a->Bool)->Maybe (Combined_widget a)->(DS.Seq (Request a),Maybe (Combined_widget a))
deliver_b judge (Just (Leaf_widget next_id (Collector intmap_request)))=let (intmap_request_true,intmap_request_false)=DIS.partitionWithKey judge intmap_request in (DIS.foldl' (DS.|>) DS.empty intmap_request_true,Just (Leaf_widget next_id (Collector intmap_request_false)))
deliver_b _ _=error "deliver_b: error 1"

deliver_with_retain::DS.Seq Int->(Int->Request a->Bool)->Engine a->Engine a
deliver_with_retain seq_id judge (Engine widget window window_map request key main_id start_id count_id time)=case get_widget_widget seq_id start_id widget of
    Leaf_widget _ (Collector intmap_request)->Engine widget window window_map (DIS.foldlWithKey' (\seq_request index this_request->if judge index this_request then seq_request DS.|> this_request else seq_request) request intmap_request) key main_id start_id count_id time
    _->error "deliver_with_retain: error 1"

deliver_with_flush::DS.Seq Int->(Int->Request a->Bool)->Engine a->Engine a
deliver_with_flush seq_id judge (Engine widget window window_map request key main_id start_id count_id time)=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in let (intmap_request,new_widget)=DIS.alterF (deliver_with_flush_a single_id) combined_id widget in Engine new_widget window window_map (DIS.foldlWithKey' (\seq_request index this_request->if judge index this_request then seq_request DS.|> this_request else seq_request) request intmap_request) key main_id start_id count_id time
deliver_with_flush_a::Int->Maybe (DIS.IntMap (Combined_widget a))->(DIS.IntMap (Request a),Maybe (DIS.IntMap (Combined_widget a)))
deliver_with_flush_a _ Nothing=error "deliver_with_flush_a: error 1"
deliver_with_flush_a single_id (Just intmap_combined_widget)=let (intmap_request,new_intmap_combined_widget)=DIS.alterF deliver_with_flush_b single_id intmap_combined_widget in (intmap_request,Just new_intmap_combined_widget)

deliver_with_flush_b::Maybe (Combined_widget a)->(DIS.IntMap (Request a),Maybe (Combined_widget a))
deliver_with_flush_b (Just (Leaf_widget next_id (Collector intmap_request)))=(intmap_request,Just (Leaf_widget next_id (Collector DIS.empty)))
deliver_with_flush_b _=error "deliver_with_flush_b:error 1"