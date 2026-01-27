{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Widget.Replace where
import Widget.Create
import Widget.Remove
import Other.Error
import Type
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS

direct_replace_widget::Data a=>Int->Int->Combined_widget_request a->Engine a->IO (Engine a)
direct_replace_widget combined_id single_id combined_widget_request (Engine widget window window_map request key main_id start_id count_id time)=do
    (intmap_combined_widget,new_widget)<-remove_widget_top combined_id single_id widget
    (new_count_id,new_new_widget)<-create_widget_top count_id combined_id single_id start_id window combined_widget_request (DIS.insert combined_id intmap_combined_widget new_widget)
    return (Engine new_new_widget window window_map request key main_id start_id new_count_id time)

replace_widget::Data a=>DS.Seq Int->Combined_widget_request a->Engine a->IO (Engine a)
replace_widget seq_id combined_widget_request (Engine widget window window_map request key main_id start_id count_id time)=case seq_id of
    DS.Empty->error "replace_widget: error 1"
    (single_id DS.:<| other_seq_id)->do
        (new_count_id,new_widget)<-replace_widget_a other_seq_id count_id start_id single_id start_id window combined_widget_request widget
        return (Engine new_widget window window_map request key main_id start_id new_count_id time)

replace_widget_a::Data a=>DS.Seq Int->Int->Int->Int->Int->DIS.IntMap Window->Combined_widget_request a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))
replace_widget_a seq_id count_id combined_id single_id start_id window combined_widget_request widget=case seq_id of
    DS.Empty->do
        (intmap_combined_widget,new_widget)<-remove_widget_top combined_id single_id widget
        create_widget_top count_id combined_id single_id start_id window combined_widget_request (DIS.insert combined_id intmap_combined_widget new_widget)
    (new_single_id DS.:<| other_seq_id)->case error_lookup_lookup "replace_widget_a: error 1" "replace_widget_a: error 2" combined_id single_id widget of
        Leaf_widget _ _->error "replace_widget_a: error 3"
        Node_widget _ _ _ _ new_combined_id->replace_widget_a other_seq_id count_id new_combined_id new_single_id start_id window combined_widget_request widget