{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Widget.Replace where
import Other.Error
import Widget.Create
import Widget.Remove
import Type
import qualified Data.Foldable as DF
import qualified Data.IntMap.Strict as DIS

replace_widget::Data a=>((Combined_widget_request a,Combined_widget a)->IO (Combined_widget_request a,Combined_widget a))->Combined_widget_request a->Int->Int->Engine a->IO (Engine a)
replace_widget transform combined_widget_request combined_id single_id (Engine widget window window_map request key main_id start_id count_id time)=let (combined_widget,_,new_widget)=error_remove_remove "replace_widget: error 1" "replace_widget: error 2" combined_id single_id widget in do
    (new_combined_widget_request,new_combined_widget)<-transform (combined_widget_request,combined_widget)
    new_new_widget<-replace_widget_a new_combined_widget new_widget
    (new_count_id,new_new_new_widget)<-create_widget_a count_id combined_id single_id start_id window new_combined_widget_request new_new_widget
    return (Engine new_new_new_widget window window_map request key main_id start_id new_count_id time)

replace_widget_a::Data a=>Combined_widget a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))
replace_widget_a (Leaf_widget _ single_widget) widget=do
    remove_single_widget single_widget
    return widget
replace_widget_a (Node_widget _ _ _ _ combined_id) widget=let (intmap_combined_widget,new_widget)=error_remove "replace_widget_a: error 1" combined_id widget in DF.foldlM (flip replace_widget_a) new_widget intmap_combined_widget