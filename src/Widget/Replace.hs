{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Widget.Replace where
import Widget.Create
import Widget.Remove
import Type
import qualified Data.IntMap.Strict as DIS

replace_widget::Data a=>Int->Int->Combined_widget_request a->Engine a->IO (Engine a)
replace_widget combined_id single_id combined_widget_request (Engine widget window window_map request key main_id start_id count_id time)=do
    (intmap_combined_widget,new_widget)<-remove_widget_a combined_id single_id widget
    (new_count_id,new_new_widget)<-create_widget_a count_id combined_id single_id start_id window combined_widget_request (DIS.insert combined_id intmap_combined_widget new_widget)
    return (Engine new_new_widget window window_map request key main_id start_id new_count_id time)