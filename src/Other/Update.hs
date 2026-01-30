{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Other.Update where
import Other.Error
import Other.Get
import Type
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS

update_engine_widget_io::(DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (DIS.IntMap (Combined_widget a))))->Engine a->IO (Engine a)
update_engine_widget_io update (Engine widget window window_map request key main_id start_id count_id time)=do
    new_widget<-update widget
    return (Engine new_widget window window_map request key main_id start_id count_id time)

update_combined_widget::DS.Seq Int->(Combined_widget a->Combined_widget a)->Engine a->Engine a
update_combined_widget seq_id update (Engine widget window window_map request key main_id start_id count_id time)=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in Engine (error_update_update "update_combined_widget: error 1" "update_combined_widget: error 2" combined_id single_id update widget) window window_map request key main_id start_id count_id time

update_engine_window::(DIS.IntMap Window->DIS.IntMap Window)->Engine a->Engine a
update_engine_window update (Engine widget window window_map request key main_id start_id count_id time)=Engine widget (update window) window_map request key main_id start_id count_id time

update_engine_window_map::(DIS.IntMap Int->DIS.IntMap Int)->Engine a->Engine a
update_engine_window_map update (Engine widget window window_map request key main_id start_id count_id time)=Engine widget window (update window_map) request key main_id start_id count_id time