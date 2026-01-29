{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Other.Update where
import Type
import qualified Data.IntMap.Strict as DIS

update_engine_widget_io::(DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (DIS.IntMap (Combined_widget a))))->Engine a->IO (Engine a)
update_engine_widget_io update (Engine widget window window_map request key main_id start_id count_id time)=do
    new_widget<-update widget
    return (Engine new_widget window window_map request key main_id start_id count_id time)

update_engine_window::(DIS.IntMap Window->DIS.IntMap Window)->Engine a->Engine a
update_engine_window update (Engine widget window window_map request key main_id start_id count_id time)=Engine widget (update window) window_map request key main_id start_id count_id time

update_engine_window_map::(DIS.IntMap Int->DIS.IntMap Int)->Engine a->Engine a
update_engine_window_map update (Engine widget window window_map request key main_id start_id count_id time)=Engine widget window (update window_map) request key main_id start_id count_id time