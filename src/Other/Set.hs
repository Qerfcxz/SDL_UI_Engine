{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Other.Set where
import Type
import qualified Data.IntMap.Strict as DIS
import qualified Data.Set as DSet

set_engine_key::DSet.Set Key->Engine a->Engine a
set_engine_key key (Engine widget window window_map request _ main_id start_id count_id time)=Engine widget window window_map request key main_id start_id count_id time

set_engine_widget::DIS.IntMap (DIS.IntMap (Combined_widget a))->Engine a->Engine a
set_engine_widget widget (Engine _ window window_map request key main_id start_id count_id time)=Engine widget window window_map request key main_id start_id count_id time

set_engine_window::DIS.IntMap Window->Engine a->Engine a
set_engine_window window (Engine widget _ window_map request key main_id start_id count_id time)=Engine widget window window_map request key main_id start_id count_id time