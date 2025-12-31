{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Engine where
import Event
import Other
import Request
import Timer
import Type
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified SDL.Raw.Basic as SRB
import qualified SDL.Raw.Enum as SRE
import qualified SDL.Raw.Event as SRE
import qualified SDL.Raw.Font as SRF

init_engine::IO ()
init_engine=do
    catch_error "init_engine: SDL.Raw.init returns error" 0 (SRB.init SRE.SDL_INIT_EVERYTHING)
    catch_error "init_engine: SDL.Raw.Font.init returns error" 0 SRF.init

quit_engine::IO ()
quit_engine=do
    SRF.quit
    SRB.quit

create_engine::Int->Int->Engine a
create_engine start_id=Engine (DIS.singleton start_id DIS.empty) DIS.empty DIS.empty DS.empty (start_id+1) start_id

run_engine::Data a=>Maybe DW.Word32->Engine a->IO ()
run_engine timer_setting engine=do
    case timer_setting of 
        Nothing->loop_engine engine
        Just time->do
            time_event_type<-SRE.registerEvents 1
            timer<-add_timer time_event_type time
            loop_engine_time time_event_type engine
            remove_timer timer

loop_engine_time::Data a=>DW.Word32->Engine a->IO()
loop_engine_time time_event_type (Engine widget window window_map request count_id start_id main_id)=do
    new_engine@(Engine _ _ new_window_map _ _ new_start_id new_main_id)<-run_request request (Engine widget window window_map DS.Empty count_id start_id main_id)
    event<-get_event new_window_map (Just time_event_type)
    case event of
        Quit->clean_engine new_engine
        _->loop_engine_time time_event_type (run_event new_start_id new_main_id (DS.singleton new_main_id) event new_engine)

loop_engine::Data a=>Engine a->IO()
loop_engine (Engine widget window window_map request count_id start_id main_id)=do
    new_engine@(Engine _ _ new_window_map _ _ new_start_id new_main_id)<-run_request request (Engine widget window window_map DS.Empty count_id start_id main_id)
    event<-get_event new_window_map Nothing
    case event of
        Quit->clean_engine new_engine
        _->loop_engine (run_event new_start_id new_main_id (DS.singleton new_main_id) event new_engine)

run_request::Data a=>DS.Seq (Request a)->Engine a->IO (Engine a)
run_request DS.Empty engine=return engine
run_request (request DS.:<| other_request) engine=do
    new_engine<-do_request request engine
    run_request other_request new_engine


run_event::Data a=>Int->Int->DS.Seq Int->Event->Engine a->Engine a
run_event start_id main_id single_id_history event engine@(Engine widget _ _ _ _ _ _)=case DIS.lookup start_id widget of
    Nothing->error "run_event: you changed start_id without proper design"
    Just intmap_combined_widget->run_event_a start_id main_id single_id_history intmap_combined_widget event engine

--涉及到run_event_b之后重新在combined_widget获得最新的Next_id的问题
run_event_a::Data a=>Int->Int->DS.Seq Int->DIS.IntMap (Combined_widget a)->Event->Engine a->Engine a
run_event_a combined_id single_id single_id_history intmap_combined_widget event engine=case DIS.lookup single_id intmap_combined_widget of
    Nothing->error "run_event_a: No such single_id"
    Just combined_widget->let new_engine@(Engine new_widget _ _ _ _ _ _)=run_event_b combined_widget event engine in case get_next_id combined_widget new_engine of
        End->new_engine
        Goto new_single_id->case DIS.lookup combined_id new_widget of
            Nothing->error "run_event_a: you changed combined_id without proper design"
            Just new_intmap_combined_widget->run_event_a combined_id new_single_id (single_id_history DS.|> new_single_id) new_intmap_combined_widget event new_engine
        Back number->case DIS.lookup combined_id new_widget of
            Nothing->error "run_event_a: you changed combined_id without proper design"
            Just new_intmap_combined_widget->let max_index=DS.length single_id_history-1 in if number<0 || max_index<number then error "run_event_a: Back number out of range" else let new_single_id=DS.index single_id_history (max_index-number) in run_event_a combined_id new_single_id (single_id_history DS.|> new_single_id) new_intmap_combined_widget event new_engine

run_event_b::Data a=>Combined_widget a->Event->Engine a->Engine a
run_event_b (Leaf_widget _ widget) event engine=run_widget event widget engine
run_event_b (Node_widget _ main_single_id combined_id) event engine=run_event combined_id main_single_id (DS.singleton main_single_id) event engine

run_widget::Data a=>Event->Single_widget a->Engine a->Engine a
run_widget event (Trigger handle) engine=handle event engine
run_widget event (Io_trigger hendle) engine=create_request (Io_request (hendle event)) engine
run_widget _ (Data _) engine=engine
run_widget _ (Font _) engine=engine
run_widget _ (Rectangle {}) engine=engine
run_widget _ (Picture {}) engine=engine
run_widget _ (Text {}) engine=engine

clean_engine::Engine a->IO ()
clean_engine engine=return()--未完待续