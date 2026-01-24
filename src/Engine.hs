{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Engine where
import Other.Error
import Other.Get
import Widget.Remove
import Event
import Request
import Timer
import Type
import qualified Data.Foldable as DF
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.C.String as FCS
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Ptr as FP
import qualified SDL.Raw.Basic as SRB
import qualified SDL.Raw.Enum as SRE
import qualified SDL.Raw.Event as SRE
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

init_engine::IO ()
init_engine=do
    catch_error "init_engine: error 1" 0 (SRB.init SRE.SDL_INIT_EVERYTHING)
    catch_error "init_engine: error 2" 0 SRF.init
    catch_error "init_engine: error 3" True (FCS.withCString "SDL_IME_SHOW_UI" (FCS.withCString "1" . SRB.setHint))

quit_engine::IO ()
quit_engine=do
    SRF.quit
    SRB.quit

create_engine::Int->Int->Engine a
create_engine start_id=Engine (DIS.singleton start_id DIS.empty) DIS.empty DIS.empty DS.empty (start_id+1) start_id

run_engine::Data a=>Maybe DW.Word32->Engine a->IO ()
run_engine timer_setting engine=do
    case timer_setting of 
        Nothing->FMA.alloca (`loop_engine` engine)
        Just time->do
            time_event_type<-SRE.registerEvents 1
            timer<-add_timer time_event_type time
            FMA.alloca (\event->loop_engine_time time_event_type event engine)
            remove_timer timer

loop_engine_time::Data a=>DW.Word32->FP.Ptr SRT.Event->Engine a->IO()
loop_engine_time time_event_type event engine=do
    new_engine@(Engine _ _ new_window_map _ _ new_start_id new_main_id)<-run_request engine
    this_event<-get_event event new_window_map (Just time_event_type)
    case this_event of
        Quit->clean_engine new_engine
        _->loop_engine_time time_event_type event (run_event new_start_id new_main_id (DS.singleton new_main_id) this_event new_engine)

loop_engine::Data a=>FP.Ptr SRT.Event->Engine a->IO()
loop_engine event engine=do
    new_engine@(Engine _ _ new_window_map _ _ new_start_id new_main_id)<-run_request engine
    this_event<-get_event event new_window_map Nothing
    case this_event of
        Quit->clean_engine new_engine
        _->loop_engine event (run_event new_start_id new_main_id (DS.singleton new_main_id) this_event new_engine)

run_request::Data a=>Engine a->IO (Engine a)
run_request (Engine widget window window_map request count_id start_id main_id)=case request of
    DS.Empty->return (Engine widget window window_map DS.Empty count_id start_id main_id)
    (this_request DS.:<| other_request)->do
        new_engine<-do_request this_request (Engine widget window window_map other_request count_id start_id main_id)
        run_request new_engine

run_event::Data a=>Int->Int->DS.Seq Int->Event->Engine a->Engine a
run_event start_id main_id single_id_history event engine@(Engine widget _ _ _ _ _ _)=case DIS.lookup start_id widget of
    Nothing->error "run_event: error 1"
    Just intmap_combined_widget->run_event_a start_id main_id single_id_history intmap_combined_widget event engine

--涉及到run_event_b之后重新在combined_widget获得最新的Next_id的问题
run_event_a::Data a=>Int->Int->DS.Seq Int->DIS.IntMap (Combined_widget a)->Event->Engine a->Engine a
run_event_a combined_id single_id single_id_history intmap_combined_widget event engine=case DIS.lookup single_id intmap_combined_widget of
    Nothing->error "run_event_a: error 1"
    Just combined_widget->let new_engine@(Engine new_widget _ _ _ _ _ _)=run_event_b combined_widget event engine in case get_next_id combined_widget event new_engine of
        End->new_engine
        Goto new_single_id->case DIS.lookup combined_id new_widget of
            Nothing->error "run_event_a: error 2"
            Just new_intmap_combined_widget->run_event_a combined_id new_single_id (single_id_history DS.|> new_single_id) new_intmap_combined_widget event new_engine
        Back number->case DIS.lookup combined_id new_widget of
            Nothing->error "run_event_a: error 3"
            Just new_intmap_combined_widget->let max_index=DS.length single_id_history-1 in if number<0 || max_index<number then error "run_event_a: error 4" else let new_single_id=DS.index single_id_history (max_index-number) in run_event_a combined_id new_single_id (single_id_history DS.|> new_single_id) new_intmap_combined_widget event new_engine

run_event_b::Data a=>Combined_widget a->Event->Engine a->Engine a
run_event_b (Leaf_widget _ widget) event engine=run_widget event widget engine
run_event_b (Node_widget _ main_single_id combined_id) event engine=run_event combined_id main_single_id (DS.singleton main_single_id) event engine

run_widget::Data a=>Event->Single_widget a->Engine a->Engine a
run_widget _ (Label_data _) engine=engine
run_widget _ (Bool_data _) engine=engine
run_widget _ (Int_data _) engine=engine
run_widget _ (Data _) engine=engine
run_widget event (Trigger handle) engine=handle event engine
run_widget event (Io_trigger handle) engine=create_request (Io_request (handle event)) engine
run_widget _ (Font _) engine=engine
run_widget _ (Block_font {}) engine=engine
run_widget _ (Rectangle {}) engine=engine
run_widget _ (Picture {}) engine=engine
run_widget _ (Text {}) engine=engine
run_widget _ (Editor {}) engine=engine

clean_engine::Data a=>Engine a->IO ()
clean_engine (Engine widget window _ _ _ _ _)=do
    DF.mapM_ (DF.mapM_ clean_widget) widget
    DF.mapM_ clean_window window

clean_widget::Data a=>Combined_widget a->IO ()
clean_widget (Leaf_widget _ single_widget)=remove_single_widget single_widget
clean_widget (Node_widget {})=return ()

clean_window::Window->IO ()
clean_window (Window _ window renderer _ _ _ _ _ _)=do
    SRV.destroyRenderer renderer
    SRV.destroyWindow window