{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Engine where
import Other.Error
import Other.Get
import Other.Set
import Widget.Remove
import Event
import Request
import Timer
import Type
import qualified Data.Foldable as DF
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet
import qualified Data.Word as DW
import qualified Foreign.C.String as FCS
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Ptr as FP
import qualified SDL.Raw.Basic as SRB
import qualified SDL.Raw.Enum as SREn
import qualified SDL.Raw.Event as SREv
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

init_engine::IO ()
init_engine=do
    catch_error "init_engine: error 1" 0 (SRB.init SREn.SDL_INIT_EVERYTHING)
    catch_error "init_engine: error 2" 0 SRF.init
    catch_error "init_engine: error 3" True (FCS.withCString "SDL_IME_SHOW_UI" (FCS.withCString "1" . SRB.setHint))

quit_engine::IO ()
quit_engine=do
    SRF.quit
    SRB.quit

create_engine::(Engine a->Event->Int)->Int->Maybe DW.Word32->Engine a
create_engine main_id start_id=Engine (DIS.singleton start_id DIS.empty) DIS.empty DIS.empty DSeq.empty DSet.empty main_id start_id (start_id+1)

run_engine::Data a=>Engine a->IO ()
run_engine engine@(Engine _ _ _ _ _ _ _ _ time)=do
    case time of 
        Nothing->FMA.alloca (`loop_engine` engine)
        Just this_time->do
            time_event_type<-SREv.registerEvents 1
            timer<-add_timer time_event_type this_time
            FMA.alloca (\event->loop_engine_time time_event_type event engine)
            remove_timer timer

loop_engine_time::Data a=>DW.Word32->FP.Ptr SRT.Event->Engine a->IO()
loop_engine_time time_event_type event engine=do
    new_engine@(Engine _ _ new_window_map _ key new_main_id new_start_id _ _)<-run_request engine
    (maybe_key,this_event)<-get_event event new_window_map key (Just time_event_type)
    case this_event of
        Quit->clean_engine new_engine
        _->case maybe_key of
            Nothing->let calculated_main_id=new_main_id new_engine this_event in loop_engine_time time_event_type event (run_event new_start_id calculated_main_id (DSeq.singleton calculated_main_id) this_event new_engine)
            Just new_key->let calculated_main_id=new_main_id new_engine this_event in loop_engine_time time_event_type event (run_event new_start_id calculated_main_id (DSeq.singleton calculated_main_id) this_event (set_engine_key new_key new_engine))

loop_engine::Data a=>FP.Ptr SRT.Event->Engine a->IO()
loop_engine event engine=do
    new_engine@(Engine _ _ new_window_map _ key new_main_id new_start_id _ _)<-run_request engine
    (maybe_key,this_event)<-get_event event new_window_map key Nothing
    case this_event of
        Quit->clean_engine new_engine
        _->case maybe_key of
            Nothing->let calculated_main_id=new_main_id new_engine this_event in loop_engine event (run_event new_start_id calculated_main_id (DSeq.singleton calculated_main_id) this_event new_engine)
            Just new_key->let calculated_main_id=new_main_id new_engine this_event in loop_engine event (run_event new_start_id calculated_main_id (DSeq.singleton calculated_main_id) this_event (set_engine_key new_key new_engine))

run_request::Data a=>Engine a->IO (Engine a)
run_request (Engine widget window window_map request key main_id start_id count_id time)=case request of
    DSeq.Empty->return (Engine widget window window_map DSeq.Empty key main_id start_id count_id time)
    (this_request DSeq.:<| other_request)->do
        new_engine<-do_request this_request (Engine widget window window_map other_request key main_id start_id count_id time)
        run_request new_engine

run_event::Data a=>Int->Int->DSeq.Seq Int->Event->Engine a->Engine a
run_event start_id main_id single_id_history event engine@(Engine widget _ _ _ _ _ _ _ _)=case DIS.lookup start_id widget of
    Nothing->error "run_event: error 1"
    Just intmap_combined_widget->run_event_a start_id main_id single_id_history intmap_combined_widget event engine

run_event_a::Data a=>Int->Int->DSeq.Seq Int->DIS.IntMap (Combined_widget a)->Event->Engine a->Engine a
run_event_a combined_id single_id single_id_history intmap_combined_widget event engine=case DIS.lookup single_id intmap_combined_widget of
    Nothing->error "run_event_a: error 1"
    Just combined_widget->let new_engine@(Engine new_widget _ _ _ _ _ _ _ _)=run_event_b combined_widget event engine in case DIS.lookup combined_id new_widget of
        Nothing->error "run_event_a: error 2"
        Just new_intmap_combined_widget->case DIS.lookup single_id new_intmap_combined_widget of
            Nothing->error "run_event_a: error 3"
            Just new_combined_widget->case get_next_id_combined_widget new_combined_widget new_engine event of
                End->new_engine
                Goto new_single_id->run_event_a combined_id new_single_id (single_id_history DSeq.|> new_single_id) new_intmap_combined_widget event new_engine
                Back number->let max_index=DSeq.length single_id_history-1 in if number<0 || max_index<number then error "run_event_a: error 4" else let new_single_id=DSeq.index single_id_history (max_index-number) in run_event_a combined_id new_single_id (single_id_history DSeq.|> new_single_id) new_intmap_combined_widget event new_engine

run_event_b::Data a=>Combined_widget a->Event->Engine a->Engine a
run_event_b (Leaf_widget _ widget) event engine=run_widget event widget engine
run_event_b (Node_widget _ main_single_id event_transform _ combined_id) event engine=case event_transform engine event of
    Nothing->engine
    Just new_event->let calculated_main_single_id=main_single_id engine event in run_event combined_id calculated_main_single_id (DSeq.singleton calculated_main_single_id) new_event engine

run_widget::Data a=>Event->Single_widget a->Engine a->Engine a
run_widget _ (Label_data _) engine=engine
run_widget _ (Bool_data _) engine=engine
run_widget _ (Int_data _) engine=engine
run_widget _ (Char_data _) engine=engine
run_widget _ (List_char_data _) engine=engine
run_widget _ (Data _) engine=engine
run_widget event (Trigger handle) engine=handle event engine
run_widget event (Io_trigger handle) engine=create_request (Request (Io (handle event)) DSeq.empty) engine
run_widget _ (Collector _) engine=engine
run_widget _ (Font _) engine=engine
run_widget _ (Block_font {}) engine=engine
run_widget _ (Rectangle {}) engine=engine
run_widget _ (Picture {}) engine=engine
run_widget _ (Text {}) engine=engine
run_widget _ (Editor {}) engine=engine

clean_engine::Data a=>Engine a->IO ()
clean_engine (Engine widget window _ _ _ _ _ _ _)=do
    DF.mapM_ (DF.mapM_ clean_widget) widget
    DF.mapM_ clean_window window

clean_widget::Data a=>Combined_widget a->IO ()
clean_widget (Leaf_widget _ single_widget)=remove_single_widget single_widget
clean_widget (Node_widget {})=return ()

clean_window::Window->IO ()
clean_window (Window _ window renderer _ _ _ _ _ _)=do
    SRV.destroyRenderer renderer
    SRV.destroyWindow window