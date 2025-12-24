{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Avoid lambda" #-}
module New where
import qualified Control.Monad as CM
import qualified Data.Int as DI
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Enum as SRE
import qualified SDL.Raw.Event as SRE
import qualified SDL.Raw.Timer as SRT
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

data Engine a=Engine (DIS.IntMap (DIS.IntMap (Combined_widget a))) (DIS.IntMap Window) (DIS.IntMap Int) (DS.Seq (Request a)) Int Int Int
--控件，窗口，请求，窗口id映射（SDL到引擎），为下一个组合控件分配的id（每创建一个组合控件就加一），组合控件起始id，主控件id

data Combined_widget_request a=Leaf_widget_request (Engine a->Id) (Single_widget_request a)|Node_widget_request (Engine a->Id) Int (DIS.IntMap (Combined_widget_request a))
--Node_widget_request的Int为主控件id

data Single_widget_request a=Data_request a|Trigger_request (Event->Engine a->Engine a)

data Request a=Present_window Int|Create_widget Int (Combined_widget_request a)|Remove_widget Int|Create_window Int [Char] FCT.CInt FCT.CInt FCT.CInt FCT.CInt|Remove_window Int|Clear_window Int DW.Word8 DW.Word8 DW.Word8 DW.Word8|Draw_Rectangle Int FCT.CInt FCT.CInt FCT.CInt FCT.CInt DW.Word8 DW.Word8 DW.Word8 DW.Word8

data Combined_widget a=Leaf_widget (Engine a->Id) (Single_widget a)|Node_widget (Engine a->Id) Int Int
--Node_widget后的参数为：后继id函数，其主控件id，其用于跳转的组合控件id

data Id=End|Goto Int|Back Int

data Single_widget a=Trigger (Event->Engine a->Engine a)|Data a

type Window=(Int,SRT.Window,SRT.Renderer)--暂定

get_renderer::Int->Engine a->SRT.Renderer
get_renderer window_id (Engine _ window _ _ _ _ _)=case DIS.lookup window_id window of
    Nothing->error "No such window"
    Just (_,_,renderer)->renderer

error_insert::[Char]->Int->a->DIS.IntMap a->DIS.IntMap a
error_insert error_message key value intmap=let (maybe_value,new_intmap)=DIS.insertLookupWithKey (\_ _ old_value->old_value) key value intmap in case maybe_value of
    Just _->error error_message
    Nothing->new_intmap

error_insert_insert::[Char]->[Char]->Int->Int->a->DIS.IntMap (DIS.IntMap a)->DIS.IntMap (DIS.IntMap a)
error_insert_insert first_error_message second_error_message first_key second_key value=DIS.alter (error_insert_insert_a first_error_message second_error_message second_key value) first_key

error_insert_insert_a::[Char]->[Char]->Int->a->Maybe (DIS.IntMap a)->Maybe (DIS.IntMap a)
error_insert_insert_a first_error_message _ _ _ Nothing=error first_error_message
error_insert_insert_a _ second_error_message key value (Just intmap)=if DIS.member key intmap then error second_error_message else Just (DIS.insert key value intmap)

to_widget::Single_widget_request a->IO (Single_widget a)
to_widget (Data_request content)=return (Data content)
to_widget (Trigger_request handle)=return (Trigger handle)

add_widget::Int->Combined_widget_request a->Engine a->IO (Engine a)
add_widget widget_id (Leaf_widget_request next_id single_widget_request) (Engine widget window window_map request count_id start_id main_id)=do
    new_single_widget<-to_widget single_widget_request
    return (Engine (error_insert_insert "You changed start_id without proper design" "No such single_id" start_id widget_id (Leaf_widget next_id new_single_widget) widget) window window_map request count_id start_id main_id)
add_widget widget_id (Node_widget_request next_id main_single_id intmap_combined_widget_request) (Engine widget window window_map request count_id start_id main_id)=do
    (new_count_id,new_widget)<-add_widget_a (count_id+1) count_id intmap_combined_widget_request (error_insert "You changed count_id without proper design" count_id DIS.empty (error_insert_insert "You changed start_id without proper design" "No such single_id" start_id widget_id (Node_widget next_id main_single_id count_id) widget))
    return (Engine new_widget window window_map request new_count_id start_id main_id)

add_widget_a::Int->Int->DIS.IntMap (Combined_widget_request a)->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))
add_widget_a count_id combined_id intmap_combined_widget_request widget=case DIS.minViewWithKey intmap_combined_widget_request of
    Nothing->return (count_id,widget)
    Just ((widget_id,combined_widget_request),other_intmap_combined_widget_request)->do
        (new_count_id,new_widget)<-add_widget_b count_id combined_id widget_id combined_widget_request widget
        add_widget_a new_count_id combined_id other_intmap_combined_widget_request new_widget

add_widget_b::Int->Int->Int->Combined_widget_request a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))
add_widget_b count_id combined_id widget_id (Leaf_widget_request next_id single_widget_request) widget=do
    new_single_widget<-to_widget single_widget_request
    return (count_id,error_insert_insert "No such combined_id" "No such single_id" combined_id widget_id (Leaf_widget next_id new_single_widget) widget)
add_widget_b count_id combined_id widget_id (Node_widget_request next_id main_single_id intmap_combined_widget_request) widget=add_widget_a (count_id+1) count_id intmap_combined_widget_request (error_insert "You changed count_id without proper design" count_id DIS.empty (error_insert_insert "No such combined_id" "No such single_id" combined_id widget_id (Node_widget next_id main_single_id count_id) widget))

error_remove::[Char]->Int->DIS.IntMap a->DIS.IntMap a
error_remove error_message key intmap=case DIS.updateLookupWithKey  (\_ _->Nothing) key intmap of
    (Nothing,_)->error error_message
    (Just _,new_intmap)->new_intmap

do_request::Request a->Engine a->IO (Engine a)
do_request (Present_window window_id) engine=do
    SRV.renderPresent (get_renderer window_id engine)
    return engine
do_request (Create_widget widget_id combined_widget_request) engine=add_widget widget_id combined_widget_request engine
do_request (Remove_widget widget_id) (Engine widget window window_map request count_id start_id main_id)=do
    new_widget<-remove_widget start_id widget_id widget
    return (Engine new_widget window window_map request count_id start_id main_id)
do_request (Create_window window_id window_name left right up down) (Engine widget window window_map request count_id start_id main_id)=do
    name<-FCS.newCString window_name
    new_window<-SRV.createWindow name left up (right-left) (down-up) SRE.SDL_WINDOW_SHOWN
    FMA.free name
    CM.when (new_window==FP.nullPtr) (error "SDL.Raw.Video.createWindow returns error")
    renderer<-SRV.createRenderer new_window (-1) SRE.SDL_RENDERER_ACCELERATED
    CM.when (renderer==FP.nullPtr) (error "SDL.Raw.Video.createRenderer returns error")
    catch_error "SDL.Raw.setRenderDrawBlendMode returns error" 0 (SRV.setRenderDrawBlendMode renderer SRE.SDL_BLENDMODE_BLEND)
    sdl_window_id<-SRV.getWindowID new_window
    let new_sdl_window_id=fromIntegral sdl_window_id in return (Engine widget (error_insert "Window_id already exists" window_id (new_sdl_window_id,new_window,renderer) window) (error_insert "You changed window_map without proper design" (fromIntegral sdl_window_id) window_id window_map) request count_id start_id main_id)
do_request (Remove_window window_id) (Engine widget window window_map request count_id start_id main_id)=case DIS.updateLookupWithKey (\_ _->Nothing) window_id window of
    (Nothing,_)->error "No such window_id"
    (Just (sdl_window_id,this_window,renderer),new_window)->do
        SRV.destroyRenderer renderer
        SRV.destroyWindow this_window
        return (Engine widget new_window (error_remove "You changed window_map without proper design" sdl_window_id window_map) request count_id start_id main_id)
do_request (Clear_window window_id red green blue alpha) engine=let renderer=get_renderer window_id engine in do
    catch_error "SDL.Raw.renderFillRect returns error" 0 (SRV.setRenderDrawColor renderer red green blue alpha)
    catch_error "SDL.Raw.renderClear returns error" 0 (SRV.renderClear renderer)
    return engine
do_request (Draw_Rectangle window_id up down left right red green blue alpha) engine=let renderer=get_renderer window_id engine in do
    catch_error "SDL.Raw.setRenderDrawColor returns error" 0 (SRV.setRenderDrawColor renderer red green blue alpha)
    FMA.alloca $ \rect->do
        FS.poke rect (SRT.Rect left up (right-left) (down-up))
        catch_error "SDL.Raw.renderFillRect returns error" 0 (SRV.renderFillRect renderer rect)
    return engine

remove_single_widget::Single_widget a->IO ()
remove_single_widget (Trigger _)=return ()
remove_single_widget (Data _)=return ()

remove_widget::Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))
remove_widget start_id widget_id widget=let (maybe_intmap_combined_widget,new_widget)=DIS.updateLookupWithKey (\_ intmap_Combined_widget->Just (DIS.delete widget_id intmap_Combined_widget)) start_id widget in case maybe_intmap_combined_widget of
    Nothing->error "You changed start_id without proper design"
    Just intmap_combined_widget->case DIS.lookup widget_id intmap_combined_widget of
        Nothing->error "No such single_id"
        Just combined_widget->remove_combined_widget combined_widget new_widget

remove_combined_widget::Combined_widget a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))
remove_combined_widget (Leaf_widget _ single_widget) widget=do
    remove_single_widget single_widget
    return widget
remove_combined_widget (Node_widget _ _ combined_id) widget=remove_group_widget combined_id widget

remove_group_widget::Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))
remove_group_widget combined_id widget=let (maybe_intmap_combined_widget,new_widget)=DIS.updateLookupWithKey (\_ _->Nothing) combined_id widget in case maybe_intmap_combined_widget of
    Nothing->error "No such combined_id"
    Just intmap_combined_widget->remove_group_widget_a intmap_combined_widget new_widget

remove_group_widget_a::DIS.IntMap (Combined_widget a)->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))
remove_group_widget_a intmap_combined_widget widget=case DIS.minViewWithKey intmap_combined_widget of
    Nothing->return widget
    Just ((_,combined_widget),other_intmap_combined_widget)->do
        new_widget<-remove_combined_widget combined_widget widget
        remove_group_widget_a other_intmap_combined_widget new_widget

add_timer::DW.Word32->DW.Word32->IO (SRT.TimerID,SRT.TimerCallback)
add_timer event_type time=do
    callback<-SRT.mkTimerCallback $ \_ _->do
        push_event event_type 0 0 0 FP.nullPtr FP.nullPtr
        return time
    timer_id<-SRT.addTimer time callback FP.nullPtr
    return(timer_id,callback)

remove_timer::(SRT.TimerID,SRT.TimerCallback)->IO ()
remove_timer (timer_id,callback)=do
    ok<-SRT.removeTimer timer_id
    CM.unless ok (error "SDL.Raw.Timer.removeTimer returns error")
    FP.freeHaskellFunPtr callback

catch_error::Eq a=>[Char]->a->IO a->IO ()
catch_error report success result=do
    flag<-result
    CM.unless (flag==success) (error report)

push_event::DW.Word32->DW.Word32->DW.Word32->DI.Int32->FP.Ptr ()->FP.Ptr ()->IO ()
push_event event_type time window_id event_code data_one data_two=FMU.with (SRT.UserEvent event_type time window_id event_code data_one data_two) (\event->catch_error "SDL.Raw.Event.pushEvent returns error" 1 (SRE.pushEvent event))

data Event=Unknown|Quit|Time|At Int Action
data Action=Close|Press_down Key|Press_up Key|Click_down FCT.CInt FCT.CInt|Click_up FCT.CInt FCT.CInt|Input [FCT.CChar]
data Key=Key_a|Key_b|Key_c|Key_d|Key_e|Key_f|Key_g|Key_h|Key_i|Key_j|Key_k|Key_l|Key_m|Key_n|Key_o|Key_p|Key_q|Key_r|Key_s|Key_t|Key_u|Key_v|Key_w|Key_x|Key_y|Key_z|Key_left|Key_right|Key_up|Key_down|Key_unknown

get_event::DIS.IntMap Int->Maybe DW.Word32->IO Event
get_event window_map maybe_event_type=FMA.alloca $ \pointer->do
    catch_error "SDL.Raw.Event.waitEvent returns error" 1 (SRE.waitEvent pointer)
    event<-FS.peek pointer
    case event of
        SRT.UserEvent event_type _ _ _ _ _->case maybe_event_type of
            Nothing->return Unknown
            Just time_event_type->if event_type==time_event_type then return Time else return Unknown
        SRT.QuitEvent {}->return Quit
        SRT.WindowEvent _ _ window_id window_event _ _->case window_event of
            SRE.SDL_WINDOWEVENT_CLOSE->do
                case DIS.lookup (fromIntegral window_id) window_map of
                    Nothing->return Unknown
                    Just window->return (At window Close)
            _->return Unknown
        SRT.MouseButtonEvent event_type _ window_id _ _ _ _ x y->case event_type of
            SRE.SDL_MOUSEBUTTONDOWN->do
                case DIS.lookup (fromIntegral window_id) window_map of
                    Nothing->return Unknown
                    Just window->return (At window (Click_down (FCT.CInt x) (FCT.CInt y)))
            SRE.SDL_MOUSEBUTTONUP->do
                case DIS.lookup (fromIntegral window_id) window_map of
                    Nothing->return Unknown
                    Just window->return (At window (Click_up (FCT.CInt x) (FCT.CInt y)))
            _->return Unknown
        SRT.TextInputEvent event_type _ window_id text->case event_type of
            SRE.SDL_TEXTINPUT->do
                case DIS.lookup (fromIntegral window_id) window_map of
                    Nothing->return Unknown
                    Just window->return (At window (Input text))
            _->return Unknown
        SRT.KeyboardEvent event_type _ window_id _ _ (SRT.Keysym _ key_code _)->case event_type of
            SRE.SDL_KEYDOWN->do
                case DIS.lookup (fromIntegral window_id) window_map of
                    Nothing->return Unknown
                    Just window->case key_code of
                        SRE.SDLK_a->return (At window (Press_down Key_a))
                        SRE.SDLK_b->return (At window (Press_down Key_b))
                        SRE.SDLK_c->return (At window (Press_down Key_c))
                        SRE.SDLK_d->return (At window (Press_down Key_d))
                        SRE.SDLK_e->return (At window (Press_down Key_e))
                        SRE.SDLK_f->return (At window (Press_down Key_f))
                        SRE.SDLK_g->return (At window (Press_down Key_g))
                        SRE.SDLK_h->return (At window (Press_down Key_h))
                        SRE.SDLK_i->return (At window (Press_down Key_i))
                        SRE.SDLK_j->return (At window (Press_down Key_j))
                        SRE.SDLK_k->return (At window (Press_down Key_k))
                        SRE.SDLK_l->return (At window (Press_down Key_l))
                        SRE.SDLK_m->return (At window (Press_down Key_m))
                        SRE.SDLK_n->return (At window (Press_down Key_n))
                        SRE.SDLK_o->return (At window (Press_down Key_o))
                        SRE.SDLK_p->return (At window (Press_down Key_p))
                        SRE.SDLK_q->return (At window (Press_down Key_q))
                        SRE.SDLK_r->return (At window (Press_down Key_r))
                        SRE.SDLK_s->return (At window (Press_down Key_s))
                        SRE.SDLK_t->return (At window (Press_down Key_t))
                        SRE.SDLK_u->return (At window (Press_down Key_u))
                        SRE.SDLK_v->return (At window (Press_down Key_v))
                        SRE.SDLK_w->return (At window (Press_down Key_w))
                        SRE.SDLK_x->return (At window (Press_down Key_x))
                        SRE.SDLK_y->return (At window (Press_down Key_y))
                        SRE.SDLK_z->return (At window (Press_down Key_z))
                        SRE.SDLK_LEFT->return (At window (Press_down Key_left))
                        SRE.SDLK_RIGHT->return (At window (Press_down Key_right))
                        SRE.SDLK_UP->return (At window (Press_down Key_up))
                        SRE.SDLK_DOWN->return (At window (Press_down Key_down))
                        _->return (At window (Press_down Key_unknown))
            SRE.SDL_KEYUP->do
                case DIS.lookup (fromIntegral window_id) window_map of
                    Nothing->return Unknown
                    Just window->case key_code of
                        SRE.SDLK_a->return (At window (Press_up Key_a))
                        SRE.SDLK_b->return (At window (Press_up Key_b))
                        SRE.SDLK_c->return (At window (Press_up Key_c))
                        SRE.SDLK_d->return (At window (Press_up Key_d))
                        SRE.SDLK_e->return (At window (Press_up Key_e))
                        SRE.SDLK_f->return (At window (Press_up Key_f))
                        SRE.SDLK_g->return (At window (Press_up Key_g))
                        SRE.SDLK_h->return (At window (Press_up Key_h))
                        SRE.SDLK_i->return (At window (Press_up Key_i))
                        SRE.SDLK_j->return (At window (Press_up Key_j))
                        SRE.SDLK_k->return (At window (Press_up Key_k))
                        SRE.SDLK_l->return (At window (Press_up Key_l))
                        SRE.SDLK_m->return (At window (Press_up Key_m))
                        SRE.SDLK_n->return (At window (Press_up Key_n))
                        SRE.SDLK_o->return (At window (Press_up Key_o))
                        SRE.SDLK_p->return (At window (Press_up Key_p))
                        SRE.SDLK_q->return (At window (Press_up Key_q))
                        SRE.SDLK_r->return (At window (Press_up Key_r))
                        SRE.SDLK_s->return (At window (Press_up Key_s))
                        SRE.SDLK_t->return (At window (Press_up Key_t))
                        SRE.SDLK_u->return (At window (Press_up Key_u))
                        SRE.SDLK_v->return (At window (Press_up Key_v))
                        SRE.SDLK_w->return (At window (Press_up Key_w))
                        SRE.SDLK_x->return (At window (Press_up Key_x))
                        SRE.SDLK_y->return (At window (Press_up Key_y))
                        SRE.SDLK_z->return (At window (Press_up Key_z))
                        SRE.SDLK_LEFT->return (At window (Press_up Key_left))
                        SRE.SDLK_RIGHT->return (At window (Press_up Key_right))
                        SRE.SDLK_UP->return (At window (Press_up Key_up))
                        SRE.SDLK_DOWN->return (At window (Press_up Key_down))
                        _->return (At window (Press_up Key_unknown))
            _->return Unknown
        _->return Unknown

create_engine::Int->Int->Engine a
create_engine start_id=Engine (DIS.singleton start_id DIS.empty) DIS.empty DIS.empty DS.empty (start_id+1) start_id

run_request::DS.Seq (Request a)->Engine a->IO (Engine a)
run_request DS.Empty engine=return engine
run_request (request DS.:<| other_request) engine=do
    new_engine<-do_request request engine
    run_request other_request new_engine

run_engine::Maybe DW.Word32->Engine a->IO ()
run_engine timer_setting engine=do
    case timer_setting of 
        Nothing->loop_engine engine
        Just time->do
            time_event_type<-SRE.registerEvents 1
            timer<-add_timer time_event_type time
            loop_engine_time time_event_type engine
            remove_timer timer

clean_engine::Engine a->IO ()
clean_engine engine=return()--未完待续

run_event::Int->Int->DS.Seq Int->Event->Engine a->Engine a
run_event start_id main_id single_id_history event engine@(Engine widget _ _ _ _ _ _)=case DIS.lookup start_id widget of
    Nothing->error "You changed start_id without proper design"
    Just intmap_combined_widget->run_event_a start_id main_id single_id_history intmap_combined_widget event engine

run_event_a::Int->Int->DS.Seq Int->DIS.IntMap (Combined_widget a)->Event->Engine a->Engine a
run_event_a combined_id single_id single_id_history intmap_combined_widget event engine=case DIS.lookup single_id intmap_combined_widget of
    Nothing->error "No such single_id"
    Just combined_widget->let new_engine@(Engine new_widget _ _ _ _ _ _)=run_event_b combined_widget event engine in case get_next_id combined_widget new_engine of
        End->new_engine
        Goto new_single_id->case DIS.lookup combined_id new_widget of
            Nothing->error "You changed combined_id without proper design"
            Just new_intmap_combined_widget->run_event_a combined_id new_single_id (single_id_history DS.|> new_single_id) new_intmap_combined_widget event new_engine
        Back number->case DIS.lookup combined_id new_widget of
            Nothing->error "You changed combined_id without proper design"
            Just new_intmap_combined_widget->let max_index=DS.length single_id_history-1 in if number<0 || max_index<number then error "Back number out of range" else let new_single_id=DS.index single_id_history (max_index-number) in run_event_a combined_id new_single_id (single_id_history DS.|> new_single_id) new_intmap_combined_widget event new_engine

run_event_b::Combined_widget a->Event->Engine a->Engine a
run_event_b (Leaf_widget _ widget) event engine=run_widget event widget engine
run_event_b (Node_widget _ main_single_id combined_id) event engine=run_event combined_id main_single_id (DS.singleton main_single_id) event engine

get_next_id::Combined_widget a->(Engine a->Id)
get_next_id (Leaf_widget next_single_id _)=next_single_id
get_next_id (Node_widget next_single_id _ _)=next_single_id

loop_engine_time::DW.Word32->Engine a->IO()
loop_engine_time time_event_type (Engine widget window window_map request count_id start_id main_id)=do
    new_engine@(Engine _ _ new_window_map _ _ new_start_id new_main_id)<-run_request request (Engine widget window window_map DS.Empty count_id start_id main_id)
    event<-get_event new_window_map (Just time_event_type)
    case event of
        Quit->clean_engine new_engine
        _->loop_engine_time time_event_type (run_event new_start_id new_main_id (DS.singleton new_main_id) event new_engine)

loop_engine::Engine a->IO()
loop_engine (Engine widget window window_map request count_id start_id main_id)=do
    new_engine@(Engine _ _ new_window_map _ _ new_start_id new_main_id)<-run_request request (Engine widget window window_map DS.Empty count_id start_id main_id)
    event<-get_event new_window_map Nothing
    case event of
        Quit->clean_engine new_engine
        _->loop_engine (run_event new_start_id new_main_id (DS.singleton new_main_id) event new_engine)

run_widget::Event->Single_widget a->Engine a->Engine a
run_widget event (Trigger handle)=handle event
run_widget _ (Data _)=id
