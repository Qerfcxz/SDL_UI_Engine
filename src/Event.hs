{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Event where
import Other
import Type
import qualified Data.ByteString as DB
import qualified Data.Int as DI
import qualified Data.IntMap.Strict as DIS
import qualified Data.Text.Encoding as DTE
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Enum as SRE
import qualified SDL.Raw.Event as SRE
import qualified SDL.Raw.Types as SRT

push_event::DW.Word32->DW.Word32->DW.Word32->DI.Int32->FP.Ptr ()->FP.Ptr ()->IO ()
push_event event_type time window_id event_code data_one data_two=FMU.with (SRT.UserEvent event_type time window_id event_code data_one data_two) (catch_error "push_event: error 1" 1 . SRE.pushEvent)

get_event::DIS.IntMap Int->Maybe DW.Word32->IO Event
get_event window_map maybe_event_type=FMA.alloca $ \pointer->do
    catch_error "get_event: error 1" 1 (SRE.waitEvent pointer)
    event<-FS.peek pointer
    case event of
        SRT.UserEvent event_type _ _ _ _ _->case maybe_event_type of
            Nothing->return Unknown
            Just time_event_type->if event_type==time_event_type then return Time else return Unknown
        SRT.QuitEvent {}->return Quit
        SRT.WindowEvent _ _ window_id window_event data_one data_two->case window_event of
            SRE.SDL_WINDOWEVENT_CLOSE->do
                case DIS.lookup (fromIntegral window_id) window_map of
                    Nothing->return Unknown
                    Just window->return (At window Close)
            SRE.SDL_WINDOWEVENT_RESIZED->do
                case DIS.lookup (fromIntegral window_id) window_map of
                    Nothing->return Unknown
                    Just window->return (Resize window (fromIntegral data_one) (fromIntegral data_two))
            _->return Unknown
        SRT.MouseWheelEvent event_type _ window_id _ _ delta_y direction->case event_type of
            SRE.SDL_MOUSEWHEEL->case DIS.lookup (fromIntegral window_id) window_map of
                Nothing->return Unknown
                Just window->case direction of
                    SRE.SDL_MOUSEWHEEL_NORMAL->return (At window (Wheel (fromIntegral delta_y)))
                    SRE.SDL_MOUSEWHEEL_FLIPPED->return (At window (Wheel (-fromIntegral delta_y)))
                    _->return Unknown
            _->return Unknown
        SRT.MouseButtonEvent event_type _ window_id _ button_code _ _ x y->case event_type of
            SRE.SDL_MOUSEBUTTONUP->case button_code of
                SRE.SDL_BUTTON_LEFT->case DIS.lookup (fromIntegral window_id) window_map of
                        Nothing->return Unknown
                        Just window->return (At window (Click Click_up Mouse_left (FCT.CInt x) (FCT.CInt y)))
                SRE.SDL_BUTTON_RIGHT->case DIS.lookup (fromIntegral window_id) window_map of
                        Nothing->return Unknown
                        Just window->return (At window (Click Click_up Mouse_right (FCT.CInt x) (FCT.CInt y)))
                _->return Unknown
            SRE.SDL_MOUSEBUTTONDOWN->case button_code of
                SRE.SDL_BUTTON_LEFT->case DIS.lookup (fromIntegral window_id) window_map of
                        Nothing->return Unknown
                        Just window->return (At window (Click Click_down Mouse_left (FCT.CInt x) (FCT.CInt y)))
                SRE.SDL_BUTTON_RIGHT->case DIS.lookup (fromIntegral window_id) window_map of
                        Nothing->return Unknown
                        Just window->return (At window (Click Click_down Mouse_right (FCT.CInt x) (FCT.CInt y)))
                _->return Unknown
            _->return Unknown
        SRT.TextInputEvent event_type _ window_id text->case event_type of
            SRE.SDL_TEXTINPUT->do
                case DIS.lookup (fromIntegral window_id) window_map of
                    Nothing->return Unknown
                    Just window->return (At window (Input (DTE.decodeUtf8 (DB.pack (map fromIntegral text)))))
            _->return Unknown
        SRT.KeyboardEvent event_type _ window_id _ _ (SRT.Keysym _ key_code _)->case event_type of
            SRE.SDL_KEYUP->do
                case DIS.lookup (fromIntegral window_id) window_map of
                    Nothing->return Unknown
                    Just window->case key_code of
                        SRE.SDLK_a->return (At window (Press Press_up Key_a))
                        SRE.SDLK_b->return (At window (Press Press_up Key_b))
                        SRE.SDLK_c->return (At window (Press Press_up Key_c))
                        SRE.SDLK_d->return (At window (Press Press_up Key_d))
                        SRE.SDLK_e->return (At window (Press Press_up Key_e))
                        SRE.SDLK_f->return (At window (Press Press_up Key_f))
                        SRE.SDLK_g->return (At window (Press Press_up Key_g))
                        SRE.SDLK_h->return (At window (Press Press_up Key_h))
                        SRE.SDLK_i->return (At window (Press Press_up Key_i))
                        SRE.SDLK_j->return (At window (Press Press_up Key_j))
                        SRE.SDLK_k->return (At window (Press Press_up Key_k))
                        SRE.SDLK_l->return (At window (Press Press_up Key_l))
                        SRE.SDLK_m->return (At window (Press Press_up Key_m))
                        SRE.SDLK_n->return (At window (Press Press_up Key_n))
                        SRE.SDLK_o->return (At window (Press Press_up Key_o))
                        SRE.SDLK_p->return (At window (Press Press_up Key_p))
                        SRE.SDLK_q->return (At window (Press Press_up Key_q))
                        SRE.SDLK_r->return (At window (Press Press_up Key_r))
                        SRE.SDLK_s->return (At window (Press Press_up Key_s))
                        SRE.SDLK_t->return (At window (Press Press_up Key_t))
                        SRE.SDLK_u->return (At window (Press Press_up Key_u))
                        SRE.SDLK_v->return (At window (Press Press_up Key_v))
                        SRE.SDLK_w->return (At window (Press Press_up Key_w))
                        SRE.SDLK_x->return (At window (Press Press_up Key_x))
                        SRE.SDLK_y->return (At window (Press Press_up Key_y))
                        SRE.SDLK_z->return (At window (Press Press_up Key_z))
                        SRE.SDLK_LEFT->return (At window (Press Press_up Key_left))
                        SRE.SDLK_RIGHT->return (At window (Press Press_up Key_right))
                        SRE.SDLK_UP->return (At window (Press Press_up Key_up))
                        SRE.SDLK_DOWN->return (At window (Press Press_up Key_down))
                        SRE.SDLK_BACKSPACE->return (At window (Press Press_up Key_backspace))
                        SRE.SDLK_DELETE->return (At window (Press Press_up Key_delete))
                        SRE.SDLK_RETURN->return (At window (Press Press_up Key_enter))
                        SRE.SDLK_ESCAPE->return (At window (Press Press_up Key_escape))
                        SRE.SDLK_TAB->return (At window (Press Press_up Key_tab))
                        SRE.SDLK_LSHIFT->return (At window (Press Press_up Key_left_shift))
                        SRE.SDLK_RSHIFT->return (At window (Press Press_up Key_right_shift))
                        SRE.SDLK_LCTRL->return (At window (Press Press_up Key_left_ctrl))
                        SRE.SDLK_RCTRL->return (At window (Press Press_up Key_right_ctrl))
                        SRE.SDLK_KP_0->return (At window (Press Press_up Key_numpad_0))
                        SRE.SDLK_KP_1->return (At window (Press Press_up Key_numpad_1))
                        SRE.SDLK_KP_2->return (At window (Press Press_up Key_numpad_2))
                        SRE.SDLK_KP_3->return (At window (Press Press_up Key_numpad_3))
                        SRE.SDLK_KP_4->return (At window (Press Press_up Key_numpad_4))
                        SRE.SDLK_KP_5->return (At window (Press Press_up Key_numpad_5))
                        SRE.SDLK_KP_6->return (At window (Press Press_up Key_numpad_6))
                        SRE.SDLK_KP_7->return (At window (Press Press_up Key_numpad_7))
                        SRE.SDLK_KP_8->return (At window (Press Press_up Key_numpad_8))
                        SRE.SDLK_KP_9->return (At window (Press Press_up Key_numpad_9))
                        SRE.SDLK_F1->return (At window (Press Press_up Key_f1))
                        SRE.SDLK_F2->return (At window (Press Press_up Key_f2))
                        SRE.SDLK_F3->return (At window (Press Press_up Key_f3))
                        SRE.SDLK_F4->return (At window (Press Press_up Key_f4))
                        SRE.SDLK_F5->return (At window (Press Press_up Key_f5))
                        SRE.SDLK_F6->return (At window (Press Press_up Key_f6))
                        SRE.SDLK_F7->return (At window (Press Press_up Key_f7))
                        SRE.SDLK_F8->return (At window (Press Press_up Key_f8))
                        SRE.SDLK_F9->return (At window (Press Press_up Key_f9))
                        SRE.SDLK_F10->return (At window (Press Press_up Key_f10))
                        SRE.SDLK_F11->return (At window (Press Press_up Key_f11))
                        SRE.SDLK_F12->return (At window (Press Press_up Key_f12))
                        _->return (At window (Press Press_up Key_unknown))
            SRE.SDL_KEYDOWN->do
                case DIS.lookup (fromIntegral window_id) window_map of
                    Nothing->return Unknown
                    Just window->case key_code of
                        SRE.SDLK_a->return (At window (Press Press_down Key_a))
                        SRE.SDLK_b->return (At window (Press Press_down Key_b))
                        SRE.SDLK_c->return (At window (Press Press_down Key_c))
                        SRE.SDLK_d->return (At window (Press Press_down Key_d))
                        SRE.SDLK_e->return (At window (Press Press_down Key_e))
                        SRE.SDLK_f->return (At window (Press Press_down Key_f))
                        SRE.SDLK_g->return (At window (Press Press_down Key_g))
                        SRE.SDLK_h->return (At window (Press Press_down Key_h))
                        SRE.SDLK_i->return (At window (Press Press_down Key_i))
                        SRE.SDLK_j->return (At window (Press Press_down Key_j))
                        SRE.SDLK_k->return (At window (Press Press_down Key_k))
                        SRE.SDLK_l->return (At window (Press Press_down Key_l))
                        SRE.SDLK_m->return (At window (Press Press_down Key_m))
                        SRE.SDLK_n->return (At window (Press Press_down Key_n))
                        SRE.SDLK_o->return (At window (Press Press_down Key_o))
                        SRE.SDLK_p->return (At window (Press Press_down Key_p))
                        SRE.SDLK_q->return (At window (Press Press_down Key_q))
                        SRE.SDLK_r->return (At window (Press Press_down Key_r))
                        SRE.SDLK_s->return (At window (Press Press_down Key_s))
                        SRE.SDLK_t->return (At window (Press Press_down Key_t))
                        SRE.SDLK_u->return (At window (Press Press_down Key_u))
                        SRE.SDLK_v->return (At window (Press Press_down Key_v))
                        SRE.SDLK_w->return (At window (Press Press_down Key_w))
                        SRE.SDLK_x->return (At window (Press Press_down Key_x))
                        SRE.SDLK_y->return (At window (Press Press_down Key_y))
                        SRE.SDLK_z->return (At window (Press Press_down Key_z))
                        SRE.SDLK_LEFT->return (At window (Press Press_down Key_left))
                        SRE.SDLK_RIGHT->return (At window (Press Press_down Key_right))
                        SRE.SDLK_UP->return (At window (Press Press_down Key_up))
                        SRE.SDLK_DOWN->return (At window (Press Press_down Key_down))
                        SRE.SDLK_BACKSPACE->return (At window (Press Press_down Key_backspace))
                        SRE.SDLK_DELETE->return (At window (Press Press_down Key_delete))
                        SRE.SDLK_RETURN->return (At window (Press Press_down Key_enter))
                        SRE.SDLK_ESCAPE->return (At window (Press Press_down Key_escape))
                        SRE.SDLK_TAB->return (At window (Press Press_down Key_tab))
                        SRE.SDLK_LSHIFT->return (At window (Press Press_down Key_left_shift))
                        SRE.SDLK_RSHIFT->return (At window (Press Press_down Key_right_shift))
                        SRE.SDLK_LCTRL->return (At window (Press Press_down Key_left_ctrl))
                        SRE.SDLK_RCTRL->return (At window (Press Press_down Key_right_ctrl))
                        SRE.SDLK_KP_0->return (At window (Press Press_down Key_numpad_0))
                        SRE.SDLK_KP_1->return (At window (Press Press_down Key_numpad_1))
                        SRE.SDLK_KP_2->return (At window (Press Press_down Key_numpad_2))
                        SRE.SDLK_KP_3->return (At window (Press Press_down Key_numpad_3))
                        SRE.SDLK_KP_4->return (At window (Press Press_down Key_numpad_4))
                        SRE.SDLK_KP_5->return (At window (Press Press_down Key_numpad_5))
                        SRE.SDLK_KP_6->return (At window (Press Press_down Key_numpad_6))
                        SRE.SDLK_KP_7->return (At window (Press Press_down Key_numpad_7))
                        SRE.SDLK_KP_8->return (At window (Press Press_down Key_numpad_8))
                        SRE.SDLK_KP_9->return (At window (Press Press_down Key_numpad_9))
                        SRE.SDLK_F1->return (At window (Press Press_down Key_f1))
                        SRE.SDLK_F2->return (At window (Press Press_down Key_f2))
                        SRE.SDLK_F3->return (At window (Press Press_down Key_f3))
                        SRE.SDLK_F4->return (At window (Press Press_down Key_f4))
                        SRE.SDLK_F5->return (At window (Press Press_down Key_f5))
                        SRE.SDLK_F6->return (At window (Press Press_down Key_f6))
                        SRE.SDLK_F7->return (At window (Press Press_down Key_f7))
                        SRE.SDLK_F8->return (At window (Press Press_down Key_f8))
                        SRE.SDLK_F9->return (At window (Press Press_down Key_f9))
                        SRE.SDLK_F10->return (At window (Press Press_down Key_f10))
                        SRE.SDLK_F11->return (At window (Press Press_down Key_f11))
                        SRE.SDLK_F12->return (At window (Press Press_down Key_f12))
                        _->return (At window (Press Press_down Key_unknown))
            _->return Unknown
        _->return Unknown