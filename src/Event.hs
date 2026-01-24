{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Event where
import Other.Error
import Type
import qualified Data.ByteString as DB
import qualified Data.Int as DI
import qualified Data.IntMap.Strict as DIS
import qualified Data.Set as DS
import qualified Data.Text.Encoding as DTE
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Enum as SREn
import qualified SDL.Raw.Event as SREv
import qualified SDL.Raw.Types as SRT

push_event::DW.Word32->DW.Word32->DW.Word32->DI.Int32->FP.Ptr ()->FP.Ptr ()->IO ()
push_event event_type time window_id event_code data_one data_two=FMU.with (SRT.UserEvent event_type time window_id event_code data_one data_two) (catch_error "push_event: error 1" 1 . SREv.pushEvent)

get_event::FP.Ptr SRT.Event->DIS.IntMap Int->DS.Set Key->Maybe DW.Word32->IO (Maybe (DS.Set Key),Event)
get_event event window_map key maybe_event_type=do
    catch_error "get_event: error 1" 1 (SREv.waitEvent event)
    this_event<-FS.peek event
    case this_event of
        SRT.QuitEvent {}->return (Nothing,Quit)
        SRT.UserEvent event_type _ _ _ _ _->case maybe_event_type of
            Nothing->return (Nothing,Unknown)
            Just time_event_type->if event_type==time_event_type then return (Nothing,Time) else return (Nothing,Unknown)
        SRT.WindowEvent _ _ window_id window_event data_one data_two->case window_event of
            SREn.SDL_WINDOWEVENT_CLOSE->do
                case DIS.lookup (fromIntegral window_id) window_map of
                    Nothing->return (Nothing,Unknown)
                    Just window->return (Nothing,At window Close)
            SREn.SDL_WINDOWEVENT_RESIZED->do
                case DIS.lookup (fromIntegral window_id) window_map of
                    Nothing->return (Nothing,Unknown)
                    Just window->return (Nothing,Resize window (fromIntegral data_one) (fromIntegral data_two))
            _->return (Nothing,Unknown)
        SRT.TextInputEvent event_type _ window_id text->case event_type of
            SREn.SDL_TEXTINPUT->do
                case DIS.lookup (fromIntegral window_id) window_map of
                    Nothing->return (Nothing,Unknown)
                    Just window->return (Nothing,At window (Input (DTE.decodeUtf8 (DB.pack (map fromIntegral text)))))
            _->return (Nothing,Unknown)
        SRT.MouseWheelEvent event_type _ window_id _ _ delta_y direction->case event_type of
            SREn.SDL_MOUSEWHEEL->case DIS.lookup (fromIntegral window_id) window_map of
                Nothing->return (Nothing,Unknown)
                Just window->case direction of
                    SREn.SDL_MOUSEWHEEL_NORMAL->return (Nothing,At window (Wheel (fromIntegral delta_y)))
                    SREn.SDL_MOUSEWHEEL_FLIPPED->return (Nothing,At window (Wheel (-fromIntegral delta_y)))
                    _->return (Nothing,Unknown)
            _->return (Nothing,Unknown)
        SRT.KeyboardEvent event_type _ window_id _ _ (SRT.Keysym _ key_code _)->case event_type of
            SREn.SDL_KEYUP->do
                case DIS.lookup (fromIntegral window_id) window_map of
                    Nothing->return (Nothing,Unknown)
                    Just window->let this_key=to_key key_code in let new_key=DS.delete this_key key in return (Just new_key,At window (Press Press_up this_key new_key))
            SREn.SDL_KEYDOWN->do
                case DIS.lookup (fromIntegral window_id) window_map of
                    Nothing->return (Nothing,Unknown)
                    Just window->let this_key=to_key key_code in let new_key=DS.insert this_key key in return (Just new_key,At window (Press Press_down this_key new_key))
            _->return (Nothing,Unknown)
        SRT.MouseButtonEvent event_type _ window_id _ button_code _ _ x y->case event_type of
            SREn.SDL_MOUSEBUTTONUP->case button_code of
                SREn.SDL_BUTTON_LEFT->case DIS.lookup (fromIntegral window_id) window_map of
                        Nothing->return (Nothing,Unknown)
                        Just window->return (Nothing,At window (Click Click_up Mouse_left (FCT.CInt x) (FCT.CInt y)))
                SREn.SDL_BUTTON_RIGHT->case DIS.lookup (fromIntegral window_id) window_map of
                        Nothing->return (Nothing,Unknown)
                        Just window->return (Nothing,At window (Click Click_up Mouse_right (FCT.CInt x) (FCT.CInt y)))
                _->return (Nothing,Unknown)
            SREn.SDL_MOUSEBUTTONDOWN->case button_code of
                SREn.SDL_BUTTON_LEFT->case DIS.lookup (fromIntegral window_id) window_map of
                        Nothing->return (Nothing,Unknown)
                        Just window->return (Nothing,At window (Click Click_down Mouse_left (FCT.CInt x) (FCT.CInt y)))
                SREn.SDL_BUTTON_RIGHT->case DIS.lookup (fromIntegral window_id) window_map of
                        Nothing->return (Nothing,Unknown)
                        Just window->return (Nothing,At window (Click Click_down Mouse_right (FCT.CInt x) (FCT.CInt y)))
                _->return (Nothing,Unknown)
            _->return (Nothing,Unknown)
        _->return (Nothing,Unknown)

to_key::SREn.Keycode->Key
to_key SREn.SDLK_a=Key_a
to_key SREn.SDLK_b=Key_b
to_key SREn.SDLK_c=Key_c
to_key SREn.SDLK_d=Key_d
to_key SREn.SDLK_e=Key_e
to_key SREn.SDLK_f=Key_f
to_key SREn.SDLK_g=Key_g
to_key SREn.SDLK_h=Key_h
to_key SREn.SDLK_i=Key_i
to_key SREn.SDLK_j=Key_j
to_key SREn.SDLK_k=Key_k
to_key SREn.SDLK_l=Key_l
to_key SREn.SDLK_m=Key_m
to_key SREn.SDLK_n=Key_n
to_key SREn.SDLK_o=Key_o
to_key SREn.SDLK_p=Key_p
to_key SREn.SDLK_q=Key_q
to_key SREn.SDLK_r=Key_r
to_key SREn.SDLK_s=Key_s
to_key SREn.SDLK_t=Key_t
to_key SREn.SDLK_u=Key_u
to_key SREn.SDLK_v=Key_v
to_key SREn.SDLK_w=Key_w
to_key SREn.SDLK_x=Key_x
to_key SREn.SDLK_y=Key_y
to_key SREn.SDLK_z=Key_z
to_key SREn.SDLK_LEFT=Key_left
to_key SREn.SDLK_RIGHT=Key_right
to_key SREn.SDLK_UP=Key_up
to_key SREn.SDLK_DOWN=Key_down
to_key SREn.SDLK_BACKSPACE=Key_backspace
to_key SREn.SDLK_DELETE=Key_delete
to_key SREn.SDLK_RETURN=Key_enter
to_key SREn.SDLK_ESCAPE=Key_esc
to_key SREn.SDLK_TAB=Key_tab
to_key SREn.SDLK_LSHIFT=Key_left_shift
to_key SREn.SDLK_RSHIFT=Key_right_shift
to_key SREn.SDLK_LCTRL=Key_left_ctrl
to_key SREn.SDLK_RCTRL=Key_right_ctrl
to_key SREn.SDLK_KP_0=Key_numpad_0
to_key SREn.SDLK_KP_1=Key_numpad_1
to_key SREn.SDLK_KP_2=Key_numpad_2
to_key SREn.SDLK_KP_3=Key_numpad_3
to_key SREn.SDLK_KP_4=Key_numpad_4
to_key SREn.SDLK_KP_5=Key_numpad_5
to_key SREn.SDLK_KP_6=Key_numpad_6
to_key SREn.SDLK_KP_7=Key_numpad_7
to_key SREn.SDLK_KP_8=Key_numpad_8
to_key SREn.SDLK_KP_9=Key_numpad_9
to_key SREn.SDLK_F1=Key_f1
to_key SREn.SDLK_F2=Key_f2
to_key SREn.SDLK_F3=Key_f3
to_key SREn.SDLK_F4=Key_f4
to_key SREn.SDLK_F5=Key_f5
to_key SREn.SDLK_F6=Key_f6
to_key SREn.SDLK_F7=Key_f7
to_key SREn.SDLK_F8=Key_f8
to_key SREn.SDLK_F9=Key_f9
to_key SREn.SDLK_F10=Key_f10
to_key SREn.SDLK_F11=Key_f11
to_key SREn.SDLK_F12=Key_f12
to_key _=Key_unknown