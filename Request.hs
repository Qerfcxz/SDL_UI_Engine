{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Request where
import Other
import Type
import Widget
import qualified Control.Monad as CM
import qualified Data.IntMap.Strict as DIS
import qualified Foreign.C.String as FCS
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Enum as SRE
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

do_request::Data a=>Request a->Engine a->IO (Engine a)
do_request (Create_widget seq_single_id combined_widget_request) engine=create_widget seq_single_id combined_widget_request engine
do_request (Remove_widget seq_single_id) engine=remove_widget seq_single_id engine
do_request (Replace_widget seq_single_id combined_widget_request) engine=replace_widget seq_single_id combined_widget_request engine
do_request (Create_window window_id window_name left right up down) (Engine widget window window_map request count_id start_id main_id)=do
    name<-FCS.newCString window_name
    new_window<-SRV.createWindow name left up (right-left) (down-up) SRE.SDL_WINDOW_SHOWN
    FMA.free name
    CM.when (new_window==FP.nullPtr) (error "do_request: SDL.Raw.Video.createWindow returns error")
    renderer<-SRV.createRenderer new_window (-1) SRE.SDL_RENDERER_ACCELERATED
    CM.when (renderer==FP.nullPtr) (error "do_request: SDL.Raw.Video.createRenderer returns error")
    catch_error "do_request: SDL.Raw.setRenderDrawBlendMode returns error" 0 (SRV.setRenderDrawBlendMode renderer SRE.SDL_BLENDMODE_BLEND)
    sdl_window_id<-SRV.getWindowID new_window
    let new_sdl_window_id=fromIntegral sdl_window_id in return (Engine widget (error_insert "do_request: window_id already exists" window_id (Window new_sdl_window_id new_window renderer) window) (error_insert "do_request: you changed window_map without proper design" (fromIntegral sdl_window_id) window_id window_map) request count_id start_id main_id)
do_request (Remove_window window_id) (Engine widget window window_map request count_id start_id main_id)=case DIS.updateLookupWithKey (\_ _->Nothing) window_id window of
    (Nothing,_)->error "do_request: no such window_id"
    (Just (Window sdl_window_id this_window renderer ),new_window)->do
        SRV.destroyRenderer renderer
        SRV.destroyWindow this_window
        return (Engine widget new_window (simple_error_remove "do_request: You changed window_map without proper design" sdl_window_id window_map) request count_id start_id main_id)
do_request (Present_window window_id) engine=do
    SRV.renderPresent (get_renderer window_id engine)
    return engine
do_request (Clear_window window_id red green blue alpha) engine=let renderer=get_renderer window_id engine in do
    catch_error "do_request: SDL.Raw.renderFillRect returns error" 0 (SRV.setRenderDrawColor renderer red green blue alpha)
    catch_error "do_request: SDL.Raw.renderClear returns error" 0 (SRV.renderClear renderer)
    return engine
do_request (Resize_window window_id left right up down) engine@(Engine _ window _ _ _ _ _)=case DIS.lookup window_id window of
    Nothing->error "do_request: no such window_id"
    Just (Window _ sdl_window _)->do
        SRV.setWindowPosition sdl_window left up
        SRV.setWindowSize sdl_window (right-left) (down-up)
        return engine
do_request (Draw_Rectangle window_id up down left right red green blue alpha) engine=let renderer=get_renderer window_id engine in do
    catch_error "do_request: SDL.Raw.setRenderDrawColor returns error" 0 (SRV.setRenderDrawColor renderer red green blue alpha)
    FMA.alloca $ \rect->do
        FS.poke rect (SRT.Rect left up (right-left) (down-up))
        catch_error "do_request: SDL.Raw.renderFillRect returns error" 0 (SRV.renderFillRect renderer rect)
    return engine