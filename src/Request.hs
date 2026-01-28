{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Request where
import Block
import Editor.Render
import Other.Error
import Other.Get
import Other.Other
import Other.Set
import Other.Update
import Text.Render
import Widget.Alter
import Widget.Create
import Widget.Remove
import Widget.Replace
import Instruction
import Type
import qualified Control.Monad as CM
import qualified Data.ByteString as DB
import qualified Data.Foldable as DF
import qualified Data.IORef as DI
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Text.Encoding as DTE
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Enum as SRE
import qualified SDL.Raw.Image as SRI
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

create_request::Data a=>Request a->Engine a->Engine a
create_request new_request (Engine widget window window_map request key main_id start_id count_id time)=Engine widget window window_map (request DS.|> new_request) key main_id start_id count_id time

do_request::Data a=>Request a->Engine a->IO (Engine a)
do_request (Request rough_request instruction) engine=case rough_request of
    Create_widget combined_widget_request seq_id->do
        new_combined_widget_request<-DF.foldlM (\this_combined_widget_request this_instruction->create_widget_instruction this_instruction engine this_combined_widget_request) combined_widget_request instruction
        let (combined_id,single_id)=get_widget_id seq_id engine in create_widget combined_id single_id new_combined_widget_request engine
    Remove_widget only seq_id->let (combined_id,single_id)=get_widget_id seq_id engine in if only then remove_widget_only combined_id single_id engine else remove_widget combined_id single_id engine
    Replace_widget combined_widget_request seq_id->let (combined_id,single_id)=get_widget_id seq_id engine in replace_widget combined_id single_id combined_widget_request engine
    Alter_widget combined_widget_request seq_id->let (combined_id,single_id)=get_widget_id seq_id engine in alter_widget combined_id single_id combined_widget_request engine
    Create_window window_id window_name left right up down->do
        Mix_create_window new_window_id new_window_name new_left new_right new_up new_down<-DF.foldlM (\mix this_instruction->create_window_instruction this_instruction engine mix) (Mix_create_window window_id window_name left right up down) instruction
        DB.useAsCString (DTE.encodeUtf8 new_window_name) $ \name->do
            let width=new_right-new_left
            let height=new_down-new_up
            new_window<-SRV.createWindow name new_left new_up width height SRE.SDL_WINDOW_RESIZABLE
            CM.when (new_window==FP.nullPtr) (error "do_request: error 1")
            renderer<-SRV.createRenderer new_window (-1) SRE.SDL_RENDERER_ACCELERATED
            CM.when (renderer==FP.nullPtr) (error "do_request: 2")
            catch_error "do_request: error 3" 0 (SRV.setRenderDrawBlendMode renderer SRE.SDL_BLENDMODE_BLEND)
            sdl_window_id<-SRV.getWindowID new_window
            let new_sdl_window_id=fromIntegral sdl_window_id in return (update_engine_window (error_insert "do_request: error 4" new_window_id (Window new_sdl_window_id new_window renderer width height 0 0 1 1)) engine)
    Remove_window window_id->case DIS.updateLookupWithKey (\_ _->Nothing) window_id (get_engine_window engine) of
        (Nothing,_)->error "do_request: error 6"
        (Just (Window sdl_window_id this_window renderer _ _ _ _ _ _),new_window)->do
            SRV.destroyRenderer renderer
            SRV.destroyWindow this_window
            return (update_engine_window_map (error_remove_simple "do_request: error 7" sdl_window_id) (set_engine_window (error_remove_simple "do_request: error 7" sdl_window_id new_window) engine))
    Present_window window_id->do
        SRV.renderPresent (get_renderer window_id engine)
        return engine
    Clear_window window_id red green blue alpha->let renderer=get_renderer window_id engine in do
        catch_error "do_request: error 8" 0 (SRV.setRenderDrawColor renderer red green blue alpha)
        catch_error "do_request: error 9" 0 (SRV.renderClear renderer)
        return engine
    Resize_window window_id left right up down->
        let width=right-left in let height=down-up in case DIS.updateLookupWithKey (\_ (Window sdl_window_id sdl_window renderer design_width design_height _ _ _ _)->let (x,y,design_size,size)=adaptive_window design_width design_height width height in Just (Window sdl_window_id sdl_window renderer design_width design_height x y design_size size)) window_id (get_engine_window engine) of
            (Nothing,_)->error "do_request: error 10"
            (Just (Window _ sdl_window _ _ _ _ _ _ _),new_window)->do
                SRV.setWindowPosition sdl_window left up
                SRV.setWindowSize sdl_window width height
                return (set_engine_window new_window engine)
    Io handle->handle engine
    Render_rectangle window_id red green blue alpha left right up down->let renderer=get_renderer window_id engine in do
        catch_error "do_request: error 11" 0 (SRV.setRenderDrawColor renderer red green blue alpha)
        FMA.alloca $ \rect->do
            FS.poke rect (SRT.Rect left up (right-left) (down-up))
            catch_error "do_request: error 12" 0 (SRV.renderFillRect renderer rect)
        return engine
    Render_picture window_id path render_flip angle x y width_multiply width_divide height_multiply height_divide->let renderer=get_renderer window_id engine in do
        surface<-DB.useAsCString (DTE.encodeUtf8 path) SRI.load
        CM.when (surface==FP.nullPtr) $ error "do_request: error 13"
        SRT.Surface _ width height _ _ _ _<-FS.peek surface
        texture<-SRV.createTextureFromSurface renderer surface
        SRV.freeSurface surface
        CM.when (texture==FP.nullPtr) $ error "do_request: error 14"
        let new_width=div (width*width_multiply) width_divide in let new_height=div (height*height_multiply) height_divide in catch_error "do_request: error 15" 0 (FMU.with (SRT.Rect (x-div new_width 2) (y-div new_height 2) new_width new_height) (\rect->SRV.renderCopyEx renderer texture FP.nullPtr rect angle FP.nullPtr (from_flip render_flip)))
        SRV.destroyTexture texture
        return engine
    Render_rectangle_widget direct seq_id->let (combined_id,single_id,transform)=get_widget_id_with_transform seq_id engine in let widget=get_engine_widget engine in if direct
        then case DF.foldlM (\this_instruction this_transform->this_transform engine rough_request this_instruction) instruction transform of
            Nothing->return engine
            Just new_instruction->do
                combined_widget<-DF.foldlM (\combined_widget this_instruction->from_widget_instruction this_instruction engine combined_widget) (error_lookup_lookup "do_request: error n" "do_request: error n" combined_id single_id widget) new_instruction
                do_request_render_rectangle_widget combined_widget engine
        else do
            combined_widget<-DF.foldlM (\combined_widget this_instruction->from_widget_instruction this_instruction engine combined_widget) (error_lookup_lookup "do_request: error n" "do_request: error n" combined_id single_id widget) instruction
            do_request_render_rectangle_widget combined_widget engine
    Render_picture_widget direct seq_id->let (combined_id,single_id,transform)=get_widget_id_with_transform seq_id engine in let widget=get_engine_widget engine in if direct
        then case DF.foldlM (\this_instruction this_transform->this_transform engine rough_request this_instruction) instruction transform of
            Nothing->return engine
            Just new_instruction->do
                combined_widget<-DF.foldlM (\combined_widget this_instruction->from_widget_instruction this_instruction engine combined_widget) (error_lookup_lookup "do_request: error n" "do_request: error n" combined_id single_id widget) new_instruction
                do_request_render_picture_widget combined_widget engine
        else do
            combined_widget<-DF.foldlM (\combined_widget this_instruction->from_widget_instruction this_instruction engine combined_widget) (error_lookup_lookup "do_request: error n" "do_request: error n" combined_id single_id widget) instruction
            do_request_render_picture_widget combined_widget engine
    Render_text_widget direct seq_id->let (combined_id,single_id,transform)=get_widget_id_with_transform seq_id engine in if direct
        then case DF.foldlM (\this_instruction this_transform->this_transform engine rough_request this_instruction) instruction transform of
            Nothing->return engine
            Just new_instruction->do
                ioref<-DI.newIORef (error "do_request: error 21")
                do_request_render_text_widget new_instruction ioref combined_id single_id engine
        else do
            ioref<-DI.newIORef (error "do_request: error 21")
            do_request_render_text_widget instruction ioref combined_id single_id engine
    Render_editor_widget direct seq_id->let (combined_id,single_id,transform)=get_widget_id_with_transform seq_id engine in if direct
        then case DF.foldlM (\this_instruction this_transform->this_transform engine rough_request this_instruction) instruction transform of
            Nothing->return engine
            Just new_instruction->do
                ioref<-DI.newIORef (error "do_request: error 21")
                do_request_render_editor_widget new_instruction ioref combined_id single_id engine
        else do
            ioref<-DI.newIORef (error "do_request: error 25")
            do_request_render_editor_widget instruction ioref combined_id single_id engine
    Update_block_font_widget direct size block_width seq_char seq_id->let (combined_id,single_id)=get_widget_id seq_id engine in do
        new_widget<-error_update_update_io "do_request: error 31" "do_request: error 32" combined_id single_id (update_block_font (get_engine_window engine) size block_width seq_char) (get_engine_widget engine)
        return (set_engine_widget new_widget engine)

do_request_render_rectangle_widget::Combined_widget a->Engine a->IO (Engine a)
do_request_render_rectangle_widget (Leaf_widget _ (Rectangle window_id red green blue alpha _ _ _ _ x y width height)) engine=let renderer=get_renderer window_id engine in do
    catch_error "do_request_render_rectangle_widget: error 1" 0 (SRV.setRenderDrawColor renderer red green blue alpha)
    FMA.alloca $ \rect->do
        FS.poke rect (SRT.Rect x y width height)
        catch_error "do_request_render_rectangle_widget: error 2" 0 (SRV.renderFillRect renderer rect)
    return engine
do_request_render_rectangle_widget _ _=error "do_request_render_rectangle_widget: error 3"

do_request_render_picture_widget::Combined_widget a->Engine a->IO (Engine a)
do_request_render_picture_widget (Leaf_widget _ (Picture window_id texture render_flip angle _ _ _ _ _ _ _ _ x y width height)) engine=let renderer=get_renderer window_id engine in do
    catch_error "do_request_render_picture_widget: error 1" 0 (FMU.with (SRT.Rect x y width height) (\rect->SRV.renderCopyEx renderer texture FP.nullPtr rect angle FP.nullPtr (from_flip render_flip)))
    return engine
do_request_render_picture_widget _ _=error "do_request_render_picture_widget: error 2"

do_request_render_text_widget::DS.Seq Instruction->DI.IORef (Combined_widget a)->Int->Int->Engine a->IO (Engine a)
do_request_render_text_widget instruction ioref combined_id single_id engine@(Engine widget window window_map request key main_id start_id count_id time)=do
    new_widget<-error_update_update_io "do_request_render_text_widget: error 1" "do_request_render_text_widget: error 2" combined_id single_id (update_widget_render ioref) widget
    combined_widget<-DI.readIORef ioref
    new_combined_widget<-DF.foldlM (\this_combined_widget this_instruction->from_widget_instruction this_instruction engine this_combined_widget) combined_widget instruction
    case new_combined_widget of
        Leaf_widget _ (Text window_id row _ _ _ _ _ _ _ _ _ _ left _ up down _ seq_row)->case DS.drop row seq_row of
            DS.Empty->return (Engine new_widget window window_map request key main_id start_id count_id time)
            (new_row DS.:<| other_seq_row)->let renderer=get_renderer_window window_id window in case new_row of
                Row seq_texture y font_height->if down<up+font_height then return (Engine new_widget window window_map request key main_id start_id count_id time) else let new_up=up-y in FMA.alloca $ \rect->do
                    render_seq_texture rect left new_up down renderer seq_texture
                    render_seq_row rect left new_up down renderer other_seq_row
                    return (Engine new_widget window window_map request key main_id start_id count_id time)
                Row_blank y font_height->if down<up+font_height then return (Engine new_widget window window_map request key main_id start_id count_id time) else let new_up=up-y in FMA.alloca $ \rect->do
                    render_seq_row rect left new_up down renderer other_seq_row
                    return (Engine new_widget window window_map request key main_id start_id count_id time)
        _->error "do_request_render_text_widget: error 3"

do_request_render_editor_widget::DS.Seq Instruction->DI.IORef (Combined_widget a)->Int->Int->Engine a->IO (Engine a)
do_request_render_editor_widget instruction ioref combined_id single_id engine@(Engine widget window window_map request key main_id start_id count_id time)=do
    new_widget<-error_update_update_io "do_request_render_editor_widget: error 1" "do_request_render_editor_widget: error 2" combined_id single_id (update_widget_render ioref) widget
    combined_widget<-DI.readIORef ioref
    new_combined_widget<-DF.foldlM (\this_combined_widget this_instruction->from_widget_instruction this_instruction engine this_combined_widget) combined_widget instruction
    case new_combined_widget of
        Leaf_widget _ (Editor window_id block_number row_number row _ font_size _ path _ typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha _ _ _ _ _ _ _ font_height block_width delta_height x y _ _ _ _ cursor seq_seq_char)->let (new_combined_id,new_single_id)=get_widget_id_widget path start_id new_widget in do
            new_new_widget<-error_update_update_io "do_request_render_editor_widget: error 3" "do_request_render_editor_widget: error 4" new_combined_id new_single_id (from_render_editor (get_renderer_window window_id window) block_number row_number row font_size typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha font_height block_width delta_height x y cursor seq_seq_char) new_widget
            return (Engine new_new_widget window window_map request key main_id start_id count_id time)
        _->error "do_request_render_editor_widget: error 5"

from_render_editor::SRT.Renderer->Int->Int->Int->Int->Typesetting->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->Cursor->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Combined_widget a->IO (Combined_widget a)
from_render_editor renderer block_number row_number row font_size typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha font_height block_width delta_height x y cursor seq_seq_char widget=case widget of
    Leaf_widget next_id (Block_font window_id red green blue alpha font)->case DIS.lookup font_size font of
        Nothing->error "from_render_editor: error 1"
        Just (this_font,height,intmap_texture)->do
            new_intmap_texture<-render_editor renderer block_number row_number row typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha font_height block_width delta_height x y cursor seq_seq_char intmap_texture
            return (Leaf_widget next_id (Block_font window_id red green blue alpha (DIS.insert font_size (this_font,height,new_intmap_texture) font)))
    _->error "from_render_editor: error 2"

update_widget_render::DI.IORef (Combined_widget a)->Combined_widget a->IO (Combined_widget a)
update_widget_render ioref widget@(Leaf_widget next_id (Text window_id row max_row _ select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row))=do
    DI.writeIORef ioref widget
    return (Leaf_widget next_id (Text window_id row max_row False select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row))
update_widget_render ioref widget@(Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size _ path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down cursor seq_seq_char))=do
    DI.writeIORef ioref widget
    return (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size False path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down cursor seq_seq_char))
update_widget_render _ _=error "update_widget_render: error 1"