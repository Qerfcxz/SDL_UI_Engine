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
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Text.Encoding as DTE
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified GHC.Stack as GS
import qualified SDL.Raw.Enum as SRE
import qualified SDL.Raw.Image as SRI
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

create_request::Data a=>Request a->Engine a->Engine a
create_request new_request (Engine widget window window_map request key main_id start_id count_id time)=Engine widget window window_map (request DS.|> new_request) key main_id start_id count_id time

do_request::GS.HasCallStack=>Data a=>Request a->Engine a->IO (Engine a)
do_request (Request raw_request instruction) engine=case raw_request of
    Create_widget combined_widget_request seq_id->do
        (new_combined_widget_request,new_seq_id)<-DF.foldlM (\this_combined_widget_request this_instruction->create_widget_instruction this_instruction engine this_combined_widget_request) (combined_widget_request,seq_id) instruction
        let (combined_id,single_id)=get_widget_id new_seq_id engine in create_widget combined_id single_id new_combined_widget_request engine
    Remove_widget transmit simple seq_id->let (combined_id,single_id,transform)=get_widget_id_with_transform seq_id engine in if transmit
        then case DF.foldlM (\this_instruction this_transform->this_transform engine raw_request this_instruction) instruction transform of
            Nothing->return engine
            Just new_instruction->let new_transform tuple=DF.foldlM (\mix this_instruction->remove_widget_instruction this_instruction engine mix) tuple new_instruction in remove_widget new_transform simple combined_id single_id engine
        else let new_transform combined_widget=DF.foldlM (\mix this_instruction->remove_widget_instruction this_instruction engine mix) combined_widget instruction in remove_widget new_transform simple combined_id single_id engine
    Replace_widget transmit combined_widget_request seq_id->let (combined_id,single_id,transform)=get_widget_id_with_transform seq_id engine in if transmit
        then case DF.foldlM (\this_instruction this_transform->this_transform engine raw_request this_instruction) instruction transform of
            Nothing->return engine
            Just new_instruction->let new_transform tuple=DF.foldlM (\mix this_instruction->replace_widget_instruction this_instruction engine mix) tuple new_instruction in replace_widget new_transform combined_widget_request combined_id single_id engine
        else let new_transform combined_widget=DF.foldlM (\mix this_instruction->replace_widget_instruction this_instruction engine mix) combined_widget instruction in replace_widget new_transform combined_widget_request combined_id single_id engine
    Alter_widget transmit combined_widget_request seq_id->let (combined_id,single_id,transform)=get_widget_id_with_transform seq_id engine in if transmit
        then case DF.foldlM (\this_instruction this_transform->this_transform engine raw_request this_instruction) instruction transform of
            Nothing->return engine
            Just new_instruction->let new_transform tuple=DF.foldlM (\mix this_instruction->alter_widget_instruction this_instruction engine mix) tuple new_instruction in alter_widget new_transform combined_widget_request combined_id single_id engine
        else let new_transform combined_widget=DF.foldlM (\mix this_instruction->alter_widget_instruction this_instruction engine mix) combined_widget instruction in alter_widget new_transform combined_widget_request combined_id single_id engine
    Create_window window_id window_name left right up down->do
        (new_window_id,new_window_name,new_left,new_right,new_up,new_down)<-DF.foldlM (\mix this_instruction->create_window_instruction this_instruction engine mix) (window_id,window_name,left,right,up,down) instruction
        DB.useAsCString (DTE.encodeUtf8 new_window_name) $ \name->do
            let width=new_right-new_left
            let height=new_down-new_up
            new_window<-SRV.createWindow name new_left new_up width height SRE.SDL_WINDOW_RESIZABLE
            CM.when (new_window==FP.nullPtr) (error "do_request: error 1")
            renderer<-SRV.createRenderer new_window (-1) SRE.SDL_RENDERER_ACCELERATED
            CM.when (renderer==FP.nullPtr) (error "do_request: error 2")
            catch_error "do_request: error 3" 0 (SRV.setRenderDrawBlendMode renderer SRE.SDL_BLENDMODE_BLEND)
            sdl_window_id<-SRV.getWindowID new_window
            let new_sdl_window_id=fromIntegral sdl_window_id in return (update_engine_window_map (error_insert "do_request: error 4" new_sdl_window_id window_id) (update_engine_window (error_insert "do_request: error 5" new_window_id (Window new_sdl_window_id new_window renderer width height 0 0 1 1)) engine))
    Remove_window window_id->do
        new_window_id<-DF.foldlM (\mix this_instruction->remove_window_instruction this_instruction engine mix) window_id instruction
        case DIS.updateLookupWithKey (\_ _->Nothing) new_window_id (get_engine_window engine) of
            (Nothing,_)->error "do_request: error 6"
            (Just (Window sdl_window_id this_window renderer _ _ _ _ _ _),new_window)->do
                SRV.destroyRenderer renderer
                SRV.destroyWindow this_window
                return (update_engine_window_map (error_remove_simple "do_request: error 7" sdl_window_id) (set_engine_window (error_remove_simple "do_request: error 8" sdl_window_id new_window) engine))
    Present_window window_id->do
        new_window_id<-DF.foldlM (\mix this_instruction->present_window_instruction this_instruction engine mix) window_id instruction
        SRV.renderPresent (get_renderer new_window_id engine)
        return engine
    Clear_window window_id red green blue alpha->do
        (new_window_id,new_red,new_green,new_blue,new_alpha)<-DF.foldlM (\mix this_instruction->clear_window_instruction this_instruction engine mix) (window_id,red,green,blue,alpha) instruction
        let renderer=get_renderer new_window_id engine
        catch_error "do_request: error 9" 0 (SRV.setRenderDrawColor renderer new_red new_green new_blue new_alpha)
        catch_error "do_request: error 10" 0 (SRV.renderClear renderer)
        return engine
    Resize_window window_id left right up down->do
        (new_window_id,new_left,new_right,new_up,new_down)<-DF.foldlM (\mix this_instruction->resize_window_instruction this_instruction engine mix) (window_id,left,right,up,down) instruction
        let width=new_right-new_left in let height=new_down-new_up in case DIS.updateLookupWithKey (\_ (Window sdl_window_id sdl_window renderer design_width design_height _ _ _ _)->let (x,y,design_size,size)=adaptive_window design_width design_height width height in Just (Window sdl_window_id sdl_window renderer design_width design_height x y design_size size)) new_window_id (get_engine_window engine) of
            (Nothing,_)->error "do_request: error 11"
            (Just (Window _ sdl_window _ _ _ _ _ _ _),new_window)->do
                SRV.setWindowPosition sdl_window new_left new_up
                SRV.setWindowSize sdl_window width height
                return (set_engine_window new_window engine)
    Min_size_window window_id width height->case get_window window_id engine of
        Window _ sdl_window _ _ _ _ _ _ _->do
            SRV.setWindowMinimumSize sdl_window width height
            return engine
    Max_size_window window_id width height->case get_window window_id engine of
        Window _ sdl_window _ _ _ _ _ _ _->do
            SRV.setWindowMaximumSize sdl_window width height
            return engine
    Whether_bordered_window window_id whether->case get_window window_id engine of
        Window _ sdl_window _ _ _ _ _ _ _->do
            SRV.setWindowBordered sdl_window whether
            return engine
    Io handle->do
        new_handle<-DF.foldlM (\mix this_instruction->io_instruction this_instruction engine mix) handle instruction
        new_handle engine
    Render_rectangle window_id red green blue alpha left right up down->do
        (new_window_id,new_red,new_green,new_blue,new_alpha,new_left,new_right,new_up,new_down)<-DF.foldlM (\mix this_instruction->render_rectangle_instruction this_instruction engine mix) (window_id,red,green,blue,alpha,left,right,up,down) instruction
        let renderer=get_renderer new_window_id engine
        catch_error "do_request: error 12" 0 (SRV.setRenderDrawColor renderer new_red new_green new_blue new_alpha)
        FMA.alloca $ \rect->do
            FS.poke rect (SRT.Rect new_left new_up (new_right-new_left) (new_down-new_up))
            catch_error "do_request: error 13" 0 (SRV.renderFillRect renderer rect)
        return engine
    Render_picture window_id path render_flip angle x y width_multiply width_divide height_multiply height_divide->do
        (new_window_id,new_path,new_render_flip,new_angle,new_x,new_y,new_width_multiply,new_width_divide,new_height_multiply,new_height_divide)<-DF.foldlM (\mix this_instruction->render_picture_instruction this_instruction engine mix) (window_id,path,render_flip,angle,x,y,width_multiply,width_divide,height_multiply,height_divide) instruction
        let renderer=get_renderer new_window_id engine
        surface<-DB.useAsCString (DTE.encodeUtf8 new_path) SRI.load
        CM.when (surface==FP.nullPtr) $ error "do_request: error 14"
        SRT.Surface _ width height _ _ _ _<-FS.peek surface
        texture<-SRV.createTextureFromSurface renderer surface
        SRV.freeSurface surface
        CM.when (texture==FP.nullPtr) $ error "do_request: error 15"
        let new_width=div (width*new_width_multiply) new_width_divide in let new_height=div (height*new_height_multiply) new_height_divide in catch_error "do_request: error 16" 0 (FMU.with (SRT.Rect (new_x-div new_width 2) (new_y-div new_height 2) new_width new_height) (\rect->SRV.renderCopyEx renderer texture FP.nullPtr rect new_angle FP.nullPtr (from_flip new_render_flip)))
        SRV.destroyTexture texture
        return engine
    Render_rectangle_widget transmit seq_id->let (combined_id,single_id,transform)=get_widget_id_with_transform seq_id engine in let widget=get_engine_widget engine in if transmit
        then case DF.foldlM (\this_instruction this_transform->this_transform engine raw_request this_instruction) instruction transform of
            Nothing->return engine
            Just new_instruction->do
                combined_widget<-DF.foldlM (\mix this_instruction->render_rectangle_widget_instruction this_instruction engine mix) (error_lookup_lookup "do_request: error 17" "do_request: error 18" combined_id single_id widget) new_instruction
                do_request_render_rectangle_widget combined_widget engine
        else do
            combined_widget<-DF.foldlM (\mix this_instruction->render_rectangle_widget_instruction this_instruction engine mix) (error_lookup_lookup "do_request: error 19" "do_request: error 20" combined_id single_id widget) instruction
            do_request_render_rectangle_widget combined_widget engine
    Render_picture_widget transmit seq_id->let (combined_id,single_id,transform)=get_widget_id_with_transform seq_id engine in let widget=get_engine_widget engine in if transmit
        then case DF.foldlM (\this_instruction this_transform->this_transform engine raw_request this_instruction) instruction transform of
            Nothing->return engine
            Just new_instruction->do
                combined_widget<-DF.foldlM (\mix this_instruction->render_picture_widget_instruction this_instruction engine mix) (error_lookup_lookup "do_request: error 21" "do_request: error 22" combined_id single_id widget) new_instruction
                do_request_render_picture_widget combined_widget engine
        else do
            combined_widget<-DF.foldlM (\mix this_instruction->render_picture_widget_instruction this_instruction engine mix) (error_lookup_lookup "do_request: error 23" "do_request: error 24" combined_id single_id widget) instruction
            do_request_render_picture_widget combined_widget engine
    Render_text_widget transmit seq_id->let (combined_id,single_id,transform)=get_widget_id_with_transform seq_id engine in if transmit
        then case DF.foldlM (\this_instruction this_transform->this_transform engine raw_request this_instruction) instruction transform of
            Nothing->return engine
            Just new_instruction->do
                let (combined_widget,new_widget)=error_get_update_update "do_request: error 25" "do_request: error 26" combined_id single_id (set_render_combined_widget False) (get_engine_widget engine)
                do_request_render_text_widget new_instruction combined_widget (set_engine_widget new_widget engine)
        else do
            let (combined_widget,new_widget)=error_get_update_update "do_request: error 27" "do_request: error 28" combined_id single_id (set_render_combined_widget False) (get_engine_widget engine)
            do_request_render_text_widget instruction combined_widget (set_engine_widget new_widget engine)
    Render_editor_widget transmit seq_id->let (combined_id,single_id,transform)=get_widget_id_with_transform seq_id engine in if transmit
        then case DF.foldlM (\this_instruction this_transform->this_transform engine raw_request this_instruction) instruction transform of
            Nothing->return engine
            Just new_instruction->do
                let (combined_widget,new_widget)=error_get_update_update "do_request: error 29" "do_request: error 30" combined_id single_id (set_render_combined_widget False) (get_engine_widget engine)
                do_request_render_editor_widget new_instruction combined_widget (set_engine_widget new_widget engine)
        else do
            let (combined_widget,new_widget)=error_get_update_update "do_request: error 31" "do_request: error 32" combined_id single_id (set_render_combined_widget False) (get_engine_widget engine)
            do_request_render_editor_widget instruction combined_widget (set_engine_widget new_widget engine)
    Update_block_font_widget transmit size block_width set_char seq_id->let (combined_id,single_id,transform)=get_widget_id_with_transform seq_id engine in if transmit
        then case DF.foldlM (\this_instruction this_transform->this_transform engine raw_request this_instruction) instruction transform of
            Nothing->return engine
            Just new_instruction->update_engine_widget_io (error_update_update_io "do_request: error 33" "do_request: error 34" combined_id single_id (update_block_font new_instruction engine size block_width set_char)) engine
        else update_engine_widget_io (error_update_update_io "do_request: error 35" "do_request: error 36" combined_id single_id (update_block_font instruction engine size block_width set_char)) engine

do_request_render_rectangle_widget::GS.HasCallStack=>Combined_widget a->Engine a->IO (Engine a)
do_request_render_rectangle_widget (Leaf_widget _ (Rectangle window_id red green blue alpha _ _ _ _ x y width height)) engine=let renderer=get_renderer window_id engine in do
    catch_error "do_request_render_rectangle_widget: error 1" 0 (SRV.setRenderDrawColor renderer red green blue alpha)
    FMA.alloca $ \rect->do
        FS.poke rect (SRT.Rect x y width height)
        catch_error "do_request_render_rectangle_widget: error 2" 0 (SRV.renderFillRect renderer rect)
    return engine
do_request_render_rectangle_widget _ _=error "do_request_render_rectangle_widget: error 3"

do_request_render_picture_widget::GS.HasCallStack=>Combined_widget a->Engine a->IO (Engine a)
do_request_render_picture_widget (Leaf_widget _ (Picture window_id texture render_flip angle _ _ _ _ _ _ _ _ x y width height)) engine=let renderer=get_renderer window_id engine in do
    catch_error "do_request_render_picture_widget: error 1" 0 (FMU.with (SRT.Rect x y width height) (\rect->SRV.renderCopyEx renderer texture FP.nullPtr rect angle FP.nullPtr (from_flip render_flip)))
    return engine
do_request_render_picture_widget _ _=error "do_request_render_picture_widget: error 2"

do_request_render_text_widget::GS.HasCallStack=>DS.Seq Instruction->Combined_widget a->Engine a->IO (Engine a)
do_request_render_text_widget instruction combined_widget engine=do
    new_combined_widget<-DF.foldlM (\mix this_instruction->render_text_widget_instruction this_instruction engine mix) combined_widget instruction
    case new_combined_widget of
        Leaf_widget _ (Text window_id row _ _ _ _ _ _ _ _ _ _ left _ up down _ seq_row)->case DS.drop row seq_row of
            DS.Empty->return engine
            (new_row DS.:<| other_seq_row)->let renderer=get_renderer window_id engine in case new_row of
                Row seq_texture y font_height->if down<up+font_height then return engine else let new_up=up-y in FMA.alloca $ \rect->do
                    render_seq_texture rect left new_up down renderer seq_texture
                    render_seq_row rect left new_up down renderer other_seq_row
                    return engine
                Row_blank y font_height->if down<up+font_height then return engine else let new_up=up-y in FMA.alloca $ \rect->do
                    render_seq_row rect left new_up down renderer other_seq_row
                    return engine
        _->error "do_request_render_text_widget: error 1"

do_request_render_editor_widget::GS.HasCallStack=>DS.Seq Instruction->Combined_widget a->Engine a->IO (Engine a)
do_request_render_editor_widget instruction combined_widget engine=do
    new_combined_widget<-DF.foldlM (\mix this_instruction->render_editor_widget_instruction this_instruction engine mix) combined_widget instruction
    let widget=get_engine_widget engine in case new_combined_widget of
        Leaf_widget _ (Editor window_id block_number row_number row _ font_size _ path _ typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha _ _ _ _ _ _ _ _ _ _ _ font_height block_width delta_height x y _ _ _ _ _ _ _ _ cursor seq_seq_char)->let (new_combined_id,new_single_id)=get_widget_id path engine in do
            new_widget<-error_update_update_io "do_request_render_editor_widget: error 1" "do_request_render_editor_widget: error 2" new_combined_id new_single_id (from_render_editor (get_renderer window_id engine) block_number row_number row font_size typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha font_height block_width delta_height x y cursor seq_seq_char) widget
            return (set_engine_widget new_widget engine)
        _->error "do_request_render_editor_widget: error 3"

from_render_editor::GS.HasCallStack=>SRT.Renderer->Int->Int->Int->Int->Typesetting->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->Cursor->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Combined_widget a->IO (Combined_widget a)
from_render_editor renderer block_number row_number row font_size typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha font_height block_width delta_height x y cursor seq_seq_char widget=case widget of
    Leaf_widget next_id (Block_font window_id red green blue alpha font)->case error_lookup "from_render_editor: error 1" font_size font of
        (this_font,height,intmap_texture)->do
            new_intmap_texture<-render_editor renderer block_number row_number row typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha font_height block_width delta_height x y cursor seq_seq_char intmap_texture
            return (Leaf_widget next_id (Block_font window_id red green blue alpha (DIS.insert font_size (this_font,height,new_intmap_texture) font)))
    _->error "from_render_editor: error 2"