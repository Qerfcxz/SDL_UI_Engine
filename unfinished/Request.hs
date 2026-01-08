{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Request where
import Other
import Type
import Widget
import qualified Control.Monad as CM
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Text.Foreign as DTF
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Enum as SRE
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

create_request::Data a=>Request a->Engine a->Engine a
create_request new_request (Engine widget window window_map request count_id start_id main_id)=Engine widget window window_map (request DS.|> new_request) count_id start_id main_id

do_request::Data a=>Request a->Engine a->IO (Engine a)
do_request (Create_widget seq_single_id combined_widget_request) engine=create_widget seq_single_id combined_widget_request engine
do_request (Remove_widget seq_single_id) engine=remove_widget seq_single_id engine
do_request (Replace_widget seq_single_id combined_widget_request) engine=replace_widget seq_single_id combined_widget_request engine
do_request (Alter_widget seq_single_id combined_widget_request) engine=alter_widget seq_single_id combined_widget_request engine
do_request (Create_window window_id window_name left right up down) (Engine widget window window_map request count_id start_id main_id)=DTF.withCString window_name $ \name->let width=right-left in let height=down-up in do
    new_window<-SRV.createWindow name left up width height SRE.SDL_WINDOW_RESIZABLE
    CM.when (new_window==FP.nullPtr) (error "do_request: SDL.Raw.Video.createWindow returns error")
    renderer<-SRV.createRenderer new_window (-1) SRE.SDL_RENDERER_ACCELERATED
    CM.when (renderer==FP.nullPtr) (error "do_request: SDL.Raw.Video.createRenderer returns error")
    catch_error "do_request: SDL.Raw.setRenderDrawBlendMode returns error" 0 (SRV.setRenderDrawBlendMode renderer SRE.SDL_BLENDMODE_BLEND)
    sdl_window_id<-SRV.getWindowID new_window
    let new_sdl_window_id=fromIntegral sdl_window_id in return (Engine widget (error_insert "do_request: window_id already exists" window_id (Window new_sdl_window_id new_window renderer width height 0 0 1 1) window) (error_insert "do_request: you changed something without proper design" (fromIntegral sdl_window_id) window_id window_map) request count_id start_id main_id)
do_request (Remove_window window_id) (Engine widget window window_map request count_id start_id main_id)=case DIS.updateLookupWithKey (\_ _->Nothing) window_id window of
    (Nothing,_)->error "do_request: no such window_id"
    (Just (Window sdl_window_id this_window renderer _ _ _ _ _ _),new_window)->do
        SRV.destroyRenderer renderer
        SRV.destroyWindow this_window
        return (Engine widget new_window (simple_error_remove "do_request: you changed something without proper design" sdl_window_id window_map) request count_id start_id main_id)
do_request (Present_window window_id) engine=do
    SRV.renderPresent (get_renderer window_id engine)
    return engine
do_request (Clear_window window_id red green blue alpha) engine=let renderer=get_renderer window_id engine in do
    catch_error "do_request: SDL.Raw.renderFillRect returns error" 0 (SRV.setRenderDrawColor renderer red green blue alpha)
    catch_error "do_request: SDL.Raw.renderClear returns error" 0 (SRV.renderClear renderer)
    return engine
do_request (Resize_window window_id left right up down) (Engine widget window window_map request count_id start_id main_id)=let width=right-left in let height=down-up in case DIS.updateLookupWithKey (\_ (Window sdl_window_id sdl_window renderer design_width design_height _ _ _ _)->let (x,y,design_size,size)=adaptive_window design_width design_height width height in Just (Window sdl_window_id sdl_window renderer design_width design_height x y design_size size)) window_id window of
    (Nothing,_)->error "do_request: no such window_id"
    (Just (Window _ sdl_window _ _ _ _ _ _ _),new_window)->do
        SRV.setWindowPosition sdl_window left up
        SRV.setWindowSize sdl_window width height
        return (Engine widget new_window window_map request count_id start_id main_id)
do_request (Io_request handle) engine=handle engine
do_request (Render_rectangle window_id red green blue alpha up down left right) engine=let renderer=get_renderer window_id engine in do
    catch_error "do_request: SDL.Raw.setRenderDrawColor returns error" 0 (SRV.setRenderDrawColor renderer red green blue alpha)
    FMA.alloca $ \rect->do
        FS.poke rect (SRT.Rect left up (right-left) (down-up))
        catch_error "do_request: SDL.Raw.renderFillRect returns error" 0 (SRV.renderFillRect renderer rect)
    return engine
do_request (Render_picture window_id path x y width_multiply width_divide height_multiply height_divide) engine=let renderer=get_renderer window_id engine in do
    surface<-DTF.withCString path SRV.loadBMP
    CM.when (surface==FP.nullPtr) $ error "do_request: SDL.Raw.Video.loadBMP returns error"
    SRT.Surface _ width height _ _ _ _<-FS.peek surface
    texture<-SRV.createTextureFromSurface renderer surface
    SRV.freeSurface surface
    CM.when (texture==FP.nullPtr) $ error "do_request: SDL.Raw.Video.createTextureFromSurface returns error"
    let new_width=div (width*width_multiply) width_divide in let new_height=div (height*height_multiply) height_divide in catch_error "do_request: SDL.Raw.Video.renderCopy returns error" 0 (FMU.with (SRT.Rect (x-div new_width 2) (y-div new_height 2) new_width new_height) (SRV.renderCopy renderer texture FP.nullPtr))
    SRV.destroyTexture texture
    return engine
do_request (Render_rectangle_widget seq_id) engine=case get_widget seq_id engine of
    Leaf_widget _ (Rectangle window_id red green blue alpha _ _ _ _ x y width height)->do
        let renderer=get_renderer window_id engine
        catch_error "do_request: SDL.Raw.setRenderDrawColor returns error" 0 (SRV.setRenderDrawColor renderer red green blue alpha)
        FMA.alloca $ \rect->do
            FS.poke rect (SRT.Rect x y width height)
            catch_error "do_request: SDL.Raw.renderFillRect returns error" 0 (SRV.renderFillRect renderer rect)
        return engine
    _->error "do_request: not a rectangle widget"
do_request (Render_picture_widget seq_id) engine=case get_widget seq_id engine of
    Leaf_widget _ (Picture window_id texture _ _ _ _ _ _ _ _ x y width height)->do
        let renderer=get_renderer window_id engine
        catch_error "do_request: SDL.Raw.Video.renderCopy returns error" 0 (FMU.with (SRT.Rect x y width height) (SRV.renderCopy renderer texture FP.nullPtr))
        return engine
    _->error "do_request: not a picture widget"
do_request (Render_text_widget seq_id) engine=case get_widget seq_id engine of
    Leaf_widget _ (Text window_id row _ _ _ _ _ _ _ _ _ left _ up down _ seq_row)->case DS.drop row seq_row of
        DS.Empty->return engine
        (new_row DS.:<| other_seq_row)->let renderer=get_renderer window_id engine in case new_row of
            Row seq_texture y row_height->if down<up+row_height then return engine else let new_up=up-y in do
                    render_seq_texture left new_up down renderer seq_texture
                    render_seq_row left new_up down renderer other_seq_row
                    return engine
            Row_blank y row_height->if down<up+row_height then return engine else let new_up=up-y in do
                render_seq_row left new_up down renderer other_seq_row
                return engine
    _->error "do_request: not a text widget"
do_request (Render_editor_widget seq_id) engine=case get_widget seq_id engine of
    Leaf_widget _ (Editor window_id _ _ _ _ row row_number _ _ _ _ _ _ select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height _ _ _ _ _ _ _ delta_height _ _ left _ up _ cursor _ intmap_intmap_texture seq_map)->let renderer=get_renderer window_id engine in do
        render_select renderer row row_number select_red select_green select_blue select_alpha font_height delta_height left up cursor intmap_intmap_texture seq_map
        let new_seq_map=DS.take row_number (DS.drop row seq_map) in render_text renderer font_height delta_height left up intmap_intmap_texture new_seq_map
        render_cursor renderer row row_number cursor_red cursor_green cursor_blue cursor_alpha font_height delta_height left up cursor
        return engine
    _->error "do_request: not a editor widget"

render_select::SRT.Renderer->Int->Int->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->Cursor->DIS.IntMap (DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int))->DS.Seq (Int,Int)->IO ()
render_select renderer row row_number select_red select_green select_blue select_alpha height delta_height left up (Cursor_double _ number_row_start _ _ _ number_row_end _ _ _ x_render_start _ x_render_end _) intmap_intmap_texture seq_map=if number_row_start==number_row_end
    then let row_start=number_row_start-row in CM.when (x_render_start/=x_render_end&&0<=row_start&&row_start<row_number) $ do
        catch_error "render_select: SDL.Raw.setRenderDrawColor returns error" 0 (SRV.setRenderDrawColor renderer select_red select_green select_blue select_alpha)
        FMA.alloca $ \rect->do
            FS.poke rect (SRT.Rect (left+x_render_start) (up+fromIntegral (number_row_start-row)*(height+delta_height)) (x_render_end-x_render_start) height)
            catch_error "render_select: SDL.Raw.renderFillRect returns error" 0 (SRV.renderFillRect renderer rect)
    else let row_start=number_row_start-row in if 0<=row_start&&row_start<row_number
        then case DS.lookup number_row_start seq_map of
            Nothing->error "render_select: you changed something without proper design"
            Just (paragraph_id_start,row_id_start)->case DIS.lookup paragraph_id_start intmap_intmap_texture of
                Nothing->error "render_select: you changed something without proper design"
                Just intmap_texture_start->case DIS.lookup row_id_start intmap_texture_start of
                    Nothing->error "render_select: you changed something without proper design"
                    Just (_,x_start,width_start,_,_)->do
                        catch_error "render_select: SDL.Raw.setRenderDrawColor returns error" 0 (SRV.setRenderDrawColor renderer select_red select_green select_blue select_alpha)
                        let new_up=up+fromIntegral row_start*(height+delta_height)
                        FMA.alloca $ \rect->let new_width=x_start+width_start-x_render_start in CM.when (new_width/=0) $ do
                            FS.poke rect (SRT.Rect (left+x_render_start) new_up new_width height)
                            catch_error "render_select: SDL.Raw.renderFillRect returns error" 0 (SRV.renderFillRect renderer rect)
                        let row_end=number_row_end-row in if row_end<row_number
                            then case DS.lookup number_row_end seq_map of
                                Nothing->error "render_select: you changed something without proper design"
                                Just (paragraph_id_end,row_id_end)->case DIS.lookup paragraph_id_end intmap_intmap_texture of
                                    Nothing->error "render_select: you changed something without proper design"
                                    Just intmap_texture_end->case DIS.lookup row_id_end intmap_texture_end of
                                        Nothing->error "render_select: you changed something without proper design"
                                        Just (_,x_end,_,_,_)->do
                                            FMA.alloca $ \rect->let new_width=x_render_end-x_end in CM.when (new_width/=0) $ do
                                                FS.poke rect (SRT.Rect (left+x_end) (up+fromIntegral row_end*(height+delta_height)) new_width height)
                                                catch_error "render_select: SDL.Raw.renderFillRect returns error" 0 (SRV.renderFillRect renderer rect)
                                            render_select_a renderer height delta_height left (new_up+height+delta_height) intmap_intmap_texture (DS.drop (number_row_start+1) (DS.take number_row_end seq_map))
                            else render_select_a renderer height delta_height left (new_up+height+delta_height) intmap_intmap_texture (DS.drop (number_row_start+1) (DS.take (row+row_number) seq_map))
        else let row_end=number_row_end-row in if row_start<0&&0<=row_end&&row_end<row_number
            then case DS.lookup number_row_end seq_map of
                Nothing->error "render_select: you changed something without proper design"
                Just (paragraph_id_end,row_id_end)->case DIS.lookup paragraph_id_end intmap_intmap_texture of
                    Nothing->error "render_select: you changed something without proper design"
                    Just intmap_texture_end->case DIS.lookup row_id_end intmap_texture_end of
                        Nothing->error "render_select: you changed something without proper design"
                        Just (_,x_end,_,_,_)->do
                            catch_error "render_select: SDL.Raw.setRenderDrawColor returns error" 0 (SRV.setRenderDrawColor renderer select_red select_green select_blue select_alpha)
                            FMA.alloca $ \rect->let new_width=x_render_end-x_end in CM.when (new_width/=0) $ do
                                FS.poke rect (SRT.Rect (left+x_end) (up+fromIntegral row_end*(height+delta_height)) new_width height)
                                catch_error "render_select: SDL.Raw.renderFillRect returns error" 0 (SRV.renderFillRect renderer rect)
                                render_select_a renderer height delta_height left up intmap_intmap_texture (DS.drop row (DS.take number_row_end seq_map))
            else CM.when (row_start<0&&row_number<=row_end) $ do
                catch_error "render_select: SDL.Raw.setRenderDrawColor returns error" 0 (SRV.setRenderDrawColor renderer select_red select_green select_blue select_alpha)
                render_select_a renderer height delta_height left up intmap_intmap_texture (DS.drop row (DS.take (row+row_number) seq_map))
render_select _ _ _ _ _ _ _ _ _ _ _ _ _ _=return ()

render_select_a::SRT.Renderer->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DIS.IntMap (DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int))->DS.Seq (Int,Int)->IO ()
render_select_a _ _ _ _ _ _ DS.Empty=return ()
render_select_a renderer height delta_height left up intmap_intmap_texture ((paragraph_id,row_id) DS.:<| seq_map)=case DIS.lookup paragraph_id intmap_intmap_texture of
    Nothing->error "render_select_a: you changed something without proper design"
    Just intmap_texture->case DIS.lookup row_id intmap_texture of
        Nothing->error "render_select_a: you changed something without proper design"
        Just (_,x,width,_,_)->do
            FMA.alloca $ \rect-> CM.when (width/=0) $ do
                FS.poke rect (SRT.Rect (left+x) up width height)
                catch_error "render_select_a: SDL.Raw.renderFillRect returns error" 0 (SRV.renderFillRect renderer rect)
            render_select_a renderer height delta_height left (up+height+delta_height) intmap_intmap_texture seq_map

render_cursor::SRT.Renderer->Int->Int->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->Cursor->IO ()
render_cursor _ _ _ _ _ _ _ _ _ _ _ Cursor_none=return ()
render_cursor renderer row row_number cursor_red cursor_green cursor_blue cursor_alpha height delta_height left up (Cursor_single number_row _ _ _ x_render _)=let new_row=number_row-row in if new_row<0||row+row_number-1<number_row then return () else do
    catch_error "render_cursor: SDL.Raw.setRenderDrawColor returns error" 0 (SRV.setRenderDrawColor renderer cursor_red cursor_green cursor_blue cursor_alpha)
    let new_up=up+fromIntegral new_row*(height+delta_height) in catch_error "render_cursor: SDL.Raw.renderDrawLine returns error" 0 (SRV.renderDrawLine renderer (left+x_render) new_up (left+x_render) (new_up+height))
render_cursor renderer row row_number cursor_red cursor_green cursor_blue cursor_alpha height delta_height left up (Cursor_double bool number_row_start _ _ _ number_row_end _ _ _ x_render_start _ x_render_end _)=if bool
    then let new_row=number_row_start-row in if new_row<0||row+row_number-1<number_row_start then return () else do
        catch_error "render_cursor: SDL.Raw.setRenderDrawColor returns error" 0 (SRV.setRenderDrawColor renderer cursor_red cursor_green cursor_blue cursor_alpha)
        let new_up=up+fromIntegral new_row*(height+delta_height) in catch_error "render_cursor: SDL.Raw.renderDrawLine returns error" 0 (SRV.renderDrawLine renderer (left+x_render_start) new_up (left+x_render_start) (new_up+height))
    else let new_row=number_row_end-row in if new_row<0||row+row_number-1<number_row_end then return () else do
        catch_error "render_cursor: SDL.Raw.setRenderDrawColor returns error" 0 (SRV.setRenderDrawColor renderer cursor_red cursor_green cursor_blue cursor_alpha)
        let new_up=up+fromIntegral new_row*(height+delta_height) in catch_error "render_cursor: SDL.Raw.renderDrawLine returns error" 0 (SRV.renderDrawLine renderer (left+x_render_end) new_up (left+x_render_end) (new_up+height))

render_text::SRT.Renderer->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DIS.IntMap (DIS.IntMap (SRT.Texture,FCT.CInt,FCT.CInt,Int,Int))->DS.Seq (Int,Int)->IO ()
render_text _ _ _ _ _ _ DS.Empty=return ()
render_text renderer height delta_height left up this_intmap_intmap_texture ((paragraph_id,row_id) DS.:<| seq_map)=case DIS.lookup paragraph_id this_intmap_intmap_texture of
    Nothing->error "render_text: you chenged something without proper design"
    Just intmap_texture->case DIS.lookup row_id intmap_texture of
        Nothing->error "render_text: you chenged something without proper design"
        Just (texture,x,width,_,_)->do
            CM.when (width/=0) (catch_error "render_text: SDL.Raw.Video.renderCopy returns error" 0 (FMU.with (SRT.Rect (left+x) up width height) (SRV.renderCopy renderer texture FP.nullPtr)))
            render_text renderer height delta_height left (up+height+delta_height) this_intmap_intmap_texture seq_map

render_seq_texture::FCT.CInt->FCT.CInt->FCT.CInt->SRT.Renderer->DS.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)->IO ()
render_seq_texture _ _ _ _ DS.Empty=return ()
render_seq_texture left up down renderer ((texture,x,y,width,height) DS.:<| seq_texture)=do
    catch_error "render_seq_texture: SDL.Raw.Video.renderCopy returns error" 0 (FMU.with (SRT.Rect (left+x) (up+y) width height) (SRV.renderCopy renderer texture FP.nullPtr))
    render_seq_texture left up down renderer seq_texture

render_seq_row::FCT.CInt->FCT.CInt->FCT.CInt->SRT.Renderer->DS.Seq Row->IO ()
render_seq_row _ _ _ _ DS.Empty=return ()
render_seq_row left up down renderer (row DS.:<| seq_row)=case row of
    Row seq_texture y row_height->if down<up+y+row_height then return () else do
        render_seq_texture left up down renderer seq_texture
        render_seq_row left up down renderer seq_row
    Row_blank y row_height->if down<up+y+row_height then return () else render_seq_row left up down renderer seq_row
