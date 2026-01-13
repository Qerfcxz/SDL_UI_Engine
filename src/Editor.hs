{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Editor where
import Other
import Type
import qualified Control.Monad as CM
import qualified Data.Char as DC
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Text.Foreign as DTF
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

render_editor::SRT.Renderer->Int->Int->Int->Typesetting->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FP.Ptr SRF.Font->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->Cursor->DS.Seq (DS.Seq Char,Int,Bool)->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))
render_editor renderer block_number row_number row typesetting text_red text_green text_blue text_alpha _ _ _ _ _ _ _ _ font height block_width delta_height x y Cursor_none seq_seq_char intmap_texture=FMA.alloca $ \text_color->do
    FS.poke text_color (color text_red text_green text_blue text_alpha)
    render_seq_seq_char renderer block_number typesetting font text_color text_red text_green text_blue text_alpha height block_width delta_height x y (DS.take row_number (DS.drop row seq_seq_char)) intmap_texture
render_editor renderer block_number row_number row typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha _ _ _ _ font height block_width delta_height x y (Cursor_single cursor_row cursor_number _) seq_seq_char intmap_texture=FMA.alloca $ \text_color->do
    FS.poke text_color (color text_red text_green text_blue text_alpha)
    if cursor_row<row||row+row_number<=cursor_row then render_seq_seq_char renderer block_number typesetting font text_color text_red text_green text_blue text_alpha height block_width delta_height x y (DS.take row_number (DS.drop row seq_seq_char)) intmap_texture else do
        new_intmap_texture<-render_seq_seq_char renderer block_number typesetting font text_color text_red text_green text_blue text_alpha height block_width delta_height x y (DS.take row_number (DS.drop row seq_seq_char)) intmap_texture
        catch_error "render_editor: SDL.Raw.setRenderDrawColor returns error" 0 (SRV.setRenderDrawColor renderer cursor_red cursor_green cursor_blue cursor_alpha)
        let new_x=x+fromIntegral cursor_number*block_width in let new_y=y+fromIntegral (cursor_row-row)*(height+delta_height) in catch_error "render_editor: SDL.Raw.renderDrawLine returns error" 0 (SRV.renderDrawLine renderer new_x new_y new_x (new_y+height))
        return new_intmap_texture
render_editor renderer block_number row_number row typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha font height block_width delta_height x y (Cursor_double cursor_where cursor_row_start cursor_number_start cursor_row_end cursor_number_end _ _) seq_seq_char intmap_texture=let (cursor_row,cursor_number)=if cursor_where then (cursor_row_start,cursor_number_start) else (cursor_row_end,cursor_number_end) in if cursor_row<row||row+row_number<=cursor_row
    then do
        render_select renderer cursor_row_start cursor_number_start cursor_row_end cursor_number_end block_number row_number row typesetting select_red select_green select_blue select_alpha block_width delta_height x y height seq_seq_char
        FMA.alloca $ \text_color->do
            FS.poke text_color (color text_red text_green text_blue text_alpha)
            render_seq_seq_char renderer block_number typesetting font text_color text_red text_green text_blue text_alpha height block_width delta_height x y (DS.take row_number (DS.drop row seq_seq_char)) intmap_texture
    else do
        render_select renderer cursor_row_start cursor_number_start cursor_row_end cursor_number_end block_number row_number row typesetting select_red select_green select_blue select_alpha block_width delta_height x y height seq_seq_char
        FMA.alloca $ \text_color->do
            FS.poke text_color (color text_red text_green text_blue text_alpha)
            new_intmap_texture<-render_seq_seq_char renderer block_number typesetting font text_color text_red text_green text_blue text_alpha height block_width delta_height x y (DS.take row_number (DS.drop row seq_seq_char)) intmap_texture
            catch_error "render_editor: SDL.Raw.setRenderDrawColor returns error" 0 (SRV.setRenderDrawColor renderer cursor_red cursor_green cursor_blue cursor_alpha)
            let new_x=x+fromIntegral cursor_number*block_width in let new_y=y+fromIntegral (cursor_row-row)*(height+delta_height) in catch_error "render_editor: SDL.Raw.renderDrawLine returns error" 0 (SRV.renderDrawLine renderer new_x new_y new_x (new_y+height))
            return new_intmap_texture

render_select::SRT.Renderer->Int->Int->Int->Int->Int->Int->Int->Typesetting->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DS.Seq Char,Int,Bool)->IO ()
render_select renderer cursor_row_start cursor_number_start cursor_row_end cursor_number_end block_number row_number row typesetting select_red select_green select_blue select_alpha block_width delta_height x y height seq_seq_char=let min_row=max cursor_row_start row in let max_row=min cursor_row_end (row+row_number-1) in if max_row<min_row then return () else if min_row==cursor_row_start
    then if max_row==cursor_row_end
        then if cursor_row_start==cursor_row_end
            then do
                _<-render_row False renderer cursor_number_start cursor_number_end cursor_row_start row select_red select_green select_blue select_alpha block_width delta_height x y height
                return ()
            else case DS.lookup min_row seq_seq_char of
                Nothing->error "render_select: you changed something without proper design"
                Just (_,number_start,_)->case DS.lookup max_row seq_seq_char of
                    Nothing->error "render_select: you changed something without proper design"
                    Just (_,number_end,_)->do
                        draw_start<-render_row False renderer cursor_number_start (typesetting_right typesetting number_start block_number) cursor_row_start row select_red select_green select_blue select_alpha block_width delta_height x y height
                        draw_end<-render_row draw_start renderer (typesetting_left typesetting number_end block_number) cursor_number_end cursor_row_end row select_red select_green select_blue select_alpha block_width delta_height x y height
                        let new_height=height+delta_height in FMA.alloca (\rect->render_select_a draw_end renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+fromIntegral (cursor_row_start-row+1)*new_height) height new_height (DS.take (cursor_row_end-cursor_row_start-1) (DS.drop (cursor_row_start+1) seq_seq_char)))
        else case DS.lookup min_row seq_seq_char of
            Nothing->error "render_select: you changed something without proper design"
            Just (_,number_start,_)->do
                draw_start<-render_row False renderer cursor_number_start (typesetting_right typesetting number_start block_number) cursor_row_start row select_red select_green select_blue select_alpha block_width delta_height x y height
                let new_height=height+delta_height in FMA.alloca (\rect->render_select_a draw_start renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+fromIntegral (cursor_row_start-row+1)*new_height) height new_height (DS.take (row+row_number-cursor_row_start-1) (DS.drop (cursor_row_start+1) seq_seq_char)))
    else if max_row==cursor_row_end
        then case DS.lookup max_row seq_seq_char of
            Nothing->error "render_select: you changed something without proper design"
            Just (_,number_end,_)->do
                draw_end<-render_row False renderer (typesetting_left typesetting number_end block_number) cursor_number_end cursor_row_end row select_red select_green select_blue select_alpha block_width delta_height x y height
                FMA.alloca (\rect->render_select_a draw_end renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x y height (height+delta_height) (DS.take (cursor_row_end-row) (DS.drop row seq_seq_char)))
        else FMA.alloca (\rect->render_select_a False renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x y height (height+delta_height) (DS.take row_number (DS.drop row seq_seq_char)))

render_select_a::Bool->SRT.Renderer->FP.Ptr SRT.Rect->Int->Typesetting->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DS.Seq Char,Int,Bool)->IO ()
render_select_a _ _ _ _ _ _ _ _ _ _ _ _ _ _ DS.Empty=return ()
render_select_a draw renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x y height row_height ((_,number,_) DS.:<| seq_seq_char)
    |draw=let left=typesetting_left typesetting number block_number in let right=typesetting_right typesetting number block_number in if left==right then render_select_a True renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+row_height) height row_height seq_seq_char else do
        FS.poke rect (SRT.Rect(x+fromIntegral left*block_width) y (fromIntegral (right-left)*block_width) height)
        catch_error "render_select_a: SDL.Raw.renderFillRect returns error" 0 (SRV.renderFillRect renderer rect)
        render_select_a True renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+row_height) height row_height seq_seq_char
    |otherwise=let left=typesetting_left typesetting number block_number in let right=typesetting_right typesetting number block_number in if left==right then render_select_a False renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+row_height) height row_height seq_seq_char else do
        catch_error "do_request: SDL.Raw.setRenderDrawColor returns error" 0 (SRV.setRenderDrawColor renderer select_red select_green select_blue select_alpha)
        FS.poke rect (SRT.Rect(x+fromIntegral left*block_width) y (fromIntegral (right-left)*block_width) height)
        catch_error "render_select_a: SDL.Raw.renderFillRect returns error" 0 (SRV.renderFillRect renderer rect)
        render_select_a True renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+row_height) height row_height seq_seq_char

render_row::Bool->SRT.Renderer->Int->Int->Int->Int->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->IO Bool
render_row draw renderer number_start number_end this_row row red green blue alpha block_width delta_height x y height
    |draw=if number_start==number_end then return True else do
        FMA.alloca $ \rect->do
            FS.poke rect (SRT.Rect(x+fromIntegral number_start*block_width) (y+fromIntegral (this_row-row)*(height+delta_height)) (fromIntegral (number_end-number_start)*block_width) height)
            catch_error "render_select_a: SDL.Raw.renderFillRect returns error" 0 (SRV.renderFillRect renderer rect)
        return True
    |otherwise=if number_start==number_end then return False else do
        catch_error "do_request: SDL.Raw.setRenderDrawColor returns error" 0 (SRV.setRenderDrawColor renderer red green blue alpha)
        FMA.alloca $ \rect->do
            FS.poke rect (SRT.Rect(x+fromIntegral number_start*block_width) (y+fromIntegral (this_row-row)*(height+delta_height)) (fromIntegral (number_end-number_start)*block_width) height)
            catch_error "render_select_a: SDL.Raw.renderFillRect returns error" 0 (SRV.renderFillRect renderer rect)
        return True

render_seq_seq_char::SRT.Renderer->Int->Typesetting->FP.Ptr SRF.Font->FP.Ptr Color->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DS.Seq Char,Int,Bool)->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))
render_seq_seq_char _ _ _ _ _ _ _ _ _ _ _ _ _ _ DS.Empty intmap_texture=return intmap_texture
render_seq_seq_char renderer block_number typesetting font text_color text_red text_green text_blue text_alpha height block_width delta_height x y ((seq_char,number,_) DS.:<| seq_seq_char) intmap_texture=do
    new_intmap_texture<-render_seq_seq_char_a renderer font text_color text_red text_green text_blue text_alpha height block_width (x+fromIntegral (typesetting_left typesetting number block_number)*block_width) y seq_char intmap_texture
    render_seq_seq_char renderer block_number typesetting font text_color text_red text_green text_blue text_alpha height block_width delta_height x (y+height+delta_height) seq_seq_char new_intmap_texture

render_seq_seq_char_a::SRT.Renderer->FP.Ptr SRF.Font->FP.Ptr Color->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq Char->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))
render_seq_seq_char_a _ _ _ _ _ _ _ _ _ _ _ DS.Empty intmap_texture=return intmap_texture 
render_seq_seq_char_a renderer font text_color text_red text_green text_blue text_alpha height block_width x y (char DS.:<| seq_char) intmap_texture=let char_ord=DC.ord char in case DIS.lookup char_ord intmap_texture of
    Nothing->do
        (texture,width)<-DTF.withCString (DT.singleton char) (to_texture_with_width renderer text_color font)
        catch_error "render_seq_seq_char_a: SDL.Raw.Video.setTextureColorMod returns error" 0 (SRV.setTextureColorMod texture text_red text_green text_blue)
        catch_error "render_seq_seq_char_a: SDL.Raw.Video.setTextureAlphaMod returns error" 0 (SRV.setTextureAlphaMod texture text_alpha)
        let width_mod=mod width block_width
        let block=if width_mod==0 then div width block_width else div width block_width+1
        let delta_x=div (block_width-width_mod) 2
        catch_error "render_seq_seq_char_a: SDL.Raw.Video.renderCopy returns error" 0 (FMU.with (SRT.Rect (x+delta_x) y width height) (SRV.renderCopy renderer texture FP.nullPtr))
        render_seq_seq_char_a renderer font text_color text_red text_green text_blue text_alpha height block_width (x+fromIntegral block*block_width) y seq_char (DIS.insert char_ord (texture,DIS.singleton (fromIntegral block_width) (fromIntegral block,delta_x),width,text_red,text_green,text_blue,text_alpha) intmap_texture)
    Just (texture,intmap_int,width,red,green,blue,alpha)->do
        let first_check=text_red/=red||text_green/=green||text_blue/=blue
        let second_check=text_alpha/=alpha
        CM.when first_check (catch_error "render_seq_seq_char_a: SDL.Raw.Video.setTextureColorMod returns error" 0 (SRV.setTextureColorMod texture text_red text_green text_blue))
        CM.when second_check (catch_error "render_seq_seq_char_a: SDL.Raw.Video.setTextureAlphaMod returns error" 0 (SRV.setTextureAlphaMod texture text_alpha))
        let this_block_width=fromIntegral block_width in case DIS.lookup this_block_width intmap_int of
            Nothing->do
                let width_mod=mod width block_width
                let block=if width_mod==0 then div width block_width else div width block_width+1
                let delta_x=div (block_width-width_mod) 2
                catch_error "render_seq_seq_char_a: SDL.Raw.Video.renderCopy returns error" 0 (FMU.with (SRT.Rect (x+delta_x) y width height) (SRV.renderCopy renderer texture FP.nullPtr))
                render_seq_seq_char_a renderer font text_color text_red text_green text_blue text_alpha height block_width (x+block*block_width) y seq_char (if first_check||second_check then DIS.insert char_ord (texture,DIS.insert this_block_width (fromIntegral block,delta_x) intmap_int,width,text_red,text_green,text_blue,text_alpha) intmap_texture else DIS.adjust (\(this_texture,this_intmap_int,this_width,this_red,this_green,this_blue,this_alpha)->(this_texture,DIS.insert this_block_width (fromIntegral block,delta_x) this_intmap_int,this_width,this_red,this_green,this_blue,this_alpha)) char_ord intmap_texture)
            Just(block,delta_x)->do
                catch_error "render_seq_seq_char_a: SDL.Raw.Video.renderCopy returns error" 0 (FMU.with (SRT.Rect (x+delta_x) y width height) (SRV.renderCopy renderer texture FP.nullPtr))
                render_seq_seq_char_a renderer font text_color text_red text_green text_blue text_alpha height block_width (x+fromIntegral block*block_width) y seq_char (if first_check||second_check then DIS.insert char_ord (texture,intmap_int,width,text_red,text_green,text_blue,text_alpha) intmap_texture else intmap_texture)

typesetting_left::Typesetting->Int->Int->Int
typesetting_left Typesetting_left _ _=0
typesetting_left Typesetting_right number block_number=block_number-number
typesetting_left Typesetting_center number block_number=div (block_number-number) 2

typesetting_right::Typesetting->Int->Int->Int
typesetting_right Typesetting_left number _=number
typesetting_right Typesetting_right _ block_number=block_number
typesetting_right Typesetting_center number block_number=div (block_number+number) 2

find_block_font_equal::DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->Int->Int->DS.Seq Int->(Int,FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))
find_block_font_equal widget design_window_size window_size start_id size seq_id=case get_widget_widget seq_id start_id widget of
    Leaf_widget _ (Block_font window_id _ _ _ _ block_font)->case DIS.lookup (div (size*fromIntegral window_size) (fromIntegral design_window_size)) block_font of
        Nothing->error "find_block_font_equal: no such font size"
        Just (font,height,intmap_texture)->(window_id,font,height,intmap_texture)
    _->error "find_block_font_equal: not a font widget"

find_block_font_near::DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->Int->Int->DS.Seq Int->(Int,FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))
find_block_font_near widget design_window_size window_size start_id size seq_id=case get_widget_widget seq_id start_id widget of
    Leaf_widget _ (Block_font window_id _ _ _ _ block_font)->let new_size=div (size*fromIntegral window_size) (fromIntegral design_window_size) in case DIS.lookupLE new_size block_font of
        Nothing->case DIS.lookupGE new_size block_font of
            Nothing->error "find_block_font_near: empty font widget"
            Just (_,(great_font,great_height,great_intmap_texture))->(window_id,great_font,great_height,great_intmap_texture)
        Just (small_size,(small_font,small_height,small_intmap_texture))->case DIS.lookupGE new_size block_font of
            Nothing->(window_id,small_font,small_height,small_intmap_texture)
            Just (great_size,(great_font,great_height,great_intmap_texture))->if 2*new_size<great_size+small_size then (window_id,small_font,small_height,small_intmap_texture) else (window_id,great_font,great_height,great_intmap_texture)
    _->error "find_block_font_near: not a font widget"

find_block_font::Texture_find->(DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->Int->Int->DS.Seq Int->(Int,FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)))
find_block_font Texture_equal=find_block_font_equal
find_block_font Texture_near=find_block_font_near

use_block_font::Texture_find->FCT.CInt->FCT.CInt->DS.Seq Int->Int->Int->(FP.Ptr SRF.Font->FCT.CInt->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)))->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))
use_block_font Texture_equal design_window_size window_size seq_id start_id size update widget=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in error_update_update_io "use_block_font: no such combined_id" "use_block_font: no such single_id" combined_id single_id (use_block_font_equal design_window_size window_size size update) widget
use_block_font Texture_near design_window_size window_size seq_id start_id size update widget=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in error_update_update_io "use_block_font: no such combined_id" "use_block_font: no such single_id" combined_id single_id (use_block_font_near design_window_size window_size size update) widget

use_block_font_a::(FP.Ptr SRF.Font->FCT.CInt->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)))->Maybe (FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))->IO (Maybe (FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)))
use_block_font_a _ Nothing=error "use_block_font_a: no such font size"
use_block_font_a update (Just (font,height,intmap_texture))=do
    new_intmap_texture<-update font height intmap_texture
    return (Just (font,height,new_intmap_texture))

use_block_font_equal::FCT.CInt->FCT.CInt->Int->(FP.Ptr SRF.Font->FCT.CInt->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)))->Combined_widget a->IO (Combined_widget a)
use_block_font_equal design_window_size window_size size update (Leaf_widget next_id (Block_font window_id red green blue alpha font))=do
    new_font<-DIS.alterF (use_block_font_a update) (div (size*fromIntegral window_size) (fromIntegral design_window_size)) font
    return (Leaf_widget next_id (Block_font window_id red green blue alpha new_font))
use_block_font_equal _ _ _ _ _=error "use_block_font_equal: not a block_font widget"

use_block_font_near::FCT.CInt->FCT.CInt->Int->(FP.Ptr SRF.Font->FCT.CInt->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)))->Combined_widget a->IO (Combined_widget a)
use_block_font_near design_window_size window_size size update (Leaf_widget next_id (Block_font window_id red green blue alpha font))=do
    new_font<-use_block_font_near_a (div (size*fromIntegral window_size) (fromIntegral design_window_size)) update font
    return (Leaf_widget next_id (Block_font window_id red green blue alpha new_font))
use_block_font_near _ _ _ _ _=error "use_block_font_near: not a block_font widget"

use_block_font_near_a::Int->(FP.Ptr SRF.Font->FCT.CInt->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)))->DIS.IntMap (FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))->IO (DIS.IntMap (FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)))
use_block_font_near_a size update font=case DIS.lookupLE size font of
    Nothing->case DIS.lookupGE size font of
        Nothing->error "use_block_font_near_a: empty block_font widget"
        Just (great_size,_)->DIS.alterF (use_block_font_a update) great_size font
    Just (small_size,_)->case DIS.lookupGE size font of
        Nothing->DIS.alterF (use_block_font_a update) small_size font
        Just (great_size,_)->if 2*size<small_size+great_size then DIS.alterF (use_block_font_a update) small_size font else DIS.alterF (use_block_font_a update) great_size font

from_seq_seq_char::SRT.Renderer->FP.Ptr Color->FP.Ptr SRF.Font->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->Int->FCT.CInt->DS.Seq (DS.Seq Char)->DS.Seq (DS.Seq Char,Int,Bool)->IO (DS.Seq (DS.Seq Char,Int,Bool))
from_seq_seq_char _ _ _ _ _ _ DS.Empty seq_seq_char=return seq_seq_char
from_seq_seq_char renderer text_color font intmap_texture block_number block_width (seq_char DS.:<| other_seq_text) seq_seq_char=do
    new_seq_text<-from_seq_seq_char_a renderer text_color font intmap_texture 0 block_number block_width DS.empty seq_char seq_seq_char
    from_seq_seq_char renderer text_color font intmap_texture block_number block_width other_seq_text new_seq_text

from_seq_seq_char_a::SRT.Renderer->FP.Ptr Color->FP.Ptr SRF.Font->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->Int->Int->FCT.CInt->DS.Seq Char->DS.Seq Char->DS.Seq (DS.Seq Char,Int,Bool)->IO (DS.Seq (DS.Seq Char,Int,Bool))
from_seq_seq_char_a _ _ _ _ number _ _ seq_char DS.Empty seq_seq_char=return (seq_seq_char DS.|> (seq_char,number,True))
from_seq_seq_char_a renderer text_color font intmap_texture number block_number block_width seq_char (char DS.:<| other_text) seq_seq_char=let char_ord=DC.ord char in case DIS.lookup char_ord intmap_texture of
    Nothing->error "from_seq_seq_char_a: no such char"
    Just (_,intmap_int,_,_,_,_,_)->case DIS.lookup (fromIntegral block_width) intmap_int of
        Nothing->error "from_seq_seq_char_a: no such block_width"
        Just (block,_)->let new_number=number+block in if new_number<=block_number then from_seq_seq_char_a renderer text_color font intmap_texture new_number block_number block_width (seq_char DS.:|> char) other_text seq_seq_char else if block_number<block then error "from_seq_seq_char_a: too large font" else from_seq_seq_char_a renderer text_color font intmap_texture block block_number block_width (DS.singleton char) other_text (seq_seq_char DS.|> (seq_char,number,False))

update_block_font::DIS.IntMap Window->Int->FCT.CInt->DS.Seq Char->Combined_widget a->IO (Combined_widget a)
update_block_font window size block_width seq_char (Leaf_widget next_id (Block_font window_id red green blue alpha font))=do
    new_font<-DIS.alterF (update_block_font_a (get_renderer_window window_id window) red green blue alpha block_width seq_char) size font
    return (Leaf_widget next_id (Block_font window_id red green blue alpha new_font))
update_block_font _ _ _ _ _=error "update_block_font: not a block_font widget"

update_block_font_a::SRT.Renderer->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->DS.Seq Char->Maybe (FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))->IO (Maybe (FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)))
update_block_font_a _ _ _ _ _ _ _ Nothing=error "update_block_font_a no such font size"
update_block_font_a renderer red green blue alpha block_width seq_char (Just (font,height,intmap_texture))=FMA.alloca $ \font_color->do
    FS.poke font_color (color red green blue alpha)
    new_intmap_texture<-update_block_font_b renderer font_color red green blue alpha font block_width seq_char intmap_texture
    return (Just (font,height,new_intmap_texture))

update_block_font_b::SRT.Renderer->FP.Ptr Color->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FP.Ptr SRF.Font->FCT.CInt->DS.Seq Char->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))
update_block_font_b _ _ _ _ _ _ _ _ DS.Empty intmap_texture=return intmap_texture
update_block_font_b renderer font_color red green blue alpha font block_width (char DS.:<| seq_char) intmap_texture=let char_ord=DC.ord char in case DIS.lookup char_ord intmap_texture of
    Nothing->do
        (texture,width)<-DTF.withCString (DT.singleton char) (to_texture_with_width renderer font_color font)
        let width_mod=mod width block_width
        let block=if width_mod==0 then div width block_width else div width block_width+1
        update_block_font_b renderer font_color red green blue alpha font block_width seq_char (DIS.insert char_ord (texture,DIS.singleton (fromIntegral block_width) (fromIntegral block,div (block_width-width_mod) 2),width,red,green,blue,alpha) intmap_texture)
    Just (texture,intmap_int,width,this_red,this_green,this_blue,this_alpha)->let this_block_width=fromIntegral block_width in if DIS.member this_block_width intmap_int then update_block_font_b renderer font_color red green blue alpha font block_width seq_char intmap_texture else do
        let width_mod=mod width block_width
        let block=if width_mod==0 then div width block_width else div width block_width+1
        update_block_font_b renderer font_color red green blue alpha font block_width seq_char (DIS.insert char_ord (texture,DIS.insert this_block_width (fromIntegral block,div (block_width-width_mod) 2) intmap_int,width,this_red,this_green,this_blue,this_alpha) intmap_texture)

--Editor Int（window_id） Int（每行几个格子） Int（显示几行） Int（当前文本框第一行是文本第几行） Int（字体大小）
--Bool（render标记） DS.Seq Int（字体路径） Texture_find（字体资源查找策略） Typesetting（排版模式） Color（文字颜色） DW.Word8 DW.Word8 DW.Word8 DW.Word8（光标颜色） DW.Word8 DW.Word8 DW.Word8 DW.Word8（选择框颜色）
--FCT.CInt（每个格子的宽度） FCT.CInt（控件最大高度） FCT.CInt（额外行间距） FCT.CInt FCT.CInt（控件中心坐标） FCT.CInt FCT.CInt（额外判定区域的长宽）
--FCT.CInt（实际的额外行间距） FCT.CInt FCT.CInt（实际的控件文字左上角坐标） FCT.CInt FCT.CInt FCT.CInt FCT.CInt（实际的判定区的四个边缘的坐标）