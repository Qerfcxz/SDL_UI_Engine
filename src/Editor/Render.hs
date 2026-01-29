{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Editor.Render where
import Editor.Typesetting
import Other.Error
import Type
import qualified Control.Monad as CM
import qualified Data.Char as DC
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

render_editor::SRT.Renderer->Int->Int->Int->Typesetting->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->Cursor->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))
render_editor renderer block_number row_number row typesetting text_red text_green text_blue text_alpha _ _ _ _ _ _ _ _ font_height block_width delta_height x y Cursor_none seq_seq_char intmap_texture=FMA.alloca (\rect->render_seq_seq_char renderer rect block_number typesetting text_red text_green text_blue text_alpha block_width x y font_height (font_height+delta_height) (DS.take row_number (DS.drop row seq_seq_char)) intmap_texture)
render_editor renderer block_number row_number row typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha _ _ _ _ font_height block_width delta_height x y (Cursor_single cursor_row cursor_block _ _) seq_seq_char intmap_texture=do
    if cursor_row<row||row+row_number<=cursor_row then FMA.alloca (\rect->render_seq_seq_char renderer rect block_number typesetting text_red text_green text_blue text_alpha block_width x y font_height (font_height+delta_height) (DS.take row_number (DS.drop row seq_seq_char)) intmap_texture) else do
        new_intmap_texture<-FMA.alloca (\rect->render_seq_seq_char renderer rect block_number typesetting text_red text_green text_blue text_alpha block_width x y font_height (font_height+delta_height) (DS.take row_number (DS.drop row seq_seq_char)) intmap_texture)
        catch_error "render_editor: error 1" 0 (SRV.setRenderDrawColor renderer cursor_red cursor_green cursor_blue cursor_alpha)
        let new_x=x+fromIntegral cursor_block*block_width in let new_y=y+fromIntegral (cursor_row-row)*(font_height+delta_height) in catch_error "render_editor: error 2" 0 (SRV.renderDrawLine renderer new_x new_y new_x (new_y+font_height-1))
        return new_intmap_texture
render_editor renderer block_number row_number row typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha font_height block_width delta_height x y (Cursor_double cursor_where cursor_row_start cursor_number_start _ _ cursor_row_end cursor_number_end _ _) seq_seq_char intmap_texture=let (cursor_row,cursor_block)=if cursor_where then (cursor_row_start,cursor_number_start) else (cursor_row_end,cursor_number_end) in if cursor_row<row||row+row_number<=cursor_row
    then FMA.alloca $ \rect->do
            render_select renderer rect cursor_row_start cursor_number_start cursor_row_end cursor_number_end block_number row_number row typesetting select_red select_green select_blue select_alpha block_width delta_height x y font_height seq_seq_char
            render_seq_seq_char renderer rect block_number typesetting text_red text_green text_blue text_alpha block_width x y font_height (font_height+delta_height) (DS.take row_number (DS.drop row seq_seq_char)) intmap_texture
    else FMA.alloca $ \rect->do
        render_select renderer rect cursor_row_start cursor_number_start cursor_row_end cursor_number_end block_number row_number row typesetting select_red select_green select_blue select_alpha block_width delta_height x y font_height seq_seq_char
        new_intmap_texture<-render_seq_seq_char renderer rect block_number typesetting text_red text_green text_blue text_alpha block_width x y font_height (font_height+delta_height) (DS.take row_number (DS.drop row seq_seq_char)) intmap_texture
        catch_error "render_editor: error 3" 0 (SRV.setRenderDrawColor renderer cursor_red cursor_green cursor_blue cursor_alpha)
        let new_x=x+fromIntegral cursor_block*block_width in let new_y=y+fromIntegral (cursor_row-row)*(font_height+delta_height) in catch_error "render_editor: error 4" 0 (SRV.renderDrawLine renderer new_x new_y new_x (new_y+font_height-1))
        return new_intmap_texture

render_select::SRT.Renderer->FP.Ptr SRT.Rect->Int->Int->Int->Int->Int->Int->Int->Typesetting->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->IO ()
render_select renderer rect cursor_row_start cursor_number_start cursor_row_end cursor_number_end block_number row_number row typesetting select_red select_green select_blue select_alpha block_width delta_height x y font_height seq_seq_char=let min_row=max cursor_row_start row in let max_row=min cursor_row_end (row+row_number-1) in if max_row<min_row then return () else if min_row==cursor_row_start
    then if max_row==cursor_row_end
        then if cursor_row_start==cursor_row_end
            then do
                _<-render_row False renderer rect cursor_number_start cursor_number_end cursor_row_start row select_red select_green select_blue select_alpha block_width delta_height x y font_height
                return ()
            else case error_lookup_sequence "render_select: error 1" min_row seq_seq_char of
                (_,number_start,_,_)->case error_lookup_sequence "render_select: error 2" max_row seq_seq_char of
                    (_,number_end,_,_)->do
                        render_start<-render_row False renderer rect cursor_number_start (typesetting_right typesetting number_start block_number) cursor_row_start row select_red select_green select_blue select_alpha block_width delta_height x y font_height
                        render_end<-render_row render_start renderer rect (typesetting_left typesetting number_end block_number) cursor_number_end cursor_row_end row select_red select_green select_blue select_alpha block_width delta_height x y font_height
                        let new_height=font_height+delta_height in render_select_a render_end renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+fromIntegral (cursor_row_start-row+1)*new_height) font_height new_height (DS.take (cursor_row_end-cursor_row_start-1) (DS.drop (cursor_row_start+1) seq_seq_char))
        else case error_lookup_sequence "render_select: error 3" min_row seq_seq_char of
            (_,number_start,_,_)->do
                render_start<-render_row False renderer rect cursor_number_start (typesetting_right typesetting number_start block_number) cursor_row_start row select_red select_green select_blue select_alpha block_width delta_height x y font_height
                let new_height=font_height+delta_height in render_select_a render_start renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+fromIntegral (cursor_row_start-row+1)*new_height) font_height new_height (DS.take (row+row_number-cursor_row_start-1) (DS.drop (cursor_row_start+1) seq_seq_char))
    else if max_row==cursor_row_end
        then case error_lookup_sequence "render_select: error 4" max_row seq_seq_char of
            (_,number_end,_,_)->do
                render_end<-render_row False renderer rect (typesetting_left typesetting number_end block_number) cursor_number_end cursor_row_end row select_red select_green select_blue select_alpha block_width delta_height x y font_height
                render_select_a render_end renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x y font_height (font_height+delta_height) (DS.take (cursor_row_end-row) (DS.drop row seq_seq_char))
        else render_select_a False renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x y font_height (font_height+delta_height) (DS.take row_number (DS.drop row seq_seq_char))

render_select_a::Bool->SRT.Renderer->FP.Ptr SRT.Rect->Int->Typesetting->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->IO ()
render_select_a _ _ _ _ _ _ _ _ _ _ _ _ _ _ DS.Empty=return ()
render_select_a render renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x y font_height height ((_,number,_,_) DS.:<| seq_seq_char)
    |render=let left=typesetting_left typesetting number block_number in let right=typesetting_right typesetting number block_number in if left==right then render_select_a True renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+height) font_height height seq_seq_char else do
        FS.poke rect (SRT.Rect (x+fromIntegral left*block_width) y (fromIntegral (right-left)*block_width) font_height)
        catch_error "render_select_a: error 1" 0 (SRV.renderFillRect renderer rect)
        render_select_a True renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+height) font_height height seq_seq_char
    |otherwise=let left=typesetting_left typesetting number block_number in let right=typesetting_right typesetting number block_number in if left==right then render_select_a False renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+height) font_height height seq_seq_char else do
        catch_error "render_select_a: error 2" 0 (SRV.setRenderDrawColor renderer select_red select_green select_blue select_alpha)
        FS.poke rect (SRT.Rect (x+fromIntegral left*block_width) y (fromIntegral (right-left)*block_width) font_height)
        catch_error "render_select_a: error 3" 0 (SRV.renderFillRect renderer rect)
        render_select_a True renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+height) font_height height seq_seq_char

render_row::Bool->SRT.Renderer->FP.Ptr SRT.Rect->Int->Int->Int->Int->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->IO Bool
render_row render renderer rect number_start number_end this_row row red green blue alpha block_width delta_height x y font_height
    |render=if number_start==number_end then return True else do
        FS.poke rect (SRT.Rect (x+fromIntegral number_start*block_width) (y+fromIntegral (this_row-row)*(font_height+delta_height)) (fromIntegral (number_end-number_start)*block_width) font_height)
        catch_error "render_row: error 1" 0 (SRV.renderFillRect renderer rect)
        return True
    |otherwise=if number_start==number_end then return False else do
        catch_error "render_row: error 2" 0 (SRV.setRenderDrawColor renderer red green blue alpha)
        FS.poke rect (SRT.Rect (x+fromIntegral number_start*block_width) (y+fromIntegral (this_row-row)*(font_height+delta_height)) (fromIntegral (number_end-number_start)*block_width) font_height)
        catch_error "render_row: error 3" 0 (SRV.renderFillRect renderer rect)
        return True

render_seq_seq_char::SRT.Renderer->FP.Ptr SRT.Rect->Int->Typesetting->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))
render_seq_seq_char _ _ _ _ _ _ _ _ _ _ _ _ _ DS.Empty intmap_texture=return intmap_texture
render_seq_seq_char renderer rect block_number typesetting text_red text_green text_blue text_alpha block_width x y font_height height ((seq_char,number,_,_) DS.:<| seq_seq_char) intmap_texture=do
    new_intmap_texture<-render_seq_seq_char_a renderer rect text_red text_green text_blue text_alpha block_width (x+fromIntegral (typesetting_left typesetting number block_number)*block_width) y font_height seq_char intmap_texture
    render_seq_seq_char renderer rect block_number typesetting text_red text_green text_blue text_alpha block_width x (y+height) font_height height seq_seq_char new_intmap_texture

render_seq_seq_char_a::SRT.Renderer->FP.Ptr SRT.Rect->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (Char,Int,FCT.CInt)->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))
render_seq_seq_char_a _ _ _ _ _ _ _ _ _ _ DS.Empty intmap_texture=return intmap_texture 
render_seq_seq_char_a renderer rect text_red text_green text_blue text_alpha block_width x y font_height ((char,block,delta_x) DS.:<| seq_char) intmap_texture=let char_ord=DC.ord char in case error_lookup "render_seq_seq_char_a: error 1" char_ord intmap_texture of
    (texture,intmap_int,width,red,green,blue,alpha)->do
        let first_check=text_red/=red||text_green/=green||text_blue/=blue
        let second_check=text_alpha/=alpha
        CM.when first_check (catch_error "render_seq_seq_char_a: error 2" 0 (SRV.setTextureColorMod texture text_red text_green text_blue))
        CM.when second_check (catch_error "render_seq_seq_char_a: error 3" 0 (SRV.setTextureAlphaMod texture text_alpha))
        FS.poke rect (SRT.Rect (x+delta_x) y width font_height)
        catch_error "render_seq_seq_char_a: error 4" 0 (SRV.renderCopy renderer texture FP.nullPtr rect)
        render_seq_seq_char_a renderer rect text_red text_green text_blue text_alpha block_width (x+fromIntegral block*block_width) y font_height seq_char (if first_check||second_check then DIS.insert char_ord (texture,intmap_int,width,text_red,text_green,text_blue,text_alpha) intmap_texture else intmap_texture)