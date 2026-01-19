{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Editor where
import Other
import Type
import qualified Control.Monad as CM
import qualified Data.ByteString as DB
import qualified Data.Char as DC
import qualified Data.Foldable as DF
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Foreign as DTF
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.Builder as DTLB
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Basic as SRB
import qualified SDL.Raw.Font as SRF
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
            else case DS.lookup min_row seq_seq_char of
                Nothing->error "render_select: error 1"
                Just (_,number_start,_,_)->case DS.lookup max_row seq_seq_char of
                    Nothing->error "render_select: error 2"
                    Just (_,number_end,_,_)->do
                        render_start<-render_row False renderer rect cursor_number_start (typesetting_right typesetting number_start block_number) cursor_row_start row select_red select_green select_blue select_alpha block_width delta_height x y font_height
                        render_end<-render_row render_start renderer rect (typesetting_left typesetting number_end block_number) cursor_number_end cursor_row_end row select_red select_green select_blue select_alpha block_width delta_height x y font_height
                        let new_height=font_height+delta_height in render_select_a render_end renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+fromIntegral (cursor_row_start-row+1)*new_height) font_height new_height (DS.take (cursor_row_end-cursor_row_start-1) (DS.drop (cursor_row_start+1) seq_seq_char))
        else case DS.lookup min_row seq_seq_char of
            Nothing->error "render_select: error 3"
            Just (_,number_start,_,_)->do
                render_start<-render_row False renderer rect cursor_number_start (typesetting_right typesetting number_start block_number) cursor_row_start row select_red select_green select_blue select_alpha block_width delta_height x y font_height
                let new_height=font_height+delta_height in render_select_a render_start renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+fromIntegral (cursor_row_start-row+1)*new_height) font_height new_height (DS.take (row+row_number-cursor_row_start-1) (DS.drop (cursor_row_start+1) seq_seq_char))
    else if max_row==cursor_row_end
        then case DS.lookup max_row seq_seq_char of
            Nothing->error "render_select: error 4"
            Just (_,number_end,_,_)->do
                render_end<-render_row False renderer rect (typesetting_left typesetting number_end block_number) cursor_number_end cursor_row_end row select_red select_green select_blue select_alpha block_width delta_height x y font_height
                render_select_a render_end renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x y font_height (font_height+delta_height) (DS.take (cursor_row_end-row) (DS.drop row seq_seq_char))
        else render_select_a False renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x y font_height (font_height+delta_height) (DS.take row_number (DS.drop row seq_seq_char))

render_select_a::Bool->SRT.Renderer->FP.Ptr SRT.Rect->Int->Typesetting->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->IO ()
render_select_a _ _ _ _ _ _ _ _ _ _ _ _ _ _ DS.Empty=return ()
render_select_a render renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x y font_height height ((_,number,_,_) DS.:<| seq_seq_char)
    |render=let left=typesetting_left typesetting number block_number in let right=typesetting_right typesetting number block_number in if left==right then render_select_a True renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+height) font_height height seq_seq_char else do
        FS.poke rect (SRT.Rect(x+fromIntegral left*block_width) y (fromIntegral (right-left)*block_width) font_height)
        catch_error "render_select_a: error 1" 0 (SRV.renderFillRect renderer rect)
        render_select_a True renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+height) font_height height seq_seq_char
    |otherwise=let left=typesetting_left typesetting number block_number in let right=typesetting_right typesetting number block_number in if left==right then render_select_a False renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+height) font_height height seq_seq_char else do
        catch_error "render_select_a: error 2" 0 (SRV.setRenderDrawColor renderer select_red select_green select_blue select_alpha)
        FS.poke rect (SRT.Rect(x+fromIntegral left*block_width) y (fromIntegral (right-left)*block_width) font_height)
        catch_error "render_select_a: error 3" 0 (SRV.renderFillRect renderer rect)
        render_select_a True renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+height) font_height height seq_seq_char

render_row::Bool->SRT.Renderer->FP.Ptr SRT.Rect->Int->Int->Int->Int->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->IO Bool
render_row render renderer rect number_start number_end this_row row red green blue alpha block_width delta_height x y font_height
    |render=if number_start==number_end then return True else do
        FS.poke rect (SRT.Rect(x+fromIntegral number_start*block_width) (y+fromIntegral (this_row-row)*(font_height+delta_height)) (fromIntegral (number_end-number_start)*block_width) font_height)
        catch_error "render_row: error 1" 0 (SRV.renderFillRect renderer rect)
        return True
    |otherwise=if number_start==number_end then return False else do
        catch_error "render_row: error 2" 0 (SRV.setRenderDrawColor renderer red green blue alpha)
        FS.poke rect (SRT.Rect(x+fromIntegral number_start*block_width) (y+fromIntegral (this_row-row)*(font_height+delta_height)) (fromIntegral (number_end-number_start)*block_width) font_height)
        catch_error "render_row: error 3" 0 (SRV.renderFillRect renderer rect)
        return True

render_seq_seq_char::SRT.Renderer->FP.Ptr SRT.Rect->Int->Typesetting->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))
render_seq_seq_char _ _ _ _ _ _ _ _ _ _ _ _ _ DS.Empty intmap_texture=return intmap_texture
render_seq_seq_char renderer rect block_number typesetting text_red text_green text_blue text_alpha block_width x y font_height height ((seq_char,number,_,_) DS.:<| seq_seq_char) intmap_texture=do
    new_intmap_texture<-render_seq_seq_char_a renderer rect text_red text_green text_blue text_alpha block_width (x+fromIntegral (typesetting_left typesetting number block_number)*block_width) y font_height seq_char intmap_texture
    render_seq_seq_char renderer rect block_number typesetting text_red text_green text_blue text_alpha block_width x (y+height) font_height height seq_seq_char new_intmap_texture

render_seq_seq_char_a::SRT.Renderer->FP.Ptr SRT.Rect->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (Char,Int,FCT.CInt)->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))
render_seq_seq_char_a _ _ _ _ _ _ _ _ _ _ DS.Empty intmap_texture=return intmap_texture 
render_seq_seq_char_a renderer rect text_red text_green text_blue text_alpha block_width x y font_height ((char,block,delta_x) DS.:<| seq_char) intmap_texture=let char_ord=DC.ord char in case DIS.lookup char_ord intmap_texture of
    Nothing->error "render_seq_seq_char_a: error 1"
    Just (texture,intmap_int,width,red,green,blue,alpha)->do
        let first_check=text_red/=red||text_green/=green||text_blue/=blue
        let second_check=text_alpha/=alpha
        CM.when first_check (catch_error "render_seq_seq_char_a: error 2" 0 (SRV.setTextureColorMod texture text_red text_green text_blue))
        CM.when second_check (catch_error "render_seq_seq_char_a: error 3" 0 (SRV.setTextureAlphaMod texture text_alpha))
        FS.poke rect (SRT.Rect (x+delta_x) y width font_height)
        catch_error "render_seq_seq_char_a: error 4" 0 (SRV.renderCopy renderer texture FP.nullPtr rect)
        render_seq_seq_char_a renderer rect text_red text_green text_blue text_alpha block_width (x+fromIntegral block*block_width) y font_height seq_char (if first_check||second_check then DIS.insert char_ord (texture,intmap_int,width,text_red,text_green,text_blue,text_alpha) intmap_texture else intmap_texture)
    
typesetting_left::Typesetting->Int->Int->Int
typesetting_left Typesetting_left _ _=0
typesetting_left Typesetting_right number block_number=block_number-number
typesetting_left Typesetting_center number block_number=div (block_number-number) 2

typesetting_right::Typesetting->Int->Int->Int
typesetting_right Typesetting_left number _=number
typesetting_right Typesetting_right _ block_number=block_number
typesetting_right Typesetting_center number block_number=div (block_number+number) 2

find_block_font_equal::DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->DS.Seq Int->Int->Int->(Int,Int,FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))
find_block_font_equal widget design_window_size window_size seq_id start_id size=case get_widget_widget seq_id start_id widget of
    Leaf_widget _ (Block_font window_id _ _ _ _ font)->let new_size=div (size*fromIntegral window_size) (fromIntegral design_window_size) in case DIS.lookup new_size font of
        Nothing->error "find_block_font_equal: error 1"
        Just (this_font,font_height,intmap_texture)->(window_id,new_size,this_font,font_height,intmap_texture)
    _->error "find_block_font_equal: error 2"

find_block_font_near::DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->DS.Seq Int->Int->Int->(Int,Int,FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))
find_block_font_near widget design_window_size window_size seq_id start_id size=case get_widget_widget seq_id start_id widget of
    Leaf_widget _ (Block_font window_id _ _ _ _ font)->let new_size=div (size*fromIntegral window_size) (fromIntegral design_window_size) in case DIS.lookupLE new_size font of
        Nothing->case DIS.lookupGE new_size font of
            Nothing->error "find_block_font_near: error 1"
            Just (great_size,(great_font,great_height,great_intmap_texture))->(window_id,great_size,great_font,great_height,great_intmap_texture)
        Just (small_size,(small_font,small_height,small_intmap_texture))->case DIS.lookupGE new_size font of
            Nothing->(window_id,small_size,small_font,small_height,small_intmap_texture)
            Just (great_size,(great_font,great_height,great_intmap_texture))->if 2*new_size<small_size+great_size then (window_id,small_size,small_font,small_height,small_intmap_texture) else (window_id,great_size,great_font,great_height,great_intmap_texture)
    _->error "find_block_font_near: error 2"

find_block_font::Block_find->(DIS.IntMap (DIS.IntMap (Combined_widget a))->FCT.CInt->FCT.CInt->DS.Seq Int->Int->Int->(Int,Int,FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)))
find_block_font Block_near=find_block_font_equal
find_block_font Block_equal=find_block_font_near

get_block_font::Block_find->Int->DIS.IntMap (FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))->(Int,FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))
get_block_font Block_equal size font=case DIS.lookup size font of
    Nothing->error "get_block_font: error 1"
    Just (this_font,font_height,intmap_texture)->(size,this_font,font_height,intmap_texture)
get_block_font Block_near size font=case DIS.lookupLE size font of
    Nothing->case DIS.lookupGE size font of
        Nothing->error "get_block_font: error 2"
        Just (great_size,(great_font,great_height,great_intmap_texture))->(great_size,great_font,great_height,great_intmap_texture)
    Just (small_size,(small_font,small_height,small_intmap_texture))->case DIS.lookupGE size font of
        Nothing->(small_size,small_font,small_height,small_intmap_texture)
        Just (great_size,(great_font,great_height,great_intmap_texture))->if 2*size<small_size+great_size then (small_size,small_font,small_height,small_intmap_texture) else (great_size,great_font,great_height,great_intmap_texture)

from_seq_seq_char::DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->Int->FCT.CInt->DS.Seq (DS.Seq Char)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)
from_seq_seq_char _ _ _ DS.Empty seq_seq_char=seq_seq_char
from_seq_seq_char intmap_texture block_number block_width (seq_char DS.:<| other_seq_text) seq_seq_char=from_seq_seq_char intmap_texture block_number block_width other_seq_text (from_seq_seq_char_a intmap_texture 0 0 block_number block_width seq_char DS.empty seq_seq_char)

from_seq_seq_char_a::DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->Int->Int->Int->FCT.CInt->DS.Seq Char->DS.Seq (Char,Int,FCT.CInt)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)
from_seq_seq_char_a _ number char_number _ _ DS.Empty seq_char seq_seq_char=seq_seq_char DS.|> (seq_char,number,char_number,True)
from_seq_seq_char_a intmap_texture number char_number block_number block_width (char DS.:<| other_text) seq_char seq_seq_char=let char_ord=DC.ord char in case DIS.lookup char_ord intmap_texture of
    Nothing->error "from_seq_seq_char_a: error 1"
    Just (_,intmap_int,_,_,_,_,_)->case DIS.lookup (fromIntegral block_width) intmap_int of
        Nothing->error "from_seq_seq_char_a: error 2"
        Just (block,delta_x)->let new_number=number+block in if new_number<=block_number then from_seq_seq_char_a intmap_texture new_number (char_number+1) block_number block_width other_text (seq_char DS.:|> (char,block,delta_x)) seq_seq_char else if block_number<block then error "from_seq_seq_char_a: error 3" else from_seq_seq_char_a intmap_texture block 1 block_number block_width other_text (DS.singleton (char,block,delta_x)) (seq_seq_char DS.|> (seq_char,number,char_number,False))

to_cursor::Int->Int->Int->Typesetting->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->(Int,Int,Int,Int)
to_cursor block_number row_number row typesetting cursor_click cursor_y font_height block_width delta_height x y seq_seq_char=let cursor_row=row+max 0 (min (row_number-1) (fromIntegral (div (cursor_y+div delta_height 2-y) (font_height+delta_height)))) in case DS.lookup cursor_row seq_seq_char of
    Nothing->error "to_cursor: error 1"
    Just (seq_char,number,_,_)->let new_x=cursor_click-x in let (number_block,number_char)=to_cursor_a new_x block_width seq_char (max 0 (min block_number (fromIntegral (div new_x block_width)))) (typesetting_left typesetting number block_number) 0 in (row+max 0 (min (row_number-1) (fromIntegral (div (cursor_y+div delta_height 2-y) (font_height+delta_height)))),number_block,number_char,number_block)

to_cursor_a::FCT.CInt->FCT.CInt->DS.Seq (Char,Int,FCT.CInt)->Int->Int->Int->(Int,Int)
to_cursor_a _ _ DS.Empty _ number_block number_char=(number_block,number_char)
to_cursor_a x block_width ((_,block,_) DS.:<| seq_char) cursor_block number_block number_char=let new_number_block=number_block+block in if cursor_block<new_number_block then if x+div (fromIntegral block*block_width) 2<fromIntegral new_number_block*block_width then (number_block,number_char) else (new_number_block,number_char+1) else to_cursor_a x block_width seq_char cursor_block new_number_block (number_char+1)

get_cursor_row::Cursor->Maybe Int
get_cursor_row Cursor_none=Nothing
get_cursor_row (Cursor_single cursor_row _ _ _)=Just cursor_row
get_cursor_row (Cursor_double bool cursor_row_start _ _ _ cursor_row_end _ _ _)=if bool then Just cursor_row_start else Just cursor_row_end

cursor_left::Int->Typesetting->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Cursor->Maybe (Int,Maybe Cursor)
cursor_left _ _ _ Cursor_none=Nothing
cursor_left block_number typesetting seq_seq_char (Cursor_single cursor_row cursor_block cursor_char _)=if cursor_char==0
    then if cursor_row==0 then Just (0,Nothing) else let new_cursor_row=cursor_row-1 in case DS.lookup new_cursor_row seq_seq_char of
        Nothing->error "cursor_left: error 1"
        Just (_,number,char_number,_)->let new_cursor_block=typesetting_right typesetting number block_number in Just (new_cursor_row,Just (Cursor_single new_cursor_row new_cursor_block char_number new_cursor_block))
    else case DS.lookup cursor_row seq_seq_char of
        Nothing->error "cursor_left: error 2"
        Just (seq_char,number,_,_)->let new_cursor_char=cursor_char-1 in case DS.lookup new_cursor_char seq_char of
            Nothing->error "cursor_left: error 3"
            Just (_,block,_)->let new_cursor_block=cursor_block-block in Just (cursor_row,Just (Cursor_single cursor_row new_cursor_block new_cursor_char (fromIntegral (typesetting_left typesetting number block_number+new_cursor_block))))
cursor_left _ _ _ (Cursor_double _ cursor_row_start cursor_block_start cursor_char_start cursor_click_start _ _ _ _)=Just (cursor_row_start,Just (Cursor_single cursor_row_start cursor_block_start cursor_char_start cursor_click_start))

cursor_right::Int->Int->Typesetting->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Cursor->Maybe (Int,Maybe Cursor)
cursor_right _ _ _ _ Cursor_none=Nothing
cursor_right max_row block_number typesetting seq_seq_char (Cursor_single cursor_row cursor_block cursor_char _)=case DS.lookup cursor_row seq_seq_char of
    Nothing->error "cursor_right: error 1"
    Just (seq_char,number,char_number,_)->if cursor_char==char_number
        then if cursor_row==max_row then Just (max_row,Nothing) else let new_cursor_row=cursor_row+1 in let new_cursor_block=typesetting_left typesetting number block_number in Just (new_cursor_row,Just (Cursor_single new_cursor_row new_cursor_block 0 (fromIntegral new_cursor_block)))
        else case DS.lookup cursor_char seq_char of 
            Nothing->error "cursor_right: error 2"
            Just (_,block,_)->let new_cursor_block=cursor_block+block in Just (cursor_row,Just (Cursor_single cursor_row new_cursor_block (cursor_char+1) (fromIntegral (typesetting_left typesetting number block_number+new_cursor_block))))
cursor_right _ _ _ _ (Cursor_double _ _ _ _ _ cursor_row_end cursor_block_end cursor_char_end cursor_click_end)=Just (cursor_row_end,Just (Cursor_single cursor_row_end cursor_block_end cursor_char_end cursor_click_end))

cursor_up::Int->Typesetting->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Cursor->Maybe (Int,Maybe Cursor)
cursor_up _ _ _ Cursor_none=Nothing
cursor_up block_number typesetting seq_seq_char (Cursor_single cursor_row cursor_block cursor_char cursor_click)=if cursor_row==0
    then if cursor_char==0 then if cursor_block==cursor_click then Just (0,Nothing) else Just (0,Just (Cursor_single cursor_row cursor_block cursor_char cursor_block)) else case DS.lookup cursor_row seq_seq_char of
        Nothing->error "cursor_up: error 1"
        Just (_,number,_,_)->let new_cursor_block=typesetting_left typesetting number block_number in Just (cursor_row,Just (Cursor_single cursor_row new_cursor_block 0 new_cursor_block))
    else let new_cursor_row=cursor_row-1 in case DS.lookup new_cursor_row seq_seq_char of
        Nothing->error "cursor_up: error 2"
        Just (seq_char,number,_,_)->let (number_block,number_char)=from_cursor_click seq_char cursor_click (typesetting_left typesetting number block_number) 0 in Just (new_cursor_row,Just (Cursor_single new_cursor_row number_block number_char cursor_click))
cursor_up block_number typesetting seq_seq_char (Cursor_double cursor_where cursor_row_start cursor_block_start cursor_char_start cursor_click_start cursor_row_end cursor_block_end cursor_char_end cursor_click_end)=let (cursor_row,cursor_block,cursor_char,cursor_click)=if cursor_where then (cursor_row_start,cursor_block_start,cursor_char_start,cursor_click_start) else (cursor_row_end,cursor_block_end,cursor_char_end,cursor_click_end) in if cursor_row==0
    then if cursor_char==0 then Just (0,Just (Cursor_single cursor_row cursor_block cursor_char cursor_click)) else case DS.lookup cursor_row seq_seq_char of
        Nothing->error "cursor_up: error 3"
        Just (_,number,_,_)->let new_cursor_block=typesetting_left typesetting number block_number in Just (cursor_row,Just (Cursor_single cursor_row new_cursor_block 0 new_cursor_block))
    else let new_cursor_row=cursor_row-1 in case DS.lookup new_cursor_row seq_seq_char of
        Nothing->error "cursor_up: error 4"
        Just (seq_char,number,_,_)->let (number_block,number_char)=from_cursor_click seq_char cursor_click (typesetting_left typesetting number block_number) 0 in Just (new_cursor_row,Just (Cursor_single new_cursor_row number_block number_char cursor_click))

cursor_down::Int->Int->Typesetting->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Cursor->Maybe (Int,Maybe Cursor)
cursor_down _ _ _ _ Cursor_none=Nothing
cursor_down max_row block_number typesetting seq_seq_char (Cursor_single cursor_row cursor_block cursor_char cursor_click)=if cursor_row==max_row
    then case DS.lookup cursor_row seq_seq_char of
        Nothing->error "cursor_down: error 1"
        Just (_,number,char_number,_)->if cursor_char==char_number then if cursor_block==cursor_click then Just (max_row,Nothing) else Just (max_row,Just (Cursor_single cursor_row cursor_block cursor_char cursor_block)) else let new_cursor_block=typesetting_right typesetting number block_number in Just (cursor_row,Just (Cursor_single cursor_row new_cursor_block char_number new_cursor_block))
    else let new_cursor_row=cursor_row+1 in case DS.lookup new_cursor_row seq_seq_char of
        Nothing->error "cursor_down: error 2"
        Just (seq_char,number,_,_)->let (number_block,number_char)=from_cursor_click seq_char cursor_click (typesetting_left typesetting number block_number) 0 in Just (new_cursor_row,Just (Cursor_single new_cursor_row number_block number_char cursor_click))
cursor_down max_row block_number typesetting seq_seq_char (Cursor_double cursor_where cursor_row_start cursor_block_start cursor_char_start cursor_click_start cursor_row_end cursor_block_end cursor_char_end cursor_click_end)=let (cursor_row,cursor_block,cursor_char,cursor_click)=if cursor_where then (cursor_row_start,cursor_block_start,cursor_char_start,cursor_click_start) else (cursor_row_end,cursor_block_end,cursor_char_end,cursor_click_end) in if cursor_row==max_row
    then case DS.lookup cursor_row seq_seq_char of
        Nothing->error "cursor_down: error 3"
        Just (_,number,char_number,_)->if cursor_char==char_number then Just (max_row,Just (Cursor_single cursor_row cursor_block cursor_char cursor_click)) else let new_cursor_block=typesetting_right typesetting number block_number in Just (cursor_row,Just (Cursor_single cursor_row new_cursor_block char_number new_cursor_block))
    else let new_cursor_row=cursor_row+1 in case DS.lookup new_cursor_row seq_seq_char of
        Nothing->error "cursor_down: error 4"
        Just (seq_char,number,_,_)->let (number_block,number_char)=from_cursor_click seq_char cursor_click (typesetting_left typesetting number block_number) 0 in Just (new_cursor_row,Just (Cursor_single new_cursor_row number_block number_char cursor_click))

from_cursor_click::DS.Seq (Char,Int,FCT.CInt)->Int->Int->Int->(Int,Int)
from_cursor_click DS.Empty _ number_block number_char=(number_block,number_char)
from_cursor_click ((_,block,_) DS.:<| seq_char) cursor_click number_block number_char=let new_number_block=number_block+block in if cursor_click<new_number_block then if cursor_click+div block 2<new_number_block then (number_block,number_char) else (new_number_block,number_char+1) else from_cursor_click seq_char cursor_click new_number_block (number_char+1)

cursor_min::Int->Typesetting->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Cursor->Maybe Cursor
cursor_min _ _ _ Cursor_none=Nothing
cursor_min block_number typesetting seq_seq_char (Cursor_single cursor_row _ cursor_char _)=if cursor_row==0&&cursor_char==0 then Nothing else case DS.lookup 0 seq_seq_char of
    Nothing->error "cursor_min: error 1"
    Just (_,number,_,_)->let new_cursor_block=typesetting_left typesetting number block_number in Just (Cursor_single 0 new_cursor_block 0 new_cursor_block)
cursor_min block_number typesetting seq_seq_char (Cursor_double {})=case DS.lookup 0 seq_seq_char of
    Nothing->error "cursor_min: error 2"
    Just (_,number,_,_)->let new_cursor_block=typesetting_left typesetting number block_number in Just (Cursor_single 0 new_cursor_block 0 new_cursor_block)

cursor_max::Int->Int->Typesetting->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Cursor->Maybe Cursor
cursor_max _ _ _ _ Cursor_none=Nothing
cursor_max max_row block_number typesetting seq_seq_char (Cursor_single cursor_row _ cursor_char _)=case DS.lookup max_row seq_seq_char of
    Nothing->error "cursor_max: error 1"
    Just (_,number,char_number,_)->if cursor_row==max_row&&cursor_char==char_number then Nothing else let new_cursor_block=typesetting_right typesetting number block_number in Just (Cursor_single max_row new_cursor_block char_number new_cursor_block)
cursor_max max_row block_number typesetting seq_seq_char (Cursor_double {})=case DS.lookup max_row seq_seq_char of
    Nothing->error "cursor_max: error 2"
    Just (_,number,char_number,_)->let new_cursor_block=typesetting_right typesetting number block_number in Just (Cursor_single max_row new_cursor_block char_number new_cursor_block)

cursor_paragraph_min::Int->Typesetting->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Cursor->Maybe (Int,Maybe Cursor)
cursor_paragraph_min _ _ _ Cursor_none=Nothing
cursor_paragraph_min block_number typesetting seq_seq_char (Cursor_single cursor_row _ cursor_char _)=case DS.lookup cursor_row seq_seq_char of
    Nothing->error "cursor_paragraph_min: error 1"
    Just (_,number,_,_)->let (new_number,new_cursor_row)=find_paragraph_min (DS.take cursor_row seq_seq_char) number cursor_row in if cursor_row==new_cursor_row&&cursor_char==0 then Just (new_cursor_row,Nothing) else let new_cursor_block=typesetting_left typesetting new_number block_number in Just (new_cursor_row,Just (Cursor_single new_cursor_row new_cursor_block 0 new_cursor_block))
cursor_paragraph_min block_number typesetting seq_seq_char (Cursor_double cursor_where cursor_row_start _ _ _ cursor_row_end _ _ _)=if cursor_where
    then case DS.lookup cursor_row_start seq_seq_char of
        Nothing->error "cursor_paragraph_min: error 2"
        Just (_,number,_,_)->let (new_number,new_cursor_row)=find_paragraph_min (DS.take cursor_row_start seq_seq_char) number cursor_row_start in let new_cursor_block=typesetting_left typesetting new_number block_number in Just (new_cursor_row,Just (Cursor_single new_cursor_row new_cursor_block 0 new_cursor_block))
    else case DS.lookup cursor_row_end seq_seq_char of
        Nothing->error "cursor_paragraph_min: error 3"
        Just (_,number,_,_)->let (new_number,new_cursor_row)=find_paragraph_min (DS.take cursor_row_end seq_seq_char) number cursor_row_end in let new_cursor_block=typesetting_left typesetting new_number block_number in Just (new_cursor_row,Just (Cursor_single new_cursor_row new_cursor_block 0 new_cursor_block))

find_paragraph_min::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Int->Int->(Int,Int)
find_paragraph_min DS.Empty number _=(number,0)
find_paragraph_min (seq_seq_char DS.:|> (_,new_number,_,end)) number row=if end then (number,row) else find_paragraph_min seq_seq_char new_number (row-1)

cursor_paragraph_max::Int->Typesetting->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Cursor->Maybe (Int,Maybe Cursor)
cursor_paragraph_max _ _ _ Cursor_none=Nothing
cursor_paragraph_max block_number typesetting seq_seq_char (Cursor_single cursor_row _ cursor_char _)=case DS.lookup cursor_row seq_seq_char of
    Nothing->error "cursor_paragraph_max: error 1"
    Just (_,number,char_number,end)->if end then if cursor_char==char_number then Just (cursor_row,Nothing) else let new_cursor_block=typesetting_right typesetting number block_number in Just (cursor_row,Just (Cursor_single cursor_row new_cursor_block char_number new_cursor_block)) else let (new_number,new_char_number,new_cursor_row)=find_paragraph_max (DS.drop (cursor_row+1) seq_seq_char) cursor_row in let new_cursor_block=typesetting_right typesetting new_number block_number in Just (new_cursor_row,Just (Cursor_single new_cursor_row new_cursor_block new_char_number new_cursor_block))
cursor_paragraph_max block_number typesetting seq_seq_char (Cursor_double cursor_where cursor_row_start _ _ _ cursor_row_end _ _ _)=if cursor_where
    then case DS.lookup cursor_row_start seq_seq_char of
        Nothing->error "cursor_paragraph_max: error 2"
        Just (_,number,char_number,end)->if end then let new_cursor_block=typesetting_right typesetting number block_number in Just (cursor_row_start,Just (Cursor_single cursor_row_start new_cursor_block char_number new_cursor_block)) else let (new_number,new_char_number,new_cursor_row)=find_paragraph_max (DS.drop (cursor_row_start+1) seq_seq_char) cursor_row_start in let new_cursor_block=typesetting_right typesetting new_number block_number in Just (new_cursor_row,Just (Cursor_single new_cursor_row new_cursor_block new_char_number new_cursor_block))
    else case DS.lookup cursor_row_end seq_seq_char of
        Nothing->error "cursor_paragraph_max: error 3"
        Just (_,number,char_number,end)->if end then let new_cursor_block=typesetting_right typesetting number block_number in Just (cursor_row_end,Just (Cursor_single cursor_row_end new_cursor_block char_number new_cursor_block)) else let (new_number,new_char_number,new_cursor_row)=find_paragraph_max (DS.drop (cursor_row_end+1) seq_seq_char) cursor_row_end in let new_cursor_block=typesetting_right typesetting new_number block_number in Just (new_cursor_row,Just (Cursor_single new_cursor_row new_cursor_block new_char_number new_cursor_block))

find_paragraph_max::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Int->(Int,Int,Int)
find_paragraph_max DS.Empty _=error "find_paragraph_max: error 1"
find_paragraph_max ((_,number,char_number,end) DS.:<| seq_seq_char) row=if end then (number,char_number,row+1) else find_paragraph_max seq_seq_char (row+1)

cursor_row_min::Int->Typesetting->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Cursor->Maybe (Int,Maybe Cursor)
cursor_row_min _ _ _ Cursor_none=Nothing
cursor_row_min block_number typesetting seq_seq_char (Cursor_single cursor_row _ cursor_char _)=if cursor_char==0 then Just (cursor_row,Nothing) else case DS.lookup cursor_row seq_seq_char of
    Nothing->error "cursor_row_min: error 1"
    Just (_,number,_,_)->let new_cursor_block=typesetting_left typesetting number block_number in Just (cursor_row,Just (Cursor_single cursor_row new_cursor_block 0 new_cursor_block))
cursor_row_min block_number typesetting seq_seq_char (Cursor_double cursor_where cursor_row_start _ _ _ cursor_row_end _ _ _)=if cursor_where
    then case DS.lookup cursor_row_start seq_seq_char of
    Nothing->error "cursor_row_min: error 2"
    Just (_,number,_,_)->let new_cursor_block=typesetting_left typesetting number block_number in Just (cursor_row_start,Just (Cursor_single cursor_row_start new_cursor_block 0 new_cursor_block))
    else case DS.lookup cursor_row_end seq_seq_char of
    Nothing->error "cursor_row_min: error 3"
    Just (_,number,_,_)->let new_cursor_block=typesetting_left typesetting number block_number in Just (cursor_row_end,Just (Cursor_single cursor_row_end new_cursor_block 0 new_cursor_block))

cursor_row_max::Int->Typesetting->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Cursor->Maybe (Int,Maybe Cursor)
cursor_row_max _ _ _ Cursor_none=Nothing
cursor_row_max block_number typesetting seq_seq_char (Cursor_single cursor_row _ cursor_char _)=case DS.lookup cursor_row seq_seq_char of
    Nothing->error "cursor_row_max: error 1"
    Just (_,number,char_number,_)->if cursor_char==char_number then Just (cursor_row,Nothing) else let new_cursor_block=typesetting_right typesetting number block_number in Just (cursor_row,Just (Cursor_single cursor_row new_cursor_block char_number new_cursor_block))
cursor_row_max block_number typesetting seq_seq_char (Cursor_double cursor_where cursor_row_start _ _ _ cursor_row_end _ _ _)=if cursor_where
    then case DS.lookup cursor_row_start seq_seq_char of
    Nothing->error "cursor_row_max: error 1"
    Just (_,number,char_number,_)->let new_cursor_block=typesetting_right typesetting number block_number in Just (cursor_row_start,Just (Cursor_single cursor_row_start new_cursor_block char_number new_cursor_block))
    else case DS.lookup cursor_row_end seq_seq_char of
    Nothing->error "cursor_row_max: error 1"
    Just (_,number,char_number,_)->let new_cursor_block=typesetting_right typesetting number block_number in Just (cursor_row_end,Just (Cursor_single cursor_row_end new_cursor_block char_number new_cursor_block))

copy::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Cursor->IO ()
copy _ Cursor_none=return ()
copy _ (Cursor_single {})=return ()
copy seq_seq_char (Cursor_double _ cursor_row_start _ cursor_char_start _ cursor_row_end _ cursor_char_end _)=if cursor_row_start==cursor_row_end
    then if cursor_char_start==cursor_char_end then return () else case DS.lookup cursor_row_start seq_seq_char of
        Nothing->error "copy: error 1"
        Just (seq_char_start,_,_,_)->catch_error "copy: error 2" 0 (DB.useAsCString (DTE.encodeUtf8 (to_text DS.Empty (DS.take (cursor_char_end-cursor_char_start) (DS.drop cursor_char_start seq_char_start)) DS.Empty False)) SRV.setClipboardText)
    else case DS.take (cursor_row_end+1-cursor_row_start) (DS.drop cursor_row_start seq_seq_char) of
        DS.Empty->error "copy: error 3"
        ((seq_char_start,_,_,end_start) DS.:<| new_seq_seq_char)->case new_seq_seq_char of
            DS.Empty->error "copy: error 4"
            (other_seq_seq_char DS.:|> (seq_char_end,_,_,_))->catch_error "copy: error 5" 0 (DB.useAsCString (DTE.encodeUtf8 (to_text other_seq_seq_char (DS.drop cursor_char_start seq_char_start) (DS.take cursor_char_end seq_char_end) end_start)) SRV.setClipboardText)

to_text::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->DS.Seq (Char,Int,FCT.CInt)->DS.Seq (Char,Int,FCT.CInt)->Bool->DT.Text
to_text seq_seq_char seq_char_start seq_char_end end_start=DTL.toStrict (DTLB.toLazyText (DF.foldMap (\(char,_,_)->DTLB.singleton char) seq_char_start<>(if end_start then DTLB.singleton '\n' else mempty)<>DF.foldMap (\(seq_char,_,_,end)->DF.foldMap (\(char,_,_)->DTLB.singleton char) seq_char<>(if end then DTLB.singleton '\n' else mempty)) seq_seq_char<>DF.foldMap (\(char,_,_)->DTLB.singleton char) seq_char_end))


from_text::DT.Text->DS.Seq Char->DS.Seq (DS.Seq Char)
from_text DT.Empty seq_char=DS.singleton seq_char
from_text (char DT.:< text) seq_char=if char=='\n' then from_text text DS.empty DS.|> seq_char else from_text text (seq_char DS.|> char)

paste::SRT.Renderer->Int->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FP.Ptr SRF.Font->FCT.CInt->Cursor->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (Maybe (Int,Cursor,DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool),Maybe (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))))
paste _ _ _ _ _ _ _ _ Cursor_none _ _=return Nothing
paste renderer block_number text_red text_green text_blue text_alpha font block_width (Cursor_single cursor_row cursor_block cursor_char _) seq_seq_char intmap_texture=do
    has<-SRV.hasClipboardText
    if has
        then do
            cstring<-SRV.getClipboardText
            text<-DTF.peekCString cstring
            SRB.free (FP.castPtr cstring)
            if DT.null text then return Nothing else FMA.alloca $ \text_color->do
                FS.poke text_color (color text_red text_green text_blue text_alpha)
                (new_cursor_row,new_cursor_block,new_cursor_char,new_seq_seq_char,new_intmap_texture)<-to_seq_seq_char renderer cursor_row cursor_block cursor_char cursor_row cursor_char block_number text_color text_red text_green text_blue text_alpha font block_width (from_text text DS.empty) seq_seq_char intmap_texture
                case new_intmap_texture of
                    Nothing->return (Just (new_cursor_row,Cursor_single new_cursor_row new_cursor_block new_cursor_char new_cursor_block,new_seq_seq_char,Nothing))
                    Just new_new_intmap_texture->return (Just (new_cursor_row,Cursor_single new_cursor_row new_cursor_block new_cursor_char new_cursor_block,new_seq_seq_char,Just new_new_intmap_texture))
        else return Nothing
paste renderer block_number text_red text_green text_blue text_alpha font block_width (Cursor_double _ cursor_row_start cursor_block_start cursor_char_start _ cursor_row_end _ cursor_char_end _) seq_seq_char intmap_texture=do
    has<-SRV.hasClipboardText
    if has
        then do
            cstring<-SRV.getClipboardText
            text<-DTF.peekCString cstring
            SRB.free (FP.castPtr cstring)
            FMA.alloca $ \text_color->do
                FS.poke text_color (color text_red text_green text_blue text_alpha)
                (new_cursor_row,new_cursor_block,new_cursor_char,new_seq_seq_char,new_intmap_texture)<-to_seq_seq_char renderer cursor_row_start cursor_block_start cursor_char_start cursor_row_end cursor_char_end block_number text_color text_red text_green text_blue text_alpha font block_width (from_text text DS.empty) seq_seq_char intmap_texture
                case new_intmap_texture of
                    Nothing->return (Just (new_cursor_row,Cursor_single new_cursor_row new_cursor_block new_cursor_char new_cursor_block,new_seq_seq_char,Nothing))
                    Just new_new_intmap_texture->return (Just (new_cursor_row,Cursor_single new_cursor_row new_cursor_block new_cursor_char new_cursor_block,new_seq_seq_char,Just new_new_intmap_texture))
        else return Nothing

to_seq_seq_char::SRT.Renderer->Int->Int->Int->Int->Int->Int->FP.Ptr Color->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FP.Ptr SRF.Font->FCT.CInt->DS.Seq (DS.Seq Char)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (Int,Int,Int,DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool),Maybe (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)))
to_seq_seq_char renderer row_start block_start char_start row_end char_end block_number text_color text_red text_green text_blue text_alpha font block_width this_seq_seq_char seq_seq_char intmap_texture=case DS.take (row_start+1) seq_seq_char of
        DS.Empty->error "to_seq_seq_char: error 1"
        (seq_seq_char_start DS.:|> (seq_char_start,_,_,_))->case DS.drop row_end seq_seq_char of
            DS.Empty->error "to_seq_seq_char: error 2"
            ((seq_char_end,_,_,end_start) DS.:<| seq_seq_char_end)->case this_seq_seq_char of
                DS.Empty->do
                    return (row_start,block_start,char_start,to_seq_seq_char_b end_start block_start char_start block_number (DS.take char_start seq_char_start) (DS.drop char_end seq_char_end) seq_seq_char_start seq_seq_char_end,Nothing)
                (this_seq_char DS.:<| other_seq_seq_char)->do
                    (row,number_block,number_char,new_seq_char,new_seq_seq_char,new_intmap_texture)<-to_seq_seq_char_a False renderer row_start block_start char_start block_number text_color text_red text_green text_blue text_alpha font block_width this_seq_char other_seq_seq_char (DS.take char_start seq_char_start) seq_seq_char_start intmap_texture
                    return (row,number_block,number_char,to_seq_seq_char_b end_start number_block number_char block_number new_seq_char (DS.drop char_end seq_char_end) new_seq_seq_char seq_seq_char_end,new_intmap_texture)

to_seq_seq_char_a::Bool->SRT.Renderer->Int->Int->Int->Int->FP.Ptr Color->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FP.Ptr SRF.Font->FCT.CInt->DS.Seq Char->DS.Seq (DS.Seq Char)->DS.Seq (Char,Int,FCT.CInt)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (Int,Int,Int,DS.Seq (Char,Int,FCT.CInt),DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool),Maybe (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)))
to_seq_seq_char_a change _ row number_block number_char _ _ _ _ _ _ _ _ DS.Empty DS.Empty seq_char seq_seq_char intmap_texture=return (row,number_block,number_char,seq_char,seq_seq_char,if change then Just intmap_texture else Nothing)
to_seq_seq_char_a change renderer row number_block number_char block_number text_color text_red text_green text_blue text_alpha font block_width DS.Empty (this_seq_char DS.:<| this_seq_seq_char) seq_char seq_seq_char intmap_texture=to_seq_seq_char_a change renderer (row+1) 0 0 block_number text_color text_red text_green text_blue text_alpha font block_width this_seq_char this_seq_seq_char DS.empty (seq_seq_char DS.|> (seq_char,number_block,number_char,True)) intmap_texture
to_seq_seq_char_a change renderer row number_block number_char block_number text_color text_red text_green text_blue text_alpha font block_width (char DS.:<| this_seq_char) this_seq_seq_char seq_char seq_seq_char intmap_texture=let char_ord=DC.ord char in case DIS.lookup char_ord intmap_texture of
    Nothing->do
        (texture,width)<-DB.useAsCString (DTE.encodeUtf8 (DT.singleton char)) (to_texture_with_width renderer text_color font)
        let width_mod=mod width block_width
        let block=if width_mod==0 then fromIntegral (div width block_width) else fromIntegral (div width block_width)+1
        let new_number_block=number_block+block
        let delta_x=if width_mod==0 then 0 else div (block_width-width_mod) 2
        if block_number<new_number_block then if block_number<block then error "to_seq_seq_char_a: error 1" else to_seq_seq_char_a True renderer (row+1) block 1 block_number text_color text_red text_green text_blue text_alpha font block_width this_seq_char this_seq_seq_char (DS.singleton (char,block,delta_x)) (seq_seq_char DS.|> (seq_char,number_block,number_char,False)) (DIS.insert char_ord (texture,DIS.singleton (fromIntegral block_width) (block,delta_x),width,text_red,text_green,text_blue,text_alpha) intmap_texture) else to_seq_seq_char_a True renderer row new_number_block (number_char+1) block_number text_color text_red text_green text_blue text_alpha font block_width this_seq_char this_seq_seq_char (seq_char DS.|> (char,block,delta_x)) seq_seq_char (DIS.insert char_ord (texture,DIS.singleton (fromIntegral block_width) (block,delta_x),width,text_red,text_green,text_blue,text_alpha) intmap_texture)
    Just (texture,intmap_int,width,red,green,blue,alpha)->case DIS.lookup (fromIntegral block_width) intmap_int of
        Nothing->do
            let width_mod=mod width block_width
            let block=if width_mod==0 then fromIntegral (div width block_width) else fromIntegral (div width block_width)+1
            let new_number_block=number_block+block
            let delta_x=if width_mod==0 then 0 else div (block_width-width_mod) 2
            if block_number<new_number_block then if block_number<block then error "to_seq_seq_char_a: error 2" else to_seq_seq_char_a True renderer (row+1) block 1 block_number text_color text_red text_green text_blue text_alpha font block_width this_seq_char this_seq_seq_char (DS.singleton (char,block,delta_x)) (seq_seq_char DS.|> (seq_char,number_block,number_char,False)) (DIS.insert char_ord (texture,DIS.insert (fromIntegral block_width) (block,delta_x) intmap_int,width,red,green,blue,alpha) intmap_texture) else to_seq_seq_char_a True renderer row new_number_block (number_char+1) block_number text_color text_red text_green text_blue text_alpha font block_width this_seq_char this_seq_seq_char (seq_char DS.|> (char,block,delta_x)) seq_seq_char (DIS.insert char_ord (texture,DIS.insert (fromIntegral block_width) (block,delta_x) intmap_int,width,red,green,blue,alpha) intmap_texture)
        Just (block,delta_x)->let new_number_block=number_block+block in if block_number<new_number_block then if block_number<block then error "to_seq_seq_char_a: error 3" else to_seq_seq_char_a change renderer (row+1) block 1 block_number text_color text_red text_green text_blue text_alpha font block_width this_seq_char this_seq_seq_char (DS.singleton (char,block,delta_x)) (seq_seq_char DS.|> (seq_char,number_block,number_char,False)) intmap_texture else to_seq_seq_char_a change renderer row new_number_block (number_char+1) block_number text_color text_red text_green text_blue text_alpha font block_width this_seq_char this_seq_seq_char (seq_char DS.|> (char,block,delta_x)) seq_seq_char intmap_texture

to_seq_seq_char_b::Bool->Int->Int->Int->DS.Seq (Char,Int,FCT.CInt)->DS.Seq (Char,Int,FCT.CInt)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)
to_seq_seq_char_b end number_block number_char block_number this_seq_char DS.Empty this_seq_seq_char seq_seq_char=if end then (this_seq_seq_char DS.|> (this_seq_char,number_block,number_char,True)) DS.>< seq_seq_char else case seq_seq_char of
    DS.Empty->this_seq_seq_char DS.|> (this_seq_char,number_block,number_char,True)
    ((seq_char,_,_,new_end) DS.:<| other_seq_seq_char)->case seq_char of
        DS.Empty->(this_seq_seq_char DS.|> (this_seq_char,number_block,number_char,True)) DS.>< other_seq_seq_char
        (char,block,delta_x) DS.:<| other_seq_char->let new_number_block=number_block+block in if block_number<new_number_block then (this_seq_seq_char DS.|> (this_seq_char,number_block,number_char,False)) DS.>< seq_seq_char else to_seq_seq_char_b new_end new_number_block (number_char+1) block_number (this_seq_char DS.|> (char,block,delta_x)) other_seq_char this_seq_seq_char other_seq_seq_char
to_seq_seq_char_b end number_block number_char block_number this_seq_char ((char,block,delta_x) DS.:<| seq_char) this_seq_seq_char seq_seq_char=let new_number_block=number_block+block in if block_number<new_number_block then to_seq_seq_char_b end block 1 block_number (DS.singleton (char,block,delta_x)) seq_char (this_seq_seq_char DS.|> (this_seq_char,number_block,number_char,False)) seq_seq_char else to_seq_seq_char_b end new_number_block (number_char+1) block_number (this_seq_char DS.|> (char,block,delta_x)) seq_char this_seq_seq_char seq_seq_char

--Editor Int（window_id） Int（每行几个格子） Int（显示几行） Int（当前文本框第一行是文本第几行） Int（字体大小）Int（实际的字体大小）
--Bool（render标记） DS.Seq Int（字体路径） Texture_find（字体资源查找策略） Typesetting（排版模式） Color（文字颜色） DW.Word8 DW.Word8 DW.Word8 DW.Word8（光标颜色） DW.Word8 DW.Word8 DW.Word8 DW.Word8（选择框颜色）
--FCT.CInt（控件最大高度） FCT.CInt（每个格子的宽度） FCT.CInt（额外行间距） FCT.CInt FCT.CInt（控件中心坐标） FCT.CInt FCT.CInt（额外判定区域的长宽）
--FCT.CInt（字体的实际高度） FCT.CInt（实际的格子的宽度）FCT.CInt（实际的额外行间距） FCT.CInt FCT.CInt（实际的控件文字左上角坐标） FCT.CInt FCT.CInt FCT.CInt FCT.CInt（实际的判定区的四个边缘的坐标）