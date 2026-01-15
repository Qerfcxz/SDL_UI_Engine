{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Editor where
import Other
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
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

render_editor::SRT.Renderer->Int->Int->Int->Typesetting->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->Cursor->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Bool)->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO ()
render_editor renderer block_number row_number row typesetting text_red text_green text_blue text_alpha _ _ _ _ _ _ _ _ font_height block_width delta_height x y Cursor_none seq_seq_char intmap_texture=FMA.alloca (\rect->render_seq_seq_char renderer rect block_number typesetting text_red text_green text_blue text_alpha block_width x y font_height (font_height+delta_height) (DS.take row_number (DS.drop row seq_seq_char)) intmap_texture)
render_editor renderer block_number row_number row typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha _ _ _ _ font_height block_width delta_height x y (Cursor_single cursor_row cursor_block _ _) seq_seq_char intmap_texture=do
    if cursor_row<row||row+row_number<=cursor_row then FMA.alloca (\rect->render_seq_seq_char renderer rect block_number typesetting text_red text_green text_blue text_alpha block_width x y font_height (font_height+delta_height) (DS.take row_number (DS.drop row seq_seq_char)) intmap_texture) else do
        new_intmap_texture<-FMA.alloca (\rect->render_seq_seq_char renderer rect block_number typesetting text_red text_green text_blue text_alpha block_width x y font_height (font_height+delta_height) (DS.take row_number (DS.drop row seq_seq_char)) intmap_texture)
        catch_error "render_editor: error 1" 0 (SRV.setRenderDrawColor renderer cursor_red cursor_green cursor_blue cursor_alpha)
        let new_x=x+fromIntegral cursor_block*block_width in let new_y=y+fromIntegral (cursor_row-row)*(font_height+delta_height) in catch_error "render_editor: error 2" 0 (SRV.renderDrawLine renderer new_x new_y new_x (new_y+font_height-1))
        return new_intmap_texture
render_editor renderer block_number row_number row typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha font_height block_width delta_height x y (Cursor_double cursor_where cursor_row_start cursor_number_start _ cursor_row_end cursor_number_end _ _ _) seq_seq_char intmap_texture=let (cursor_row,cursor_block)=if cursor_where then (cursor_row_start,cursor_number_start) else (cursor_row_end,cursor_number_end) in if cursor_row<row||row+row_number<=cursor_row
    then FMA.alloca $ \rect->do
            render_select renderer rect cursor_row_start cursor_number_start cursor_row_end cursor_number_end block_number row_number row typesetting select_red select_green select_blue select_alpha block_width delta_height x y font_height seq_seq_char
            render_seq_seq_char renderer rect block_number typesetting text_red text_green text_blue text_alpha block_width x y font_height (font_height+delta_height) (DS.take row_number (DS.drop row seq_seq_char)) intmap_texture
    else FMA.alloca $ \rect->do
        render_select renderer rect cursor_row_start cursor_number_start cursor_row_end cursor_number_end block_number row_number row typesetting select_red select_green select_blue select_alpha block_width delta_height x y font_height seq_seq_char
        new_intmap_texture<-render_seq_seq_char renderer rect block_number typesetting text_red text_green text_blue text_alpha block_width x y font_height (font_height+delta_height) (DS.take row_number (DS.drop row seq_seq_char)) intmap_texture
        catch_error "render_editor: error 3" 0 (SRV.setRenderDrawColor renderer cursor_red cursor_green cursor_blue cursor_alpha)
        let new_x=x+fromIntegral cursor_block*block_width in let new_y=y+fromIntegral (cursor_row-row)*(font_height+delta_height) in catch_error "render_editor: error 4" 0 (SRV.renderDrawLine renderer new_x new_y new_x (new_y+font_height-1))
        return new_intmap_texture

render_select::SRT.Renderer->FP.Ptr SRT.Rect->Int->Int->Int->Int->Int->Int->Int->Typesetting->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Bool)->IO ()
render_select renderer rect cursor_row_start cursor_number_start cursor_row_end cursor_number_end block_number row_number row typesetting select_red select_green select_blue select_alpha block_width delta_height x y font_height seq_seq_char=let min_row=max cursor_row_start row in let max_row=min cursor_row_end (row+row_number-1) in if max_row<min_row then return () else if min_row==cursor_row_start
    then if max_row==cursor_row_end
        then if cursor_row_start==cursor_row_end
            then do
                _<-render_row False renderer rect cursor_number_start cursor_number_end cursor_row_start row select_red select_green select_blue select_alpha block_width delta_height x y font_height
                return ()
            else case DS.lookup min_row seq_seq_char of
                Nothing->error "render_select: error 1"
                Just (_,number_start,_)->case DS.lookup max_row seq_seq_char of
                    Nothing->error "render_select: error 2"
                    Just (_,number_end,_)->do
                        render_start<-render_row False renderer rect cursor_number_start (typesetting_right typesetting number_start block_number) cursor_row_start row select_red select_green select_blue select_alpha block_width delta_height x y font_height
                        render_end<-render_row render_start renderer rect (typesetting_left typesetting number_end block_number) cursor_number_end cursor_row_end row select_red select_green select_blue select_alpha block_width delta_height x y font_height
                        let new_height=font_height+delta_height in render_select_a render_end renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+fromIntegral (cursor_row_start-row+1)*new_height) font_height new_height (DS.take (cursor_row_end-cursor_row_start-1) (DS.drop (cursor_row_start+1) seq_seq_char))
        else case DS.lookup min_row seq_seq_char of
            Nothing->error "render_select: error 3"
            Just (_,number_start,_)->do
                render_start<-render_row False renderer rect cursor_number_start (typesetting_right typesetting number_start block_number) cursor_row_start row select_red select_green select_blue select_alpha block_width delta_height x y font_height
                let new_height=font_height+delta_height in render_select_a render_start renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x (y+fromIntegral (cursor_row_start-row+1)*new_height) font_height new_height (DS.take (row+row_number-cursor_row_start-1) (DS.drop (cursor_row_start+1) seq_seq_char))
    else if max_row==cursor_row_end
        then case DS.lookup max_row seq_seq_char of
            Nothing->error "render_select: error 4"
            Just (_,number_end,_)->do
                render_end<-render_row False renderer rect (typesetting_left typesetting number_end block_number) cursor_number_end cursor_row_end row select_red select_green select_blue select_alpha block_width delta_height x y font_height
                render_select_a render_end renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x y font_height (font_height+delta_height) (DS.take (cursor_row_end-row) (DS.drop row seq_seq_char))
        else render_select_a False renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x y font_height (font_height+delta_height) (DS.take row_number (DS.drop row seq_seq_char))

render_select_a::Bool->SRT.Renderer->FP.Ptr SRT.Rect->Int->Typesetting->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Bool)->IO ()
render_select_a _ _ _ _ _ _ _ _ _ _ _ _ _ _ DS.Empty=return ()
render_select_a render renderer rect block_number typesetting select_red select_green select_blue select_alpha block_width x y font_height height ((_,number,_) DS.:<| seq_seq_char)
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

render_seq_seq_char::SRT.Renderer->FP.Ptr SRT.Rect->Int->Typesetting->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Bool)->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO ()
render_seq_seq_char _ _ _ _ _ _ _ _ _ _ _ _ _ DS.Empty _=return ()
render_seq_seq_char renderer rect block_number typesetting text_red text_green text_blue text_alpha block_width x y font_height height ((seq_char,number,_) DS.:<| seq_seq_char) intmap_texture=do
    render_seq_seq_char_a renderer rect text_red text_green text_blue text_alpha block_width (x+fromIntegral (typesetting_left typesetting number block_number)*block_width) y font_height seq_char intmap_texture
    render_seq_seq_char renderer rect block_number typesetting text_red text_green text_blue text_alpha block_width x (y+height) font_height height seq_seq_char intmap_texture

render_seq_seq_char_a::SRT.Renderer->FP.Ptr SRT.Rect->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DS.Seq (Char,Int,FCT.CInt)->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO ()
render_seq_seq_char_a _ _ _ _ _ _ _ _ _ _ DS.Empty _=return () 
render_seq_seq_char_a renderer rect text_red text_green text_blue text_alpha block_width x y font_height ((char,block,delta_x) DS.:<| seq_char) intmap_texture=let char_ord=DC.ord char in case DIS.lookup char_ord intmap_texture of
    Nothing->error "render_seq_seq_char_a: error 1"
    Just (texture,_,width,red,green,blue,alpha)->do
        CM.when (text_red/=red||text_green/=green||text_blue/=blue) (catch_error "render_seq_seq_char_a: error 2" 0 (SRV.setTextureColorMod texture text_red text_green text_blue))
        CM.when (text_alpha/=alpha) (catch_error "render_seq_seq_char_a: error 3" 0 (SRV.setTextureAlphaMod texture text_alpha))
        FS.poke rect (SRT.Rect (x+delta_x) y width font_height)
        catch_error "render_seq_seq_char_a: error 4" 0 (SRV.renderCopy renderer texture FP.nullPtr rect)
        render_seq_seq_char_a renderer rect text_red text_green text_blue text_alpha block_width (x+fromIntegral block*block_width) y font_height seq_char intmap_texture
    
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

from_seq_seq_char::DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->Int->FCT.CInt->DS.Seq (DS.Seq Char)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Bool)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Bool)
from_seq_seq_char _ _ _ DS.Empty seq_seq_char=seq_seq_char
from_seq_seq_char intmap_texture block_number block_width (seq_char DS.:<| other_seq_text) seq_seq_char=from_seq_seq_char intmap_texture block_number block_width other_seq_text (from_seq_seq_char_a intmap_texture 0 block_number block_width seq_char DS.empty seq_seq_char)

from_seq_seq_char_a::DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->Int->Int->FCT.CInt->DS.Seq Char->DS.Seq (Char,Int,FCT.CInt)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Bool)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Bool)
from_seq_seq_char_a _ number _ _ DS.Empty seq_char seq_seq_char=seq_seq_char DS.|> (seq_char,number,True)
from_seq_seq_char_a intmap_texture number block_number block_width (char DS.:<| other_text) seq_char seq_seq_char=let char_ord=DC.ord char in case DIS.lookup char_ord intmap_texture of
    Nothing->error "from_seq_seq_char_a: error 1"
    Just (_,intmap_int,_,_,_,_,_)->case DIS.lookup (fromIntegral block_width) intmap_int of
        Nothing->error "from_seq_seq_char_a: error 2"
        Just (block,delta_x)->let new_number=number+block in if new_number<=block_number then from_seq_seq_char_a intmap_texture new_number block_number block_width other_text (seq_char DS.:|> (char,block,delta_x)) seq_seq_char else if block_number<block then error "from_seq_seq_char_a: error 3" else from_seq_seq_char_a intmap_texture block block_number block_width other_text (DS.singleton (char,block,delta_x)) (seq_seq_char DS.|> (seq_char,number,False))

to_cursor::Int->Int->Int->Typesetting->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Bool)->(Int,Int,Int,FCT.CInt)
to_cursor block_number row_number row typesetting cursor_x cursor_y font_height block_width delta_height x y intmap_texture seq_seq_char=let cursor_row=row+max 0 (min (row_number-1) (fromIntegral (div (cursor_y+div delta_height 2-y) (font_height+delta_height)))) in case DS.lookup cursor_row seq_seq_char of
    Nothing->error "to_cursor: error 1"
    Just (seq_char,number,_)->let new_x=cursor_x-x in let cursor_block=max 0 (min block_number (fromIntegral (div new_x block_width))) in let (number_block,number_char)=to_cursor_a new_x block_width intmap_texture seq_char cursor_block (typesetting_left typesetting number block_number) 0 in (row+max 0 (min (row_number-1) (fromIntegral (div (cursor_y+div delta_height 2-y) (font_height+delta_height)))),number_block,number_char,new_x)

to_cursor_a::FCT.CInt->FCT.CInt->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->DS.Seq (Char,Int,FCT.CInt)->Int->Int->Int->(Int,Int)
to_cursor_a _ _ _ DS.Empty _ number_block number_char=(number_block,number_char)
to_cursor_a x block_width intmap_texture ((char,_,_) DS.:<| seq_char) cursor_block number_block number_char=case DIS.lookup (DC.ord char) intmap_texture of
    Nothing->error "to_cursor_a: error 1"
    Just (_,intmap_int,_,_,_,_,_)->case DIS.lookup (fromIntegral block_width) intmap_int of
        Nothing->error "to_cursor_a: error 2"
        Just (block,_)->let new_number_block=number_block+block in if cursor_block<new_number_block then if x+div (fromIntegral block*block_width) 2<fromIntegral new_number_block*block_width then (number_block,number_char) else (new_number_block,number_char+1) else to_cursor_a x block_width intmap_texture seq_char cursor_block new_number_block (number_char+1)

--Editor Int（window_id） Int（每行几个格子） Int（显示几行） Int（当前文本框第一行是文本第几行） Int（字体大小）Int（实际的字体大小）
--Bool（render标记） DS.Seq Int（字体路径） Texture_find（字体资源查找策略） Typesetting（排版模式） Color（文字颜色） DW.Word8 DW.Word8 DW.Word8 DW.Word8（光标颜色） DW.Word8 DW.Word8 DW.Word8 DW.Word8（选择框颜色）
--FCT.CInt（控件最大高度） FCT.CInt（每个格子的宽度） FCT.CInt（额外行间距） FCT.CInt FCT.CInt（控件中心坐标） FCT.CInt FCT.CInt（额外判定区域的长宽）
--FCT.CInt（字体的实际高度） FCT.CInt（实际的格子的宽度）FCT.CInt（实际的额外行间距） FCT.CInt FCT.CInt（实际的控件文字左上角坐标） FCT.CInt FCT.CInt FCT.CInt FCT.CInt（实际的判定区的四个边缘的坐标）