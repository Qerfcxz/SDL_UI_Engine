{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Editor.Core where
import Editor.Typesetting
import Other.Error
import Other.Other
import Other.Text
import Type
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

paste::SRT.Renderer->Int->Typesetting->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FP.Ptr SRF.Font->FCT.CInt->Cursor->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (Maybe (Int,Cursor,DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool),Maybe (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))))
paste _ _ _ _ _ _ _ _ _ Cursor_none _ _=return Nothing
paste renderer block_number typesetting text_red text_green text_blue text_alpha font block_width (Cursor_single cursor_row cursor_block cursor_char _) seq_seq_char intmap_texture=do
    has<-SRV.hasClipboardText
    if has
        then do
            cstring<-SRV.getClipboardText
            text<-DTF.peekCString cstring
            SRB.free (FP.castPtr cstring)
            if DT.null text then return Nothing else FMA.alloca $ \text_color->do
                FS.poke text_color (color text_red text_green text_blue text_alpha)
                (new_cursor_row,new_cursor_block,new_cursor_char,new_seq_seq_char,new_intmap_texture)<-to_seq_seq_char renderer cursor_row cursor_block cursor_char cursor_row cursor_char block_number typesetting text_color text_red text_green text_blue text_alpha font block_width (from_text text DS.empty) seq_seq_char intmap_texture
                case new_intmap_texture of
                    Nothing->return (Just (new_cursor_row,Cursor_single new_cursor_row new_cursor_block new_cursor_char new_cursor_block,new_seq_seq_char,Nothing))
                    Just new_new_intmap_texture->return (Just (new_cursor_row,Cursor_single new_cursor_row new_cursor_block new_cursor_char new_cursor_block,new_seq_seq_char,Just new_new_intmap_texture))
        else return Nothing
paste renderer block_number typesetting text_red text_green text_blue text_alpha font block_width (Cursor_double _ cursor_row_start cursor_block_start cursor_char_start _ cursor_row_end _ cursor_char_end _) seq_seq_char intmap_texture=do
    has<-SRV.hasClipboardText
    if has
        then do
            cstring<-SRV.getClipboardText
            text<-DTF.peekCString cstring
            SRB.free (FP.castPtr cstring)
            FMA.alloca $ \text_color->do
                FS.poke text_color (color text_red text_green text_blue text_alpha)
                (new_cursor_row,new_cursor_block,new_cursor_char,new_seq_seq_char,new_intmap_texture)<-to_seq_seq_char renderer cursor_row_start cursor_block_start cursor_char_start cursor_row_end cursor_char_end block_number typesetting text_color text_red text_green text_blue text_alpha font block_width (from_text text DS.empty) seq_seq_char intmap_texture
                case new_intmap_texture of
                    Nothing->return (Just (new_cursor_row,Cursor_single new_cursor_row new_cursor_block new_cursor_char new_cursor_block,new_seq_seq_char,Nothing))
                    Just new_new_intmap_texture->return (Just (new_cursor_row,Cursor_single new_cursor_row new_cursor_block new_cursor_char new_cursor_block,new_seq_seq_char,Just new_new_intmap_texture))
        else return Nothing

from_text::DT.Text->DS.Seq Char->DS.Seq (DS.Seq Char)
from_text DT.Empty seq_char=DS.singleton seq_char
from_text (char DT.:< text) seq_char=if char=='\n' then seq_char DS.<| from_text text DS.empty else from_text text (seq_char DS.|> char)

to_seq_seq_char::SRT.Renderer->Int->Int->Int->Int->Int->Int->Typesetting->FP.Ptr Color->DW.Word8->DW.Word8->DW.Word8->DW.Word8->FP.Ptr SRF.Font->FCT.CInt->DS.Seq (DS.Seq Char)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)->IO (Int,Int,Int,DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool),Maybe (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8)))
to_seq_seq_char renderer row_start block_start char_start row_end char_end block_number typesetting text_color text_red text_green text_blue text_alpha font block_width this_seq_seq_char seq_seq_char intmap_texture=case DS.take (row_start+1) seq_seq_char of
        DS.Empty->error "to_seq_seq_char: error 1"
        (seq_seq_char_start DS.:|> (seq_char_start,number_start,_,_))->case DS.drop row_end seq_seq_char of
            DS.Empty->error "to_seq_seq_char: error 2"
            ((seq_char_end,_,_,end_start) DS.:<| seq_seq_char_end)->case this_seq_seq_char of
                DS.Empty->do
                    let new_block_start=block_start-typesetting_left typesetting number_start block_number
                    let (number,new_seq_seq_char)=to_seq_seq_char_b end_start Nothing new_block_start char_start block_number (DS.take char_start seq_char_start) (DS.drop char_end seq_char_end) seq_seq_char_start seq_seq_char_end
                    return (row_start,new_block_start+typesetting_left typesetting number block_number,char_start,new_seq_seq_char,Nothing)
                (this_seq_char DS.:<| other_seq_seq_char)->do
                    (row,number_block,number_char,new_seq_char,new_seq_seq_char,new_intmap_texture)<-to_seq_seq_char_a False renderer row_start (block_start-typesetting_left typesetting number_start block_number) char_start block_number text_color text_red text_green text_blue text_alpha font block_width this_seq_char other_seq_seq_char (DS.take char_start seq_char_start) seq_seq_char_start intmap_texture
                    let (number,new_new_seq_seq_char)=to_seq_seq_char_b end_start Nothing number_block number_char block_number new_seq_char (DS.drop char_end seq_char_end) new_seq_seq_char seq_seq_char_end in return (row,number_block+typesetting_left typesetting number block_number,number_char,new_new_seq_seq_char,new_intmap_texture)

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

to_seq_seq_char_b::Bool->Maybe Int->Int->Int->Int->DS.Seq (Char,Int,FCT.CInt)->DS.Seq (Char,Int,FCT.CInt)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->(Int,DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool))
to_seq_seq_char_b end number number_block number_char block_number this_seq_char DS.Empty this_seq_seq_char seq_seq_char=if end then (to_seq_seq_char_c number number_block,(this_seq_seq_char DS.|> (this_seq_char,number_block,number_char,True)) DS.>< seq_seq_char) else case seq_seq_char of
    DS.Empty->(to_seq_seq_char_c number number_block,this_seq_seq_char DS.|> (this_seq_char,number_block,number_char,True))
    ((seq_char,_,_,new_end) DS.:<| other_seq_seq_char)->case seq_char of
        DS.Empty->(to_seq_seq_char_c number number_block,(this_seq_seq_char DS.|> (this_seq_char,number_block,number_char,True)) DS.>< other_seq_seq_char)
        (char,block,delta_x) DS.:<| other_seq_char->let new_number_block=number_block+block in if block_number<new_number_block then (to_seq_seq_char_c number number_block,(this_seq_seq_char DS.|> (this_seq_char,number_block,number_char,False)) DS.>< seq_seq_char) else to_seq_seq_char_b new_end number new_number_block (number_char+1) block_number (this_seq_char DS.|> (char,block,delta_x)) other_seq_char this_seq_seq_char other_seq_seq_char
to_seq_seq_char_b end number number_block number_char block_number this_seq_char ((char,block,delta_x) DS.:<| seq_char) this_seq_seq_char seq_seq_char=let new_number_block=number_block+block in if block_number<new_number_block then to_seq_seq_char_b end (to_seq_seq_char_d number_block number) block 1 block_number (DS.singleton (char,block,delta_x)) seq_char (this_seq_seq_char DS.|> (this_seq_char,number_block,number_char,False)) seq_seq_char else to_seq_seq_char_b end number new_number_block (number_char+1) block_number (this_seq_char DS.|> (char,block,delta_x)) seq_char this_seq_seq_char seq_seq_char

to_seq_seq_char_c::Maybe Int->Int->Int
to_seq_seq_char_c Nothing number=number
to_seq_seq_char_c (Just number) _=number

to_seq_seq_char_d::Int->Maybe Int->Maybe Int
to_seq_seq_char_d number Nothing=Just number
to_seq_seq_char_d _ (Just number)=Just number

--Editor Int（window_id） Int（每行几个格子） Int（显示几行） Int（当前文本框第一行是文本第几行） Int（字体大小）Int（实际的字体大小）
--Bool（render标记） DS.Seq Int（字体路径） Texture_find（字体资源查找策略） Typesetting（排版模式） Color（文字颜色） DW.Word8 DW.Word8 DW.Word8 DW.Word8（光标颜色） DW.Word8 DW.Word8 DW.Word8 DW.Word8（选择框颜色）
--FCT.CInt（控件最大高度） FCT.CInt（每个格子的宽度） FCT.CInt（额外行间距） FCT.CInt FCT.CInt（控件中心坐标） FCT.CInt FCT.CInt（额外判定区域的长宽）
--FCT.CInt（字体的实际高度） FCT.CInt（实际的格子的宽度）FCT.CInt（实际的额外行间距） FCT.CInt FCT.CInt（实际的控件文字左上角坐标） FCT.CInt FCT.CInt FCT.CInt FCT.CInt（实际的判定区的四个边缘的坐标）