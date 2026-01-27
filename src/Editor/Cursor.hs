{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Editor.Cursor where
import Editor.Typesetting
import Type
import qualified Data.Sequence as DS
import qualified Foreign.C.Types as FCT

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
        Just (seq_char,_,_,_)->let new_cursor_char=cursor_char-1 in case DS.lookup new_cursor_char seq_char of
            Nothing->error "cursor_left: error 3"
            Just (_,block,_)->let new_cursor_block=cursor_block-block in Just (cursor_row,Just (Cursor_single cursor_row new_cursor_block new_cursor_char new_cursor_block))
cursor_left _ _ _ (Cursor_double _ cursor_row_start cursor_block_start cursor_char_start cursor_click_start _ _ _ _)=Just (cursor_row_start,Just (Cursor_single cursor_row_start cursor_block_start cursor_char_start cursor_click_start))

cursor_right::Int->Int->Typesetting->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Cursor->Maybe (Int,Maybe Cursor)
cursor_right _ _ _ _ Cursor_none=Nothing
cursor_right max_row block_number typesetting seq_seq_char (Cursor_single cursor_row cursor_block cursor_char _)=case DS.lookup cursor_row seq_seq_char of
    Nothing->error "cursor_right: error 1"
    Just (seq_char,number,char_number,_)->if cursor_char==char_number
        then if cursor_row==max_row then Just (max_row,Nothing) else let new_cursor_row=cursor_row+1 in let new_cursor_block=typesetting_left typesetting number block_number in Just (new_cursor_row,Just (Cursor_single new_cursor_row new_cursor_block 0 new_cursor_block))
        else case DS.lookup cursor_char seq_char of 
            Nothing->error "cursor_right: error 2"
            Just (_,block,_)->let new_cursor_block=cursor_block+block in Just (cursor_row,Just (Cursor_single cursor_row new_cursor_block (cursor_char+1) new_cursor_block))
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
    Just (_,number,_,_)->let (new_cursor_row,new_number)=find_paragraph_min (DS.take cursor_row seq_seq_char) cursor_row number in if cursor_row==new_cursor_row&&cursor_char==0 then Just (new_cursor_row,Nothing) else let new_cursor_block=typesetting_left typesetting new_number block_number in Just (new_cursor_row,Just (Cursor_single new_cursor_row new_cursor_block 0 new_cursor_block))
cursor_paragraph_min block_number typesetting seq_seq_char (Cursor_double cursor_where cursor_row_start _ _ _ cursor_row_end _ _ _)=if cursor_where
    then case DS.lookup cursor_row_start seq_seq_char of
        Nothing->error "cursor_paragraph_min: error 2"
        Just (_,number,_,_)->let (new_cursor_row,new_number)=find_paragraph_min (DS.take cursor_row_start seq_seq_char) cursor_row_start number in let new_cursor_block=typesetting_left typesetting new_number block_number in Just (new_cursor_row,Just (Cursor_single new_cursor_row new_cursor_block 0 new_cursor_block))
    else case DS.lookup cursor_row_end seq_seq_char of
        Nothing->error "cursor_paragraph_min: error 3"
        Just (_,number,_,_)->let (new_cursor_row,new_number)=find_paragraph_min (DS.take cursor_row_end seq_seq_char) cursor_row_end number in let new_cursor_block=typesetting_left typesetting new_number block_number in Just (new_cursor_row,Just (Cursor_single new_cursor_row new_cursor_block 0 new_cursor_block))

find_paragraph_min::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Int->Int->(Int,Int)
find_paragraph_min DS.Empty _ number=(0,number)
find_paragraph_min (seq_seq_char DS.:|> (_,new_number,_,end)) row number=if end then (row,number) else find_paragraph_min seq_seq_char (row-1) new_number

cursor_paragraph_max::Int->Typesetting->DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Cursor->Maybe (Int,Maybe Cursor)
cursor_paragraph_max _ _ _ Cursor_none=Nothing
cursor_paragraph_max block_number typesetting seq_seq_char (Cursor_single cursor_row _ cursor_char _)=case DS.lookup cursor_row seq_seq_char of
    Nothing->error "cursor_paragraph_max: error 1"
    Just (_,number,char_number,end)->if end then if cursor_char==char_number then Just (cursor_row,Nothing) else let new_cursor_block=typesetting_right typesetting number block_number in Just (cursor_row,Just (Cursor_single cursor_row new_cursor_block char_number new_cursor_block)) else let (new_cursor_row,new_number,new_char_number)=find_paragraph_max (DS.drop (cursor_row+1) seq_seq_char) cursor_row in let new_cursor_block=typesetting_right typesetting new_number block_number in Just (new_cursor_row,Just (Cursor_single new_cursor_row new_cursor_block new_char_number new_cursor_block))
cursor_paragraph_max block_number typesetting seq_seq_char (Cursor_double cursor_where cursor_row_start _ _ _ cursor_row_end _ _ _)=if cursor_where
    then case DS.lookup cursor_row_start seq_seq_char of
        Nothing->error "cursor_paragraph_max: error 2"
        Just (_,number,char_number,end)->if end then let new_cursor_block=typesetting_right typesetting number block_number in Just (cursor_row_start,Just (Cursor_single cursor_row_start new_cursor_block char_number new_cursor_block)) else let (new_cursor_row,new_number,new_char_number)=find_paragraph_max (DS.drop (cursor_row_start+1) seq_seq_char) cursor_row_start in let new_cursor_block=typesetting_right typesetting new_number block_number in Just (new_cursor_row,Just (Cursor_single new_cursor_row new_cursor_block new_char_number new_cursor_block))
    else case DS.lookup cursor_row_end seq_seq_char of
        Nothing->error "cursor_paragraph_max: error 3"
        Just (_,number,char_number,end)->if end then let new_cursor_block=typesetting_right typesetting number block_number in Just (cursor_row_end,Just (Cursor_single cursor_row_end new_cursor_block char_number new_cursor_block)) else let (new_cursor_row,new_number,new_char_number)=find_paragraph_max (DS.drop (cursor_row_end+1) seq_seq_char) cursor_row_end in let new_cursor_block=typesetting_right typesetting new_number block_number in Just (new_cursor_row,Just (Cursor_single new_cursor_row new_cursor_block new_char_number new_cursor_block))

find_paragraph_max::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Int->(Int,Int,Int)
find_paragraph_max DS.Empty _=error "find_paragraph_max: error 1"
find_paragraph_max ((_,number,char_number,end) DS.:<| seq_seq_char) row=if end then (row+1,number,char_number) else find_paragraph_max seq_seq_char (row+1)

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
        Nothing->error "cursor_row_max: error 2"
        Just (_,number,char_number,_)->let new_cursor_block=typesetting_right typesetting number block_number in Just (cursor_row_start,Just (Cursor_single cursor_row_start new_cursor_block char_number new_cursor_block))
    else case DS.lookup cursor_row_end seq_seq_char of
        Nothing->error "cursor_row_max: error 3"
        Just (_,number,char_number,_)->let new_cursor_block=typesetting_right typesetting number block_number in Just (cursor_row_end,Just (Cursor_single cursor_row_end new_cursor_block char_number new_cursor_block))