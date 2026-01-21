{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Editor.Select where
import Editor.Cursor
import Editor.Typesetting
import Type
import qualified Data.Sequence as DS
import qualified Foreign.C.Types as FCT

start_select::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Int
start_select DS.Empty _ _=error "start_select: error 1"
start_select ((_,number,_,_) DS.:<| _) typesetting block_number=typesetting_left typesetting number block_number

end_select::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->(Int,Int)
end_select DS.Empty _ _=error "end_select: error 1"
end_select (_ DS.:|> (_,number,char_number,_)) typesetting block_number=(typesetting_right typesetting number block_number,char_number)

min_select_all::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Int->Cursor->Maybe Cursor
min_select_all _ _ _ _ Cursor_none=Nothing
min_select_all seq_seq_char typesetting block_number max_row (Cursor_single {})=let number_block_start=start_select seq_seq_char typesetting block_number in let (number_block_end,number_char_end)=end_select seq_seq_char typesetting block_number in Just (Cursor_double True 0 number_block_start 0 number_block_start max_row number_block_end number_char_end number_block_end)
min_select_all seq_seq_char typesetting block_number max_row (Cursor_double _ cursor_row_start _ cursor_char_start _ cursor_row_end _ cursor_char_end _)=let number_block_start=start_select seq_seq_char typesetting block_number in let (number_block_end,number_char_end)=end_select seq_seq_char typesetting block_number in if cursor_row_start==0&&cursor_char_start==number_block_start&&cursor_row_end==max_row&&cursor_char_end==number_block_end then Nothing else Just (Cursor_double True 0 number_block_start 0 number_block_start max_row number_block_end number_char_end number_block_end)

max_select_all::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Int->Cursor->Maybe Cursor
max_select_all _ _ _ _ Cursor_none=Nothing
max_select_all seq_seq_char typesetting block_number max_row (Cursor_single {})=let number_block_start=start_select seq_seq_char typesetting block_number in let (number_block_end,number_char_end)=end_select seq_seq_char typesetting block_number in Just (Cursor_double False 0 number_block_start 0 number_block_start max_row number_block_end number_char_end number_block_end)
max_select_all seq_seq_char typesetting block_number max_row (Cursor_double _ cursor_row_start _ cursor_char_start _ cursor_row_end _ cursor_char_end _)=let number_block_start=start_select seq_seq_char typesetting block_number in let (number_block_end,number_char_end)=end_select seq_seq_char typesetting block_number in if cursor_row_start==0&&cursor_char_start==number_block_start&&cursor_row_end==max_row&&cursor_char_end==number_block_end then Nothing else Just (Cursor_double False 0 number_block_start 0 number_block_start max_row number_block_end number_char_end number_block_end)

select_start::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Cursor->Maybe Cursor
select_start _ _ _ Cursor_none=Nothing
select_start seq_seq_char typesetting block_number (Cursor_single cursor_row cursor_block cursor_char cursor_click)=let number_block=start_select seq_seq_char typesetting block_number in if cursor_row==0&&cursor_char==0 then Nothing else Just (Cursor_double True 0 number_block 0 number_block cursor_row cursor_block cursor_char cursor_click)
select_start seq_seq_char typesetting block_number (Cursor_double cursor_where cursor_row_start cursor_block_start cursor_char_start cursor_click_start cursor_row_end cursor_block_end cursor_char_end cursor_click_end)=let number_block=start_select seq_seq_char typesetting block_number in if cursor_where then if cursor_row_start==0&&cursor_char_start==0 then Nothing else Just (Cursor_double True 0 number_block 0 number_block cursor_row_end cursor_block_end cursor_char_end cursor_click_end) else Just (Cursor_double True 0 number_block 0 number_block cursor_row_start cursor_block_start cursor_char_start cursor_click_start)

select_end::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Int->Cursor->Maybe Cursor
select_end _ _ _ _ Cursor_none=Nothing
select_end seq_seq_char typesetting block_number max_row (Cursor_single cursor_row cursor_block cursor_char cursor_click)=let (number_block,number_char)=end_select seq_seq_char typesetting block_number in if cursor_row==max_row&&cursor_char==number_char then Nothing else Just (Cursor_double False cursor_row cursor_block cursor_char cursor_click max_row number_block number_char number_block)
select_end seq_seq_char typesetting block_number max_row (Cursor_double cursor_where cursor_row_start cursor_block_start cursor_char_start cursor_click_start cursor_row_end cursor_block_end cursor_char_end cursor_click_end)=let (number_block,number_char)=end_select seq_seq_char typesetting block_number in if cursor_where then Just (Cursor_double False cursor_row_end cursor_block_end cursor_char_end cursor_click_end max_row number_block number_char number_block) else if cursor_row_end==max_row&&cursor_char_end==number_char then Nothing else Just (Cursor_double False cursor_row_start cursor_block_start cursor_char_start cursor_click_start max_row number_block number_char number_block)

start_select_paragraph::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Int->(Int,Int)
start_select_paragraph seq_seq_char typesetting block_number row=case DS.lookup row seq_seq_char of
    Nothing->error "start_select_paragraph: error 1"
    Just (_,number,_,_)->let (new_row,new_number)=find_paragraph_min (DS.take row seq_seq_char) row number in (new_row,typesetting_left typesetting new_number block_number)

end_select_paragraph::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Int->(Int,Int,Int)
end_select_paragraph seq_seq_char typesetting block_number row=case DS.lookup row seq_seq_char of
    Nothing->error "end_select_paragraph: error 1"
    Just (_,number,char_number,end)->if end then (row,number,char_number) else let (new_row,new_number,new_char_number)=find_paragraph_max (DS.drop (row+1) seq_seq_char) row in (new_row,typesetting_right typesetting new_number block_number,new_char_number)

min_select_paragraph_all::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Cursor->Maybe (Int,Maybe Cursor)
min_select_paragraph_all _ _ _ Cursor_none=Nothing
min_select_paragraph_all seq_seq_char typesetting block_number (Cursor_single cursor_row _ _ _)=let (row_start,number_block_start)=start_select_paragraph seq_seq_char typesetting block_number cursor_row in let (row_end,number_block_end,number_char_end)=end_select_paragraph seq_seq_char typesetting block_number cursor_row in Just (row_start,Just (Cursor_double True row_start number_block_start 0 number_block_start row_end number_block_end number_char_end number_block_end))
min_select_paragraph_all seq_seq_char typesetting block_number (Cursor_double cursor_where cursor_row_start _ cursor_char_start _ cursor_row_end _ cursor_char_end _)=let row=if cursor_where then cursor_row_start else cursor_row_end in let (row_start,number_block_start)=start_select_paragraph seq_seq_char typesetting block_number row in let (row_end,number_block_end,number_char_end)=end_select_paragraph seq_seq_char typesetting block_number row in if cursor_where&&cursor_row_start==row_start&&cursor_char_start==0&&cursor_row_end==row_end&&cursor_char_end==number_char_end then Just (row_start,Nothing) else Just (row_start,Just (Cursor_double True row_start number_block_start 0 number_block_start row_end number_block_end number_char_end number_block_end))

max_select_paragraph_all::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Cursor->Maybe (Int,Maybe Cursor)
max_select_paragraph_all _ _ _ Cursor_none=Nothing
max_select_paragraph_all seq_seq_char typesetting block_number (Cursor_single cursor_row _ _ _)=let (row_start,number_block_start)=start_select_paragraph seq_seq_char typesetting block_number cursor_row in let (row_end,number_block_end,number_char_end)=end_select_paragraph seq_seq_char typesetting block_number cursor_row in Just (row_end,Just (Cursor_double False row_start number_block_start 0 number_block_start row_end number_block_end number_char_end number_block_end))
max_select_paragraph_all seq_seq_char typesetting block_number (Cursor_double cursor_where cursor_row_start _ cursor_char_start _ cursor_row_end _ cursor_char_end _)=let row=if cursor_where then cursor_row_start else cursor_row_end in let (row_start,number_block_start)=start_select_paragraph seq_seq_char typesetting block_number row in let (row_end,number_block_end,number_char_end)=end_select_paragraph seq_seq_char typesetting block_number row in if not cursor_where&&cursor_row_start==row_start&&cursor_char_start==0&&cursor_row_end==row_end&&cursor_char_end==number_char_end then Just (row_end,Nothing) else Just (row_end,Just (Cursor_double False row_start number_block_start 0 number_block_start row_end number_block_end number_char_end number_block_end))

select_paragraph_start::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Cursor->Maybe (Int,Maybe Cursor)
select_paragraph_start _ _ _ Cursor_none=Nothing
select_paragraph_start seq_seq_char typesetting block_number (Cursor_single cursor_row cursor_block cursor_char cursor_click)=let (row,number_block)=start_select_paragraph seq_seq_char typesetting block_number cursor_row in if cursor_row==row&&cursor_char==0 then Just (row,Nothing) else Just (row,Just (Cursor_double True row number_block 0 number_block cursor_row cursor_block cursor_char cursor_click))
select_paragraph_start seq_seq_char typesetting block_number (Cursor_double cursor_where cursor_row_start cursor_block_start cursor_char_start cursor_click_start cursor_row_end cursor_block_end cursor_char_end cursor_click_end)=if cursor_where then let (row,number_block)=start_select_paragraph seq_seq_char typesetting block_number cursor_row_start in if cursor_row_start==row&&cursor_char_start==0 then Just (row,Nothing) else Just (row,Just (Cursor_double True row number_block 0 number_block cursor_row_end cursor_block_end cursor_char_end cursor_click_end)) else let (row,number_block)=start_select_paragraph seq_seq_char typesetting block_number cursor_row_end in if cursor_row_end==row&&cursor_char_end==0 then Just (row,Nothing) else if (row,0)<(cursor_row_start,cursor_char_start) then Just (row,Just (Cursor_double True row number_block 0 number_block cursor_row_start cursor_block_start cursor_char_start cursor_click_start)) else Just (row,Just (Cursor_double False cursor_row_start cursor_block_start cursor_char_start cursor_click_start row number_block 0 number_block))

select_paragraph_end::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Cursor->Maybe (Int,Maybe Cursor)
select_paragraph_end _ _ _ Cursor_none=Nothing
select_paragraph_end seq_seq_char typesetting block_number (Cursor_single cursor_row cursor_block cursor_char cursor_click)=let (row,number_block,number_char)=end_select_paragraph seq_seq_char typesetting block_number cursor_row in if cursor_row==row&&cursor_char==number_char then Just (row,Nothing) else Just (row,Just (Cursor_double False cursor_row cursor_block cursor_char cursor_click row number_block number_char number_block))
select_paragraph_end seq_seq_char typesetting block_number (Cursor_double cursor_where cursor_row_start cursor_block_start cursor_char_start cursor_click_start cursor_row_end cursor_block_end cursor_char_end cursor_click_end)=if cursor_where then let (row,number_block,number_char)=end_select_paragraph seq_seq_char typesetting block_number cursor_row_start in if cursor_row_start==row&&cursor_char_start==number_char then Just (row,Nothing) else if (cursor_row_end,cursor_char_end)<(row,number_char) then Just (row,Just (Cursor_double False cursor_row_end cursor_block_end cursor_char_end cursor_click_end row number_block number_char number_block)) else Just (row,Just (Cursor_double True row number_block number_char number_block cursor_row_end cursor_block_end cursor_char_end cursor_click_end)) else let (row,number_block,number_char)=end_select_paragraph seq_seq_char typesetting block_number cursor_row_end in if cursor_row_end==row&&cursor_char_end==number_char then Just (row,Nothing) else Just (row,Just (Cursor_double False cursor_row_start cursor_block_start cursor_char_start cursor_click_start row number_block number_char number_block))

select_row::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Int->(Int,Int,Int)
select_row seq_seq_char typesetting block_number row=case DS.lookup row seq_seq_char of
    Nothing->error "select_row: error 1"
    Just (_,number,char_number,_)->(typesetting_left typesetting number block_number,typesetting_right typesetting number block_number,char_number)

start_select_row::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Int->Int
start_select_row seq_seq_char typesetting block_number row=case DS.lookup row seq_seq_char of
    Nothing->error "start_select_row: error 1"
    Just (_,number,_,_)->typesetting_left typesetting number block_number

end_select_row::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Int->(Int,Int)
end_select_row seq_seq_char typesetting block_number row=case DS.lookup row seq_seq_char of
    Nothing->error "end_select_row: error 1"
    Just (_,number,char_number,_)->(typesetting_right typesetting number block_number,char_number)

min_select_row_all::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Cursor->Maybe (Int,Maybe Cursor)
min_select_row_all _ _ _ Cursor_none=Nothing
min_select_row_all seq_seq_char typesetting block_number (Cursor_single cursor_row _ _ _)=let (number_block_start,number_block_end,number_char_end)=select_row seq_seq_char typesetting block_number cursor_row in Just (cursor_row,Just (Cursor_double True cursor_row number_block_start 0 number_block_start cursor_row number_block_end number_char_end number_block_end))
min_select_row_all seq_seq_char typesetting block_number (Cursor_double cursor_where cursor_row_start _ cursor_char_start _ cursor_row_end _ cursor_char_end _)=if cursor_where then let (number_block_start,number_block_end,number_char_end)=select_row seq_seq_char typesetting block_number cursor_row_start in if cursor_char_start==0&&cursor_row_end==cursor_row_start&&cursor_char_end==number_char_end then Just (cursor_row_start,Nothing) else Just (cursor_row_start,Just (Cursor_double True cursor_row_start number_block_start 0 number_block_start cursor_row_start number_block_end number_char_end number_block_end)) else let (number_block_start,number_block_end,number_char_end)=select_row seq_seq_char typesetting block_number cursor_row_end in Just (cursor_row_end,Just (Cursor_double True cursor_row_end number_block_start 0 number_block_start cursor_row_end number_block_end number_char_end number_block_end))

max_select_row_all::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Cursor->Maybe (Int,Maybe Cursor)
max_select_row_all _ _ _ Cursor_none=Nothing
max_select_row_all seq_seq_char typesetting block_number (Cursor_single cursor_row _ _ _)=let (number_block_start,number_block_end,number_char_end)=select_row seq_seq_char typesetting block_number cursor_row in Just (cursor_row,Just (Cursor_double False cursor_row number_block_start 0 number_block_start cursor_row number_block_end number_char_end number_block_end))
max_select_row_all seq_seq_char typesetting block_number (Cursor_double cursor_where cursor_row_start _ cursor_char_start _ cursor_row_end _ cursor_char_end _)=if cursor_where then let (number_block_start,number_block_end,number_char_end)=select_row seq_seq_char typesetting block_number cursor_row_start in Just (cursor_row_start,Just (Cursor_double False cursor_row_start number_block_start 0 number_block_start cursor_row_start number_block_end number_char_end number_block_end)) else let (number_block_start,number_block_end,number_char_end)=select_row seq_seq_char typesetting block_number cursor_row_end in if cursor_row_start==cursor_row_end&&cursor_char_start==0&&cursor_char_end==number_char_end then Just (cursor_row_end,Nothing) else Just (cursor_row_end,Just (Cursor_double False cursor_row_end number_block_start 0 number_block_start cursor_row_end number_block_end number_char_end number_block_end))

select_row_start::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Cursor->Maybe (Int,Maybe Cursor)
select_row_start _ _ _ Cursor_none=Nothing
select_row_start seq_seq_char typesetting block_number (Cursor_single cursor_row cursor_block cursor_char cursor_click)=let number_block=start_select_row seq_seq_char typesetting block_number cursor_row in if cursor_char==0 then Just (cursor_row,Nothing) else Just (cursor_row,Just (Cursor_double True cursor_row number_block 0 number_block cursor_row cursor_block cursor_char cursor_click))
select_row_start seq_seq_char typesetting block_number (Cursor_double cursor_where cursor_row_start cursor_block_start cursor_char_start cursor_click_start cursor_row_end cursor_block_end cursor_char_end cursor_click_end)=if cursor_where then let number_block=start_select_row seq_seq_char typesetting block_number cursor_row_start in if cursor_char_start==0 then Just (cursor_row_start,Nothing) else Just (cursor_row_start,Just (Cursor_double True cursor_row_start number_block 0 number_block cursor_row_end cursor_block_end cursor_char_end cursor_click_end)) else let number_block=start_select_row seq_seq_char typesetting block_number cursor_row_end in if cursor_char_end==0 then Just (cursor_row_end,Nothing) else if (cursor_row_end,0)<(cursor_row_start,cursor_char_start) then Just (cursor_row_end,Just (Cursor_double True cursor_row_end number_block 0 number_block cursor_row_start cursor_block_start cursor_char_start cursor_click_start)) else Just (cursor_row_end,Just (Cursor_double False cursor_row_start cursor_block_start cursor_char_start cursor_click_start cursor_row_end number_block 0 number_block))

select_row_end::DS.Seq (DS.Seq (Char,Int,FCT.CInt),Int,Int,Bool)->Typesetting->Int->Cursor->Maybe (Int,Maybe Cursor)
select_row_end _ _ _ Cursor_none=Nothing
select_row_end seq_seq_char typesetting block_number (Cursor_single cursor_row cursor_block cursor_char cursor_click)=let (number_block,number_char)=end_select_row seq_seq_char typesetting block_number cursor_row in if cursor_char==number_char then Just (cursor_row,Nothing) else Just (cursor_row,Just (Cursor_double False cursor_row cursor_block cursor_char cursor_click cursor_row number_block number_char number_block))
select_row_end seq_seq_char typesetting block_number (Cursor_double cursor_where cursor_row_start cursor_block_start cursor_char_start cursor_click_start cursor_row_end cursor_block_end cursor_char_end cursor_click_end)=if cursor_where then let (number_block,number_char)=end_select_row seq_seq_char typesetting block_number cursor_row_start in if cursor_char_start==number_char then Just (cursor_row_start,Nothing) else if (cursor_row_end,cursor_char_end)<(cursor_row_start,number_char) then Just (cursor_row_start,Just (Cursor_double False cursor_row_end cursor_block_end cursor_char_end cursor_click_end cursor_row_start number_block number_char number_block)) else Just (cursor_row_start,Just (Cursor_double True cursor_row_start number_block number_char number_block cursor_row_end cursor_block_end cursor_char_end cursor_click_end)) else let (number_block,number_char)=end_select_row seq_seq_char typesetting block_number cursor_row_end in if cursor_char_end==number_char then Just (cursor_row_end,Nothing) else Just (cursor_row_end,Just (Cursor_double False cursor_row_start cursor_block_start cursor_char_start cursor_click_start cursor_row_end number_block number_char number_block))
