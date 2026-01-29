{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Widget.Text where
import Other.Error
import Other.Get
import Widget.Create
import Type
import qualified Data.Foldable as DF
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet

create_text_trigger::Bool->DSet.Set (Press,DSet.Set Key)->DSet.Set (Press,DSet.Set Key)->DSet.Set (Press,DSet.Set Key)->DSet.Set (Press,DSet.Set Key)->DSet.Set (Click,Mouse)->(Engine a->Event->Id)->DSeq.Seq Int->DSeq.Seq (DSeq.Seq Int)->Engine a->IO (Engine a)
create_text_trigger wheel up_press down_press min_press max_press down_click next_id seq_id seq_seq_id engine=let (combined_id,single_id)=get_widget_id seq_id engine in create_widget combined_id single_id (Leaf_widget_request next_id (Trigger_request (\event (Engine widget window window_map request key main_id start_id count_id time)->let new_widget=DF.foldl' (\this_widget this_seq_id->create_text_trigger_a wheel up_press down_press min_press max_press down_click this_seq_id start_id event this_widget) widget seq_seq_id in Engine new_widget window window_map request key main_id start_id count_id time))) engine

create_text_trigger_a::Bool->DSet.Set (Press,DSet.Set Key)->DSet.Set (Press,DSet.Set Key)->DSet.Set (Press,DSet.Set Key)->DSet.Set (Press,DSet.Set Key)->DSet.Set (Click,Mouse)->DSeq.Seq Int->Int->Event->DIS.IntMap (DIS.IntMap (Combined_widget a))->DIS.IntMap (DIS.IntMap (Combined_widget a))
create_text_trigger_a wheel up_press down_press min_press max_press down_click seq_id start_id event widget=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in let intmap_combined_widget=error_lookup "create_text_trigger_a: error 1" combined_id widget in case error_lookup "create_text_trigger_a: error 2" single_id intmap_combined_widget of
    combined_widget->case create_text_trigger_b wheel up_press down_press min_press max_press down_click event combined_widget of
        Nothing->widget
        Just new_combined_widget->DIS.insert combined_id (DIS.insert single_id new_combined_widget intmap_combined_widget) widget

create_text_trigger_b::Bool->DSet.Set (Press,DSet.Set Key)->DSet.Set (Press,DSet.Set Key)->DSet.Set (Press,DSet.Set Key)->DSet.Set (Press,DSet.Set Key)->DSet.Set (Click,Mouse)->Event->Combined_widget a->Maybe (Combined_widget a)
create_text_trigger_b wheel up_press down_press min_press max_press down_click event (Leaf_widget next_id (Text window_id row max_row render select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row))=if select
    then case event of
        At this_window_id action->if window_id==this_window_id
            then case action of
                Wheel delta_y->if wheel then let new_row=max 0 (min max_row (row-delta_y)) in if row==new_row then Nothing else Just (Leaf_widget next_id (Text window_id new_row max_row True select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row)) else Nothing
                Press press _ key->if DSet.member (press,key) up_press then if 0<row then Just (Leaf_widget next_id (Text window_id (row-1) max_row True select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row)) else Nothing else if DSet.member (press,key) down_press then if row<max_row then Just (Leaf_widget next_id (Text window_id (row+1) max_row True select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row)) else Nothing else if DSet.member (press,key) min_press then if row==0 then Nothing else Just (Leaf_widget next_id (Text window_id 0 max_row True select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row)) else if DSet.member (press,key) max_press then if row==max_row then Nothing else Just (Leaf_widget next_id (Text window_id max_row max_row True select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row)) else Nothing
                Click click mouse x y->if DSet.member (click,mouse) down_click&&(x<left||right<x||y<up||down<y) then Just (Leaf_widget next_id (Text window_id row max_row render False find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row)) else Nothing
                _->Nothing
            else Nothing
        _->Nothing
    else case event of
        At this_window_id action->if window_id==this_window_id
            then case action of
                Click click mouse x y->if DSet.member (click,mouse) down_click&&left<=x&&x<=right&&up<=y&&y<=down then Just (Leaf_widget next_id (Text window_id row max_row render True find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row)) else Nothing
                _->Nothing
            else Nothing
        _->Nothing
create_text_trigger_b _ _ _ _ _ _ _ _=error "create_text_trigger_b: error 1"