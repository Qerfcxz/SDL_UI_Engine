{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Widget.Widget where
import Other.Error
import Other.Get
import Text.Font
import Text.From
import Text.Max
import Widget.Create
import Widget.Remove
import Type
import qualified Control.Monad as CM
import qualified Data.Foldable as DF
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS

create_widget_trigger::(Event->Engine a->Id)->DS.Seq Int->DIS.IntMap (DS.Seq (DS.Seq Int))->Engine a->IO (Engine a)
create_widget_trigger next_id seq_id id_map=create_widget seq_id (Leaf_widget_request next_id (Io_trigger_request (create_widget_trigger_a id_map)))

create_widget_trigger_a::DIS.IntMap (DS.Seq (DS.Seq Int))->Event->Engine a->IO (Engine a)
create_widget_trigger_a id_map event engine=case event of
    Resize window_id _ _->case DIS.lookup window_id id_map of
        Nothing->return engine
        Just seq_seq_id->CM.foldM (flip update_widget) engine seq_seq_id
    _->return engine

update_widget::DS.Seq Int->Engine a->IO (Engine a)
update_widget seq_id (Engine widget window window_map request count_id start_id main_id)=do
    new_widget<-update_combined_widget start_id seq_id (update_widget_a start_id window widget) widget
    return (Engine new_widget window window_map request count_id start_id main_id)

update_widget_a::Int->DIS.IntMap Window->DIS.IntMap (DIS.IntMap (Combined_widget a))->Combined_widget a->IO (Combined_widget a)
update_widget_a _ window _ (Leaf_widget next_id (Rectangle window_id red green blue alpha left right up down _ _ _ _))=let (x,y,design_size,size)=get_transform_window window_id window in return (Leaf_widget next_id (Rectangle window_id red green blue alpha left right up down (x+div (left*size) design_size) (y+div (up*size) design_size) (div ((right-left)*size) design_size) (div ((down-up)*size) design_size)))
update_widget_a _ window _ (Leaf_widget next_id (Picture window_id texture x y width_multiply width_divide height_multiply height_divide width height _ _ _ _))=let (window_x,window_y,design_size,size)=get_transform_window window_id window in let new_width=div (width*width_multiply) width_divide in let new_height=div (height*height_multiply) height_divide in return (Leaf_widget next_id (Picture window_id texture x y width_multiply width_divide height_multiply height_divide width height (window_x+div ((x-div new_width 2)*size) design_size) (window_y+div ((y-div new_height 2)*size) design_size) (div (new_width*size) design_size) (div (new_height*size) design_size)))
update_widget_a start_id window widget (Leaf_widget next_id (Text window_id row _ _ select find delta_height left right up down _ _ _ _ _ seq_paragraph seq_row))=do
    DF.mapM_ clean_row seq_row
    let (renderer,x,y,design_size,size)=get_renderer_with_transform_window window_id window
    let new_delta_height=div (delta_height*size) design_size
    new_seq_row<-from_paragraph widget renderer (find_font find) window_id start_id design_size size 0 (div ((right-left)*size) design_size) new_delta_height seq_paragraph DS.empty
    let new_up=(y+div (up*size) design_size) in let new_down=(y+div (down*size) design_size) in let max_row=find_max new_seq_row new_up new_down in return (Leaf_widget next_id (Text window_id (max 0 (min row max_row)) max_row True select find delta_height left right up down new_delta_height (x+div (left*size) design_size) (x+div (right*size) design_size) new_up new_down seq_paragraph new_seq_row))
update_widget_a _ _ _ _=error "update_widget_a: error 1"

update_combined_widget::Int->DS.Seq Int->(Combined_widget a->IO (Combined_widget a))->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))
update_combined_widget start_id seq_single_id update widget=case seq_single_id of
    DS.Empty->error "update_combined_widget: error 1"
    single_id DS.:<| other_seq_single_id->update_combined_widget_a start_id single_id other_seq_single_id update widget

update_combined_widget_a::Int->Int->DS.Seq Int->(Combined_widget a->IO (Combined_widget a))->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))
update_combined_widget_a combined_id single_id seq_single_id update widget=case seq_single_id of
    DS.Empty->error_update_update_io "update_combined_widget_a: error 1" "update_combined_widget_a: error 2" combined_id single_id update widget
    (new_single_id DS.:<| other_seq_single_id)->case DIS.lookup single_id widget of
        Nothing->error "update_combined_widget_a: error 3"
        Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
            Nothing->error "update_combined_widget_a: error 4"
            Just new_combined_widget->case new_combined_widget of
                Leaf_widget _ _->error "update_combined_widget_a: error 5"
                Node_widget _ _ new_combined_id->update_combined_widget_a new_combined_id new_single_id other_seq_single_id update widget