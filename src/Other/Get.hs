{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Other.Get where
import Other.Error
import Type
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Foreign.C.Types as FCT
import qualified SDL.Raw.Types as SRT

get_engine_main_id::Engine a->(Engine a->Event->Int)
get_engine_main_id (Engine _ _ _ _ _ main_id _ _ _)=main_id

get_main_id::DS.Seq Int->Engine a->(Engine a->Event->Int)
get_main_id seq_id engine=case get_widget seq_id engine of
    Node_widget _ main_id _ _ _->main_id
    _->error "get_main_id: error 1"

get_next_id::DS.Seq Int->Engine a->(Engine a->Event->Id)
get_next_id seq_id engine=case get_widget seq_id engine of
    Leaf_widget next_id _->next_id
    Node_widget next_id _ _ _ _->next_id

get_renderer::Int->Engine a->SRT.Renderer
get_renderer window_id (Engine _ window _ _ _ _ _ _ _)=case DIS.lookup window_id window of
    Nothing->error "get_renderer: error 1"
    Just (Window _ _ renderer _ _ _ _ _ _)->renderer

get_renderer_window::Int->DIS.IntMap Window->SRT.Renderer
get_renderer_window window_id window=case DIS.lookup window_id window of
    Nothing->error "get_renderer_window: error 1"
    Just (Window _ _ renderer _ _ _ _ _ _)->renderer

get_transform_window::Int->DIS.IntMap Window->(FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)
get_transform_window window_id window=case DIS.lookup window_id window of
    Nothing->error "get_transform_window: error 1"
    Just (Window _ _ _ _ _ x y design_size size)->(x,y,design_size,size)

get_renderer_with_size_window::Int->DIS.IntMap Window->(SRT.Renderer,FCT.CInt,FCT.CInt)
get_renderer_with_size_window window_id window=case DIS.lookup window_id window of
    Nothing->error "get_renderer_with_size_window: error 1"
    Just (Window _ _ renderer _ _ _ _ design_size size)->(renderer,design_size,size)

get_renderer_with_transform_window::Int->DIS.IntMap Window->(SRT.Renderer,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)
get_renderer_with_transform_window window_id window=case DIS.lookup window_id window of
    Nothing->error "get_renderer_with_transform_window: error 1"
    Just (Window _ _ renderer _ _ x y design_size size)->(renderer,x,y,design_size,size)

get_next_id_combined_widget::Combined_widget a->(Engine a->Event->Id)
get_next_id_combined_widget (Leaf_widget next_single_id _)=next_single_id
get_next_id_combined_widget (Node_widget next_single_id _ _ _ _)=next_single_id

get_widget::DS.Seq Int->Engine a->Combined_widget a
get_widget seq_id (Engine widget _ _ _ _ _ start_id _ _)=case seq_id of
    DS.Empty->error "get_widget: error 1"
    (single_id DS.:<| other_seq_single_id)->get_widget_widget_a start_id single_id other_seq_single_id widget

get_widget_widget::DS.Seq Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->Combined_widget a
get_widget_widget seq_id start_id widget=case seq_id of
    DS.Empty->error "get_widget_widget: error 1"
    single_id DS.:<| other_seq_single_id->get_widget_widget_a start_id single_id other_seq_single_id widget

get_widget_widget_a::Int->Int->DS.Seq Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->Combined_widget a
get_widget_widget_a combined_id single_id seq_id widget=let combined_widget=error_lookup_lookup "get_widget_widget_a: error 1" "get_widget_widget_a: error 2" combined_id single_id widget in case seq_id of
    DS.Empty->combined_widget
    (new_single_id DS.:<| other_seq_single_id)->case combined_widget of
        Leaf_widget _ _->error "get_widget_a: error 3"
        Node_widget _ _ _ _ new_combined_id->get_widget_widget_a new_combined_id new_single_id other_seq_single_id widget

get_widget_with_id_widget::DS.Seq Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->(Int,Int,Combined_widget a)
get_widget_with_id_widget seq_id start_id widget=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in case DIS.lookup combined_id widget of
    Nothing->error "get_widget_with_id_widget: error 1"
    Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
        Nothing->error "get_widget_with_id_widget: error 2"
        Just combined_widget->(combined_id,single_id,combined_widget)

get_widget_id_widget::DS.Seq Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->(Int,Int)
get_widget_id_widget seq_id start_id widget=case seq_id of
    DS.Empty->error "get_widget_id_widget: error 1"
    single_id DS.:<| other_seq_single_id->get_widget_id_widget_a other_seq_single_id start_id single_id widget

get_widget_id_widget_a::DS.Seq Int->Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->(Int,Int)
get_widget_id_widget_a seq_id combined_id single_id widget=case seq_id of
    DS.Empty->(combined_id,single_id)
    (new_single_id DS.:<| other_seq_single_id)->case error_lookup_lookup "get_widget_id_widget_a: error 1" "get_widget_id_widget_a: error 2" combined_id single_id widget of
        Leaf_widget _ _->error "get_widget_id_widget_a: error 3"
        Node_widget _ _ _ _ new_combined_id->get_widget_id_widget_a other_seq_single_id new_combined_id new_single_id widget