{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Other.Get where
import Other.Error
import Type
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Foreign.C.Types as FCT
import qualified SDL.Raw.Types as SRT

get_render::DS.Seq Int->Engine a->Bool
get_render seq_id engine=case get_widget seq_id engine of
    Leaf_widget _ (Text _ _ _ render _ _ _ _ _ _ _ _ _ _ _ _ _ _)->render
    Leaf_widget _ (Editor _ _ _ _ _ _ render _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)->render
    _->error "get_render: error 1"

get_engine_widget::Engine a->DIS.IntMap (DIS.IntMap (Combined_widget a))
get_engine_widget (Engine widget _ _ _ _ _ _ _ _)=widget

get_engine_window::Engine a->DIS.IntMap Window
get_engine_window (Engine _ window _ _ _ _ _ _ _)=window

get_renderer::Int->Engine a->SRT.Renderer
get_renderer window_id (Engine _ window _ _ _ _ _ _ _)=case error_lookup "get_renderer: error 1" window_id window of
    Window _ _ renderer _ _ _ _ _ _->renderer

get_renderer_window::Int->DIS.IntMap Window->SRT.Renderer
get_renderer_window window_id window=case error_lookup "get_renderer_window: error 1" window_id window of
    Window _ _ renderer _ _ _ _ _ _->renderer

get_adaptive::Int->Engine a->(FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)
get_adaptive window_id (Engine _ window _ _ _ _ _ _ _)=case error_lookup "get_window_size: error 1" window_id window of
    Window _ _ _ _ _ x y design_size size->(x,y,design_size,size)

get_adaptive_window::Int->DIS.IntMap Window->(FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)
get_adaptive_window window_id window=case error_lookup "get_adaptive_window: error 1" window_id window of
    Window _ _ _ _ _ x y design_size size->(x,y,design_size,size)

get_renderer_with_adaptive_window::Int->DIS.IntMap Window->(SRT.Renderer,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)
get_renderer_with_adaptive_window window_id window=case error_lookup "get_renderer_with_adaptive_window: error 1" window_id window of
    (Window _ _ renderer _ _ x y design_size size)->(renderer,x,y,design_size,size)

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
        Leaf_widget _ _->error "get_widget_widget_a: error 3"
        Node_widget _ _ _ _ new_combined_id->get_widget_widget_a new_combined_id new_single_id other_seq_single_id widget

get_widget_with_id_widget::DS.Seq Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->(Int,Int,Combined_widget a)
get_widget_with_id_widget seq_id start_id widget=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in (combined_id,single_id,error_lookup_lookup "get_widget_with_id_widget: error 1" "get_widget_with_id_widget: error 2" combined_id single_id widget)

get_widget_id_with_transform::DS.Seq Int->Engine a->(Int,Int,DS.Seq (Engine a->Raw_request a->DS.Seq Instruction->Maybe (DS.Seq Instruction)))
get_widget_id_with_transform DS.Empty _=error "get_widget_id_with_transform: error 1"
get_widget_id_with_transform (single_id DS.:<| seq_id) (Engine widget _ _ _ _ _ start_id _ _)=get_widget_id_with_transform_a start_id single_id seq_id widget DS.empty

get_widget_id_with_transform_a::Int->Int->DS.Seq Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->DS.Seq (Engine a->Raw_request a->DS.Seq Instruction->Maybe (DS.Seq Instruction))->(Int,Int,DS.Seq (Engine a->Raw_request a->DS.Seq Instruction->Maybe (DS.Seq Instruction)))
get_widget_id_with_transform_a combined_id single_id seq_id widget transform=case error_lookup_lookup "get_widget_id_with_transform_a: error 1" "get_widget_id_with_transform_a: error 2" combined_id single_id widget of
    Leaf_widget {}->if DS.null seq_id then (combined_id,single_id,transform) else error "get_widget_id_with_transform_a: error 3"
    Node_widget _ _ _ new_transform new_combined_id->case seq_id of
        DS.Empty->(combined_id,single_id,transform)
        (new_single_id DS.:<| new_seq_id)->get_widget_id_with_transform_a new_combined_id new_single_id new_seq_id widget (transform DS.|> new_transform)

get_widget_id::DS.Seq Int->Engine a->(Int,Int)
get_widget_id seq_id (Engine widget _ _ _ _ _ start_id _ _)=get_widget_id_widget seq_id start_id widget

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