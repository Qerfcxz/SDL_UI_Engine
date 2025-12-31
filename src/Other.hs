{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Other where
import Type
import qualified Control.Monad as CM
import qualified Data.Foldable as DF
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Video as SRV

catch_error::Eq a=>[Char]->a->IO a->IO ()
catch_error error_message success result=do
    flag<-result
    CM.unless (flag==success) (error error_message)

simple_error_remove::[Char]->Int->DIS.IntMap a->DIS.IntMap a
simple_error_remove error_message key intmap=case DIS.updateLookupWithKey (\_ _->Nothing) key intmap of
    (Nothing,_)->error error_message
    (Just _,new_intmap)->new_intmap

error_remove::[Char]->Int->DIS.IntMap a->(a,DIS.IntMap a)
error_remove error_message key intmap=case DIS.updateLookupWithKey (\_ _->Nothing) key intmap of
    (Nothing,_)->error error_message
    (Just value,new_intmap)->(value,new_intmap)

error_remove_remove::[Char]->[Char]->Int->Int->DIS.IntMap (DIS.IntMap a)->(a,DIS.IntMap a,DIS.IntMap (DIS.IntMap a))
error_remove_remove first_error_message second_error_message first_key second_key intmap_intmap=case DIS.updateLookupWithKey (\_ _->Nothing) first_key intmap_intmap of
    (Nothing,_)->error first_error_message
    (Just intmap,new_intmap_intmap)->case DIS.updateLookupWithKey (\_ _->Nothing) second_key intmap of
        (Nothing,_)->error second_error_message
        (Just value,new_intmap)->(value,new_intmap,new_intmap_intmap)

error_insert::[Char]->Int->a->DIS.IntMap a->DIS.IntMap a
error_insert error_message key value intmap=let (maybe_value,new_intmap)=DIS.insertLookupWithKey (\_ _ old_value->old_value) key value intmap in case maybe_value of
    Just _->error error_message
    Nothing->new_intmap

error_insert_insert::[Char]->[Char]->Int->Int->a->DIS.IntMap (DIS.IntMap a)->DIS.IntMap (DIS.IntMap a)
error_insert_insert first_error_message second_error_message first_key second_key value=DIS.alter (error_insert_insert_a first_error_message second_error_message second_key value) first_key

error_insert_insert_a::[Char]->[Char]->Int->a->Maybe (DIS.IntMap a)->Maybe (DIS.IntMap a)
error_insert_insert_a first_error_message _ _ _ Nothing=error first_error_message
error_insert_insert_a _ second_error_message key value (Just intmap)=if DIS.member key intmap then error second_error_message else Just (DIS.insert key value intmap)

error_update::[Char]->Int->(a->a)->DIS.IntMap a->DIS.IntMap a
error_update error_message key update=DIS.alter (error_update_a error_message update) key

error_update_a::[Char]->(a->a)->Maybe a->Maybe a
error_update_a error_message _ Nothing=error error_message
error_update_a _ update (Just value)=Just (update value)

error_update_update::[Char]->[Char]->Int->Int->(a->IO a)->DIS.IntMap (DIS.IntMap a)->IO (DIS.IntMap (DIS.IntMap a))
error_update_update first_error_message second_error_message first_key second_key update=DIS.alterF (error_update_update_a first_error_message second_error_message second_key update) first_key

error_update_update_a::[Char]->[Char]->DIS.Key->(a->IO a)->Maybe (DIS.IntMap a)->IO (Maybe (DIS.IntMap a))
error_update_update_a first_error_message _ _ _ Nothing=error first_error_message
error_update_update_a _ second_error_message key update (Just intmap)=Just <$> DIS.alterF (error_update_update_b second_error_message update) key intmap

error_update_update_b::[Char]->(a->IO a)->Maybe a->IO (Maybe a)
error_update_update_b error_message _ Nothing=error error_message
error_update_update_b _ update (Just value)=do
    new_value<-update value
    return (Just new_value)

get_renderer::Int->Engine a->SRT.Renderer
get_renderer window_id (Engine _ window _ _ _ _ _)=case DIS.lookup window_id window of
    Nothing->error "get_renderer: No such window"
    Just (Window _ _ renderer _ _ _ _ _ _)->renderer

get_renderer_window::Int->DIS.IntMap Window->SRT.Renderer
get_renderer_window window_id window=case DIS.lookup window_id window of
    Nothing->error "get_renderer: No such window"
    Just (Window _ _ renderer _ _ _ _ _ _)->renderer

get_transform::Int->DIS.IntMap Window->(FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)
get_transform window_id window=case DIS.lookup window_id window of
    Nothing->error "get_transform: No such window"
    Just (Window _ _ _ _ _ x y design_size size)->(x,y,design_size,size)

get_renderer_with_transform::Int->DIS.IntMap Window->(SRT.Renderer,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)
get_renderer_with_transform window_id window=case DIS.lookup window_id window of
    Nothing->error "get_renderer_with_transform: No such window"
    Just (Window _ _ renderer _ _ x y design_size size)->(renderer,x,y,design_size,size)

get_next_id::Combined_widget a->(Engine a->Id)
get_next_id (Leaf_widget next_single_id _)=next_single_id
get_next_id (Node_widget next_single_id _ _)=next_single_id

get_font::DS.Seq Int->Engine a->DIS.IntMap (FP.Ptr SRF.Font)
get_font seq_single_id engine=case get_combined_widget seq_single_id engine of
    Leaf_widget _ (Font font)->font
    _->error "get_font: not a font widget"

get_combined_widget_widget::Int->DS.Seq Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->Combined_widget a
get_combined_widget_widget start_id seq_single_id widget=case DS.viewl seq_single_id of
    DS.EmptyL->error "get_combined_widget_widget: empty seq_single_id"
    single_id DS.:< other_seq_single_id->get_combined_widget_a start_id single_id other_seq_single_id widget

get_combined_widget::DS.Seq Int->Engine a->Combined_widget a
get_combined_widget seq_single_id (Engine widget _ _ _ _ start_id _)=case DS.viewl seq_single_id of
    DS.EmptyL->error "get_combined_widget: empty seq_single_id"
    (single_id DS.:< other_seq_single_id)->get_combined_widget_a start_id single_id other_seq_single_id widget

get_combined_widget_a::Int->Int->DS.Seq Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->Combined_widget a
get_combined_widget_a combined_id single_id seq_single_id widget=case DIS.lookup combined_id widget of
    Nothing->error "get_combined_widget_a: no such combined_id"
    Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
        Nothing->error "get_combined_widget_a: no such single_id"
        Just combined_widget->case DS.viewl seq_single_id of
            DS.EmptyL->combined_widget
            (new_single_id DS.:< other_seq_single_id)->case combined_widget of
                Leaf_widget _ _->error "get_combined_widget_a: wrong seq_single_id"
                Node_widget _ _ new_combined_id->get_combined_widget_a new_combined_id new_single_id other_seq_single_id widget

update_combined_widget::Int->DS.Seq Int->(Combined_widget a->IO (Combined_widget a))->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))
update_combined_widget start_id seq_single_id update widget=case DS.viewl seq_single_id of
    DS.EmptyL->error "get_combined_widget: empty seq_single_id"
    single_id DS.:< other_seq_single_id->update_combined_widget_a start_id single_id other_seq_single_id update widget

update_combined_widget_a::Int->Int->DS.Seq Int->(Combined_widget a->IO (Combined_widget a))->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))
update_combined_widget_a combined_id single_id seq_single_id update widget=case DS.viewl seq_single_id of
    DS.EmptyL->error_update_update "update_combined_widget_a: no such combined_id" "update_combined_widget_a: no such single_id" combined_id single_id update widget
    (new_single_id DS.:< other_seq_single_id)->case DIS.lookup single_id widget of
        Nothing->error "get_combined_widget_with_id_a: no such combined_id"
        Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
            Nothing->error "get_combined_widget_with_id_a: no such single_id"
            Just new_combined_widget->case new_combined_widget of
                Leaf_widget _ _->error "get_combined_widget_with_id_a: wrong seq_single_id"
                Node_widget _ _ new_combined_id->update_combined_widget_a new_combined_id new_single_id other_seq_single_id update widget

clean_row::Row->IO ()
clean_row (Blank _ _)=return ()
clean_row (Row seq_texture _ _)=DF.mapM_ (\(texture,_,_,_,_)->SRV.destroyTexture texture) seq_texture

adaptive_window::FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->(FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)
adaptive_window design_x design_y x y=let new_x=design_y*x in let new_y=design_x*y in if new_x<new_y then let common=gcd design_x x in (0,div (new_y-new_x) (2*design_x),div design_x common,div x common) else let common=gcd design_y y in (div (new_x-new_y) (2*design_y),0,div design_y common,div y common)

color::DW.Word8->DW.Word8->DW.Word8->DW.Word8->Color
color=SRT.Color