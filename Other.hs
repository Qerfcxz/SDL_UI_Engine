{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Other where
import Type
import qualified Control.Monad as CM
import qualified Data.IntMap.Strict as DIS
import qualified SDL.Raw.Types as SRT

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

get_renderer::Int->Engine a->SRT.Renderer
get_renderer window_id (Engine _ window _ _ _ _ _)=case DIS.lookup window_id window of
    Nothing->error "No such window"
    Just (Window _ _ renderer)->renderer

get_next_id::Combined_widget a->(Engine a->Id)
get_next_id (Leaf_widget next_single_id _)=next_single_id
get_next_id (Node_widget next_single_id _ _)=next_single_id