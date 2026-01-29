{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Other.Error where
import qualified Control.Monad as CM
import qualified Data.IntMap.Strict as DIS

catch_error::Eq a=>[Char]->a->IO a->IO ()
catch_error error_message success result=do
    flag<-result
    CM.unless (flag==success) (error error_message)

error_lookup_lookup::[Char]->[Char]->Int->Int->DIS.IntMap (DIS.IntMap a)->a
error_lookup_lookup first_error_message second_error_message first_key second_key intmap_intmap=case DIS.lookup first_key intmap_intmap of
    Nothing->error first_error_message
    Just intmap->case DIS.lookup second_key intmap of
        Nothing->error second_error_message
        Just value->value

error_remove_simple::[Char]->Int->DIS.IntMap a->DIS.IntMap a
error_remove_simple error_message key intmap=case DIS.updateLookupWithKey (\_ _->Nothing) key intmap of
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

error_replace_replace::[Char]->[Char]->Int->Int->a->DIS.IntMap (DIS.IntMap a)->DIS.IntMap (DIS.IntMap a)
error_replace_replace first_error_message second_error_message first_key second_key value=DIS.alter (error_replace_replace_a first_error_message second_error_message second_key value) first_key

error_replace_replace_a::[Char]->[Char]->Int->a->Maybe (DIS.IntMap a)->Maybe (DIS.IntMap a)
error_replace_replace_a first_error_message _ _ _ Nothing=error first_error_message
error_replace_replace_a _ second_error_message key value (Just intmap)=if DIS.member key intmap then Just (DIS.insert key value intmap) else error second_error_message

error_update::[Char]->Int->(a->a)->DIS.IntMap a->DIS.IntMap a
error_update error_message key update=DIS.alter (error_update_a error_message update) key

error_update_a::[Char]->(a->a)->Maybe a->Maybe a
error_update_a error_message _ Nothing=error error_message
error_update_a _ update (Just value)=Just (update value)

error_get_update_update::[Char]->[Char]->Int->Int->(a->a)->DIS.IntMap (DIS.IntMap a)->(a,DIS.IntMap (DIS.IntMap a))
error_get_update_update first_error_message second_error_message first_key second_key update=DIS.alterF (error_get_update_update_a first_error_message second_error_message second_key update) first_key

error_get_update_update_a::[Char]->[Char]->DIS.Key->(a->a)->Maybe (DIS.IntMap a)->(a,Maybe (DIS.IntMap a))
error_get_update_update_a first_error_message _ _ _ Nothing=error first_error_message
error_get_update_update_a _ second_error_message key update (Just intmap)=let (value,new_intmap)=DIS.alterF (error_get_update_update_b second_error_message update) key intmap in (value,Just new_intmap)

error_get_update_update_b::[Char]->(a->a)->Maybe a->(a,Maybe a)
error_get_update_update_b error_message _ Nothing=error error_message
error_get_update_update_b _ update (Just value)=(value,Just (update value))

error_update_update::[Char]->[Char]->Int->Int->(a->a)->DIS.IntMap (DIS.IntMap a)->DIS.IntMap (DIS.IntMap a)
error_update_update first_error_message second_error_message first_key second_key update=DIS.alter (error_update_update_a first_error_message second_error_message second_key update) first_key

error_update_update_a::[Char]->[Char]->DIS.Key->(a->a)->Maybe (DIS.IntMap a)->Maybe (DIS.IntMap a)
error_update_update_a first_error_message _ _ _ Nothing=error first_error_message
error_update_update_a _ second_error_message key update (Just intmap)=Just (DIS.alter (error_update_update_b second_error_message update) key intmap)

error_update_update_b::[Char]->(a->a)->Maybe a->Maybe a
error_update_update_b error_message _ Nothing=error error_message
error_update_update_b _ update (Just value)=Just (update value)

error_update_update_io::[Char]->[Char]->Int->Int->(a->IO a)->DIS.IntMap (DIS.IntMap a)->IO (DIS.IntMap (DIS.IntMap a))
error_update_update_io first_error_message second_error_message first_key second_key update=DIS.alterF (error_update_update_io_a first_error_message second_error_message second_key update) first_key

error_update_update_io_a::[Char]->[Char]->DIS.Key->(a->IO a)->Maybe (DIS.IntMap a)->IO (Maybe (DIS.IntMap a))
error_update_update_io_a first_error_message _ _ _ Nothing=error first_error_message
error_update_update_io_a _ second_error_message key update (Just intmap)=Just <$> DIS.alterF (error_update_update_io_b second_error_message update) key intmap

error_update_update_io_b::[Char]->(a->IO a)->Maybe a->IO (Maybe a)
error_update_update_io_b error_message _ Nothing=error error_message
error_update_update_io_b _ update (Just value)=do
    new_value<-update value
    return (Just new_value)