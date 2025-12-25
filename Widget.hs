{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Widget where
import Other
import Type
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS

create_single_widget::Single_widget_request a->IO (Single_widget a)
create_single_widget (Data_request content)=return (Data content)
create_single_widget (Trigger_request handle)=return (Trigger handle)

create_widget::DS.Seq Int->Combined_widget_request a->Engine a->IO (Engine a)
create_widget seq_single_id combined_widget_request (Engine widget window window_map request count_id start_id main_id)=case DS.viewl seq_single_id of
    DS.EmptyL->error "create_widget: empty seq_single_id"
    (single_id DS.:< other_seq_single_id)->do
        (new_count_id,new_widget)<-create_widget_a count_id start_id single_id other_seq_single_id combined_widget_request widget
        return (Engine new_widget window window_map request new_count_id start_id main_id)


create_widget_a::Int->Int->Int->DS.Seq Int->Combined_widget_request a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))
create_widget_a count_id combined_id single_id seq_single_id combined_widget_request widget=case DS.viewl seq_single_id of
    DS.EmptyL->create_widget_top count_id combined_id single_id combined_widget_request widget
    (new_single_id DS.:< other_seq_single_id)->case DIS.lookup combined_id widget of
        Nothing->error "create_widget_a: no such combined_id"
        Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
            Nothing->error "create_widget_a: no such single_id"
            Just (Leaf_widget _ _)->error "create_widget_a: wrong seq_single_id"
            Just (Node_widget _ _ new_combined_id)->create_widget_a count_id new_combined_id new_single_id other_seq_single_id combined_widget_request widget

create_widget_top::Int->Int->Int->Combined_widget_request a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))
create_widget_top count_id combined_id single_id (Leaf_widget_request next_id single_widget_request) widget=do
    new_single_widget<-create_single_widget single_widget_request
    return (count_id,error_insert_insert "create_widget_top: no such combined_id" "create_widget_top: no such single_id" combined_id single_id (Leaf_widget next_id new_single_widget) widget)
create_widget_top count_id combined_id single_id (Node_widget_request next_id main_single_id intmap_combined_widget_request) widget=DIS.foldlWithKey (create_widget_top_a count_id)  (return (count_id+1,error_insert "create_widget_top: You changed count_id without proper design" count_id DIS.empty (error_insert_insert "create_widget_top: no such combined_id" "create_widget_top: no such single_id" combined_id single_id (Node_widget next_id main_single_id count_id) widget))) intmap_combined_widget_request

create_widget_top_a::Int->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))->Int->Combined_widget_request a->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))
create_widget_top_a combined_id io single_id combined_widget_request=do
    (count_id,widget)<-io
    create_widget_top count_id combined_id single_id combined_widget_request widget

remove_single_widget::Data a=>Single_widget a->IO ()
remove_single_widget (Data content)=remove_data content
remove_single_widget (Trigger _)=return ()

remove_widget::Data a=>DS.Seq Int->Engine a->IO (Engine a)
remove_widget seq_single_id (Engine widget window window_map request count_id start_id main_id)=case DS.viewl seq_single_id of
    DS.EmptyL->error "remove_widget: empty seq_single_id"
    (single_id DS.:< other_seq_single_id)->do
        (_,new_widget)<-remove_widget_a start_id start_id single_id other_seq_single_id widget
        return (Engine new_widget window window_map request count_id start_id main_id)

remove_widget_a::Data a=>Int->Int->Int->DS.Seq Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Bool,DIS.IntMap (DIS.IntMap (Combined_widget a)))
remove_widget_a start_id combined_id single_id seq_single_id widget=case DS.viewl seq_single_id of
    DS.EmptyL->do
        (intmap_combined_widget,new_widget)<-remove_widget_top combined_id single_id widget
        if DIS.null intmap_combined_widget&&(start_id/=combined_id) then return (True,new_widget) else return (False,DIS.insert combined_id intmap_combined_widget new_widget)
    (new_single_id DS.:< other_seq_single_id)->case DIS.lookup combined_id widget of
        Nothing->error "remove_widget_a: no such combined_id"
        Just intmap_combined_widget->case DIS.updateLookupWithKey (\_ _->Nothing) single_id intmap_combined_widget of
            (Nothing,_)->error "remove_widget_a: no such single_id"
            (Just (Leaf_widget _ _),_)->error "remove_widget_a: wrong seq_single_id"
            (Just (Node_widget _ _ new_combined_id),new_intmap_combined_widget)->do
                (bool,new_widget)<-remove_widget_a start_id new_combined_id new_single_id other_seq_single_id widget
                if bool then if DIS.null new_intmap_combined_widget&&(start_id/=combined_id) then return (True,DIS.delete combined_id new_widget) else return (False,DIS.insert combined_id new_intmap_combined_widget new_widget) else return (False,new_widget)

remove_widget_top::Data a=>Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (Combined_widget a),DIS.IntMap (DIS.IntMap (Combined_widget a)))
remove_widget_top combined_id single_id widget=let (combined_widget,intmap_Combined_widget,new_widget)=error_remove_remove "remove_widget_top: no such combined_id" "remove_widget_top: no such single_id" combined_id single_id widget in case combined_widget of
    (Leaf_widget _ single_widget)->do
        remove_single_widget single_widget
        return (intmap_Combined_widget,new_widget)
    (Node_widget _ _ new_combined_id)->let (new_intmap_combined_widget,new_new_widget)=error_remove "remove_widget_top: you changed combined_id without proper design" new_combined_id new_widget in do
        new_new_new_widget<-DIS.foldl remove_widget_top_a (return new_new_widget) new_intmap_combined_widget
        return (intmap_Combined_widget,new_new_new_widget)

remove_widget_top_a::Data a=>IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))->Combined_widget a->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))
remove_widget_top_a io (Leaf_widget _ single_widget)=do
    widget<-io
    remove_single_widget single_widget
    return widget
remove_widget_top_a io (Node_widget _ _ combined_id)=do
    widget<-io
    let (intmap_combined_widget,new_widget)=error_remove "remove_widget_top: you changed combined_id without proper design" combined_id widget
    DIS.foldl remove_widget_top_a (return new_widget) intmap_combined_widget

replace_widget::Data a=>DS.Seq Int->Combined_widget_request a->Engine a->IO (Engine a)
replace_widget seq_single_id combined_widget_request (Engine widget window window_map request count_id start_id main_id)=case DS.viewl seq_single_id of
    DS.EmptyL->error "replace_widget: empty seq_single_id"
    (single_id DS.:< other_seq_single_id)->do
        (new_count_id,new_widget)<-replace_widget_a count_id start_id single_id other_seq_single_id combined_widget_request widget
        return (Engine new_widget window window_map request new_count_id start_id main_id)

replace_widget_a::Data a=>Int->Int->Int->DS.Seq Int->Combined_widget_request a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))
replace_widget_a count_id combined_id single_id seq_single_id combined_widget_request widget=case DS.viewl seq_single_id of
    DS.EmptyL->do
        (intmap_combined_widget,new_widget)<-remove_widget_top combined_id single_id widget
        create_widget_top count_id combined_id single_id combined_widget_request (DIS.insert combined_id intmap_combined_widget new_widget)
    (new_single_id DS.:< other_seq_single_id)->case DIS.lookup combined_id widget of
        Nothing->error "replace_widget_a: no such combined_id"
        Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
            Nothing->error "replace_widget_a: no such single_id"
            Just (Leaf_widget _ _)->error "replace_widget_a: wrong seq_single_id"
            Just (Node_widget _ _ new_combined_id)->replace_widget_a count_id new_combined_id new_single_id other_seq_single_id combined_widget_request widget
