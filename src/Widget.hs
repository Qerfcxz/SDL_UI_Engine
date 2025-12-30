{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Widget where
import Other
import Text
import Type
import qualified Control.Monad as CM
import qualified Data.Foldable as DF
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Text.Foreign as DTF
import qualified Foreign.C.String as FCS
import qualified Foreign.Ptr as FP
import qualified SDL.Raw.Font as SRF

create_single_widget::Int->DIS.IntMap Window->Single_widget_request a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Single_widget a)
create_single_widget _ _ (Data_request content) _=return (Data content)
create_single_widget _ _(Trigger_request handle) _=return (Trigger handle)
create_single_widget _ _(Io_trigger_request handle) _=return (Io_trigger handle)
create_single_widget _ _ (Font_request path size) _=do
    font<-DTF.withCString path (`create_font` size)
    return (Font font)
create_single_widget start_id window (Text_request window_id row find delta_height left right up down seq_paragraph) widget=case DIS.lookup window_id window of
    Nothing->error "create_single_widget: no such window"
    Just (Window _ _ renderer _ _ x y design_window_size window_size)->do
        seq_row<-from_paragraph widget renderer (find_font find) window_id start_id design_window_size window_size 0 0 (right-left) delta_height seq_paragraph DS.Empty
        return (Text window_id row find delta_height left right up down (x+div (left*window_size) design_window_size) (x+div (right*window_size) design_window_size) (y+div (up*window_size) design_window_size) (y+div (down*window_size) design_window_size) seq_paragraph seq_row)

create_font::FCS.CString->DS.Seq Int->IO (DIS.IntMap (FP.Ptr SRF.Font))
create_font _ DS.Empty=return DIS.empty
create_font path (size DS.:<| other_size)=do
    font<-create_font path other_size
    new_font<-SRF.openFont path (fromIntegral size)
    CM.when (new_font==FP.nullPtr) $ error "create_font: SDL.Raw.Font.openFont returns error"
    return (DIS.insert size new_font font)

create_widget::DS.Seq Int->Combined_widget_request a->Engine a->IO (Engine a)
create_widget seq_single_id combined_widget_request (Engine widget window window_map request count_id start_id main_id)=case seq_single_id of
    DS.Empty->error "create_widget: empty seq_single_id"
    (single_id DS.:<| other_seq_single_id)->do
        (new_count_id,new_widget)<-create_widget_a other_seq_single_id count_id start_id single_id start_id window combined_widget_request widget
        return (Engine new_widget window window_map request new_count_id start_id main_id)

create_widget_a::DS.Seq Int->Int->Int->Int->Int->DIS.IntMap Window->Combined_widget_request a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))
create_widget_a seq_single_id count_id combined_id single_id start_id window combined_widget_request widget=case seq_single_id of
    DS.Empty->create_widget_top count_id combined_id single_id start_id window combined_widget_request widget
    (new_single_id DS.:<| other_seq_single_id)->case DIS.lookup combined_id widget of
        Nothing->error "create_widget_a: no such combined_id"
        Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
            Nothing->error "create_widget_a: no such single_id"
            Just (Leaf_widget _ _)->error "create_widget_a: wrong seq_single_id"
            Just (Node_widget _ _ new_combined_id)->create_widget_a other_seq_single_id count_id new_combined_id new_single_id start_id window combined_widget_request widget

create_widget_top::Int->Int->Int->Int->DIS.IntMap Window->Combined_widget_request a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))
create_widget_top count_id combined_id single_id start_id window (Leaf_widget_request next_id single_widget_request) widget=do
    new_single_widget<-create_single_widget start_id window single_widget_request widget
    return (count_id,error_insert_insert "create_widget_top: no such combined_id" "create_widget_top: no such single_id" combined_id single_id (Leaf_widget next_id new_single_widget) widget)
create_widget_top count_id combined_id single_id start_id window (Node_widget_request next_id main_single_id intmap_combined_widget_request) widget=DIS.foldlWithKey (\io this_single_id->create_widget_top_a count_id this_single_id start_id window io)  (return (count_id+1,error_insert "create_widget_top: you changed count_id without proper design" count_id DIS.empty (error_insert_insert "create_widget_top: no such combined_id" "create_widget_top: no such single_id" combined_id single_id (Node_widget next_id main_single_id count_id) widget))) intmap_combined_widget_request

create_widget_top_a::Int->Int->Int->DIS.IntMap Window->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))->Combined_widget_request a->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))
create_widget_top_a combined_id single_id start_id window io combined_widget_request=do
    (count_id,widget)<-io
    create_widget_top count_id combined_id single_id start_id window combined_widget_request widget

remove_single_widget::Data a=>Single_widget a->IO ()
remove_single_widget (Data content)=clean_data content
remove_single_widget (Trigger _)=return ()
remove_single_widget (Io_trigger _)=return ()
remove_single_widget (Font intmap_font)=do
    _<-DIS.traverseWithKey (\_ font->SRF.closeFont font) intmap_font
    return ()
remove_single_widget (Text _ _ _ _ _ _ _ _ _ _ _ _ _ seq_row)=DF.mapM_ clean_row seq_row

remove_widget::Data a=>DS.Seq Int->Engine a->IO (Engine a)
remove_widget seq_single_id (Engine widget window window_map request count_id start_id main_id)=case seq_single_id of
    DS.Empty->error "remove_widget: empty seq_single_id"
    (single_id DS.:<| other_seq_single_id)->do
        (_,new_widget)<-remove_widget_a other_seq_single_id start_id start_id single_id widget
        return (Engine new_widget window window_map request count_id start_id main_id)

remove_widget_a::Data a=>DS.Seq Int->Int->Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Bool,DIS.IntMap (DIS.IntMap (Combined_widget a)))
remove_widget_a seq_single_id start_id combined_id single_id widget=case seq_single_id of
    DS.Empty->do
        (intmap_combined_widget,new_widget)<-remove_widget_top combined_id single_id widget
        if DIS.null intmap_combined_widget&&(start_id/=combined_id) then return (True,new_widget) else return (False,DIS.insert combined_id intmap_combined_widget new_widget)
    (new_single_id DS.:<| other_seq_single_id)->case DIS.lookup combined_id widget of
        Nothing->error "remove_widget_a: no such combined_id"
        Just intmap_combined_widget->case DIS.updateLookupWithKey (\_ _->Nothing) single_id intmap_combined_widget of
            (Nothing,_)->error "remove_widget_a: no such single_id"
            (Just (Leaf_widget _ _),_)->error "remove_widget_a: wrong seq_single_id"
            (Just (Node_widget _ _ new_combined_id),new_intmap_combined_widget)->do
                (bool,new_widget)<-remove_widget_a other_seq_single_id start_id new_combined_id new_single_id widget
                if bool then if DIS.null new_intmap_combined_widget&&(start_id/=combined_id) then return (True,DIS.delete combined_id new_widget) else return (False,DIS.insert combined_id new_intmap_combined_widget new_widget) else return (False,new_widget)

remove_widget_top::Data a=>Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (Combined_widget a),DIS.IntMap (DIS.IntMap (Combined_widget a)))
remove_widget_top combined_id single_id widget=let (combined_widget,intmap_combined_widget,new_widget)=error_remove_remove "remove_widget_top: no such combined_id" "remove_widget_top: no such single_id" combined_id single_id widget in case combined_widget of
    (Leaf_widget _ single_widget)->do
        remove_single_widget single_widget
        return (intmap_combined_widget,new_widget)
    (Node_widget _ _ new_combined_id)->let (new_intmap_combined_widget,new_new_widget)=error_remove "remove_widget_top: you changed combined_id without proper design" new_combined_id new_widget in do
        new_new_new_widget<-DIS.foldl remove_widget_top_a (return new_new_widget) new_intmap_combined_widget
        return (intmap_combined_widget,new_new_new_widget)

remove_widget_top_a::Data a=>IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))->Combined_widget a->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))
remove_widget_top_a io (Leaf_widget _ single_widget)=do
    widget<-io
    remove_single_widget single_widget
    return widget
remove_widget_top_a io (Node_widget _ _ combined_id)=do
    widget<-io
    let (intmap_combined_widget,new_widget)=error_remove "remove_widget_top_a: you changed combined_id without proper design" combined_id widget
    DIS.foldl remove_widget_top_a (return new_widget) intmap_combined_widget

replace_widget::Data a=>DS.Seq Int->Combined_widget_request a->Engine a->IO (Engine a)
replace_widget seq_single_id combined_widget_request (Engine widget window window_map request count_id start_id main_id)=case seq_single_id of
    DS.Empty->error "replace_widget: empty seq_single_id"
    (single_id DS.:<| other_seq_single_id)->do
        (new_count_id,new_widget)<-replace_widget_a other_seq_single_id count_id start_id single_id start_id window combined_widget_request widget
        return (Engine new_widget window window_map request new_count_id start_id main_id)

replace_widget_a::Data a=>DS.Seq Int->Int->Int->Int->Int->DIS.IntMap Window->Combined_widget_request a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))
replace_widget_a seq_single_id count_id combined_id single_id start_id window combined_widget_request widget=case seq_single_id of
    DS.Empty->do
        (intmap_combined_widget,new_widget)<-remove_widget_top combined_id single_id widget
        create_widget_top count_id combined_id single_id start_id window combined_widget_request (DIS.insert combined_id intmap_combined_widget new_widget)
    (new_single_id DS.:<| other_seq_single_id)->case DIS.lookup combined_id widget of
        Nothing->error "replace_widget_a: no such combined_id"
        Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
            Nothing->error "replace_widget_a: no such single_id"
            Just (Leaf_widget _ _)->error "replace_widget_a: wrong seq_single_id"
            Just (Node_widget _ _ new_combined_id)->replace_widget_a other_seq_single_id count_id new_combined_id new_single_id start_id window combined_widget_request widget

create_text_trigger::(Engine a->Id)->DS.Seq Int->DIS.IntMap (DS.Seq (DS.Seq Int))->Engine a->IO (Engine a)
create_text_trigger next_id seq_id id_map=create_widget seq_id (Leaf_widget_request next_id (Io_trigger_request (create_text_trigger_a id_map)))

create_text_trigger_a::DIS.IntMap (DS.Seq (DS.Seq Int))->Event->Engine a->IO (Engine a)
create_text_trigger_a id_map event engine=case event of
    Resize window_id _ _->case DIS.lookup window_id id_map of
        Nothing->return engine
        Just seq_seq_id->CM.foldM (flip update_text) engine seq_seq_id
    _->return engine

create_window_trigger::(Engine a->Id)->DS.Seq Int->DS.Seq Int->Engine a->IO (Engine a)
create_window_trigger next_id seq_id seq_window_id=create_widget seq_id (Leaf_widget_request next_id (Io_trigger_request (create_window_trigger_a seq_window_id)))

create_window_trigger_a::DS.Seq Int->Event->Engine a->IO (Engine a)
create_window_trigger_a seq_window_id event engine@(Engine widget window window_map request count_id start_id main_id)=case event of
    Resize window_id width height->case DS.elemIndexL window_id seq_window_id of
        Nothing->return engine
        Just _->return (Engine widget (error_update "create_window_trigger_a: no such window_id" window_id (\(Window this_window_id this_window renderer design_width design_height _ _ _ _)->let (x,y,design_size,size)=adaptive_window design_width design_height width height in Window this_window_id this_window renderer design_width design_height x y design_size size) window) window_map request count_id start_id main_id)
    _->return engine

get_single_widget_id::DS.Seq Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->(Int,Int)
get_single_widget_id seq_single_id start_id widget=case seq_single_id of
    DS.Empty->error "get_single_widget_id: empty seq_single_id"
    single_id DS.:<| other_seq_single_id->get_single_widget_id_a other_seq_single_id start_id single_id widget

get_single_widget_id_widget::DS.Seq Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->(Int,Int)
get_single_widget_id_widget seq_single_id start_id widget=case seq_single_id of
    DS.Empty->error "get_single_widget_id_widget: empty seq_single_id"
    single_id DS.:<| other_seq_single_id->get_single_widget_id_a other_seq_single_id start_id single_id widget

get_single_widget_id_a::DS.Seq Int->Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->(Int,Int)
get_single_widget_id_a seq_single_id combined_id single_id widget=case seq_single_id of
    DS.Empty->(combined_id,single_id)
    (new_single_id DS.:<| other_seq_single_id)->case DIS.lookup combined_id widget of
        Nothing->error "get_single_widget_id_a: no such combined_id"
        Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
            Nothing->error "get_single_widget_id_a: no such single_id"
            Just (Leaf_widget _ _)->error "get_single_widget_id_a: wrong seq_single_id"
            Just (Node_widget _ _ new_combined_id)->get_single_widget_id_a other_seq_single_id new_combined_id new_single_id widget

alter_widget::Data a=>DS.Seq Int->Combined_widget_request a->Engine a->IO (Engine a)
alter_widget seq_single_id combined_widget_request (Engine widget window window_map request count_id start_id main_id)=let (combined_id,single_id)=get_single_widget_id_widget seq_single_id start_id widget in do
    new_widget<-error_update_update "alter_widget: no such combined_id" "alter_widget: no such  single_id" combined_id single_id (alter_widget_a start_id window widget combined_widget_request) widget
    return (Engine new_widget window window_map request count_id start_id main_id)

alter_widget_a::Data a=>Int->DIS.IntMap Window->DIS.IntMap (DIS.IntMap (Combined_widget a))->Combined_widget_request a->Combined_widget a->IO (Combined_widget a)
alter_widget_a start_id window widget (Leaf_widget_request next_id single_widget_request) this_widget=case single_widget_request of
    Data_request content->case this_widget of
        (Leaf_widget _ (Data this_content))->do
            clean_data this_content
            return (Leaf_widget next_id (Data content))
        _->error "alter_widget_a: not a data widget"
    Trigger_request handle->case this_widget of
        Leaf_widget _ (Trigger _)-> return (Leaf_widget next_id (Trigger handle))
        _->error "alter_widget_a: not a trigger widget"
    Io_trigger_request handle->case this_widget of
        Leaf_widget _ (Io_trigger _)-> return (Leaf_widget next_id (Io_trigger handle))
        _->error "alter_widget_a: not a io_trigger widget"
    Font_request path size->case this_widget of
        Leaf_widget _ (Font intmap_font)->do
            _<-DIS.traverseWithKey (\_ font->SRF.closeFont font) intmap_font
            font<-DTF.withCString path (`create_font` size)
            return (Leaf_widget next_id (Font font))
        _->error "alter_widget_a: not a font widget"
    Text_request window_id row find delta_height left right up down seq_paragraph->case this_widget of
        Leaf_widget _ (Text _ _ _ _ _ _ _ _ _ _ _ _ _ seq_row)->case DIS.lookup window_id window of
            Nothing->error "alter_widget_a: no such window_id"
            Just (Window _ _ renderer _ _ x y design_window_size window_size)->do
                DF.mapM_ clean_row seq_row
                new_seq_row<-from_paragraph widget renderer (find_font find) window_id start_id design_window_size window_size 0 0 (right-left) delta_height seq_paragraph DS.Empty
                return (Leaf_widget next_id (Text window_id row find delta_height left right up down (x+div (left*window_size) design_window_size) (x+div (right*window_size) design_window_size) (y+div (up*window_size) design_window_size) (y+div (down*window_size) design_window_size) seq_paragraph new_seq_row))
        _->error "alter_widget_a: not a Text widget"
alter_widget_a _ _ _ _ _=error "you can't alter node_widget"