{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Widget where
import Editor
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
import qualified Foreign.Storable as FS
import qualified Foreign.Marshal.Alloc as FMA
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

create_single_widget::Int->DIS.IntMap Window->Single_widget_request a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Single_widget a)
create_single_widget _ _ (Data_request content) _=return (Data content)
create_single_widget _ _ (Trigger_request handle) _=return (Trigger handle)
create_single_widget _ _ (Io_trigger_request handle) _=return (Io_trigger handle)
create_single_widget _ _ (Font_request path size) _=do
    font<-DTF.withCString path (`create_font` size)
    return (Font font)
create_single_widget _ window (Rectangle_request window_id red green blue alpha left right up down) _=case DIS.lookup window_id window of
    Nothing->error "create_single_widget: no such window"
    Just (Window _ _ _ _ _ x y design_size size)->return (Rectangle window_id red green blue alpha left right up down (x+div (left*size) design_size) (y+div (up*size) design_size) (div ((right-left)*size) design_size) (div ((down-up)*size) design_size))
create_single_widget _ window (Picture_request window_id path x y width_multiply width_divide height_multiply height_divide) _=case DIS.lookup window_id window of
    Nothing->error "create_single_widget: no such window_id"
    Just (Window _ _ renderer _ _ window_x window_y design_size size)->do
        surface<-DTF.withCString path SRV.loadBMP
        CM.when (surface==FP.nullPtr) $ error "create_single_widget: SDL.Raw.Font.renderUTF8_Blended returns error"
        (SRT.Surface _ width height _ _ _ _)<-FS.peek surface
        texture<-SRV.createTextureFromSurface renderer surface
        SRV.freeSurface surface
        CM.when (texture==FP.nullPtr) $ error "create_single_widget: SDL.Raw.Video.createTextureFromSurface returns error"
        let new_width=div (width*width_multiply) width_divide in let new_height=div (height*height_multiply) height_divide in return (Picture window_id texture x y width_multiply width_divide height_multiply height_divide width height (window_x+div ((x-div new_width 2)*size) design_size) (window_y+div ((y-div new_height 2)*size) design_size) (div (new_width*size) design_size) (div (new_height*size) design_size))
create_single_widget start_id window (Text_request window_id row find delta_height left right up down seq_paragraph) widget=case DIS.lookup window_id window of
    Nothing->error "create_single_widget: no such window"
    Just (Window _ _ renderer _ _ x y design_size size)->let new_delta_height=div (delta_height*size) design_size in do
        seq_row<-from_paragraph widget renderer (find_font find) window_id start_id design_size size 0 (div ((right-left)*size) design_size) new_delta_height seq_paragraph DS.empty
        let new_up=y+div (up*size) design_size in let new_down=y+div (down*size) design_size in let max_row=find_max seq_row new_up new_down in return (Text window_id (max 0 (min row max_row)) max_row False find delta_height left right up down new_delta_height (x+div (left*size) design_size) (x+div (right*size) design_size) new_up new_down seq_paragraph seq_row)
create_single_widget start_id window (Editor_request window_id paragraph_id row_id row font_size path find typesetting text_red text_green text_blue text_alpha select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha delta_height extra_width extra_height left right up down text) widget=case DIS.lookup window_id window of
    Nothing->error "create_single_widget: no such window"
    Just (Window _ _ renderer _ _ x y design_size size)->let font=find_font find widget design_size size start_id font_size path in do
        text_color<-FMA.malloc
        FS.poke text_color (color text_red text_green text_blue text_alpha)
        editor_text renderer window_id paragraph_id row_id paragraph_id row_id row font_size path font find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha x y design_size size delta_height extra_width extra_height left right up down text

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
create_widget_top count_id combined_id single_id start_id window (Node_widget_request next_id main_single_id intmap_combined_widget_request) widget=DIS.foldlWithKey (\io this_single_id->create_widget_top_a count_id this_single_id start_id window io)  (return (count_id+1,error_insert "create_widget_top: you changed something without proper design" count_id DIS.empty (error_insert_insert "create_widget_top: no such combined_id" "create_widget_top: no such single_id" combined_id single_id (Node_widget next_id main_single_id count_id) widget))) intmap_combined_widget_request

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
remove_single_widget (Rectangle {})=return ()
remove_single_widget (Picture _ texture _ _ _ _ _ _ _ _ _ _ _ _)=SRV.destroyTexture texture
remove_single_widget (Text _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ seq_row)=DF.mapM_ clean_row seq_row
remove_single_widget (Editor _ _ _ _ _ _ _ _ _ _ _ _ this_color _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ intmap_intmap_texture _)=do
    FMA.free this_color
    DF.mapM_ (DF.mapM_ (\(texture,_,_,_,_)->CM.unless (texture==FP.nullPtr) (SRV.destroyTexture texture))) intmap_intmap_texture

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
    (Node_widget _ _ new_combined_id)->let (new_intmap_combined_widget,new_new_widget)=error_remove "remove_widget_top: you changed something without proper design" new_combined_id new_widget in do
        new_new_new_widget<-DIS.foldl remove_widget_top_a (return new_new_widget) new_intmap_combined_widget
        return (intmap_combined_widget,new_new_new_widget)

remove_widget_top_a::Data a=>IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))->Combined_widget a->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))
remove_widget_top_a io (Leaf_widget _ single_widget)=do
    widget<-io
    remove_single_widget single_widget
    return widget
remove_widget_top_a io (Node_widget _ _ combined_id)=do
    widget<-io
    let (intmap_combined_widget,new_widget)=error_remove "remove_widget_top_a: you changed something without proper design" combined_id widget
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
update_widget_a _ window _ (Leaf_widget next_id (Rectangle window_id red green blue alpha left right up down _ _ _ _))=let (x,y,design_size,size)=get_transform window_id window in return (Leaf_widget next_id (Rectangle window_id red green blue alpha left right up down (x+div (left*size) design_size) (y+div (up*size) design_size) (div ((right-left)*size) design_size) (div ((down-up)*size) design_size)))
update_widget_a _ window _ (Leaf_widget next_id (Picture window_id texture x y width_multiply width_divide height_multiply height_divide width height _ _ _ _))=let (window_x,window_y,design_size,size)=get_transform window_id window in let new_width=div (width*width_multiply) width_divide in let new_height=div (height*height_multiply) height_divide in return (Leaf_widget next_id (Picture window_id texture x y width_multiply width_divide height_multiply height_divide width height (window_x+div ((x-div new_width 2)*size) design_size) (window_y+div ((y-div new_height 2)*size) design_size) (div (new_width*size) design_size) (div (new_height*size) design_size)))
update_widget_a start_id window widget (Leaf_widget next_id (Text window_id row _ bool find delta_height left right up down _ _ _ _ _ seq_paragraph seq_row))=do
    DF.mapM_ clean_row seq_row
    let (renderer,x,y,design_size,size)=get_renderer_with_transform window_id window
    let new_delta_height=div (delta_height*size) design_size
    new_seq_row<-from_paragraph widget renderer (find_font find) window_id start_id design_size size 0 (div ((right-left)*size) design_size) new_delta_height seq_paragraph DS.empty
    let new_up=(y+div (up*size) design_size) in let new_down=(y+div (down*size) design_size) in let max_row=find_max new_seq_row new_up new_down in return (Leaf_widget next_id (Text window_id (max 0 (min row max_row)) max_row bool find delta_height left right up down new_delta_height (x+div (left*size) design_size) (x+div (right*size) design_size) new_up new_down seq_paragraph new_seq_row))
update_widget_a _ _ _ _=error "update_widget_a: wrong widget"

create_window_trigger::(Event->Engine a->Id)->DS.Seq Int->DS.Seq Int->Engine a->IO (Engine a)
create_window_trigger next_id seq_id seq_window_id=create_widget seq_id (Leaf_widget_request next_id (Trigger_request (create_window_trigger_a seq_window_id)))

create_window_trigger_a::DS.Seq Int->Event->Engine a->Engine a
create_window_trigger_a seq_window_id event engine@(Engine widget window window_map request count_id start_id main_id)=case event of
    Resize window_id width height->case DS.elemIndexL window_id seq_window_id of
        Nothing->engine
        Just _->Engine widget (error_update "create_window_trigger_a: no such window_id" window_id (\(Window this_window_id this_window renderer design_width design_height _ _ _ _)->let (x,y,design_size,size)=adaptive_window design_width design_height width height in Window this_window_id this_window renderer design_width design_height x y design_size size) window) window_map request count_id start_id main_id
    _->engine

create_text_trigger::Bool->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->(Event->Engine a->Id)->DS.Seq Int->DS.Seq (DS.Seq Int)->Engine a->IO (Engine a)
create_text_trigger bool up_press down_press min_press max_press select_click next_id seq_id seq_seq_id=create_widget seq_id (Leaf_widget_request next_id (Trigger_request (\event (Engine widget window window_map request count_id start_id main_id)->let (new_widget,new_request)=DF.foldl' (\this_widget_request this_seq_id->create_text_trigger_a bool up_press down_press min_press max_press select_click this_seq_id start_id event this_widget_request) (widget,request) seq_seq_id in Engine new_widget window window_map new_request count_id start_id main_id)))

create_text_trigger_a::Bool->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->DS.Seq Int->Int->Event->(DIS.IntMap (DIS.IntMap (Combined_widget a)),DS.Seq (Request a))->(DIS.IntMap (DIS.IntMap (Combined_widget a)),DS.Seq (Request a))
create_text_trigger_a bool up_press down_press min_press max_press select_click seq_id start_id event (widget,request)=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in let (this_bool,new_widget)=DIS.alterF (create_text_trigger_b up_press down_press min_press max_press select_click single_id event) combined_id widget in (new_widget,if bool&&this_bool then request DS.|> Render_text_widget seq_id else request)

create_text_trigger_b::DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->Int->Event->Maybe (DIS.IntMap (Combined_widget a))->(Bool,Maybe (DIS.IntMap (Combined_widget a)))
create_text_trigger_b _ _ _ _ _ _ _ Nothing=error "create_text_trigger_b: no such combined_id"
create_text_trigger_b up_press down_press min_press max_press select_click single_id event (Just intmap_combined_widget)=let (bool,new_intmap_combined_widget)=DIS.alterF (create_text_trigger_c up_press down_press min_press max_press select_click event) single_id intmap_combined_widget in (bool,Just new_intmap_combined_widget)

create_text_trigger_c::DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->Event->Maybe (Combined_widget a)->(Bool,Maybe (Combined_widget a))
create_text_trigger_c _ _ _ _ _ _ Nothing=error "create_text_trigger_c: no such single_id"
create_text_trigger_c up_press down_press min_press max_press select_click event (Just widget)=let (new_widget,bool)=create_text_trigger_d up_press down_press min_press max_press select_click event widget in (bool,Just new_widget)

create_text_trigger_d::DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->Event->Combined_widget a->(Combined_widget a,Bool)
create_text_trigger_d up_press down_press min_press max_press select_click event widget@(Leaf_widget next_id (Text window_id row max_row bool find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row))=if bool
    then case event of
        At this_window_id action->if window_id==this_window_id
            then case action of
                Press press key->if belong (press,key) up_press then if 0<row then (Leaf_widget next_id (Text window_id (row-1) max_row bool find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row),True) else (widget,False) else if belong (press,key) down_press then if row<max_row then (Leaf_widget next_id (Text window_id (row+1) max_row bool find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row),True) else (widget,False) else if belong (press,key) min_press then if row==0 then (widget,False) else (Leaf_widget next_id (Text window_id 0 max_row bool find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row),True) else if belong (press,key) max_press then if row==max_row then (widget,False) else (Leaf_widget next_id (Text window_id max_row max_row bool find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row),True) else (widget,False)
                Click click mouse x y->if belong (click,mouse) select_click&&(x<left||right<x||y<up||down<y) then (Leaf_widget next_id (Text window_id row max_row False find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row),False) else (widget,False)
                _->(widget,False)
            else (widget,False)
        _->(widget,False)
    else case event of
        At this_window_id action->if window_id==this_window_id
            then case action of
                Click click mouse x y->if belong (click,mouse) select_click&&left<=x&&x<=right&&up<=y&&y<=down then (Leaf_widget next_id (Text window_id row max_row True find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row),False) else (widget,False)
                _->(widget,False)
            else (widget,False)
        _->(widget,False)
create_text_trigger_d _ _ _ _ _ _ widget=(widget,False)

get_widget_id::DS.Seq Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->(Int,Int)
get_widget_id seq_single_id start_id widget=case seq_single_id of
    DS.Empty->error "get_widget_id: empty seq_single_id"
    single_id DS.:<| other_seq_single_id->get_widget_id_a other_seq_single_id start_id single_id widget

get_widget_id_widget::DS.Seq Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->(Int,Int)
get_widget_id_widget seq_single_id start_id widget=case seq_single_id of
    DS.Empty->error "get_widget_id_widget: empty seq_single_id"
    single_id DS.:<| other_seq_single_id->get_widget_id_a other_seq_single_id start_id single_id widget

get_widget_id_a::DS.Seq Int->Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->(Int,Int)
get_widget_id_a seq_single_id combined_id single_id widget=case seq_single_id of
    DS.Empty->(combined_id,single_id)
    (new_single_id DS.:<| other_seq_single_id)->case DIS.lookup combined_id widget of
        Nothing->error "get_widget_id_a: no such combined_id"
        Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
            Nothing->error "get_widget_id_a: no such single_id"
            Just (Leaf_widget _ _)->error "get_widget_id_a: wrong seq_single_id"
            Just (Node_widget _ _ new_combined_id)->get_widget_id_a other_seq_single_id new_combined_id new_single_id widget

alter_widget::Data a=>DS.Seq Int->Combined_widget_request a->Engine a->IO (Engine a)
alter_widget seq_single_id combined_widget_request (Engine widget window window_map request count_id start_id main_id)=let (combined_id,single_id)=get_widget_id_widget seq_single_id start_id widget in do
    new_widget<-error_update_update_io "alter_widget: no such combined_id" "alter_widget: no such single_id" combined_id single_id (alter_widget_a start_id window widget combined_widget_request) widget
    return (Engine new_widget window window_map request count_id start_id main_id)

alter_widget_a::Data a=>Int->DIS.IntMap Window->DIS.IntMap (DIS.IntMap (Combined_widget a))->Combined_widget_request a->Combined_widget a->IO (Combined_widget a)
alter_widget_a start_id window widget (Leaf_widget_request next_id single_widget_request) (Leaf_widget _ single_widget)=do
    new_single_widget<-alter_single_widget start_id window widget single_widget_request single_widget
    return (Leaf_widget next_id new_single_widget)
alter_widget_a _ _ _ _ _=error"alter_widget_a: you can't alter node_widget yet"--未完待续

alter_single_widget::Data a=>Int->DIS.IntMap Window->DIS.IntMap (DIS.IntMap (Combined_widget a))->Single_widget_request a->Single_widget a->IO (Single_widget a)
alter_single_widget _ _ _ (Data_request content) this_widget=case this_widget of
    Data this_content->do
        clean_data this_content
        return (Data content)
    _->error "alter_single_widget: not a data widget"
alter_single_widget _ _ _ (Trigger_request handle) this_widget=case this_widget of
    Trigger _->return (Trigger handle)
    _->error "alter_single_widget: not a trigger widget"
alter_single_widget _ _ _ (Io_trigger_request handle) this_widget=case this_widget of
    Trigger _->return (Io_trigger handle)
    _->error "alter_single_widget: not a io_trigger widget"
alter_single_widget _ _ _ (Font_request path size) this_widget=case this_widget of
    Font intmap_font->do
        _<-DIS.traverseWithKey (\_ font->SRF.closeFont font) intmap_font
        font<-DTF.withCString path (`create_font` size)
        return (Font font)
    _->error "alter_single_widget: not a font widget"
alter_single_widget _ window _ (Rectangle_request window_id red green blue alpha left right up down) this_widget=case this_widget of
    Rectangle {}->case DIS.lookup window_id window of
        Nothing->error "alter_single_widget: no such window_id"
        Just (Window _ _ _ _ _ x y design_size size)->return (Rectangle window_id red green blue alpha left right up down (x+div (left*size) design_size) (y+div (up*size) design_size) (div ((right-left)*size) design_size) (div ((down-up)*size) design_size))
    _->error "alter_single_widget: not a rectangle widget"
alter_single_widget _ window _ (Picture_request window_id path x y width_multiply width_divide height_multiply height_divide) this_widget=case this_widget of
    Picture _ texture _ _ _ _ _ _ _ _ _ _ _ _->case DIS.lookup window_id window of
        Nothing->error "alter_single_widget: no such window_id"
        Just (Window _ _ renderer _ _ window_x window_y design_size size)->do
            SRV.destroyTexture texture
            surface<-DTF.withCString path SRV.loadBMP
            CM.when (surface==FP.nullPtr) $ error "alter_single_widget: SDL.Raw.Font.renderUTF8_Blended returns error"
            (SRT.Surface _ width height _ _ _ _)<-FS.peek surface
            new_texture<-SRV.createTextureFromSurface renderer surface
            SRV.freeSurface surface
            CM.when (new_texture==FP.nullPtr) $ error "alter_single_widget: SDL.Raw.Video.createTextureFromSurface returns error"
            let new_width=div (width*width_multiply) width_divide in let new_height=div (height*height_multiply) height_divide in return (Picture window_id new_texture x y width_multiply width_divide height_multiply height_divide width height (window_x+div ((x-div new_width 2)*size) design_size) (window_y+div ((y-div new_height 2)*size) design_size) (div (new_width*size) design_size) (div (new_height*size) design_size))
    _->error "alter_single_widget: not a picture widget"
alter_single_widget start_id window widget (Text_request window_id row find delta_height left right up down seq_paragraph) this_widget=case this_widget of
    Text _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ seq_row->case DIS.lookup window_id window of
        Nothing->error "alter_single_widget: no such window_id"
        Just (Window _ _ renderer _ _ x y design_size size)->do
            DF.mapM_ clean_row seq_row
            let new_delta_height=div (delta_height*size) design_size
            new_seq_row<-from_paragraph widget renderer (find_font find) window_id start_id design_size size 0 (div ((right-left)*size) design_size) new_delta_height seq_paragraph DS.empty
            let new_up=y+div (up*size) design_size in let new_down=y+div (down*size) design_size in let max_row=find_max new_seq_row new_up new_down in return (Text window_id (max 0 (min max_row row)) max_row False find delta_height left right up down new_delta_height (x+div (left*size) design_size) (x+div (right*size) design_size) new_up new_down seq_paragraph new_seq_row)
    _->error "alter_single_widget: not a text widget"
alter_single_widget start_id window widget (Editor_request window_id paragraph_id row_id row font_size path find typesetting text_red text_green text_blue text_alpha select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha delta_height extra_width extra_height left right up down text) this_widget=case this_widget of
    Editor _ _ _ _ _ _ _ _ _ _ _ _ text_color _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ intmap_intmap_texture _->case DIS.lookup window_id window of
        Nothing->error "alter_single_widget: no such window"
        Just (Window _ _ renderer _ _ x y design_size size)->let font=find_font find widget design_size size start_id font_size path in do
            DF.mapM_ (DF.mapM_ (\(texture,_,_,_,_)->CM.unless (texture==FP.nullPtr) (SRV.destroyTexture texture))) intmap_intmap_texture
            FS.poke text_color (color text_red text_green text_blue text_alpha)
            editor_text renderer window_id paragraph_id row_id paragraph_id row_id row font_size path font find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha x y design_size size delta_height extra_width extra_height left right up down text
    _->error "alter_single_widget: not a editor widget"

create_editor_trigger::DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->DS.Seq (Click,Mouse)->(Event->Engine a->Id)->DS.Seq Int->DS.Seq (DS.Seq Int)->Engine a->IO (Engine a)
create_editor_trigger left_press right_press up_press down_press min_press max_press text_up_press text_down_press text_min_press text_max_press cursor_min_press cursor_max_press backspace_press delete_press enter_press copy_press paste_press cut_press select_all_press start_click end_click next_id seq_id seq_seq_id=create_widget seq_id (Leaf_widget_request next_id (Io_trigger_request (create_editor_trigger_a left_press right_press up_press down_press min_press max_press text_up_press text_down_press text_min_press text_max_press cursor_min_press cursor_max_press backspace_press delete_press enter_press copy_press paste_press cut_press select_all_press start_click end_click seq_seq_id)))

create_editor_trigger_a::DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->DS.Seq (Click,Mouse)->DS.Seq (DS.Seq Int)->Event->Engine a->IO (Engine a)
create_editor_trigger_a left_press right_press up_press down_press min_press max_press text_up_press text_down_press text_min_press text_max_press cursor_min_press cursor_max_press backspace_press delete_press enter_press copy_press paste_press cut_press select_all_press start_click end_click seq_seq_id event (Engine widget window window_map request count_id start_id main_id)=do
    (new_widget,new_request)<-CM.foldM (\widget_request seq_id->create_editor_trigger_b left_press right_press up_press down_press min_press max_press text_up_press text_down_press text_min_press text_max_press cursor_min_press cursor_max_press backspace_press delete_press enter_press copy_press paste_press cut_press select_all_press start_click end_click seq_id start_id event window widget_request) (widget,request) seq_seq_id
    return (Engine new_widget window window_map new_request count_id start_id main_id)

create_editor_trigger_b::DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->DS.Seq (Click,Mouse)->DS.Seq Int->Int->Event->DIS.IntMap Window->(DIS.IntMap (DIS.IntMap (Combined_widget a)),DS.Seq (Request a))->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)),DS.Seq (Request a))
create_editor_trigger_b left_press right_press up_press down_press min_press max_press text_up_press text_down_press text_min_press text_max_press cursor_min_press cursor_max_press backspace_press delete_press enter_press copy_press paste_press cut_press select_all_press start_click end_click seq_id start_id event window (widget,request)=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in do
    (bool,new_widget)<-create_editor_trigger_c left_press right_press up_press down_press min_press max_press text_up_press text_down_press text_min_press text_max_press cursor_min_press cursor_max_press backspace_press delete_press enter_press copy_press paste_press cut_press select_all_press start_click end_click combined_id single_id start_id event window widget
    return (new_widget,if bool then request DS.|> Render_editor_widget seq_id else request)

create_editor_trigger_c::DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->DS.Seq (Click,Mouse)->Int->Int->Int->Event->DIS.IntMap Window->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Bool,DIS.IntMap (DIS.IntMap (Combined_widget a)))
create_editor_trigger_c left_press right_press up_press down_press min_press max_press text_up_press text_down_press text_min_press text_max_press cursor_min_press cursor_max_press backspace_press delete_press enter_press copy_press paste_press cut_press select_all_press start_click end_click combined_id single_id start_id event window widget=case DIS.lookup combined_id widget of
    Nothing->error "create_editor_trigger_c: no such combined_id"
    Just intmap_combined_widget->do
        (bool,new_intmap_combined_widget)<-create_editor_trigger_d left_press right_press up_press down_press min_press max_press text_up_press text_down_press text_min_press text_max_press cursor_min_press cursor_max_press backspace_press delete_press enter_press copy_press paste_press cut_press select_all_press start_click end_click single_id start_id event window widget intmap_combined_widget
        return (bool,DIS.insert combined_id new_intmap_combined_widget widget)

create_editor_trigger_d::DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->DS.Seq (Click,Mouse)->Int->Int->Event->DIS.IntMap Window->DIS.IntMap (DIS.IntMap (Combined_widget a))->DIS.IntMap (Combined_widget a)->IO (Bool,DIS.IntMap (Combined_widget a))
create_editor_trigger_d left_press right_press up_press down_press min_press max_press text_up_press text_down_press text_min_press text_max_press cursor_min_press cursor_max_press backspace_press delete_press enter_press copy_press paste_press cut_press select_all_press start_click end_click single_id start_id event window widget intmap_combined_widget=case DIS.lookup single_id intmap_combined_widget of
    Nothing->error "create_editor_trigger_d: no such single_id"
    Just combined_widget->do
        (new_widget,bool)<-create_editor_trigger_e left_press right_press up_press down_press min_press max_press text_up_press text_down_press text_min_press text_max_press cursor_min_press cursor_max_press backspace_press delete_press enter_press copy_press paste_press cut_press select_all_press start_click end_click start_id event window widget combined_widget
        return (bool,DIS.insert single_id new_widget intmap_combined_widget)

create_editor_trigger_e::DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->DS.Seq (Click,Mouse)->Int->Event->DIS.IntMap Window->DIS.IntMap (DIS.IntMap (Combined_widget a))->Combined_widget a->IO (Combined_widget a,Bool)
create_editor_trigger_e left_press right_press up_press down_press min_press max_press text_up_press text_down_press text_min_press text_max_press cursor_min_press cursor_max_press backspace_press delete_press enter_press copy_press paste_press cut_press select_all_press start_click end_click start_id event window widget this_widget@(Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down cursor intmap_text intmap_intmap_texture seq_map))=case cursor of
    Cursor_none->case event of
        At this_window_id action->if window_id==this_window_id
            then case action of
                Click click mouse x y->if belong (click,mouse) start_click&&left<=x+extra_width&&x<=right+extra_width&&up<=y+extra_height&&y<=down+extra_height
                    then case DIS.lookup window_id window of
                        Nothing->error "create_editor_trigger: no such window_id"
                        Just (Window _ _ _ _ _ _ _ design_window_size window_size)->let font=find_font find widget design_window_size window_size start_id size path in do
                            (new_cursor_number_row,new_cursor_paragraph_id,new_cursor_row_id,new_cursor_text_number,new_cursor_x_render,new_cursor_x_click)<-to_cursor intmap_text intmap_intmap_texture seq_map font row (DS.length seq_map) font_height delta_height left up x y
                            return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down (Cursor_single new_cursor_number_row new_cursor_paragraph_id new_cursor_row_id new_cursor_text_number new_cursor_x_render new_cursor_x_click) intmap_text intmap_intmap_texture seq_map),True)
                    else return (this_widget,False)
                _->return (this_widget,False)
            else return (this_widget,False)
        _->return (this_widget,False)
    Cursor_single cursor_number_row cursor_paragraph_id cursor_row_id cursor_text_number cursor_x_render cursor_x_click->case event of
        At this_window_id action->if window_id==this_window_id
            then case action of
                Press press key->if belong (press,key) left_press
                    then case DIS.lookup window_id window of
                        Nothing->error "create_editor_trigger_e: no such window_id"
                        Just (Window _ _ _ _ _ _ _ design_window_size window_size)->let font=find_font find widget design_window_size window_size start_id size path in do
                            maybe_cursor<-left_cursor row font intmap_text intmap_intmap_texture seq_map cursor
                            case maybe_cursor of
                                Nothing->return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down cursor intmap_text intmap_intmap_texture seq_map),False)
                                Just (new_cursor,new_row)->return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id new_row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down new_cursor intmap_text intmap_intmap_texture seq_map),True)
                    else if belong (press,key) right_press
                        then case DIS.lookup window_id window of
                            Nothing->error "create_editor_trigger_e: no such window_id"
                            Just (Window _ _ _ _ _ _ _ design_window_size window_size)->let font=find_font find widget design_window_size window_size start_id size path in do
                                maybe_cursor<-right_cursor row row_number font intmap_text intmap_intmap_texture seq_map cursor
                                case maybe_cursor of
                                    Nothing->return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down cursor intmap_text intmap_intmap_texture seq_map),False)
                                    Just (new_cursor,new_row)->return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id new_row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down new_cursor intmap_text intmap_intmap_texture seq_map),True)
                        else if belong (press,key) up_press
                            then case DIS.lookup window_id window of
                                Nothing->error "create_editor_trigger_e: no such window_id"
                                Just (Window _ _ _ _ _ _ _ design_window_size window_size)->let font=find_font find widget design_window_size window_size start_id size path in do
                                    maybe_cursor<-up_cursor row font intmap_text intmap_intmap_texture seq_map cursor
                                    case maybe_cursor of
                                        Nothing->return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down cursor intmap_text intmap_intmap_texture seq_map),False)
                                        Just (new_cursor,new_row)->return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id new_row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down new_cursor intmap_text intmap_intmap_texture seq_map),True)
                            else if belong (press,key) down_press
                                then case DIS.lookup window_id window of
                                    Nothing->error "create_editor_trigger_e: no such window_id"
                                    Just (Window _ _ _ _ _ _ _ design_window_size window_size)->let font=find_font find widget design_window_size window_size start_id size path in do
                                        maybe_cursor<-down_cursor row row_number font intmap_text intmap_intmap_texture seq_map cursor
                                        case maybe_cursor of
                                            Nothing->return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down cursor intmap_text intmap_intmap_texture seq_map),False)
                                            Just (new_cursor,new_row)->return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id new_row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down new_cursor intmap_text intmap_intmap_texture seq_map),True)
                                else if belong (press,key) min_press
                                    then case min_cursor cursor intmap_intmap_texture seq_map of
                                        Nothing->return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down cursor intmap_text intmap_intmap_texture seq_map),False)
                                        Just new_cursor->return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id 0 row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down new_cursor intmap_text intmap_intmap_texture seq_map),True)
                                    else if belong (press,key) max_press
                                        then case max_cursor cursor intmap_intmap_texture seq_map of
                                            Nothing->return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down cursor intmap_text intmap_intmap_texture seq_map),False)
                                            Just new_cursor->return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id (max 0 (max_row-row_number)) row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down new_cursor intmap_text intmap_intmap_texture seq_map),True)
                                        else if belong (press,key) text_up_press
                                            then if 0<row
                                                then return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id (row-1) row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down cursor intmap_text intmap_intmap_texture seq_map),True)
                                                else return (this_widget,False)
                                            else if belong (press,key) text_down_press
                                                then if row+row_number<max_row
                                                    then return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id (row+1) row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down cursor intmap_text intmap_intmap_texture seq_map),True)
                                                    else return (this_widget,False)
                                                else if belong (press,key) text_min_press
                                                    then if row==0
                                                        then return (this_widget,False)
                                                        else return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id 0 row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down cursor intmap_text intmap_intmap_texture seq_map),True)
                                                    else if belong (press,key) text_max_press
                                                        then let new_row=max 0 (max_row-row_number) in if row==new_row
                                                            then return (this_widget,False)
                                                            else return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id new_row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down cursor intmap_text intmap_intmap_texture seq_map),True)
                                                        else if belong (press,key) cursor_min_press
                                                            then if cursor_number_row==row
                                                                then return (this_widget,False)
                                                                else if cursor_number_row+row_number<max_row
                                                                    then return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id cursor_number_row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down cursor intmap_text intmap_intmap_texture seq_map),True)
                                                                    else return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id (max 0 (max_row-row_number)) row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down cursor intmap_text intmap_intmap_texture seq_map),True)
                                                            else if belong (press,key) cursor_max_press
                                                                then if cursor_number_row==row+row_number-1
                                                                    then return (this_widget,False)
                                                                    else if row_number<=cursor_number_row
                                                                        then return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id (cursor_number_row+1-row_number) row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down cursor intmap_text intmap_intmap_texture seq_map),True)
                                                                        else return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id 0 row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down cursor intmap_text intmap_intmap_texture seq_map),True)
                                                                else return (this_widget,False)
                Click click mouse x y->if belong (click,mouse) start_click
                    then if left<=x+extra_width&&x<=right+extra_width&&up<=y+extra_height&&y<=down+extra_height
                        then case DIS.lookup window_id window of
                            Nothing->error "create_editor_trigger: no such window_id"
                            Just (Window _ _ _ _ _ _ _ design_window_size window_size)->let font=find_font find widget design_window_size window_size start_id size path in do
                                (new_cursor_number_row,new_cursor_paragraph_id,new_cursor_row_id,new_cursor_text_number,new_cursor_x_render,new_cursor_x_click)<-to_cursor intmap_text intmap_intmap_texture seq_map font row (DS.length seq_map) font_height delta_height left up x y
                                if cursor_number_row==new_cursor_number_row then if cursor_x_click==new_cursor_x_click then return (this_widget,False) else if cursor_text_number==new_cursor_text_number then return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down (Cursor_single new_cursor_number_row new_cursor_paragraph_id new_cursor_row_id new_cursor_text_number new_cursor_x_render new_cursor_x_click) intmap_text intmap_intmap_texture seq_map),False) else return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down (Cursor_single new_cursor_number_row new_cursor_paragraph_id new_cursor_row_id new_cursor_text_number new_cursor_x_render new_cursor_x_click) intmap_text intmap_intmap_texture seq_map),True) else return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down (Cursor_single new_cursor_number_row new_cursor_paragraph_id new_cursor_row_id new_cursor_text_number new_cursor_x_render new_cursor_x_click) intmap_text intmap_intmap_texture seq_map),True)
                        else return (Leaf_widget next_id (Editor window_id origin_paragraph_id origin_row_id paragraph_id row_id row row_number max_row size path find typesetting text_color select_red select_green select_blue select_alpha cursor_red cursor_green cursor_blue cursor_alpha font_height design_delta_height design_extra_width design_extra_height design_left design_right design_up design_down delta_height extra_width extra_height left right up down Cursor_none intmap_text intmap_intmap_texture seq_map),True)
                    else return (this_widget,False)
                _->return (this_widget,False)
            else return (this_widget,False)
        _->return (this_widget,False)
    Cursor_double {}->error "unfinished"
create_editor_trigger_e _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _=error "not a editor widget"
