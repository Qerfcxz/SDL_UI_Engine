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
import qualified Data.Word as DW
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
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
create_single_widget _ _ (Block_font_request window_id red green blue alpha path size) _=do
    font<-DTF.withCString path (`create_block_font` size)
    return (Block_font window_id red green blue alpha font)
create_single_widget _ window (Rectangle_request window_id red green blue alpha left right up down) _=case DIS.lookup window_id window of
    Nothing->error "create_single_widget: error 1"
    Just (Window _ _ _ _ _ x y design_size size)->return (Rectangle window_id red green blue alpha left right up down (x+div (left*size) design_size) (y+div (up*size) design_size) (div ((right-left)*size) design_size) (div ((down-up)*size) design_size))
create_single_widget _ window (Picture_request window_id path x y width_multiply width_divide height_multiply height_divide) _=case DIS.lookup window_id window of
    Nothing->error "create_single_widget: error 2"
    Just (Window _ _ renderer _ _ window_x window_y design_size size)->do
        surface<-DTF.withCString path SRV.loadBMP
        CM.when (surface==FP.nullPtr) $ error "create_single_widget: error 3"
        (SRT.Surface _ width height _ _ _ _)<-FS.peek surface
        texture<-SRV.createTextureFromSurface renderer surface
        SRV.freeSurface surface
        CM.when (texture==FP.nullPtr) $ error "create_single_widget: error 4"
        let new_width=div (width*width_multiply) width_divide in let new_height=div (height*height_multiply) height_divide in return (Picture window_id texture x y width_multiply width_divide height_multiply height_divide width height (window_x+div ((x-div new_width 2)*size) design_size) (window_y+div ((y-div new_height 2)*size) design_size) (div (new_width*size) design_size) (div (new_height*size) design_size))
create_single_widget start_id window (Text_request window_id row find delta_height left right up down seq_paragraph) widget=case DIS.lookup window_id window of
    Nothing->error "create_single_widget: error 5"
    Just (Window _ _ renderer _ _ x y design_size size)->let new_delta_height=div (delta_height*size) design_size in do
        seq_row<-from_paragraph widget renderer (find_font find) window_id start_id design_size size 0 (div ((right-left)*size) design_size) new_delta_height seq_paragraph DS.empty
        let new_up=y+div (up*size) design_size in let new_down=y+div (down*size) design_size in let max_row=find_max seq_row new_up new_down in return (Text window_id (max 0 (min row max_row)) max_row False False find delta_height left right up down new_delta_height (x+div (left*size) design_size) (x+div (right*size) design_size) new_up new_down seq_paragraph seq_row)
create_single_widget start_id window (Editor_request window_id block_number font_size path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha block_width height delta_height x y extra_width extra_height seq_text) widget=let (window_x,window_y,design_size,size)=get_transform_window window_id window in let (this_window_id,new_font_size,_,font_height,intmap_texture)=find_block_font find widget design_size size path start_id font_size in if window_id==this_window_id
    then FMA.alloca $ \text_color->do
        FS.poke text_color (color text_red text_green text_blue text_alpha)
        let new_block_width=div (block_width*size) design_size
        let new_delta_height=div (delta_height*size) design_size
        let new_height=div ((height+delta_height)*size) design_size
        let half_width=div (fromIntegral block_number*new_block_width) 2
        let half_height=div (div (height*size) design_size) 2
        return (Editor window_id block_number (fromIntegral (div new_height (font_height+new_delta_height))) 0 font_size new_font_size False path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height block_width delta_height x y extra_width extra_height font_height new_block_width new_delta_height (window_x+div (x*size) design_size-div (fromIntegral block_number*new_block_width) 2) (window_y+div (y*size) design_size-div new_height 2) (window_x+div ((x-extra_width)*size) design_size-half_width) (window_x+div ((x+extra_width)*size) design_size+half_width) (window_y+div ((y-extra_height)*size) design_size-half_height) (window_y+div ((y+extra_height)*size) design_size+half_height) Cursor_none (from_seq_seq_char intmap_texture block_number new_block_width seq_text DS.empty))
    else error "create_single_widget: error 6"

create_font::FCS.CString->DS.Seq Int->IO (DIS.IntMap (FP.Ptr SRF.Font))
create_font _ DS.Empty=return DIS.empty
create_font path (size DS.:<| other_size)=do
    font<-create_font path other_size
    new_font<-SRF.openFont path (fromIntegral size)
    CM.when (new_font==FP.nullPtr) $ error "create_font: error 1"
    return (DIS.insert size new_font font)

create_block_font::FCS.CString->DS.Seq Int->IO (DIS.IntMap (FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt)))
create_block_font _ DS.Empty=return DIS.empty
create_block_font path (size DS.:<| other_size)=do
    font<-create_block_font path other_size
    new_font<-SRF.openFont path (fromIntegral size)
    CM.when (new_font==FP.nullPtr) $ error "create_block_font: error 1"
    ascent<-SRF.fontAscent new_font
    descent<-SRF.fontDescent new_font
    return (DIS.insert size (new_font,ascent-descent,DIS.empty) font)

create_widget::DS.Seq Int->Combined_widget_request a->Engine a->IO (Engine a)
create_widget seq_single_id combined_widget_request (Engine widget window window_map request count_id start_id main_id)=case seq_single_id of
    DS.Empty->error "create_widget: error 1"
    (single_id DS.:<| other_seq_single_id)->do
        (new_count_id,new_widget)<-create_widget_a other_seq_single_id count_id start_id single_id start_id window combined_widget_request widget
        return (Engine new_widget window window_map request new_count_id start_id main_id)

create_widget_a::DS.Seq Int->Int->Int->Int->Int->DIS.IntMap Window->Combined_widget_request a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))
create_widget_a seq_single_id count_id combined_id single_id start_id window combined_widget_request widget=case seq_single_id of
    DS.Empty->create_widget_top count_id combined_id single_id start_id window combined_widget_request widget
    (new_single_id DS.:<| other_seq_single_id)->case DIS.lookup combined_id widget of
        Nothing->error "create_widget_a: error 1"
        Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
            Nothing->error "create_widget_a: error 2"
            Just (Leaf_widget _ _)->error "create_widget_a: error 3"
            Just (Node_widget _ _ new_combined_id)->create_widget_a other_seq_single_id count_id new_combined_id new_single_id start_id window combined_widget_request widget

create_widget_top::Int->Int->Int->Int->DIS.IntMap Window->Combined_widget_request a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))
create_widget_top count_id combined_id single_id start_id window (Leaf_widget_request next_id single_widget_request) widget=do
    new_single_widget<-create_single_widget start_id window single_widget_request widget
    return (count_id,error_insert_insert "create_widget_top: error 1" "create_widget_top: error 2" combined_id single_id (Leaf_widget next_id new_single_widget) widget)
create_widget_top count_id combined_id single_id start_id window (Node_widget_request next_id main_single_id intmap_combined_widget_request) widget=DIS.foldlWithKey (\io this_single_id->create_widget_top_a count_id this_single_id start_id window io)  (return (count_id+1,error_insert "create_widget_top: you changed something without proper design" count_id DIS.empty (error_insert_insert "create_widget_top: error 3" "create_widget_top: error 4" combined_id single_id (Node_widget next_id main_single_id count_id) widget))) intmap_combined_widget_request

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
remove_single_widget (Block_font _ _ _ _ _ font)=do
    _<-DIS.traverseWithKey (\_ this_font->clean_block_font this_font) font
    return ()
remove_single_widget (Rectangle {})=return ()
remove_single_widget (Picture _ texture _ _ _ _ _ _ _ _ _ _ _ _)=SRV.destroyTexture texture
remove_single_widget (Text _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ seq_row)=DF.mapM_ clean_row seq_row
remove_single_widget (Editor {})=return ()

clean_block_font::(FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt))->IO ()
clean_block_font (font,_,intmap_texture)=do
    _<-DIS.traverseWithKey (\_ (texture,_,_)->SRV.destroyTexture texture) intmap_texture
    SRF.closeFont font

remove_widget::Data a=>DS.Seq Int->Engine a->IO (Engine a)
remove_widget seq_single_id (Engine widget window window_map request count_id start_id main_id)=case seq_single_id of
    DS.Empty->error "remove_widget: error 1"
    (single_id DS.:<| other_seq_single_id)->do
        (_,new_widget)<-remove_widget_a other_seq_single_id start_id start_id single_id widget
        return (Engine new_widget window window_map request count_id start_id main_id)

remove_widget_a::Data a=>DS.Seq Int->Int->Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Bool,DIS.IntMap (DIS.IntMap (Combined_widget a)))
remove_widget_a seq_single_id start_id combined_id single_id widget=case seq_single_id of
    DS.Empty->do
        (intmap_combined_widget,new_widget)<-remove_widget_top combined_id single_id widget
        if DIS.null intmap_combined_widget&&(start_id/=combined_id) then return (True,new_widget) else return (False,DIS.insert combined_id intmap_combined_widget new_widget)
    (new_single_id DS.:<| other_seq_single_id)->case DIS.lookup combined_id widget of
        Nothing->error "remove_widget_a: error 1"
        Just intmap_combined_widget->case DIS.updateLookupWithKey (\_ _->Nothing) single_id intmap_combined_widget of
            (Nothing,_)->error "remove_widget_a: error 2"
            (Just (Leaf_widget _ _),_)->error "remove_widget_a: error 3"
            (Just (Node_widget _ _ new_combined_id),new_intmap_combined_widget)->do
                (bool,new_widget)<-remove_widget_a other_seq_single_id start_id new_combined_id new_single_id widget
                if bool then if DIS.null new_intmap_combined_widget&&(start_id/=combined_id) then return (True,DIS.delete combined_id new_widget) else return (False,DIS.insert combined_id new_intmap_combined_widget new_widget) else return (False,new_widget)

remove_widget_top::Data a=>Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (Combined_widget a),DIS.IntMap (DIS.IntMap (Combined_widget a)))
remove_widget_top combined_id single_id widget=let (combined_widget,intmap_combined_widget,new_widget)=error_remove_remove "remove_widget_top: error 1" "remove_widget_top: error 2" combined_id single_id widget in case combined_widget of
    (Leaf_widget _ single_widget)->do
        remove_single_widget single_widget
        return (intmap_combined_widget,new_widget)
    (Node_widget _ _ new_combined_id)->let (new_intmap_combined_widget,new_new_widget)=error_remove "remove_widget_top: error 3" new_combined_id new_widget in do
        new_new_new_widget<-DIS.foldl remove_widget_top_a (return new_new_widget) new_intmap_combined_widget
        return (intmap_combined_widget,new_new_new_widget)

remove_widget_top_a::Data a=>IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))->Combined_widget a->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))
remove_widget_top_a io (Leaf_widget _ single_widget)=do
    widget<-io
    remove_single_widget single_widget
    return widget
remove_widget_top_a io (Node_widget _ _ combined_id)=do
    widget<-io
    let (intmap_combined_widget,new_widget)=error_remove "remove_widget_top_a: error 1" combined_id widget
    DIS.foldl remove_widget_top_a (return new_widget) intmap_combined_widget

replace_widget::Data a=>DS.Seq Int->Combined_widget_request a->Engine a->IO (Engine a)
replace_widget seq_single_id combined_widget_request (Engine widget window window_map request count_id start_id main_id)=case seq_single_id of
    DS.Empty->error "replace_widget: error 1"
    (single_id DS.:<| other_seq_single_id)->do
        (new_count_id,new_widget)<-replace_widget_a other_seq_single_id count_id start_id single_id start_id window combined_widget_request widget
        return (Engine new_widget window window_map request new_count_id start_id main_id)

replace_widget_a::Data a=>DS.Seq Int->Int->Int->Int->Int->DIS.IntMap Window->Combined_widget_request a->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (Int,DIS.IntMap (DIS.IntMap (Combined_widget a)))
replace_widget_a seq_single_id count_id combined_id single_id start_id window combined_widget_request widget=case seq_single_id of
    DS.Empty->do
        (intmap_combined_widget,new_widget)<-remove_widget_top combined_id single_id widget
        create_widget_top count_id combined_id single_id start_id window combined_widget_request (DIS.insert combined_id intmap_combined_widget new_widget)
    (new_single_id DS.:<| other_seq_single_id)->case DIS.lookup combined_id widget of
        Nothing->error "replace_widget_a: error 1"
        Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
            Nothing->error "replace_widget_a: error 2"
            Just (Leaf_widget _ _)->error "replace_widget_a: error 3"
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

create_window_trigger::(Event->Engine a->Id)->DS.Seq Int->DS.Seq Int->Engine a->IO (Engine a)
create_window_trigger next_id seq_id seq_window_id=create_widget seq_id (Leaf_widget_request next_id (Trigger_request (create_window_trigger_a seq_window_id)))

create_window_trigger_a::DS.Seq Int->Event->Engine a->Engine a
create_window_trigger_a seq_window_id event engine@(Engine widget window window_map request count_id start_id main_id)=case event of
    Resize window_id width height->case DS.elemIndexL window_id seq_window_id of
        Nothing->engine
        Just _->Engine widget (error_update "create_window_trigger_a: error 1" window_id (\(Window this_window_id this_window renderer design_width design_height _ _ _ _)->let (x,y,design_size,size)=adaptive_window design_width design_height width height in Window this_window_id this_window renderer design_width design_height x y design_size size) window) window_map request count_id start_id main_id
    _->engine

create_text_trigger::Bool->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->(Event->Engine a->Id)->DS.Seq Int->DS.Seq (DS.Seq Int)->Engine a->IO (Engine a)
create_text_trigger wheel up_press down_press min_press max_press down_click next_id seq_id seq_seq_id=create_widget seq_id (Leaf_widget_request next_id (Trigger_request (\event (Engine widget window window_map request count_id start_id main_id)->let new_widget=DF.foldl' (\this_widget this_seq_id->create_text_trigger_a wheel up_press down_press min_press max_press down_click this_seq_id start_id event this_widget) widget seq_seq_id in Engine new_widget window window_map request count_id start_id main_id)))

create_text_trigger_a::Bool->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->DS.Seq Int->Int->Event->DIS.IntMap (DIS.IntMap (Combined_widget a))->DIS.IntMap (DIS.IntMap (Combined_widget a))
create_text_trigger_a wheel up_press down_press min_press max_press down_click seq_id start_id event widget=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in case DIS.lookup combined_id widget of
    Nothing->error "create_text_trigger_a: error 1"
    Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
        Nothing->error "create_text_trigger_a: error 2"
        Just combined_widget->case create_text_trigger_b wheel up_press down_press min_press max_press down_click event combined_widget of
            Nothing->widget
            Just new_combined_widget->DIS.insert combined_id (DIS.insert single_id new_combined_widget intmap_combined_widget) widget

create_text_trigger_b::Bool->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->Event->Combined_widget a->Maybe (Combined_widget a)
create_text_trigger_b wheel up_press down_press min_press max_press down_click event (Leaf_widget next_id (Text window_id row max_row render select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row))=if select
    then case event of
        At this_window_id action->if window_id==this_window_id
            then case action of
                Wheel delta_y->if wheel then let new_row=max 0 (min max_row (row-delta_y)) in if row==new_row then Nothing else Just (Leaf_widget next_id (Text window_id new_row max_row True select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row)) else Nothing
                Press press key->if belong (press,key) up_press then if 0<row then Just (Leaf_widget next_id (Text window_id (row-1) max_row True select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row)) else Nothing else if belong (press,key) down_press then if row<max_row then Just (Leaf_widget next_id (Text window_id (row+1) max_row True select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row)) else Nothing else if belong (press,key) min_press then if row==0 then Nothing else Just (Leaf_widget next_id (Text window_id 0 max_row True select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row)) else if belong (press,key) max_press then if row==max_row then Nothing else Just (Leaf_widget next_id (Text window_id max_row max_row True select find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row)) else Nothing
                Click click mouse x y->if belong (click,mouse) down_click&&(x<left||right<x||y<up||down<y) then Just (Leaf_widget next_id (Text window_id row max_row render False find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row)) else Nothing
                _->Nothing
            else Nothing
        _->Nothing
    else case event of
        At this_window_id action->if window_id==this_window_id
            then case action of
                Click click mouse x y->if belong (click,mouse) down_click&&left<=x&&x<=right&&up<=y&&y<=down then Just (Leaf_widget next_id (Text window_id row max_row render True find design_delta_height design_left design_right design_up design_down delta_height left right up down seq_paragraph seq_row)) else Nothing
                _->Nothing
            else Nothing
        _->Nothing
create_text_trigger_b _ _ _ _ _ _ _ _=error "create_text_trigger_b: error 1"

create_editor_trigger::Bool->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->DS.Seq (Click,Mouse)->(Event->Engine a->Id)->DS.Seq Int->DS.Seq (DS.Seq Int)->Engine a->IO (Engine a)
create_editor_trigger wheel up_press down_press min_press max_press min_trace_press max_trace_press cursor_left_press cursor_right_press cursor_up_press cursor_down_press cursor_min_press cursor_max_press cursor_row_min_press cursor_row_max_press cursor_paragraph_min_press cursor_paragraph_max_press copy_press paste_press cut_press select_all_press select_row_all_press select_swap_press select_left_press select_right_press select_up_press select_down_press backspace_press delete_press return_press exit_press up_click down_click next_id seq_id seq_seq_id=create_widget seq_id (Leaf_widget_request next_id (Io_trigger_request (create_editor_trigger_a wheel up_press down_press min_press max_press min_trace_press max_trace_press cursor_left_press cursor_right_press cursor_up_press cursor_down_press cursor_min_press cursor_max_press cursor_row_min_press cursor_row_max_press cursor_paragraph_min_press cursor_paragraph_max_press copy_press paste_press cut_press select_all_press select_row_all_press select_swap_press select_left_press select_right_press select_up_press select_down_press backspace_press delete_press return_press exit_press up_click down_click seq_seq_id)))

create_editor_trigger_a::Bool->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->DS.Seq (Click,Mouse)->DS.Seq (DS.Seq Int)->Event->Engine a->IO (Engine a)
create_editor_trigger_a wheel up_press down_press min_press max_press min_trace_press max_trace_press cursor_left_press cursor_right_press cursor_up_press cursor_down_press cursor_min_press cursor_max_press cursor_row_min_press cursor_row_max_press cursor_paragraph_min_press cursor_paragraph_max_press copy_press paste_press cut_press select_all_press select_row_all_press select_swap_press select_left_press select_right_press select_up_press select_down_press backspace_press delete_press return_press exit_press up_click down_click seq_seq_id event (Engine widget window window_map request count_id start_id main_id)=do
    new_widget<-DF.foldlM (\this_widget this_seq_id->create_editor_trigger_b wheel up_press down_press min_press max_press min_trace_press max_trace_press cursor_left_press cursor_right_press cursor_up_press cursor_down_press cursor_min_press cursor_max_press cursor_row_min_press cursor_row_max_press cursor_paragraph_min_press cursor_paragraph_max_press copy_press paste_press cut_press select_all_press select_row_all_press select_swap_press select_left_press select_right_press select_up_press select_down_press backspace_press delete_press return_press exit_press up_click down_click this_seq_id start_id event this_widget) widget seq_seq_id
    return (Engine new_widget window window_map request count_id start_id main_id)

create_editor_trigger_b::Bool->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->DS.Seq (Click,Mouse)->DS.Seq Int->Int->Event->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))
create_editor_trigger_b wheel up_press down_press min_press max_press min_trace_press max_trace_press cursor_left_press cursor_right_press cursor_up_press cursor_down_press cursor_min_press cursor_max_press cursor_row_min_press cursor_row_max_press cursor_paragraph_min_press cursor_paragraph_max_press copy_press paste_press cut_press select_all_press select_row_all_press select_swap_press select_left_press select_right_press select_up_press select_down_press backspace_press delete_press return_press exit_press up_click down_click seq_id start_id event widget=let (combined_id,single_id)=get_widget_id_widget seq_id start_id widget in case DIS.lookup combined_id widget of
    Nothing->error "create_editor_trigger_b: error 1"
    Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
        Nothing->error "create_editor_trigger_b: error 2"
        Just combined_widget->case combined_widget of
            Leaf_widget _ (Editor _ _ _ _ _ font_size _ path _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)->case get_widget_widget path start_id widget of
                Leaf_widget _ (Block_font _ _ _ _ _ font)->case DIS.lookup font_size font of
                    Nothing->error "create_editor_trigger_b: error 3"
                    Just (this_font,_,intmap_texture)->do
                        (maybe_intmap_texture,maybe_new_combined_widget)<-create_editor_trigger_d wheel up_press down_press min_press max_press min_trace_press max_trace_press cursor_left_press cursor_right_press cursor_up_press cursor_down_press cursor_min_press cursor_max_press cursor_row_min_press cursor_row_max_press cursor_paragraph_min_press cursor_paragraph_max_press copy_press paste_press cut_press select_all_press select_row_all_press select_swap_press select_left_press select_right_press select_up_press select_down_press backspace_press delete_press return_press exit_press up_click down_click event this_font intmap_texture combined_widget
                        case maybe_intmap_texture of
                            Nothing->case maybe_new_combined_widget of
                                Nothing->return widget
                                Just new_combined_widget->return (error_replace_replace "create_editor_trigger_b: error 4" "create_editor_trigger_b: error 5" combined_id single_id new_combined_widget widget)
                            Just new_intmap_texture->case maybe_new_combined_widget of
                                Nothing->update_combined_widget start_id path (create_editor_trigger_c font_size new_intmap_texture) widget
                                Just new_combined_widget->do
                                    new_widget<-update_combined_widget start_id path (create_editor_trigger_c font_size new_intmap_texture) widget
                                    return (error_replace_replace "create_editor_trigger_b: error 6" "create_editor_trigger_b: error 7" combined_id single_id new_combined_widget new_widget)
                _->error "create_editor_trigger_b: error 8"
            _->error "create_editor_trigger_b: error 9"

create_editor_trigger_c::Int->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt)->Combined_widget a->IO (Combined_widget a)
create_editor_trigger_c size intmap_texture (Leaf_widget next_id (Block_font window_id red green blue alpha font))=return (Leaf_widget next_id (Block_font window_id red green blue alpha (DIS.adjust (\(this_font,height,_)->(this_font,height,intmap_texture)) size font)))
create_editor_trigger_c _ _ _=error "create_editor_trigger_c: error 1"

create_editor_trigger_d::Bool->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Press,Key)->DS.Seq (Click,Mouse)->DS.Seq (Click,Mouse)->Event->FP.Ptr SRF.Font->DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt)->Combined_widget a->IO (Maybe (DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt)),Maybe (Combined_widget a))
create_editor_trigger_d wheel up_press down_press min_press max_press min_trace_press max_trace_press cursor_left_press cursor_right_press cursor_up_press cursor_down_press cursor_min_press cursor_max_press cursor_row_min_press cursor_row_max_press cursor_paragraph_min_press cursor_paragraph_max_press copy_press paste_press cut_press select_all_press select_row_all_press select_swap_press select_left_press select_right_press select_up_press select_down_press backspace_press delete_press return_press exit_press up_click down_click event font intmap_texture (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size render path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down cursor seq_seq_char))=case event of
    At this_window_id action->if window_id==this_window_id
        then case action of
            Wheel delta_y->if wheel
                then case cursor of
                    Cursor_none->return (Nothing,Nothing)
                    _->let new_row=max 0 (min (DS.length seq_seq_char-row_number) (row-delta_y)) in if row==new_row then return (Nothing,Nothing) else return (Nothing,Just (Leaf_widget next_id (Editor window_id block_number row_number new_row design_font_size font_size True path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down cursor seq_seq_char)))
                else return (Nothing,Nothing)
            Press press key->if belong (press,key) up_press
                then if 0<row then return (Nothing,Just (Leaf_widget next_id (Editor window_id block_number row_number (row-1) design_font_size font_size True path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down cursor seq_seq_char))) else return (Nothing,Nothing)
                else if belong (press,key) down_press
                    then if row<DS.length seq_seq_char-row_number then return (Nothing,Just (Leaf_widget next_id (Editor window_id block_number row_number (row+1) design_font_size font_size True path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down cursor seq_seq_char))) else return (Nothing,Nothing)
                    else if belong (press,key) min_press
                        then if row==0 then return (Nothing,Nothing) else return (Nothing,Just (Leaf_widget next_id (Editor window_id block_number row_number 0 design_font_size font_size True path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down cursor seq_seq_char)))
                        else if belong (press,key) max_press
                            then let new_row=DS.length seq_seq_char-row_number in if row==new_row then return (Nothing,Nothing) else return (Nothing,Just (Leaf_widget next_id (Editor window_id block_number row_number new_row design_font_size font_size True path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down cursor seq_seq_char)))
                            else if belong (press,key) min_trace_press
                                then case get_cursor_row cursor of
                                    Nothing->return (Nothing,Nothing)
                                    Just cursor_row->let new_cursor_row=min (DS.length seq_seq_char-row_number) cursor_row in if new_cursor_row==row then return (Nothing,Nothing) else return (Nothing,Just (Leaf_widget next_id (Editor window_id block_number row_number new_cursor_row design_font_size font_size True path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down cursor seq_seq_char)))
                                else if belong (press,key) max_trace_press
                                    then case get_cursor_row cursor of
                                        Nothing->return (Nothing,Nothing)
                                        Just cursor_row->let new_cursor_row=max 0 (cursor_row+1-row_number) in if new_cursor_row==row then return (Nothing,Nothing) else return (Nothing,Just (Leaf_widget next_id (Editor window_id block_number row_number new_cursor_row design_font_size font_size True path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down cursor seq_seq_char)))
                                    else if belong (press,key) cursor_left_press
                                        then case cursor_left block_number typesetting block_width seq_seq_char cursor of
                                            Nothing->return (Nothing,Nothing)
                                            Just (new_cursor_row,maybe_cursor)->return (Nothing,Just (Leaf_widget next_id (Editor window_id block_number row_number (if row<=new_cursor_row&&new_cursor_row<row+row_number then row else new_cursor_row) design_font_size font_size True path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down (maybe_handle maybe_cursor id cursor) seq_seq_char)))
                                        else if belong (press,key) cursor_right_press
                                            then case cursor_right (DS.length seq_seq_char-1) block_number typesetting block_width seq_seq_char cursor of
                                                Nothing->return (Nothing,Nothing)
                                                Just (new_cursor_row,maybe_cursor)->return (Nothing,Just (Leaf_widget next_id (Editor window_id block_number row_number (if row<=new_cursor_row&&new_cursor_row<row+row_number then row else max 0 (new_cursor_row+1-row_number)) design_font_size font_size True path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down (maybe_handle maybe_cursor id cursor) seq_seq_char)))
                                            else return (Nothing,Nothing)
            Click click mouse click_x click_y->case cursor of
                Cursor_none->if belong (click,mouse) down_click&&left<=click_x&&click_x<=right&&up<=click_y&&click_y<=down then let (cursor_row,cursor_block,cursor_char,cursor_x)=to_cursor block_number (min row_number (DS.length seq_seq_char-row)) row typesetting click_x click_y font_height block_width delta_height x y intmap_texture seq_seq_char in return (Nothing,Just (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size True path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down (Cursor_single cursor_row cursor_block cursor_char cursor_x) seq_seq_char))) else return (Nothing,Nothing)
                Cursor_single cursor_row cursor_block cursor_char cursor_x->if belong (click,mouse) up_click then if left<=click_x&&click_x<=right&&up<=click_y&&click_y<=down then let (new_cursor_row,new_cursor_block,new_cursor_char,new_cursor_x)=to_cursor block_number (min row_number (DS.length seq_seq_char-row)) row typesetting click_x click_y font_height block_width delta_height x y intmap_texture seq_seq_char in if cursor_row==new_cursor_row&&cursor_block==new_cursor_block then return (Nothing,Just (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size render path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down cursor seq_seq_char))) else if (cursor_row,cursor_block)<(new_cursor_row,new_cursor_block) then return (Nothing,Just (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size True path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down (Cursor_double False cursor_row cursor_block cursor_char new_cursor_row new_cursor_block new_cursor_char cursor_x new_cursor_x) seq_seq_char))) else return (Nothing,Just (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size True path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down (Cursor_double True new_cursor_row new_cursor_block new_cursor_char cursor_row cursor_block cursor_char new_cursor_x cursor_x) seq_seq_char))) else return (Nothing,Nothing) else if belong (click,mouse) down_click then if left<=click_x&&click_x<=right&&up<=click_y&&click_y<=down then let (new_cursor_row,new_cursor_block,new_cursor_char,new_cursor_x)=to_cursor block_number (min row_number (DS.length seq_seq_char-row)) row typesetting click_x click_y font_height block_width delta_height x y intmap_texture seq_seq_char in return (Nothing,Just (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size (not (cursor_row==new_cursor_row&&cursor_block==new_cursor_block)) path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down (Cursor_single new_cursor_row new_cursor_block new_cursor_char new_cursor_x) seq_seq_char))) else return (Nothing,Just (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size True path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down Cursor_none seq_seq_char))) else return (Nothing,Nothing)
                Cursor_double {}->if belong (click,mouse) down_click then if left<=click_x&&click_x<=right&&up<=click_y&&click_y<=down then let (cursor_row,cursor_block,cursor_char,cursor_x)=to_cursor block_number (min row_number (DS.length seq_seq_char-row)) row typesetting click_x click_y font_height block_width delta_height x y intmap_texture seq_seq_char in return (Nothing,Just (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size True path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down (Cursor_single cursor_row cursor_block cursor_char cursor_x) seq_seq_char))) else return (Nothing,Just (Leaf_widget next_id (Editor window_id block_number row_number row design_font_size font_size True path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height design_block_width design_delta_height design_x design_y design_extra_width design_extra_height font_height block_width delta_height x y left right up down Cursor_none seq_seq_char))) else return (Nothing,Nothing)
            _->return (Nothing,Nothing)
        else return (Nothing,Nothing)
    _->return (Nothing,Nothing)
create_editor_trigger_d _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _=error "create_editor_trigger_d: error 1"

alter_widget::Data a=>DS.Seq Int->Combined_widget_request a->Engine a->IO (Engine a)
alter_widget seq_single_id combined_widget_request (Engine widget window window_map request count_id start_id main_id)=let (combined_id,single_id)=get_widget_id_widget seq_single_id start_id widget in do
    new_widget<-error_update_update_io "alter_widget: error 1" "alter_widget: error 2" combined_id single_id (alter_widget_a start_id window widget combined_widget_request) widget
    return (Engine new_widget window window_map request count_id start_id main_id)

alter_widget_a::Data a=>Int->DIS.IntMap Window->DIS.IntMap (DIS.IntMap (Combined_widget a))->Combined_widget_request a->Combined_widget a->IO (Combined_widget a)
alter_widget_a start_id window widget (Leaf_widget_request next_id single_widget_request) (Leaf_widget _ single_widget)=do
    new_single_widget<-alter_single_widget start_id window widget single_widget_request single_widget
    return (Leaf_widget next_id new_single_widget)
alter_widget_a _ _ _ _ _=error"alter_widget_a: error 1"--未完待续

alter_single_widget::Data a=>Int->DIS.IntMap Window->DIS.IntMap (DIS.IntMap (Combined_widget a))->Single_widget_request a->Single_widget a->IO (Single_widget a)
alter_single_widget _ _ _ (Data_request content) this_widget=case this_widget of
    Data this_content->do
        clean_data this_content
        return (Data content)
    _->error "alter_single_widget: error 1"
alter_single_widget _ _ _ (Trigger_request handle) this_widget=case this_widget of
    Trigger _->return (Trigger handle)
    _->error "alter_single_widget: error 2"
alter_single_widget _ _ _ (Io_trigger_request handle) this_widget=case this_widget of
    Io_trigger _->return (Io_trigger handle)
    _->error "alter_single_widget: error 3"
alter_single_widget _ _ _ (Font_request path size) this_widget=case this_widget of
    Font intmap_font->do
        _<-DIS.traverseWithKey (\_ font->SRF.closeFont font) intmap_font
        font<-DTF.withCString path (`create_font` size)
        return (Font font)
    _->error "alter_single_widget: error 4"
alter_single_widget _ _ _ (Block_font_request window_id red green blue alpha path size) this_widget=case this_widget of
    Block_font _ _ _ _ _ font->do
        _<-DIS.traverseWithKey (\_ this_font->clean_block_font this_font) font
        new_font<-DTF.withCString path (`create_block_font` size)
        return (Block_font window_id red green blue alpha new_font)
    _->error "alter_single_widget: error 5"
alter_single_widget _ window _ (Rectangle_request window_id red green blue alpha left right up down) this_widget=case this_widget of
    Rectangle {}->case DIS.lookup window_id window of
        Nothing->error "alter_single_widget: error 6"
        Just (Window _ _ _ _ _ x y design_size size)->return (Rectangle window_id red green blue alpha left right up down (x+div (left*size) design_size) (y+div (up*size) design_size) (div ((right-left)*size) design_size) (div ((down-up)*size) design_size))
    _->error "alter_single_widget: error 7"
alter_single_widget _ window _ (Picture_request window_id path x y width_multiply width_divide height_multiply height_divide) this_widget=case this_widget of
    Picture _ texture _ _ _ _ _ _ _ _ _ _ _ _->case DIS.lookup window_id window of
        Nothing->error "alter_single_widget: error 8"
        Just (Window _ _ renderer _ _ window_x window_y design_size size)->do
            SRV.destroyTexture texture
            surface<-DTF.withCString path SRV.loadBMP
            CM.when (surface==FP.nullPtr) $ error "alter_single_widget: error 9"
            (SRT.Surface _ width height _ _ _ _)<-FS.peek surface
            new_texture<-SRV.createTextureFromSurface renderer surface
            SRV.freeSurface surface
            CM.when (new_texture==FP.nullPtr) $ error "alter_single_widget: error 10"
            let new_width=div (width*width_multiply) width_divide in let new_height=div (height*height_multiply) height_divide in return (Picture window_id new_texture x y width_multiply width_divide height_multiply height_divide width height (window_x+div ((x-div new_width 2)*size) design_size) (window_y+div ((y-div new_height 2)*size) design_size) (div (new_width*size) design_size) (div (new_height*size) design_size))
    _->error "alter_single_widget: error 11"
alter_single_widget start_id window widget (Text_request window_id row find delta_height left right up down seq_paragraph) this_widget=case this_widget of
    Text _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ seq_row->case DIS.lookup window_id window of
        Nothing->error "alter_single_widget: error 12"
        Just (Window _ _ renderer _ _ x y design_size size)->do
            DF.mapM_ clean_row seq_row
            let new_delta_height=div (delta_height*size) design_size
            new_seq_row<-from_paragraph widget renderer (find_font find) window_id start_id design_size size 0 (div ((right-left)*size) design_size) new_delta_height seq_paragraph DS.empty
            let new_up=y+div (up*size) design_size in let new_down=y+div (down*size) design_size in let max_row=find_max new_seq_row new_up new_down in return (Text window_id (max 0 (min max_row row)) max_row False False find delta_height left right up down new_delta_height (x+div (left*size) design_size) (x+div (right*size) design_size) new_up new_down seq_paragraph new_seq_row)
    _->error "alter_single_widget: error 13"
alter_single_widget start_id window widget (Editor_request window_id block_number font_size path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha block_width height delta_height x y extra_width extra_height seq_text) this_widget=case this_widget of
    Editor {}->let (window_x,window_y,design_size,size)=get_transform_window window_id window in let (this_window_id,new_font_size,_,font_height,intmap_texture)=find_block_font find widget design_size size path start_id font_size in if window_id==this_window_id
        then FMA.alloca $ \text_color->do
            FS.poke text_color (color text_red text_green text_blue text_alpha)
            let new_block_width=div (block_width*size) design_size
            let new_delta_height=div (delta_height*size) design_size
            let new_height=div ((height+delta_height)*size) design_size
            let half_width=div (fromIntegral block_number*new_block_width) 2
            let half_height=div (div (height*size) design_size) 2
            return (Editor window_id block_number (fromIntegral (div new_height (font_height+new_delta_height))) 0 font_size new_font_size False path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height block_width delta_height x y extra_width extra_height font_height new_block_width new_delta_height (window_x+div (x*size) design_size-div (fromIntegral block_number*new_block_width) 2) (window_y+div (y*size) design_size-div new_height 2) (window_x+div ((x-extra_width)*size) design_size-half_width) (window_x+div ((x+extra_width)*size) design_size+half_width) (window_y+div ((y-extra_height)*size) design_size-half_height) (window_y+div ((y+extra_height)*size) design_size+half_height) Cursor_none (from_seq_seq_char intmap_texture block_number new_block_width seq_text DS.empty))
        else error "alter_single_widget: error 14"
    _->error "alter_single_widget: error 15"