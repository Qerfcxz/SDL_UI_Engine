{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Other where
import Type
import qualified Control.Monad as CM
import qualified Data.ByteString as DB
import qualified Data.Foldable as DF
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified Foreign.Marshal.Alloc as FMA
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Video as SRV

belong::Eq a=>a->DS.Seq a->Bool
belong value seq_value=case DS.elemIndexL value seq_value of
    Nothing->False
    Just _->True

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

alter_io::[Char]->Int->a->(a->IO ())->DIS.IntMap a->IO (DIS.IntMap a)
alter_io error_message key value act=DIS.alterF (alter_io_a error_message value act) key

alter_io_a::[Char]->a->(a->IO ())->Maybe a->IO (Maybe a)
alter_io_a error_message _ _ Nothing=error error_message
alter_io_a _ new_value act (Just value)=do
    act value
    return (Just new_value)

delete_io::[Char]->Int->(a->IO ())->DIS.IntMap a->IO (DIS.IntMap a)
delete_io error_message key act=DIS.alterF (delete_io_a error_message act) key

delete_io_a::[Char]->(a->IO ())->Maybe a->IO (Maybe a)
delete_io_a error_message _ Nothing=error error_message
delete_io_a _ act (Just value)=do
    act value
    return Nothing

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

get_renderer::Int->Engine a->SRT.Renderer
get_renderer window_id (Engine _ window _ _ _ _ _)=case DIS.lookup window_id window of
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

get_next_id::Combined_widget a->(Event->Engine a->Id)
get_next_id (Leaf_widget next_single_id _)=next_single_id
get_next_id (Node_widget next_single_id _ _)=next_single_id

get_font::DS.Seq Int->Engine a->DIS.IntMap (FP.Ptr SRF.Font)
get_font seq_single_id engine=case get_widget seq_single_id engine of
    Leaf_widget _ (Font font)->font
    _->error "get_font: error 1"

get_widget_widget::DS.Seq Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->Combined_widget a
get_widget_widget seq_single_id start_id widget=case seq_single_id of
    DS.Empty->error "get_widget_widget: error 1"
    single_id DS.:<| other_seq_single_id->get_widget_a start_id single_id other_seq_single_id widget

get_widget::DS.Seq Int->Engine a->Combined_widget a
get_widget seq_single_id (Engine widget _ _ _ _ start_id _)=case seq_single_id of
    DS.Empty->error "get_widget: error 1"
    (single_id DS.:<| other_seq_single_id)->get_widget_a start_id single_id other_seq_single_id widget

get_widget_a::Int->Int->DS.Seq Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->Combined_widget a
get_widget_a combined_id single_id seq_single_id widget=case DIS.lookup combined_id widget of
    Nothing->error "get_widget_a: error 1"
    Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
        Nothing->error "get_widget_a: error 2"
        Just combined_widget->case seq_single_id of
            DS.Empty->combined_widget
            (new_single_id DS.:<| other_seq_single_id)->case combined_widget of
                Leaf_widget _ _->error "get_widget_a: error 3"
                Node_widget _ _ new_combined_id->get_widget_a new_combined_id new_single_id other_seq_single_id widget

get_widget_id_widget::DS.Seq Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->(Int,Int)
get_widget_id_widget seq_single_id start_id widget=case seq_single_id of
    DS.Empty->error "get_widget_id_widget: error 1"
    single_id DS.:<| other_seq_single_id->get_widget_id_widget_a other_seq_single_id start_id single_id widget

get_widget_id_widget_a::DS.Seq Int->Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->(Int,Int)
get_widget_id_widget_a seq_single_id combined_id single_id widget=case seq_single_id of
    DS.Empty->(combined_id,single_id)
    (new_single_id DS.:<| other_seq_single_id)->case DIS.lookup combined_id widget of
        Nothing->error "get_widget_id_widget_a: error 1"
        Just intmap_combined_widget->case DIS.lookup single_id intmap_combined_widget of
            Nothing->error "get_widget_id_widget_a: error 2"
            Just (Leaf_widget _ _)->error "get_widget_id_widget_a: error 3"
            Just (Node_widget _ _ new_combined_id)->get_widget_id_widget_a other_seq_single_id new_combined_id new_single_id widget

clean_row::Row->IO ()
clean_row (Row seq_texture _ _)=DF.mapM_ (\(texture,_,_,_,_)->SRV.destroyTexture texture) seq_texture
clean_row (Row_blank _ _)=return ()

adaptive_window::FCT.CInt->FCT.CInt->FCT.CInt->FCT.CInt->(FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)
adaptive_window design_x design_y x y=let new_x=design_y*x in let new_y=design_x*y in if new_x<new_y then let common=gcd design_x x in (0,div (new_y-new_x) (2*design_x),div design_x common,div x common) else let common=gcd design_y y in (div (new_x-new_y) (2*design_y),0,div design_y common,div y common)

color::DW.Word8->DW.Word8->DW.Word8->DW.Word8->Color
color=SRT.Color

get_width::FP.Ptr SRF.Font->DT.Text->IO FCT.CInt
get_width font text=FMA.alloca $ \width->FMA.alloca $ \height->DB.useAsCString (DTE.encodeUtf8 text) $ \new_text->do
    catch_error "get_width: error 1" 0 (SRF.sizeUTF8 font new_text width height)
    FS.peek width

cut_text::FCT.CInt->FP.Ptr SRF.Font->DT.Text->IO (Int,Int,FCT.CInt,DT.Text,DT.Text)
cut_text width font text=let text_length=DT.length text in do
    (left_length,last_width,left_text,right_text)<-cut_text_a 0 text_length width width font text
    return (left_length,text_length-left_length,last_width,left_text,right_text)

cut_text_a::Int->Int->FCT.CInt->FCT.CInt->FP.Ptr SRF.Font->DT.Text->IO (Int,FCT.CInt,DT.Text,DT.Text)
cut_text_a left right last_width width font text=if left==right then let (left_text,right_text)=DT.splitAt left text in return (left,last_width,left_text,right_text) else let middle=div (left+right+1) 2 in let left_text=DT.take middle text in do
    new_width<-get_width font left_text
    let new_new_width=width-new_width in if new_new_width==0 then return (middle,0,left_text,DT.drop middle text) else if 0<new_new_width then cut_text_a middle right new_new_width width font text else cut_text_a left (middle-1) last_width width font text

to_texture::SRT.Renderer->FP.Ptr Color->FP.Ptr SRF.Font->FCS.CString->IO SRT.Texture
to_texture renderer text_color font text=do
    surface<-SRF.renderUTF8_Blended font text text_color
    CM.when (surface==FP.nullPtr) $ error "to_texture: error 1"
    texture<-SRV.createTextureFromSurface renderer surface
    SRV.freeSurface surface
    CM.when (texture==FP.nullPtr) $ error "to_texture: error 2"
    return texture

to_texture_with_width::SRT.Renderer->FP.Ptr Color->FP.Ptr SRF.Font->FCS.CString->IO (SRT.Texture,FCT.CInt)
to_texture_with_width renderer text_color font text=do
    surface<-SRF.renderUTF8_Blended font text text_color
    CM.when (surface==FP.nullPtr) $ error "to_texture_with_width: error 1"
    this_surface<-FS.peek surface
    let width=SRT.surfaceW this_surface
    texture<-SRV.createTextureFromSurface renderer surface
    SRV.freeSurface surface
    CM.when (texture==FP.nullPtr) $ error "to_texture_with_width: error 2"
    return (texture,width)

check_render::DS.Seq Int->Engine a->Bool
check_render seq_id engnie=case get_widget seq_id engnie of
    Leaf_widget _ (Text _ _ _ render _ _ _ _ _ _ _ _ _ _ _ _ _ _)->render
    Leaf_widget _ (Editor _ _ _ _ _ _ render _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)->render
    _->error "check_render: error 1"