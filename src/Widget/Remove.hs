{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Widget.Remove where
import Other.Error
import Type
import qualified Data.Foldable as DF
import qualified Data.IntMap.Strict as DIS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

remove_single_widget::Data a=>Single_widget a->IO ()
remove_single_widget (Label_data {})=return ()
remove_single_widget (Bool_data {})=return ()
remove_single_widget (Int_data {})=return ()
remove_single_widget (Char_data {})=return ()
remove_single_widget (List_char_data {})=return ()
remove_single_widget (Data content)=clean_data content
remove_single_widget (Trigger {})=return ()
remove_single_widget (Io_trigger {})=return ()
remove_single_widget (Collector {})=return ()
remove_single_widget (Font intmap_font)=do
    _<-DIS.traverseWithKey (\_ font->SRF.closeFont font) intmap_font
    return ()
remove_single_widget (Block_font _ _ _ _ _ font)=do
    _<-DIS.traverseWithKey (\_ this_font->clean_block_font this_font) font
    return ()
remove_single_widget (Rectangle {})=return ()
remove_single_widget (Picture _ texture _ _ _ _ _ _ _ _ _ _ _ _ _ _)=SRV.destroyTexture texture
remove_single_widget (Text _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ seq_row)=DF.mapM_ clean_row seq_row
remove_single_widget (Editor {})=return ()

clean_row::Row->IO ()
clean_row (Row seq_texture _ _)=DF.mapM_ (\(texture,_,_,_,_)->SRV.destroyTexture texture) seq_texture
clean_row (Row_blank _ _)=return ()

clean_block_font::(FP.Ptr SRF.Font,FCT.CInt,DIS.IntMap (SRT.Texture,DIS.IntMap (Int,FCT.CInt),FCT.CInt,DW.Word8,DW.Word8,DW.Word8,DW.Word8))->IO ()
clean_block_font (font,_,intmap_texture)=do
    _<-DIS.traverseWithKey (\_ (texture,_,_,_,_,_,_)->SRV.destroyTexture texture) intmap_texture
    SRF.closeFont font

remove_widget_only::Data a=>(Combined_widget a->IO (Combined_widget a))->Int->Int->Engine a->IO (Engine a)
remove_widget_only transform combined_id single_id (Engine widget window window_map request key main_id start_id count_id time)=do
    (intmap_combined_widget,new_widget)<-remove_widget_a transform combined_id single_id widget
    return (Engine (DIS.insert combined_id intmap_combined_widget new_widget) window window_map request key main_id start_id count_id time)

remove_widget::Data a=>(Combined_widget a->IO (Combined_widget a))->Int->Int->Engine a->IO (Engine a)
remove_widget transform combined_id single_id (Engine widget window window_map request key main_id start_id count_id time)=do
    (intmap_combined_widget,new_widget)<-remove_widget_a transform combined_id single_id widget
    return (Engine (if DIS.null intmap_combined_widget&&(start_id/=combined_id) then new_widget else DIS.insert combined_id intmap_combined_widget new_widget) window window_map request key main_id start_id count_id time)

remove_widget_a::Data a=>(Combined_widget a->IO (Combined_widget a))->Int->Int->DIS.IntMap (DIS.IntMap (Combined_widget a))->IO (DIS.IntMap (Combined_widget a),DIS.IntMap (DIS.IntMap (Combined_widget a)))
remove_widget_a transform combined_id single_id widget=let (combined_widget,intmap_combined_widget,new_widget)=error_remove_remove "remove_widget_a: error 1" "remove_widget_a: error 2" combined_id single_id widget in do
    new_combined_widget<-transform combined_widget
    case new_combined_widget of
        (Leaf_widget _ single_widget)->do
            remove_single_widget single_widget
            return (intmap_combined_widget,new_widget)
        (Node_widget _ _ _ _ new_combined_id)->let (new_intmap_combined_widget,new_new_widget)=error_remove "remove_widget_a: error 3" new_combined_id new_widget in do
            new_new_new_widget<-DIS.foldl remove_widget_b (return new_new_widget) new_intmap_combined_widget
            return (intmap_combined_widget,new_new_new_widget)

remove_widget_b::Data a=>IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))->Combined_widget a->IO (DIS.IntMap (DIS.IntMap (Combined_widget a)))
remove_widget_b io (Leaf_widget _ single_widget)=do
    widget<-io
    remove_single_widget single_widget
    return widget
remove_widget_b io (Node_widget _ _ _ _ combined_id)=do
    widget<-io
    let (intmap_combined_widget,new_widget)=error_remove "remove_widget_b: error 1" combined_id widget
    DIS.foldl remove_widget_b (return new_widget) intmap_combined_widget