{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Widget.Alter where
import Editor.Block
import Editor.From
import Other.Error
import Other.Get
import Other.Other
import Text.Font
import Text.From
import Text.Max
import Widget.Create
import Widget.Remove
import Type
import qualified Control.Monad as CM
import qualified Data.ByteString as DB
import qualified Data.Foldable as DF
import qualified Data.IntMap.Strict as DIS
import qualified Data.Sequence as DS
import qualified Data.Text.Encoding as DTE
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Font as SRF
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

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
alter_single_widget _ _ _ (Label_data_request label) this_widget=case this_widget of
    Data this_content->do
        clean_data this_content
        return (Label_data label)
    _->error "alter_single_widget: error 1"
alter_single_widget _ _ _ (Bool_data_request bool) this_widget=case this_widget of
    Data this_content->do
        clean_data this_content
        return (Bool_data bool)
    _->error "alter_single_widget: error 2"
alter_single_widget _ _ _ (Int_data_request int) this_widget=case this_widget of
    Data this_content->do
        clean_data this_content
        return (Int_data int)
    _->error "alter_single_widget: error 3"
alter_single_widget _ _ _ (Data_request content) this_widget=case this_widget of
    Data this_content->do
        clean_data this_content
        return (Data content)
    _->error "alter_single_widget: error 4"
alter_single_widget _ _ _ (Trigger_request handle) this_widget=case this_widget of
    Trigger _->return (Trigger handle)
    _->error "alter_single_widget: error 5"
alter_single_widget _ _ _ (Io_trigger_request handle) this_widget=case this_widget of
    Io_trigger _->return (Io_trigger handle)
    _->error "alter_single_widget: error 6"
alter_single_widget _ _ _ (Font_request path size) this_widget=case this_widget of
    Font intmap_font->do
        _<-DIS.traverseWithKey (\_ font->SRF.closeFont font) intmap_font
        font<-DB.useAsCString (DTE.encodeUtf8 path) (`create_font` size)
        return (Font font)
    _->error "alter_single_widget: error 7"
alter_single_widget _ _ _ (Block_font_request window_id red green blue alpha path size) this_widget=case this_widget of
    Block_font _ _ _ _ _ font->do
        _<-DIS.traverseWithKey (\_ this_font->clean_block_font this_font) font
        new_font<-DB.useAsCString (DTE.encodeUtf8 path) (`create_block_font` size)
        return (Block_font window_id red green blue alpha new_font)
    _->error "alter_single_widget: error 8"
alter_single_widget _ window _ (Rectangle_request window_id red green blue alpha left right up down) this_widget=case this_widget of
    Rectangle {}->case DIS.lookup window_id window of
        Nothing->error "alter_single_widget: error 9"
        Just (Window _ _ _ _ _ x y design_size size)->return (Rectangle window_id red green blue alpha left right up down (x+div (left*size) design_size) (y+div (up*size) design_size) (div ((right-left)*size) design_size) (div ((down-up)*size) design_size))
    _->error "alter_single_widget: error 10"
alter_single_widget _ window _ (Picture_request window_id path x y width_multiply width_divide height_multiply height_divide) this_widget=case this_widget of
    Picture _ texture _ _ _ _ _ _ _ _ _ _ _ _->case DIS.lookup window_id window of
        Nothing->error "alter_single_widget: error 11"
        Just (Window _ _ renderer _ _ window_x window_y design_size size)->do
            SRV.destroyTexture texture
            surface<-DB.useAsCString (DTE.encodeUtf8 path) SRV.loadBMP
            CM.when (surface==FP.nullPtr) $ error "alter_single_widget: error 12"
            (SRT.Surface _ width height _ _ _ _)<-FS.peek surface
            new_texture<-SRV.createTextureFromSurface renderer surface
            SRV.freeSurface surface
            CM.when (new_texture==FP.nullPtr) $ error "alter_single_widget: error 13"
            let new_width=div (width*width_multiply) width_divide in let new_height=div (height*height_multiply) height_divide in return (Picture window_id new_texture x y width_multiply width_divide height_multiply height_divide width height (window_x+div ((x-div new_width 2)*size) design_size) (window_y+div ((y-div new_height 2)*size) design_size) (div (new_width*size) design_size) (div (new_height*size) design_size))
    _->error "alter_single_widget: error 14"
alter_single_widget start_id window widget (Text_request window_id row find delta_height left right up down seq_paragraph) this_widget=case this_widget of
    Text _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ seq_row->case DIS.lookup window_id window of
        Nothing->error "alter_single_widget: error 15"
        Just (Window _ _ renderer _ _ x y design_size size)->do
            DF.mapM_ clean_row seq_row
            let new_delta_height=div (delta_height*size) design_size
            new_seq_row<-from_paragraph widget renderer (find_font find) window_id start_id design_size size 0 (div ((right-left)*size) design_size) new_delta_height seq_paragraph DS.empty
            let new_up=y+div (up*size) design_size in let new_down=y+div (down*size) design_size in let max_row=find_max new_seq_row new_up new_down in return (Text window_id (max 0 (min max_row row)) max_row False False find delta_height left right up down new_delta_height (x+div (left*size) design_size) (x+div (right*size) design_size) new_up new_down seq_paragraph new_seq_row)
    _->error "alter_single_widget: error 16"
alter_single_widget start_id window widget (Editor_request window_id block_number font_size path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha block_width height delta_height x y extra_width extra_height seq_text) this_widget=case this_widget of
    Editor {}->let (window_x,window_y,design_size,size)=get_transform_window window_id window in let new_block_width=div (block_width*size) design_size in let (new_font_size,_,font_height,intmap_texture)=find_block_font find widget block_width new_block_width design_size size path start_id font_size in FMA.alloca $ \text_color->do
        FS.poke text_color (color text_red text_green text_blue text_alpha)
        let new_delta_height=div (delta_height*size) design_size
        let new_height=div ((height+delta_height)*size) design_size
        let half_width=div (fromIntegral block_number*new_block_width) 2
        let half_height=div (div (height*size) design_size) 2
        return (Editor window_id block_number (fromIntegral (div new_height (font_height+new_delta_height))) 0 font_size new_font_size False path find typesetting text_red text_green text_blue text_alpha cursor_red cursor_green cursor_blue cursor_alpha select_red select_green select_blue select_alpha height block_width delta_height x y extra_width extra_height font_height new_block_width new_delta_height (window_x+div (x*size) design_size-div (fromIntegral block_number*new_block_width) 2) (window_y+div (y*size) design_size-div new_height 2) (window_x+div ((x-extra_width)*size) design_size-half_width) (window_x+div ((x+extra_width)*size) design_size+half_width) (window_y+div ((y-extra_height)*size) design_size-half_height) (window_y+div ((y+extra_height)*size) design_size+half_height) Cursor_none (from_seq_seq_char intmap_texture block_number new_block_width seq_text DS.empty))
    _->error "alter_single_widget: error 17"