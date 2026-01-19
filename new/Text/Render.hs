{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Text.Render where
import Other.Error
import Type
import qualified Data.Sequence as DS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Raw.Types as SRT
import qualified SDL.Raw.Video as SRV

render_seq_texture::FP.Ptr SRT.Rect->FCT.CInt->FCT.CInt->FCT.CInt->SRT.Renderer->DS.Seq (SRT.Texture,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)->IO ()
render_seq_texture _ _ _ _ _ DS.Empty=return ()
render_seq_texture rect left up down renderer ((texture,x,y,width,height) DS.:<| seq_texture)=do
    FS.poke rect (SRT.Rect (left+x) (up+y) width height)
    catch_error "render_seq_texture: error 1" 0 (SRV.renderCopy renderer texture FP.nullPtr rect)
    render_seq_texture rect left up down renderer seq_texture

render_seq_row::FP.Ptr SRT.Rect->FCT.CInt->FCT.CInt->FCT.CInt->SRT.Renderer->DS.Seq Row->IO ()
render_seq_row _ _ _ _ _ DS.Empty=return ()
render_seq_row rect left up down renderer (row DS.:<| seq_row)=case row of
    Row seq_texture y font_height->if down<up+y+font_height then return () else do
        render_seq_texture rect left up down renderer seq_texture
        render_seq_row rect left up down renderer seq_row
    Row_blank y font_height->if down<up+y+font_height then return () else render_seq_row rect left up down renderer seq_row