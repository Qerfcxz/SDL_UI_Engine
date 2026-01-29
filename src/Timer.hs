{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Timer where
import Event
import qualified Control.Monad as CM
import qualified Data.Word as DW
import qualified Foreign.Ptr as FP
import qualified SDL.Raw.Timer as SRTi
import qualified SDL.Raw.Types as SRTy

add_timer::DW.Word32->DW.Word32->IO (SRTy.TimerID,SRTy.TimerCallback)
add_timer event_type time=do
    callback<-SRTy.mkTimerCallback $ \_ _->do
        push_event event_type 0 0 0 FP.nullPtr FP.nullPtr
        return time
    timer_id<-SRTi.addTimer time callback FP.nullPtr
    return (timer_id,callback)

remove_timer::(SRTy.TimerID,SRTy.TimerCallback)->IO ()
remove_timer (timer_id,callback)=do
    ok<-SRTi.removeTimer timer_id
    CM.unless ok (error "remove_timer: error 1")
    FP.freeHaskellFunPtr callback