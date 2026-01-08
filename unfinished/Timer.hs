{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Timer where
import Event
import qualified Control.Monad as CM
import qualified Data.Word as DW
import qualified Foreign.Ptr as FP
import qualified SDL.Raw.Timer as SRT
import qualified SDL.Raw.Types as SRT

add_timer::DW.Word32->DW.Word32->IO (SRT.TimerID,SRT.TimerCallback)
add_timer event_type time=do
    callback<-SRT.mkTimerCallback $ \_ _->do
        push_event event_type 0 0 0 FP.nullPtr FP.nullPtr
        return time
    timer_id<-SRT.addTimer time callback FP.nullPtr
    return(timer_id,callback)

remove_timer::(SRT.TimerID,SRT.TimerCallback)->IO ()
remove_timer (timer_id,callback)=do
    ok<-SRT.removeTimer timer_id
    CM.unless ok (error "remove_timer: SDL.Raw.Timer.removeTimer returns error")
    FP.freeHaskellFunPtr callback
