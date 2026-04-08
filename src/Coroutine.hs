{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Coroutine where
import Type

to_coroutine::Do_coroutine a b->Coroutine a
to_coroutine (Do_coroutine a)=a (const Done)

do_coroutine_wait::a->Int->Do_coroutine b a
do_coroutine_wait a b=Do_coroutine (\c->Then (Wait b) (c a))

do_coroutine_loop::a->Int->Do_coroutine b a->Do_coroutine b a
do_coroutine_loop a b c=Do_coroutine (\d->Then (Loop b (to_coroutine c)) (d a))

do_coroutine_forever::a->Do_coroutine b a->Do_coroutine b a
do_coroutine_forever _ a=Do_coroutine (\_->Forever (to_coroutine a))

do_coroutine_if::a->(Engine b->Bool)->Do_coroutine b a->Do_coroutine b a->Do_coroutine b a
do_coroutine_if a b c d=Do_coroutine (\e->Then (If b (to_coroutine c) (to_coroutine d)) (e a))

do_coroutine_fork::a->Do_coroutine b a->Do_coroutine b a->Do_coroutine b a
do_coroutine_fork a b c=Do_coroutine (\d->Then (Fork (to_coroutine b) (to_coroutine c)) (d a))

do_coroutine_emit::a->(Engine b->Engine b)->Do_coroutine b a
do_coroutine_emit a b=Do_coroutine (\c->Then (Emit b) (c a))

do_wait::Int->Do_coroutine a ()
do_wait=do_coroutine_wait ()

do_loop::Int->Do_coroutine a ()->Do_coroutine a ()
do_loop=do_coroutine_loop ()

do_forever::Do_coroutine a ()->Do_coroutine a ()
do_forever=do_coroutine_forever ()

do_if::(Engine a->Bool)->Do_coroutine a ()->Do_coroutine a ()->Do_coroutine a ()
do_if=do_coroutine_if ()

do_fork::Do_coroutine a ()->Do_coroutine a ()->Do_coroutine a ()
do_fork=do_coroutine_fork ()

do_emit::(Engine a->Engine a)->Do_coroutine a ()
do_emit=do_coroutine_emit ()