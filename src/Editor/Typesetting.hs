{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Editor.Typesetting where
import Type

typesetting_left::Typesetting->Int->Int->Int
typesetting_left Typesetting_left _ _=0
typesetting_left Typesetting_right number block_number=block_number-number
typesetting_left Typesetting_center number block_number=div (block_number-number) 2

typesetting_right::Typesetting->Int->Int->Int
typesetting_right Typesetting_left number _=number
typesetting_right Typesetting_right _ block_number=block_number
typesetting_right Typesetting_center number block_number=div (block_number+number) 2