{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.ThePanamaHat where

import Data.List.NonEmpty (fromList)
import qualified Data.Map.Strict as Map
import Pong.Data
import Pong.Lang
import Pong.Util

program200 :: Text
program200 =
  "def main(a : unit) : int =\
  \  let\
  \    xs =\
  \      Nil()\
  \    in\
  \      match xs\
  \        { Nil => 401\
  \        | Cons(y, ys) => 402\
  \        }\
  \"

-- "

program201 :: Program () SourceExpr
program201 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [((), "a")])
              ( ()
              , eLet
                  ((), "xs")
                  (eCon ((), "Nil"))
                  ( ePat
                      (eVar ((), "xs"))
                      [ ([((), "Nil")], eLit (PInt 401))
                      , ([((), "Cons"), ((), "y"), ((), "ys")], eLit (PInt 402))
                      ]
                  )
              )
          )
        ]
    )

program202 :: Program MonoType TypedExpr
program202 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [(tUnit, "a")])
              ( tInt
              , eLet
                  (tCon "List" [tVar 0], "xs")
                  (eCon (tCon "List" [tVar 0], "Nil"))
                  ( ePat
                      (eVar (tCon "List" [tVar 1], "xs"))
                      [ ([(tCon "List" [tVar 1], "Nil")], eLit (PInt 401))
                      , ([(tVar 1 ~> tCon "List" [tVar 1] ~> tCon "List" [tVar 1], "Cons"), (tVar 1, "y"), (tCon "List" [tVar 1], "ys")], eLit (PInt 402))
                      ]
                  )
              )
          )
        ]
    )
