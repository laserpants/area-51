{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.JackOfClubs where

import Data.List.NonEmpty (fromList, toList)
import qualified Data.Map.Strict as Map
import Pong.Data
import Pong.Lang
import Pong.Util

program4 :: Text
program4 =
  "def main(a : unit) : int =\
  \  let\
  \    id =\
  \      lam(x) =>\
  \        x\
  \    in\
  \      let\
  \        add =\
  \          lam(x) =>\
  \            lam(y) =>\
  \              x + y\
  \        in\
  \          let\
  \            add2 =\
  \              add(2)\
  \            in\
  \              (id(add2))(id(3)) + add(4, 5)\
  \"

-- "

program5 :: Program () SourceExpr
program5 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [((), "a")])
              ( ()
              , eLet
                  ((), "id")
                  (eLam () [((), "x")] (eVar ((), "x")))
                  ( eLet
                      ((), "add")
                      ( eLam
                          ()
                          [((), "x")]
                          ( eLam
                              ()
                              [((), "y")]
                              ( eOp2
                                  ((), OAdd)
                                  (eVar ((), "x"))
                                  (eVar ((), "y"))
                              )
                          )
                      )
                      ( eLet
                          ((), "add2")
                          (eApp () (eVar ((), "add")) [eLit (PInt 2)])
                          ( eOp2
                              ((), OAdd)
                              ( eApp
                                  ()
                                  (eApp () (eVar ((), "id")) [eVar ((), "add2")])
                                  [ eApp () (eVar ((), "id")) [eLit (PInt 3)]
                                  ]
                              )
                              (eApp () (eVar ((), "add")) [eLit (PInt 4), eLit (PInt 5)])
                          )
                      )
                  )
              )
          )
        ]
    )

program6 :: Program MonoType TypedExpr
program6 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [(tUnit, "a")])
              ( tInt
              , eLet
                  (tVar 0 ~> tVar 0, "id")
                  (eLam () [(tVar 0, "x")] (eVar (tVar 0, "x")))
                  ( eLet
                      (tVar 1 ~> tVar 1 ~> tVar 1, "add")
                      ( eLam
                          ()
                          [(tVar 1, "x")]
                          ( eLam
                              ()
                              [(tVar 1, "y")]
                              ( eOp2
                                  (tVar 1 ~> tVar 1 ~> tVar 1, OAdd)
                                  (eVar (tVar 1, "x"))
                                  (eVar (tVar 1, "y"))
                              )
                          )
                      )
                      ( eLet
                          (tInt ~> tInt, "add2")
                          (eApp (tInt ~> tInt) (eVar (tInt ~> tInt ~> tInt, "add")) [eLit (PInt 2)])
                          ( eOp2
                              oAddInt
                              ( eApp
                                  tInt
                                  (eApp (tInt ~> tInt) (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id")) [eVar (tInt ~> tInt, "add2")])
                                  [ eApp tInt (eVar (tInt ~> tInt, "id")) [eLit (PInt 3)]
                                  ]
                              )
                              (eApp tInt (eVar (tInt ~> tInt ~> tInt, "add")) [eLit (PInt 4), eLit (PInt 5)])
                          )
                      )
                  )
              )
          )
        ]
    )

program7 :: Program MonoType TypedExpr
program7 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [(tUnit, "a")])
              ( tInt
              , eLet
                  (tVar 0 ~> tVar 0, "id")
                  (eLam () [(tVar 0, "x")] (eVar (tVar 0, "x")))
                  ( eLet
                      (tVar 1 ~> tVar 1 ~> tVar 1, "add")
                      ( eLam
                          ()
                          [(tVar 1, "x"), (tVar 1, "y")]
                          ( eOp2
                              (tVar 1 ~> tVar 1 ~> tVar 1, OAdd)
                              (eVar (tVar 1, "x"))
                              (eVar (tVar 1, "y"))
                          )
                      )
                      ( eLet
                          (tInt ~> tInt, "add2")
                          (eApp (tInt ~> tInt) (eVar (tInt ~> tInt ~> tInt, "add")) [eLit (PInt 2)])
                          ( eOp2
                              oAddInt
                              ( eApp
                                  tInt
                                  (eApp (tInt ~> tInt) (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id")) [eVar (tInt ~> tInt, "add2")])
                                  [ eApp tInt (eVar (tInt ~> tInt, "id")) [eLit (PInt 3)]
                                  ]
                              )
                              (eApp tInt (eVar (tInt ~> tInt ~> tInt, "add")) [eLit (PInt 4), eLit (PInt 5)])
                          )
                      )
                  )
              )
          )
        ]
    )

program8 :: Program MonoType TypedExpr
program8 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [(tUnit, "a")])
              ( tInt
              , eLet
                  ((tInt ~> tInt) ~> tInt ~> tInt, "$var_id_3")
                  (eLam () [(tInt ~> tInt, "x")] (eVar (tInt ~> tInt, "x")))
                  ( eLet
                      (tInt ~> tInt, "$var_id_4")
                      (eLam () [(tInt, "x")] (eVar (tInt, "x")))
                      ( eLet
                          (tInt ~> tInt ~> tInt, "$var_add_1")
                          ( eLam
                              ()
                              [(tInt, "x")]
                              ( eLam
                                  ()
                                  [(tInt, "y")]
                                  ( eOp2
                                      oAddInt
                                      (eVar (tInt, "x"))
                                      (eVar (tInt, "y"))
                                  )
                              )
                          )
                          ( eLet
                              (tInt ~> tInt ~> tInt, "$var_add_2")
                              ( eLam
                                  ()
                                  [(tInt, "x")]
                                  ( eLam
                                      ()
                                      [(tInt, "y")]
                                      ( eOp2
                                          oAddInt
                                          (eVar (tInt, "x"))
                                          (eVar (tInt, "y"))
                                      )
                                  )
                              )
                              ( eLet
                                  (tInt ~> tInt, "add2")
                                  (eApp (tInt ~> tInt) (eVar (tInt ~> tInt ~> tInt, "$var_add_1")) [eLit (PInt 2)])
                                  ( eOp2
                                      oAddInt
                                      ( eApp
                                          tInt
                                          (eApp (tInt ~> tInt) (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "$var_id_3")) [eVar (tInt ~> tInt, "add2")])
                                          [ eApp tInt (eVar (tInt ~> tInt, "$var_id_4")) [eLit (PInt 3)]
                                          ]
                                      )
                                      (eApp tInt (eVar (tInt ~> tInt ~> tInt, "$var_add_2")) [eLit (PInt 4), eLit (PInt 5)])
                                  )
                              )
                          )
                      )
                  )
              )
          )
        ]
    )

program9 :: Program MonoType TypedExpr
program9 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [(tUnit, "a")])
              ( tInt
              , eLet
                  ((tInt ~> tInt) ~> tInt ~> tInt, "$var_id_3")
                  (eLam () [(tInt ~> tInt, "x")] (eVar (tInt ~> tInt, "x")))
                  ( eLet
                      (tInt ~> tInt, "$var_id_4")
                      (eLam () [(tInt, "x")] (eVar (tInt, "x")))
                      ( eLet
                          (tInt ~> tInt ~> tInt, "$var_add_1")
                          ( eLam
                              ()
                              [(tInt, "x"), (tInt, "y")]
                              ( eOp2
                                  oAddInt
                                  (eVar (tInt, "x"))
                                  (eVar (tInt, "y"))
                              )
                          )
                          ( eLet
                              (tInt ~> tInt ~> tInt, "$var_add_2")
                              ( eLam
                                  ()
                                  [(tInt, "x"), (tInt, "y")]
                                  ( eOp2
                                      oAddInt
                                      (eVar (tInt, "x"))
                                      (eVar (tInt, "y"))
                                  )
                              )
                              ( eLet
                                  (tInt ~> tInt, "add2")
                                  (eApp (tInt ~> tInt) (eVar (tInt ~> tInt ~> tInt, "$var_add_1")) [eLit (PInt 2)])
                                  ( eOp2
                                      oAddInt
                                      ( eApp
                                          tInt
                                          (eApp (tInt ~> tInt) (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "$var_id_3")) [eVar (tInt ~> tInt, "add2")])
                                          [ eApp tInt (eVar (tInt ~> tInt, "$var_id_4")) [eLit (PInt 3)]
                                          ]
                                      )
                                      (eApp tInt (eVar (tInt ~> tInt ~> tInt, "$var_add_2")) [eLit (PInt 4), eLit (PInt 5)])
                                  )
                              )
                          )
                      )
                  )
              )
          )
        ]
    )

program10 :: Program MonoType Ast
program10 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [(tUnit, "a")])
              ( tInt
              , eLet
                  ((tInt ~> tInt) ~> tInt ~> tInt, "$var_id_3")
                  (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "$lam1"))
                  ( eLet
                      (tInt ~> tInt, "$var_id_4")
                      (eVar (tInt ~> tInt, "$lam2"))
                      ( eLet
                          (tInt ~> tInt ~> tInt, "$var_add_1")
                          (eVar (tInt ~> tInt ~> tInt, "$lam4"))
                          ( eLet
                              (tInt ~> tInt ~> tInt, "$var_add_2")
                              (eVar (tInt ~> tInt ~> tInt, "$lam6"))
                              ( eLet
                                  (tInt ~> tInt, "add2")
                                  (eCall (tInt ~> tInt ~> tInt, "$var_add_1") [eLit (PInt 2)])
                                  ( eOp2
                                      oAddInt
                                      ( eCall
                                          ((tInt ~> tInt) ~> tInt ~> tInt, "$var_id_3")
                                          [ eVar (tInt ~> tInt, "add2")
                                          , eCall (tInt ~> tInt, "$var_id_4") [eLit (PInt 3)]
                                          ]
                                      )
                                      (eCall (tInt ~> tInt ~> tInt, "$var_add_2") [eLit (PInt 4), eLit (PInt 5)])
                                  )
                              )
                          )
                      )
                  )
              )
          )
        ,
          ( (Scheme ((tInt ~> tInt) ~> tInt ~> tInt), "$lam1")
          , Function
              (fromList [(tInt ~> tInt, "x"), (tInt, "$v0")])
              ( tInt
              , eCall (tInt ~> tInt, "x") [eVar (tInt, "$v0")]
              )
          )
        ,
          ( (Scheme (tInt ~> tInt), "$lam2")
          , Function
              (fromList [(tInt, "x")])
              ( tInt
              , eVar (tInt, "x")
              )
          )
        ,
          ( (Scheme (tInt ~> tInt ~> tInt), "$lam3")
          , Function
              (fromList [(tInt, "x"), (tInt, "y")])
              ( tInt
              , eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))
              )
          )
        ,
          ( (Scheme (tInt ~> tInt ~> tInt), "$lam4")
          , Function
              (fromList [(tInt, "x"), (tInt, "$v0")])
              ( tInt
              , eCall (tInt ~> tInt ~> tInt, "$lam3") [eVar (tInt, "x"), eVar (tInt, "$v0")]
              )
          )
        ,
          ( (Scheme (tInt ~> tInt ~> tInt), "$lam5")
          , Function
              (fromList [(tInt, "x"), (tInt, "y")])
              ( tInt
              , eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))
              )
          )
        ,
          ( (Scheme (tInt ~> tInt ~> tInt), "$lam6")
          , Function
              (fromList [(tInt, "x"), (tInt, "$v0")])
              ( tInt
              , eCall (tInt ~> tInt ~> tInt, "$lam5") [eVar (tInt, "x"), eVar (tInt, "$v0")]
              )
          )
        ]
    )

--
-- def main(a : unit) : int =
--   let
--     $var_id_3 =
--       $lam1
--     in
--       let
--         $var_id_4 =
--           $lam2
--         in
--           let
--             $var_add_1 =
--               $lam3
--             in
--               let
--                 $var_add_2 =
--                   $lam4
--                  in
--                    let
--                      add2 =
--                        $var_add_1(2)
--                      in
--                        ($var_id_3(add2))($var_id_4(3)) + $var_add_2(4, 5)
--
-- def $lam1(x : int -> int, $v0 : int) : int =
--   x($v0)
--
-- def $lam2(x : int) : int =
--   x
--
-- def $lam3(x : int, y : int) : int =
--   x + y
--
-- def $lam4(x : int, y : int) : int =
--   x + y
--

program11 :: Program MonoType Ast
program11 =
  Program
    ( Map.fromList
        [
          ( (Scheme (tUnit ~> tInt), "main")
          , Function
              (fromList [(tUnit, "a")])
              ( tInt
              , eLet
                  ((tInt ~> tInt) ~> tInt ~> tInt, "$var_id_3")
                  (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "$lam1"))
                  ( eLet
                      (tInt ~> tInt, "$var_id_4")
                      (eVar (tInt ~> tInt, "$lam2"))
                      ( eLet
                          (tInt ~> tInt ~> tInt, "$var_add_1")
                          (eVar (tInt ~> tInt ~> tInt, "$lam3"))
                          ( eLet
                              (tInt ~> tInt ~> tInt, "$var_add_2")
                              (eVar (tInt ~> tInt ~> tInt, "$lam4"))
                              ( eLet
                                  (tInt ~> tInt, "add2")
                                  (eCall (tInt ~> tInt ~> tInt, "$var_add_1") [eLit (PInt 2)])
                                  ( eOp2
                                      oAddInt
                                      ( eCall
                                          ((tInt ~> tInt) ~> tInt ~> tInt, "$var_id_3")
                                          [ eVar (tInt ~> tInt, "add2")
                                          , eCall (tInt ~> tInt, "$var_id_4") [eLit (PInt 3)]
                                          ]
                                      )
                                      (eCall (tInt ~> tInt ~> tInt, "$var_add_2") [eLit (PInt 4), eLit (PInt 5)])
                                  )
                              )
                          )
                      )
                  )
              )
          )
        ,
          ( (Scheme ((tInt ~> tInt) ~> tInt ~> tInt), "$lam1")
          , Function
              (fromList [(tInt ~> tInt, "x"), (tInt, "$v0")])
              ( tInt
              , eCall (tInt ~> tInt, "x") [eVar (tInt, "$v0")]
              )
          )
        ,
          ( (Scheme (tInt ~> tInt), "$lam2")
          , Function
              (fromList [(tInt, "x")])
              ( tInt
              , eVar (tInt, "x")
              )
          )
        ,
          ( (Scheme (tInt ~> tInt ~> tInt), "$lam3")
          , Function
              (fromList [(tInt, "x"), (tInt, "y")])
              ( tInt
              , eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))
              )
          )
        ,
          ( (Scheme (tInt ~> tInt ~> tInt), "$lam4")
          , Function
              (fromList [(tInt, "x"), (tInt, "y")])
              ( tInt
              , eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))
              )
          )
        ]
    )
