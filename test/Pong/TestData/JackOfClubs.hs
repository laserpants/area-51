{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.JackOfClubs where

import Data.List.NonEmpty (fromList)
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

program5 :: Module () SourceExpr
program5 =
  Module
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

program6 :: Module MonoType TypedExpr
program6 =
  Module
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

-- program7 :: Module MonoType TypedExpr
-- program7 =
--  Module
--    ( Map.fromList
--        [
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [(tUnit, "a")])
--              ( tInt
--              , eLet
--                  (tVar 0 ~> tVar 0, "id")
--                  (eLam () [(tVar 0, "x")] (eVar (tVar 0, "x")))
--                  ( eLet
--                      (tVar 1 ~> tVar 1 ~> tVar 1, "add")
--                      ( eLam
--                          ()
--                          [(tVar 1, "x"), (tVar 1, "y")]
--                          ( eOp2
--                              (tVar 1 ~> tVar 1 ~> tVar 1, OAdd)
--                              (eVar (tVar 1, "x"))
--                              (eVar (tVar 1, "y"))
--                          )
--                      )
--                      ( eLet
--                          (tInt ~> tInt, "add2")
--                          (eApp (tInt ~> tInt) (eVar (tInt ~> tInt ~> tInt, "add")) [eLit (PInt 2)])
--                          ( eOp2
--                              oAddInt
--                              ( eApp
--                                  tInt
--                                  (eApp (tInt ~> tInt) (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id")) [eVar (tInt ~> tInt, "add2")])
--                                  [ eApp tInt (eVar (tInt ~> tInt, "id")) [eLit (PInt 3)]
--                                  ]
--                              )
--                              (eApp tInt (eVar (tInt ~> tInt ~> tInt, "add")) [eLit (PInt 4), eLit (PInt 5)])
--                          )
--                      )
--                  )
--              )
--          )
--        ]
--    )
--
-- program8 :: Module MonoType TypedExpr
-- program8 =
--  Module
--    ( Map.fromList
--        [
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [(tUnit, "a")])
--              ( tInt
--              , eLet
--                  ((tInt ~> tInt) ~> tInt ~> tInt, "id-3")
--                  (eLam () [(tInt ~> tInt, "x")] (eVar (tInt ~> tInt, "x")))
--                  ( eLet
--                      (tInt ~> tInt, "id-4")
--                      (eLam () [(tInt, "x")] (eVar (tInt, "x")))
--                      ( eLet
--                          (tVar 0 ~> tVar 0, "id")
--                          (eLam () [(tVar 0, "x")] (eVar (tVar 0, "x")))
--                          ( eLet
--                              (tInt ~> tInt ~> tInt, "add-1")
--                              ( eLam
--                                  ()
--                                  [(tInt, "x")]
--                                  ( eLam
--                                      ()
--                                      [(tInt, "y")]
--                                      ( eOp2
--                                          oAddInt
--                                          (eVar (tInt, "x"))
--                                          (eVar (tInt, "y"))
--                                      )
--                                  )
--                              )
--                              ( eLet
--                                  (tInt ~> tInt ~> tInt, "add-2")
--                                  ( eLam
--                                      ()
--                                      [(tInt, "x")]
--                                      ( eLam
--                                          ()
--                                          [(tInt, "y")]
--                                          ( eOp2
--                                              oAddInt
--                                              (eVar (tInt, "x"))
--                                              (eVar (tInt, "y"))
--                                          )
--                                      )
--                                  )
--                                  ( eLet
--                                      (tVar 1 ~> tVar 1 ~> tVar 1, "add")
--                                      ( eLam
--                                          ()
--                                          [(tVar 1, "x")]
--                                          ( eLam
--                                              ()
--                                              [(tVar 1, "y")]
--                                              ( eOp2
--                                                  (tVar 1 ~> tVar 1 ~> tVar 1, OAdd)
--                                                  (eVar (tVar 1, "x"))
--                                                  (eVar (tVar 1, "y"))
--                                              )
--                                          )
--                                      )
--                                      ( eLet
--                                          (tInt ~> tInt, "add2")
--                                          (eApp (tInt ~> tInt) (eVar (tInt ~> tInt ~> tInt, "add-1")) [eLit (PInt 2)])
--                                          ( eOp2
--                                              oAddInt
--                                              ( eApp
--                                                  tInt
--                                                  (eApp (tInt ~> tInt) (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id-3")) [eVar (tInt ~> tInt, "add2")])
--                                                  [ eApp tInt (eVar (tInt ~> tInt, "id-4")) [eLit (PInt 3)]
--                                                  ]
--                                              )
--                                              (eApp tInt (eVar (tInt ~> tInt ~> tInt, "add-2")) [eLit (PInt 4), eLit (PInt 5)])
--                                          )
--                                      )
--                                  )
--                              )
--                          )
--                      )
--                  )
--              )
--          )
--        ]
--    )
--
-- program9 :: Module MonoType TypedExpr
-- program9 =
--  Module
--    ( Map.fromList
--        [
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [(tUnit, "a")])
--              ( tInt
--              , eLet
--                  ((tInt ~> tInt) ~> tInt ~> tInt, "id-3")
--                  (eLam () [(tInt ~> tInt, "x")] (eVar (tInt ~> tInt, "x")))
--                  ( eLet
--                      (tInt ~> tInt, "id-4")
--                      (eLam () [(tInt, "x")] (eVar (tInt, "x")))
--                      ( eLet
--                          (tVar 0 ~> tVar 0, "id")
--                          (eLam () [(tVar 0, "x")] (eVar (tVar 0, "x")))
--                          ( eLet
--                              (tInt ~> tInt ~> tInt, "add-1")
--                              ( eLam
--                                  ()
--                                  [(tInt, "x"), (tInt, "y")]
--                                  ( eOp2
--                                      oAddInt
--                                      (eVar (tInt, "x"))
--                                      (eVar (tInt, "y"))
--                                  )
--                              )
--                              ( eLet
--                                  (tInt ~> tInt ~> tInt, "add-2")
--                                  ( eLam
--                                      ()
--                                      [(tInt, "x"), (tInt, "y")]
--                                      ( eOp2
--                                          oAddInt
--                                          (eVar (tInt, "x"))
--                                          (eVar (tInt, "y"))
--                                      )
--                                  )
--                                  ( eLet
--                                      (tVar 1 ~> tVar 1 ~> tVar 1, "add")
--                                      ( eLam
--                                          ()
--                                          [(tVar 1, "x"), (tVar 1, "y")]
--                                          ( eOp2
--                                              (tVar 1 ~> tVar 1 ~> tVar 1, OAdd)
--                                              (eVar (tVar 1, "x"))
--                                              (eVar (tVar 1, "y"))
--                                          )
--                                      )
--                                      ( eLet
--                                          (tInt ~> tInt, "add2")
--                                          (eApp (tInt ~> tInt) (eVar (tInt ~> tInt ~> tInt, "add-1")) [eLit (PInt 2)])
--                                          ( eOp2
--                                              oAddInt
--                                              ( eApp
--                                                  tInt
--                                                  (eApp (tInt ~> tInt) (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id-3")) [eVar (tInt ~> tInt, "add2")])
--                                                  [ eApp tInt (eVar (tInt ~> tInt, "id-4")) [eLit (PInt 3)]
--                                                  ]
--                                              )
--                                              (eApp tInt (eVar (tInt ~> tInt ~> tInt, "add-2")) [eLit (PInt 4), eLit (PInt 5)])
--                                          )
--                                      )
--                                  )
--                              )
--                          )
--                      )
--                  )
--              )
--          )
--        ]
--    )
--
----  def main(a : unit) : int =
----    let
----      id =
----        lam(x) =>
----          x
----      in
----        let
----          add =
----            lam(x) =>
----              lam(y) =>
----                x + y
----          in
----            let
----              add2 =
----                add(2)
----              in
----                (id(add2))(id(3)) + add(4, 5)
--
----
---- let
----   id-3 =
----     lam(x) => x
----   in
----     let
----       id-4 =
----         lam(x) => x
----       in
----         let
----           id =
----             lam(x) => x
----           in
----             let
----               add-1 =
----                 lam(x) =>
----                   lam(y) =>
----                     x + y
----               in
----                 let
----                   add-2 =
----                     lam(x) =>
----                       lam(y) =>
----                         x + y
----                   in
----                     let
----                       add =
----                         lam(x) =>
----                           lam(y) =>
----                             x + y
----                       in
----                         let
----                           add2 =
----                             add-1(2)
----                           in
----                             (id-3(add2))(id-4(3))
----                             +
----                             add-2(4, 5)
----
----
--
---------------------------------------------------------------------------------
--
---- def main(a : unit) : int =
----   let
----     add2 =
----       add-1(2)
----     in
----       id-3(add2, id-4(3))
----       +
----       add-2(4, 5)
----
---- def $lam2(x : int) : int =
----   x
----
---- def $lam4(x : int, y : int) : int =
----   x + y
----
---- def $lam5(x : int, $v0 : int) : int =
----   $lam4(x, $v0)
----
---- def $lam6(x : int, y : int) : int =
----   x + y
----
---- def $lam7(x : int, $v0 : int) : int =
----   $lam6(x, $v0)
----
---- def $lam1(x : int -> int, $v0 : int) : int =
----   x($v0)
----
---- def $lam8(x : '1, y : '1) : '1 =
----   x + y
----
---- def $lam9(x : '1, $v0 : '1) : '1 =
----   $lam8(x, $v0)
----
---- def $lam3(x : '0) : '0 =
----   x
----
--
---- ((Scheme (Fix (TArr (Fix (TVar "a0")) (Fix (TArr (Fix (TVar "a0")) (Fix (TVar "a0")))))),"$lam8"),
----   Function ((Fix (TVar 1),"x") :| [(Fix (TVar 1),"y")]) (Fix (TVar 1),Fix (EOp2 (Fix (TArr (Fix (TVar 1)) (Fix (TArr (Fix (TVar 1)) (Fix (TVar 1))))),OAdd) (Fix (EVar (Fix (TVar 1),"x"))) (Fix (EVar (Fix (TVar 1),"y")))))),((Scheme (Fix (TArr (Fix (TVar "a0")) (Fix (TArr (Fix (TVar "a0")) (Fix (TVar "a0")))))),"$lam9"),Function ((Fix (TVar 1),"x") :| [(Fix (TVar 1),"$v0")]) (Fix (TVar 1),Fix (ECall () (Fix (TArr (Fix (TVar 1)) (Fix (TArr (Fix (TVar 1)) (Fix (TVar 1))))),"$lam8") [Fix (EVar (Fix (TVar 1),"x")),Fix (EVar (Fix (TVar 1),"$v0"))]))),((Scheme (Fix (TArr (Fix (TVar "a0")) (Fix (TVar "a0")))),"$lam3"),Function ((Fix (TVar 0),"x") :| []) (Fix (TVar 0),Fix (EVar (Fix (TVar 0),"x"))))])
--
----  def main(a : unit) : int =
----    let
----      id =
----        lam(x) =>
----          x
----      in
----        let
----          add =
----            lam(x) =>
----              lam(y) =>
----                x + y
----          in
----            let
----              add2 =
----                add(2)
----              in
----                (id(add2))(id(3)) + add(4, 5)\
--
-- program10 :: Module MonoType Ast
-- program10 =
--  Module
--    ( Map.fromList
--        [
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [(tUnit, "a")])
--              ( tInt
--              , eLet
--                  ((tInt ~> tInt) ~> tInt ~> tInt, "id-3")
--                  (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "$lam1"))
--                  ( eLet
--                      (tInt ~> tInt, "id-4")
--                      (eVar (tInt ~> tInt, "$lam2"))
--                      ( eLet
--                          (tInt ~> tInt ~> tInt, "add-1")
--                          (eVar (tInt ~> tInt ~> tInt, "$lam4"))
--                          ( eLet
--                              (tInt ~> tInt ~> tInt, "add-2")
--                              (eVar (tInt ~> tInt ~> tInt, "$lam6"))
--                              ( eLet
--                                  (tInt ~> tInt, "add2")
--                                  (eCall (tInt ~> tInt ~> tInt, "add-1") [eLit (PInt 2)])
--                                  ( eOp2
--                                      oAddInt
--                                      ( eCall
--                                          ((tInt ~> tInt) ~> tInt ~> tInt, "id-3")
--                                          [ eVar (tInt ~> tInt, "add2")
--                                          , eCall (tInt ~> tInt, "id-4") [eLit (PInt 3)]
--                                          ]
--                                      )
--                                      (eCall (tInt ~> tInt ~> tInt, "add-2") [eLit (PInt 4), eLit (PInt 5)])
--                                  )
--                              )
--                          )
--                      )
--                  )
--              )
--          )
--        ,
--          ( (Scheme ((tInt ~> tInt) ~> tInt ~> tInt), "$lam1")
--          , Function
--              (fromList [(tInt ~> tInt, "x"), (tInt, "$v0")])
--              ( tInt
--              , eCall (tInt ~> tInt, "x") [eVar (tInt, "$v0")]
--              )
--          )
--        ,
--          ( (Scheme (tInt ~> tInt), "$lam2")
--          , Function
--              (fromList [(tInt, "x")])
--              ( tInt
--              , eVar (tInt, "x")
--              )
--          )
--        ,
--          ( (Scheme (tInt ~> tInt ~> tInt), "$lam3")
--          , Function
--              (fromList [(tInt, "x"), (tInt, "y")])
--              ( tInt
--              , eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))
--              )
--          )
--        ,
--          ( (Scheme (tInt ~> tInt ~> tInt), "$lam4")
--          , Function
--              (fromList [(tInt, "x"), (tInt, "$v0")])
--              ( tInt
--              , eCall (tInt ~> tInt ~> tInt, "$lam3") [eVar (tInt, "x"), eVar (tInt, "$v0")]
--              )
--          )
--        ,
--          ( (Scheme (tInt ~> tInt ~> tInt), "$lam5")
--          , Function
--              (fromList [(tInt, "x"), (tInt, "y")])
--              ( tInt
--              , eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))
--              )
--          )
--        ,
--          ( (Scheme (tInt ~> tInt ~> tInt), "$lam6")
--          , Function
--              (fromList [(tInt, "x"), (tInt, "$v0")])
--              ( tInt
--              , eCall (tInt ~> tInt ~> tInt, "$lam5") [eVar (tInt, "x"), eVar (tInt, "$v0")]
--              )
--          )
--        ]
--    )
--
----
---- def main(a : unit) : int =
----   let
----     id-3 =                          : (int ~> int) ~> int ~> int
----       $lam1
----     in
----       let
----         id-4 =
----           $lam2
----         in
----           let
----             add-1 =
----               $lam3
----             in
----               let
----                 add-2 =
----                   $lam4
----                 in
----                   let
----                     add2 =               : int -> int
----                       add-1(2)
----                     in
----                       (id-3(add2))($var_id-4(3)) + $var_add-2(4, 5)
----
---- def $lam1(x : int -> int, $v0 : int) : int =
----   x($v0)
----
---- def $lam2(x : int) : int =
----   x
----
---- def $lam3(x : int, y : int) : int =
----   x + y
----
---- def $lam4(x : int, y : int) : int =
----   x + y
----
--
-- program11 :: Module MonoType Ast
-- program11 =
--  Module
--    ( Map.fromList
--        [
--          ( (Scheme (tUnit ~> tInt), "main")
--          , Function
--              (fromList [(tUnit, "a")])
--              ( tInt
--              , eLet
--                  ((tInt ~> tInt) ~> tInt ~> tInt, "id-3")
--                  (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "$lam1"))
--                  ( eLet
--                      (tInt ~> tInt, "id-4")
--                      (eVar (tInt ~> tInt, "$lam2"))
--                      ( eLet
--                          (tInt ~> tInt ~> tInt, "add-1")
--                          (eVar (tInt ~> tInt ~> tInt, "$lam3"))
--                          ( eLet
--                              (tInt ~> tInt ~> tInt, "add-2")
--                              (eVar (tInt ~> tInt ~> tInt, "$lam4"))
--                              ( eLet
--                                  (tInt ~> tInt, "add2")
--                                  (eCall (tInt ~> tInt ~> tInt, "add-1") [eLit (PInt 2)])
--                                  ( eOp2
--                                      oAddInt
--                                      ( eCall
--                                          ((tInt ~> tInt) ~> tInt ~> tInt, "id-3")
--                                          [ eVar (tInt ~> tInt, "add2")
--                                          , eCall (tInt ~> tInt, "id-4") [eLit (PInt 3)]
--                                          ]
--                                      )
--                                      (eCall (tInt ~> tInt ~> tInt, "add-2") [eLit (PInt 4), eLit (PInt 5)])
--                                  )
--                              )
--                          )
--                      )
--                  )
--              )
--          )
--        ,
--          ( (Scheme ((tInt ~> tInt) ~> tInt ~> tInt), "$lam1")
--          , Function
--              (fromList [(tInt ~> tInt, "x"), (tInt, "$v0")])
--              ( tInt
--              , eCall (tInt ~> tInt, "x") [eVar (tInt, "$v0")]
--              )
--          )
--        ,
--          ( (Scheme (tInt ~> tInt), "$lam2")
--          , Function
--              (fromList [(tInt, "x")])
--              ( tInt
--              , eVar (tInt, "x")
--              )
--          )
--        ,
--          ( (Scheme (tInt ~> tInt ~> tInt), "$lam3")
--          , Function
--              (fromList [(tInt, "x"), (tInt, "y")])
--              ( tInt
--              , eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))
--              )
--          )
--        ,
--          ( (Scheme (tInt ~> tInt ~> tInt), "$lam4")
--          , Function
--              (fromList [(tInt, "x"), (tInt, "y")])
--              ( tInt
--              , eOp2 oAddInt (eVar (tInt, "x")) (eVar (tInt, "y"))
--              )
--          )
--        ]
--    )
