{-# LANGUAGE OverloadedStrings #-}

module Pong.TestData.AnEnvelopeForJohnStJohn where

import Pong.Data
import Pong.Lang

expr0 :: SourceExpr
expr0 =
 eLet
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

expr1 :: TypedExpr
expr1 =
 eLet
   (tVar 3 ~> tVar 3, "id")
   (eLam () [(tVar 3, "x")] (eVar (tVar 3, "x")))
   ( eLet
       (tVar 7 ~> tVar 7 ~> tVar 7, "add")
       ( eLam
           ()
           [(tVar 7, "x")]
           ( eLam
               ()
               [(tVar 7, "y")]
               ( eOp2
                   (tVar 7 ~> tVar 7 ~> tVar 7, OAdd)
                   (eVar (tVar 7, "x"))
                   (eVar (tVar 7, "y"))
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

-- expr2 :: TypedExpr
-- expr2 =
--  eLet
--    (tVar 0 ~> tVar 0, "id")
--    (eLam () [(tVar 0, "x")] (eVar (tVar 0, "x")))
--    ( eLet
--        (tVar 1 ~> tVar 1 ~> tVar 1, "add")
--        ( eLam
--            ()
--            [(tVar 1, "x")]
--            ( eLam
--                ()
--                [(tVar 1, "y")]
--                ( eOp2
--                    (tVar 1 ~> tVar 1 ~> tVar 1, OAdd)
--                    (eVar (tVar 1, "x"))
--                    (eVar (tVar 1, "y"))
--                )
--            )
--        )
--        ( eLet
--            (tInt ~> tInt, "add2")
--            (eApp (tInt ~> tInt) (eVar (tInt ~> tInt ~> tInt, "add")) [eLit (PInt 2)])
--            ( eOp2
--                oAddInt
--                ( eApp
--                    tInt
--                    (eApp (tInt ~> tInt) (eVar ((tInt ~> tInt) ~> tInt ~> tInt, "id")) [eVar (tInt ~> tInt, "add2")])
--                    [ eApp tInt (eVar (tInt ~> tInt, "id")) [eLit (PInt 3)]
--                    ]
--                )
--                (eApp tInt (eVar (tInt ~> tInt ~> tInt, "add")) [eLit (PInt 4), eLit (PInt 5)])
--            )
--        )
--    )
