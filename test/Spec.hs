{-# LANGUAGE OverloadedStrings #-}

import Pong.Data
import Pong.Lang
import Pong.Type
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Pong.Type" $ do
      describe "tagExpr" $ do
        let source :: SourceExpr
            source =
              eLet
                ((), "z")
                ( eLam
                    ()
                    [((), "f")]
                    ( eLam
                        ()
                        [((), "g")]
                        ( eLam
                            ()
                            [((), "b")]
                            ( eApp
                                ()
                                (eApp () (eVar ((), "f")) [eLam () [((), "x")] (eVar ((), "x"))])
                                [eApp () (eVar ((), "g")) [eVar ((), "b")]]
                            )
                        )
                    )
                )
                ( eApp
                    ()
                    (eVar ((), "z"))
                    [ eLam () [((), "x")] (eVar ((), "x"))
                    , eLam () [((), "x")] (eVar ((), "x"))
                    , eLit (PInt 1)
                    ]
                )
            tagged :: TaggedExpr
            tagged =
              eLet
                (1, "z")
                ( eLam
                    ()
                    [(2, "f")]
                    ( eLam
                        ()
                        [(3, "g")]
                        ( eLam
                            ()
                            [(4, "b")]
                            ( eApp
                                5
                                (eApp 6 (eVar (7, "f")) [eLam () [(8, "x")] (eVar (9, "x"))])
                                [eApp 10 (eVar (11, "g")) [eVar (12, "b")]]
                            )
                        )
                    )
                )
                ( eApp
                    13
                    (eVar (14, "z"))
                    [ eLam () [(15, "x")] (eVar (16, "x"))
                    , eLam () [(17, "x")] (eVar (18, "x"))
                    , eLit (PInt 1)
                    ]
                )
         in it "001" (Right tagged == typeCheck (tagExpr source))

typeCheck :: TypeChecker a -> Either TypeError a
typeCheck = evalTypeChecker 1 mempty
