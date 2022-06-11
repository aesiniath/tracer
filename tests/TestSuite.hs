{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (finally)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = do
    finally (hspec suite) (putStrLn ".")

suite :: Spec
suite = do
    describe "This thing" $ do
        it "does what it is supposed to" $ do
            True `shouldBe` True
