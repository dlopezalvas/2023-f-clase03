module Spec where
import PdePreludat
import Clase03
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      1 `shouldBe` 1

