{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Heist.Splices.I18nSpec (spec) where

import Test.Hspec ( shouldBe, it, describe, Spec )
import Heist
    ( defaultInterpretedSplices,
      emptyHeistConfig,
      initHeist,
      loadTemplates,
      hcErrorNotBound,
      hcNamespace,
      hcSpliceConfig,
      scInterpretedSplices,
      scLoadTimeSplices,
      scTemplateLocations,
      HeistState )
import Heist.Splices.I18n
    ( i18nTag, i18nTranslate, i18nDumb, i18nGenPOT )
import System.Directory ( createDirectoryIfMissing )
import Control.Lens ( (&), (.~) )
import Control.Monad.Trans.Except ( ExceptT(ExceptT), runExceptT )
import Data.Map.Syntax ( (##) )
import qualified Heist.Interpreted as I
import qualified Data.ByteString.Lazy as B
import System.FilePath.Posix ( takeDirectory )
import Data.Maybe ( fromMaybe )
import Data.Binary.Builder ( toLazyByteString )
import Data.Text.I18n.Po ( getL10n )
import qualified Data.Map as Map
import Data.IORef (newIORef, readIORef, IORef)
import Data.Text.I18n.Types (CtxMap, Msgid(..))
import Data.ByteString (ByteString)


deriving instance Read Msgid

spec :: Spec
spec = do
  describe "Test I18n Dumb" $ do
    it "Simple Dumb" $ do
      hs <- loadDumb "./test/testcase/case1"
      genPage hs "case1" "./test/testcase/case1/case1.result"
      a <- cmpFiles "./test/testcase/case1/case1.result" "./test/testcase/case1/case1.expect"
      a `shouldBe` True

    it "Simple Dumb 2" $ do
      hs <- loadDumb "./test/testcase/case2"
      genPage hs "case2" "./test/testcase/case2/case2.result"
      a <- cmpFiles "./test/testcase/case2/case2.result" "./test/testcase/case2/case2.expect"
      a `shouldBe` True

  describe "Test I18n Translate" $ do
    it "Simple Translate" $ do
      hs <- loadTranslate "./test/testcase/case3"
      genPage hs "case3" "./test/testcase/case3/case3.result"
      a <- cmpFiles "./test/testcase/case3/case3.result" "./test/testcase/case3/case3.expect"
      a `shouldBe` True

    it "Translate with Context" $ do
      hs <- loadTranslate "./test/testcase/case4"
      genPage hs "case4" "./test/testcase/case4/case4.result"
      a <- cmpFiles "./test/testcase/case4/case4.result" "./test/testcase/case4/case4.expect"
      a `shouldBe` True

  describe "Test I18n Gen POT" $ do
    it "Generate CtxMap" $ do
      spd <- newIORef Map.empty
      hs <- loadGenPot spd "./test/testcase/case5"
      genPageDumb hs "case5"
      genPageDumb hs "case5_1"
      result <- readIORef spd
      expect <- readFile "./test/testcase/case5/case5.expect"
      (result == read expect) `shouldBe` True


cmpFiles :: FilePath -> FilePath -> IO Bool
cmpFiles a b = do
    aContents <- readFile a
    bContents <- readFile b
    return (aContents == bContents)


loadDumb :: FilePath -> IO (HeistState IO)
loadDumb baseDir = do
    tmap <- runExceptT $ do
        let sc = mempty & scLoadTimeSplices .~ (do
                                                defaultInterpretedSplices
                                                )
                        & scInterpretedSplices .~ (do
                                        i18nTag ## i18nDumb
                                     )
                        & scTemplateLocations .~ [loadTemplates baseDir]
        ExceptT $ initHeist $ emptyHeistConfig & hcNamespace .~ ""
                                     & hcErrorNotBound .~ False
                                     & hcSpliceConfig .~ sc
    either (error . concat) return tmap

loadTranslate :: [Char] -> IO (HeistState IO)
loadTranslate baseDir = do
    (l10n, _) <- getL10n "./test/testcase/locale"
    tmap <- runExceptT $ do
        let sc = mempty & scLoadTimeSplices .~ (do
                                                defaultInterpretedSplices
                                                )
                        & scInterpretedSplices .~ (do
                                        i18nTag ## i18nTranslate l10n "zh"
                                     )
                        & scTemplateLocations .~ [loadTemplates baseDir]
        ExceptT $ initHeist $ emptyHeistConfig & hcNamespace .~ ""
                                     & hcErrorNotBound .~ False
                                     & hcSpliceConfig .~ sc
    either (error . concat) return tmap

loadGenPot :: IORef CtxMap -> FilePath -> IO (HeistState IO)
loadGenPot simplepotData baseDir = do
    tmap <- runExceptT $ do
        let sc = mempty & scLoadTimeSplices .~ (do
                                                defaultInterpretedSplices
                                                )
                        & scInterpretedSplices .~ (do
                                        i18nTag ## i18nGenPOT simplepotData
                                     )
                        & scTemplateLocations .~ [loadTemplates baseDir]
        ExceptT $ initHeist $ emptyHeistConfig & hcNamespace .~ ""
                                     & hcErrorNotBound .~ False
                                     & hcSpliceConfig .~ sc
    either (error . concat) return tmap


genPage :: HeistState IO -> ByteString -> FilePath -> IO ()
genPage hs template_name target = do
    runtime <- I.renderTemplate hs template_name
    let b = fst $ fromMaybe (error "should not fail") runtime
    createDirectoryIfMissing True $ takeDirectory target
    B.writeFile target $ toLazyByteString b

genPageDumb :: HeistState IO -> ByteString -> IO ()
genPageDumb hs template_name = do
    runtime <- I.renderTemplate hs template_name
    let !_ = fromMaybe (error "should not fail") runtime
    return ()