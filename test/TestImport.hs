module TestImport
    ( module TestImport
    , module ClassyPrelude
    , module Foundation
    , module Test.Hspec
    , module Yesod.Test
    ) where

import Application           (makeFoundation, makeLogWare)
import ClassyPrelude hiding  (Handler)
import Foundation            
import Test.Hspec            
import Yesod.Default.Config2 (ignoreEnv, loadYamlSettings)
import Yesod.Test            

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
    settings <- loadYamlSettings
        ["config/test-settings.yml", "config/settings.yml"]
        []
        ignoreEnv
    foundation <- makeFoundation settings
    logWare <- liftIO $ makeLogWare foundation
    return (foundation, logWare)
