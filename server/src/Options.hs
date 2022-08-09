{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A wrapper around optparse-applicative for dealing with variables that can be read from the environment.
-- It's currently very rigid (it only works if your data type is a product of options), but we can add features as necessary.
module Options (Option (..), runOptionParser) where

import Control.Applicative
import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help.Pretty as Opt
import qualified Options.Applicative.Types as Opt
import qualified System.Environment as System

data Option a = Option
  { optHelp :: String,
    optMetaVar :: String,
    optLong :: String,
    optShort :: Char,
    optEnvKey :: String,
    optReadM :: Opt.ReadM a,
    optDefault :: a,
    optShow :: a -> String
  }

runOptionParser ::
  forall a.
  ((forall b. Option b -> Opt.Parser b) -> Opt.Parser a) ->
  IO a
runOptionParser desc = do
  env <- Map.fromList <$> System.getEnvironment
  Opt.execParser (Opt.info (parser env Opt.<**> Opt.helper) mempty)
  where
    runReadM :: forall r. Opt.ReadM r -> String -> Maybe r
    runReadM (Opt.ReadM f) = either (const Nothing) Just . runExcept . runReaderT f

    parser :: Map String String -> Opt.Parser a
    parser env = desc $ \(Option help meta long short envKey readM def showFn) ->
      let fromEnv = Map.lookup envKey env >>= runReadM readM
          doc =
            Opt.vsep
              [ Opt.text help,
                Opt.indent 2 . Opt.text $
                  "Environment variable: " <> envKey <> maybe " (not set)" (\a -> " (" <> showFn a <> ")") fromEnv,
                Opt.indent 2 . Opt.text $
                  "Default: " <> showFn def
              ]
       in Opt.option
            readM
            ( Opt.long long
                <> Opt.short short
                <> Opt.metavar meta
                <> Opt.helpDoc (Just doc)
            )
            <|> pure (fromMaybe def fromEnv)
