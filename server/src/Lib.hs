{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Lib (app) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, withExceptT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Coerce (coerce)
import Data.Either (isLeft)
import Data.Maybe (fromMaybe)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.IO.Exception (ExitCode (ExitSuccess))
import Network.Wai.Handler.Warp (run)
import Servant
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified System.Process as Proc

-- | Top-level API. Examples:
--   localhost:8081/ps
--   localhost:8081/procps/ps?sys=aarch64-linux
--  TODO API documentation
type API =
  Get '[JSON] NoContent
    :<|> Capture "binary-name" BinaryName :> QueryParam "sys" Platform :> Get '[OctetStream] ByteString
    :<|> Capture "package-name" PackageName :> Capture "binary-name" BinaryName :> QueryParam "sys" Platform :> Get '[OctetStream] ByteString

newtype BinaryName = BinaryName {unBinaryName :: String}
  deriving newtype (ToField, FromHttpApiData)

newtype PackageName = PackageName {unPackageName :: String}
  deriving newtype (Eq, Ord, FromField, FromHttpApiData, ToField)

data Platform
  = X86_64_Linux
  | AARCH_64_Linux

instance Show Platform where
  show X86_64_Linux = "x86_64-linux"
  show AARCH_64_Linux = "aarch64-linux"

instance FromHttpApiData Platform where
  parseQueryParam "aarch64-linux" = pure AARCH_64_Linux
  parseQueryParam "x86_64-linux" = pure X86_64_Linux
  parseQueryParam sys = Left $ "Unknown platform: " <> sys

-- | A BinaryTriplet uniquely identifies a binary in nixpkgs.
-- A BinaryTriplet is not a proof that that binary exists and is buildable, though.
data BinaryTriplet = BinaryTriplet
  { _tripBinary :: BinaryName,
    _tripPackage :: PackageName,
    _tripPlatform :: Platform
  }

newtype ApplicationDB = ApplicationDB FilePath

newtype ProgramDB = ProgramDB FilePath

-- TODO make address/port configurable
-- TODO read config parameters from the command line
-- TODO Make nix sources configurable?
data ServerConfig = ServerConfig
  { _programDB :: ProgramDB,
    _binaryDB :: ApplicationDB
  }

-- | Serve the API
-- This is a matter of
--   1. Filling in missing package/platform parameters to construct a proper 'Triplet'
--   2. Building the triplet
-- If an error occurs, this is returned as a 400
server :: ServerConfig -> Server API
server (ServerConfig programDB appDB) =
  pure NoContent
    :<|> (\bin -> handle bin Nothing)
    :<|> (\pkg bin -> handle bin (Just pkg))
  where
    handle :: BinaryName -> Maybe PackageName -> Maybe Platform -> Handler ByteString
    handle bin mpkg msys =
      Handler . withExceptT (\err -> err400 {errBody = BSL.pack err}) $ do
        let sys = fromMaybe X86_64_Linux msys
        pkg <- maybe (resolvePackageName programDB bin sys) pure mpkg
        buildTriplet appDB (BinaryTriplet bin pkg sys)

-- | Check the Nix programs database for a package that provides the given binary
-- If there is a candidate package with the same name as the binary, that package is given priority.
-- If not, the alphabetically first package is chosen.
resolvePackageName :: ProgramDB -> BinaryName -> Platform -> ExceptT String IO PackageName
resolvePackageName (ProgramDB dbpath) bin sys = do
  pkgs <- liftIO . withConnection dbpath $ \conn ->
    query conn "SELECT package FROM programs WHERE binary = ? AND platform = ?" (bin, show sys)
  case fmap fromOnly pkgs of
    [] -> throwError $ "No known package provides " <> unBinaryName bin <> " for " <> show sys <> ". Consider manually specifying the package."
    candidates -> pure $ if coerce bin `elem` candidates then coerce bin else minimum candidates

-- | 1. See if we know this BinaryTriplet to be unbuildable
--   2. If buildable, build it
--   3. Bump the count
buildTriplet :: ApplicationDB -> BinaryTriplet -> ExceptT String IO ByteString
buildTriplet appDB trip@(BinaryTriplet bin pkg sys) = withApplicationDB appDB $ \conn -> do
  errorFlags :: [Only Bool] <- liftIO $ query conn "SELECT error FROM binaries WHERE binary = ? AND package = ? AND platform = ?" (bin, pkg, show sys)
  case errorFlags of
    [Only True] -> do
      bump conn
      -- TODO
      -- Right now, upon a cached failure, we can't tell the user any more than this.
      -- Maybe we should store some more information about where the error came from, or a part of the nix log?
      throwError "Known failure"
    [] -> do
      -- TODO thread safety issue: we have an unguarded insert with a primary key, this throws an exception if in the meantime someone else makes the same request
      -- TODO maybe just use an atomic SQL transaction is enough?
      res <- liftIO $ nixBuild trip
      liftIO $
        execute
          conn
          "INSERT INTO binaries (binary, package, platform, hits, error) VALUES (?,?,?,?,?)"
          (bin, pkg, show sys, 1 :: Int, isLeft res)
      ExceptT $ pure res -- equiv to either throwError pure
    [Only False] -> do
      -- TODO Don't invoke nix?
      res <- liftIO $ nixBuild trip
      case res of
        Left _ -> error "what to do" -- TODO
        Right bs -> bs <$ bump conn
    _ : _ : _ -> error "impossible"
  where
    -- TODO is this the right way to do thread safety? or should we take more care on the application side?
    bump conn = liftIO $ execute conn "UPDATE binaries SET hits = hits + 1 WHERE binary = ? AND package = ? AND platform = ?" (bin, pkg, show sys)

nixBuild :: BinaryTriplet -> IO (Either String ByteString)
nixBuild (BinaryTriplet bin pkg sys) = runExceptT $ do
  -- TODO stream build process to stderr
  -- TODO make sure we can't get any XSS-like shenanigans
  (exit, stdout, stderr) <-
    liftIO $
      Proc.readProcessWithExitCode
        "nix"
        ( ["build", "nixpkgs#legacyPackages." <> show sys <> ".pkgsStatic." <> unPackageName pkg]
            -- Don't make a result symlink, just print it to stdout
            <> ["--no-link", "--print-out-paths"]
            -- Only allow access to files from NIX_PATH. Derivations can't read files like `/etc/shadow`.
            <> ["--option", "restrict-eval", "true"]
            -- Disallow IFD.  Probably not necessary, since Nixpkgs doesn't contain derivations that do IFD, but better to be on the safe side.
            <> ["--option", "allow-import-from-derivation", "false"]
            -- Make sure the sandbox is always used.
            <> ["--option", "sandbox-fallback", "false"]
        )
        ""
  liftIO . putStrLn $
    unlines
      [ "Finished a Nix build call",
        "Exit code: " <> show exit,
        "stdout: " <> stdout,
        "stderr: " <> stderr
      ]
  case (exit, lines stdout) of
    (ExitSuccess, [result]) -> do
      let fullPath = result </> "bin" </> unBinaryName bin
      liftIO . putStrLn $ "Successfully built " <> fullPath
      fileExists <- liftIO $ Dir.doesFileExist fullPath
      unless fileExists $ throwError "Package did not produce expected binary"
      isExecutable <- liftIO $ Dir.executable <$> Dir.getPermissions fullPath
      unless isExecutable $ throwError "Binary specified by the triplet was not an executable"
      liftIO $ BS.readFile fullPath
    _ -> throwError $ unlines ["An error occurred.", "  Exit code: " <> show exit, "  error:" <> stderr]

-- TODO rename name field to binary
-- TODO maybe we should save (and even index by) store path?
withApplicationDB :: ApplicationDB -> (Connection -> ExceptT e IO a) -> ExceptT e IO a
withApplicationDB (ApplicationDB path) k = ExceptT $
  withConnection path $ \conn -> do
    execute_
      conn
      " CREATE TABLE IF NOT EXISTS binaries \
      \ ( binary   TEXT NOT NULL \
      \ , package  TEXT NOT NULL \
      \ , platform   TEXT NOT NULL \
      \ , hits     INTEGER NOT NULL \
      \ , error    BOOLEAN NOT NULL \
      \ , PRIMARY KEY (binary, package, platform) )"
    runExceptT (k conn)

-- TODO multithreading
app :: IO ()
app = run 8081 (serve (Proxy @API) (server (ServerConfig progs bins)))
  where
    bins = ApplicationDB "appdb.sqlite"
    progs = ProgramDB "/nix/var/nix/profiles/per-user/root/channels/nixos/programs.sqlite"
