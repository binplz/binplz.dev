{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}

module Lib (ServerConfig (..), ProgramDB (..), ApplicationDB (..), mainWithConfig) where

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (concurrently)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Char (isHexDigit)
import Data.Coerce (coerce)
import Data.Either (isLeft)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Time.Clock.System as Time
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField (FromField (..), Field, returnError, fieldData)
import Database.SQLite.Simple.ToField (ToField (..))
import Database.SQLite.Simple.Ok (Ok)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import Network.Wai.Handler.Warp (Port, run)
import Servant
import qualified System.Directory as Dir
import System.FilePath ((</>))
import System.IO (Handle)
import qualified System.IO as IO
import qualified System.Process as Proc

type GetBinaryWithOptions = QueryParam "sys" Platform :> QueryParam "nixpkgs-commit" NixpkgsCommit :> Get '[OctetStream] BSL.ByteString

-- | Top-level API. Examples:
--   localhost:8081/ps
--   localhost:8081/procps/ps?sys=aarch64-linux
--  TODO API documentation
type API =
  Get '[PlainText] NoContent
    :<|> Capture "binary-name" BinaryName :> GetBinaryWithOptions
    :<|> Capture "package-name" PackageName :> Capture "binary-name" BinaryName :> GetBinaryWithOptions

newtype BinaryName = BinaryName {unBinaryName :: String}
  deriving newtype (Show, ToField, FromHttpApiData)

newtype PackageName = PackageName {unPackageName :: String}
  deriving newtype (Eq, Show, Ord, FromField, FromHttpApiData, ToField)

data Platform
  = X86_64_Linux
  | AARCH_64_Linux
  | ARMV7_Linux

instance Show Platform where
  show X86_64_Linux = "x86_64-linux"
  show AARCH_64_Linux = "aarch64-linux"
  show ARMV7_Linux = "armv7l-linux"

instance FromHttpApiData Platform where
  parseQueryParam "aarch64-linux" = pure AARCH_64_Linux
  parseQueryParam "x86_64-linux" = pure X86_64_Linux
  parseQueryParam "armv7l-linux" = pure ARMV7_Linux
  parseQueryParam sys = Left $ "Unknown platform: " <> sys

newtype NixpkgsCommit = NixpkgsCommit { unNixpkgsCommit :: Text }
  deriving newtype (Show, ToField)

instance FromField NixpkgsCommit where
  fromField :: Field -> Ok NixpkgsCommit
  fromField rawCommitField =
    case fieldData rawCommitField of
      SQLText rawCommit ->
        either
          (returnError ConversionFailed rawCommitField . Text.unpack)
          pure
          (mkNixpkgsCommit rawCommit)
      _ -> returnError Incompatible rawCommitField "NixpkgsCommit field is not SQLText"

mkNixpkgsCommit :: Text -> Either Text NixpkgsCommit
mkNixpkgsCommit rawCommit
  | Text.length rawCommit < 3 = Left "too short. must be at least than 3 characters"
  | Text.length rawCommit > 40 = Left "too long. must be at most 40 characters"
  | not (Text.all isHexDigit rawCommit) = Left "can only consist of hexidecimal characters (0-9 and a-e)"
  | otherwise = Right $ NixpkgsCommit rawCommit

instance FromHttpApiData NixpkgsCommit where
  parseQueryParam = mkNixpkgsCommit

-- | A BinInfo uniquely identifies a binary in nixpkgs.
-- A BinInfo is not a proof that that binary exists and is buildable, though.
data BinInfo = BinInfo
  { _tripBinary :: BinaryName,
    _tripPackage :: PackageName,
    _tripPlatform :: Platform,
    _tripCommit :: NixpkgsCommit
  }
  deriving (Show)

newtype ApplicationDB = ApplicationDB {unApplicationDB :: FilePath}
  deriving newtype (Show)

newtype ProgramDB = ProgramDB {unProgramDB :: FilePath}
  deriving newtype (Show)

-- TODO make address/port configurable
-- TODO read config parameters from the command line
-- TODO Make nix sources configurable?
data ServerConfig = ServerConfig
  { _programDB :: ProgramDB,
    _applicationDB :: ApplicationDB,
    _port :: Port,
    _logdir :: FilePath
  }
  deriving (Show)

-- | Serve the API
-- This is a matter of
--   1. Filling in missing package/platform parameters to construct a proper 'Triplet'
--   2. Building the triplet
-- If an error occurs, this is returned as a 400
server :: ServerConfig -> Server API
server (ServerConfig programDB appDB _ logdir) =
  redirectToDocs
    :<|> (\bin -> handle bin Nothing)
    :<|> (\pkg bin -> handle bin (Just pkg))
  where
    handle :: BinaryName -> Maybe PackageName -> Maybe Platform -> Maybe NixpkgsCommit -> Handler BSL.ByteString
    handle bin mpkg msys mcommit = Handler $ do
      mbin <- liftIO . runExceptT $ do
        commit <- resolveCommit mcommit
        let sys = fromMaybe X86_64_Linux msys
        pkg <- maybe (resolvePackageName programDB bin sys) pure mpkg
        buildTriplet appDB (BinInfo bin pkg sys commit)
      liftIO $ do
        time <- Time.systemSeconds <$> Time.getSystemTime
        Dir.createDirectoryIfMissing True logdir
        let logstring =
              Aeson.encode . Aeson.object $
                [ "time" .= time,
                  "binary" .= unBinaryName bin,
                  "package" .= maybe Aeson.Null (Aeson.String . Text.pack . unPackageName) mpkg,
                  "platform" .= maybe Aeson.Null (Aeson.String . Text.pack . show) msys,
                  "commit" .= maybe Aeson.Null (Aeson.String . unNixpkgsCommit) mcommit,
                  "result" .= either (Aeson.String . Text.pack) (const "success") mbin
                ]
        Lazy.appendFile (logdir </> "requests.log") (logstring <> "\n")
      case mbin of
        Left err -> throwError (err400 {errBody = BSL.pack err})
        Right ok -> pure ok

-- | The default Nixpkgs commit we use when the user doesn't provide one.
--
-- This is nixos-22.05 as of 2022-08-29.
defaultNixpkgsCommit :: NixpkgsCommit
defaultNixpkgsCommit = NixpkgsCommit "0ba2543f8c855d7be8e90ef6c8dc89c1617e8a08"

-- | Figure out what 'NixpkgsCommit' to use based on what the user passes in.
--
-- If the user doesn't specify a 'NixpkgsCommit', then just use our
-- 'defaultNixpkgsCommit'.
--
-- If the user does specify a 'NixpkgsCommit', make sure it is a prefix of
-- our 'defaultNixpkgsCommit', and then just use 'defaultNixpkgsCommit'.
--
-- TODO: At some point, we'd like to lift the restriction on what commits an
-- end-user can specify.
resolveCommit :: Maybe NixpkgsCommit -> ExceptT String IO NixpkgsCommit
resolveCommit Nothing = pure defaultNixpkgsCommit
resolveCommit (Just (NixpkgsCommit rawCommit)) = do
  unless (Text.isPrefixOf rawCommit $ unNixpkgsCommit defaultNixpkgsCommit) $
    throwError $ "nixpkgs-commit parameter value " <> Text.unpack rawCommit <> " is not allowed"
  pure defaultNixpkgsCommit

redirectToDocs :: forall a. Handler a
redirectToDocs = throwError err301 {errHeaders = [("Location", "https://docs.binplz.dev")]}

-- | Check the Nix programs database for a package that provides the given binary
-- If there is a candidate package with the same name as the binary, that package is given priority.
-- If not, the alphabetically first package is chosen.
resolvePackageName :: ProgramDB -> BinaryName -> Platform -> ExceptT String IO PackageName
resolvePackageName (ProgramDB dbpath) bin sys = do
  pkgs <- liftIO . withConnection dbpath $ \conn ->
    query conn "SELECT package FROM programs WHERE name = ? AND system = ?" (bin, show sys)
  case fmap fromOnly pkgs of
    [] -> throwError $ "No known package provides " <> unBinaryName bin <> " for " <> show sys <> ". Consider manually specifying the package."
    candidates -> pure $ if coerce bin `elem` candidates then coerce bin else minimum candidates

-- | 1. See if we know this BinInfo to be unbuildable
--   2. If buildable, build it
--   3. Bump the count
buildTriplet :: ApplicationDB -> BinInfo -> ExceptT String IO BSL.ByteString
buildTriplet appDB trip@(BinInfo bin pkg sys commit) = withApplicationDB appDB $ \conn -> do
  liftIO . putStrLn $ "At Nixpkgs commit " <> show commit <> " building " <> show trip
  errorFlags :: [Only Bool] <-
    liftIO $
      query
        conn
        "SELECT error FROM binaries WHERE binary = ? AND package = ? AND platform = ? AND nixpkgsCommit = ?"
        (bin, pkg, show sys, commit)
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
          "INSERT INTO binaries (binary, package, platform, nixpkgsCommit, hits, error) VALUES (?, ?, ?, ?, ?, ?)"
          (bin, pkg, show sys, commit, 1 :: Int, isLeft res)
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

-- | Write the data from the input 'Handle' to the output 'Handle' and collect
-- the read bytes into a lazy bytestring.
--
-- A prefix is added to each line to support giving extra context to which process is running like
-- request-42> Waiting for locks on /nix/store/...
teePrefix :: ByteString -> Handle -> Handle -> IO Lazy.ByteString
teePrefix prefix input output = do
  str <- Lazy.hGetContents input
  result <- BSL.fromChunks . concat <$> traverse teeLine (BSL.lines str)
  IO.hClose input
  pure result
  where
    teeLine line = do
      BS.hPut output prefix
      -- add a trailing newline to the line
      let chunks = BSL.toChunks line <> [BS8.singleton '\n']
      traverse (\chunk -> chunk <$ BS.hPut output chunk) chunks

streamCommand :: (Handle -> IO Lazy.ByteString) -> (Handle -> IO Lazy.ByteString) -> FilePath -> [String] -> ExceptT String IO (ExitCode, BSL.ByteString, BSL.ByteString)
streamCommand onStdout onStderr cmd args = do
  let process =
        (Proc.proc cmd args)
          { Proc.std_in = Proc.CreatePipe,
            Proc.std_out = Proc.CreatePipe,
            Proc.std_err = Proc.CreatePipe
          }
  ExceptT $
    liftIO $
      Proc.withCreateProcess process $ \minh mouth merrh processHandle -> do
        case (minh, mouth, merrh) of
          (Just inh, Just outh, Just errh) -> do
            IO.hClose inh
            (stdout, stderr) <- concurrently (onStdout outh) (onStderr errh)
            exit <- Proc.waitForProcess processHandle
            pure $ pure (exit, stdout, stderr)
          _ -> pure $ throwError "couldn't read process output"

nixBuild :: BinInfo -> IO (Either String BSL.ByteString)
nixBuild (BinInfo bin pkg sys commit) = runExceptT $ do
  currThreadId <- liftIO myThreadId
  let binLogPrefix =
        unBinaryName bin <> "," <>
        unPackageName pkg <> "," <>
        show sys <> "," <>
        take 7 (Text.unpack $ unNixpkgsCommit commit) <> "," <>
        show currThreadId
  -- TODO stream build process to stderr
  -- TODO make sure we can't get any XSS-like shenanigans
  (exit, stdout, stderr) <-
    streamCommand
      (\out -> teePrefix (fromString $ "out: " <> binLogPrefix <> "> ") out IO.stdout)
      (\err -> teePrefix (fromString $ "err: " <> binLogPrefix <> "> ") err IO.stderr)
      "nix"
      ( ["build", "github:NixOS/nixpkgs/" <> Text.unpack (unNixpkgsCommit commit) <> "#legacyPackages." <> show sys <> ".pkgsStatic." <> unPackageName pkg]
          -- Don't make a result symlink, just print it to stdout
          <> ["--no-link", "--print-out-paths"]
          -- Print build logs to stderr
          <> ["--print-build-logs"]
          -- Only allow access to files from NIX_PATH. Derivations can't read files like `/etc/shadow`.
          <> ["--option", "restrict-eval", "true"]
          -- Disallow IFD.  Probably not necessary, since Nixpkgs doesn't contain derivations that do IFD, but better to be on the safe side.
          <> ["--option", "allow-import-from-derivation", "false"]
          -- Make sure the sandbox is always used.
          <> ["--option", "sandbox-fallback", "false"]
      )
  liftIO . putStrLn $
    unlines
      [ "Finished a Nix build call",
        "Exit code: " <> show exit
      ]
  case (exit, BSL.lines stdout) of
    (ExitSuccess, [result]) -> do
      let fullPath = UTF8.toString result </> "bin" </> unBinaryName bin
      liftIO . putStrLn $ "Successfully built " <> fullPath
      fileExists <- liftIO $ Dir.doesFileExist fullPath
      unless fileExists $ throwError "Package did not produce expected binary"
      isExecutable <- liftIO $ Dir.executable <$> Dir.getPermissions fullPath
      unless isExecutable $ throwError "Binary specified by the triplet was not an executable"
      liftIO $ BSL.readFile fullPath
    _ -> throwError $ unlines ["An error occurred.", "  Exit code: " <> show exit, "  error:" <> UTF8.toString stderr]

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
      \ , platform TEXT NOT NULL \
      \ , nixpkgsCommit TEXT NOT NULL \
      \ , hits     INTEGER NOT NULL \
      \ , error    BOOLEAN NOT NULL \
      \ , PRIMARY KEY (binary, package, platform, nixpkgsCommit) )"
    runExceptT (k conn)

-- TODO multithreading
mainWithConfig :: ServerConfig -> IO ()
mainWithConfig config = do
  putStrLn "Starting binplz-server with config:"
  print config
  run (_port config) (serve (Proxy @API) (server config))
