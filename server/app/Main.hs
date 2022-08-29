import Lib
import Options (Option (..), runOptionParser)
import qualified Options.Applicative as Opt
import qualified System.IO as IO

getConfig :: IO ServerConfig
getConfig = runOptionParser $ \envOption ->
  ServerConfig
    <$> envOption
      ( Option
          { optHelp = "Path to the nix program database.",
            optMetaVar = "PATH",
            optLong = "program-db",
            optShort = 'd',
            optEnvKey = "BINPLZ_NIX_PROGRAM_DB",
            optReadM = ProgramDB <$> Opt.str,
            optDefault = ProgramDB "/nix/var/nix/profiles/per-user/root/channels/nixos/programs.sqlite",
            optShow = unProgramDB
          }
      )
    <*> envOption
      ( Option
          { optHelp = "Path to the application database.",
            optMetaVar = "PATH",
            optLong = "app-db",
            optShort = 'a',
            optEnvKey = "BINPLZ_APPLICATION_DB",
            optReadM = ApplicationDB <$> Opt.str,
            optDefault = ApplicationDB "appdb.sqlite",
            optShow = unApplicationDB
          }
      )
    <*> envOption
      ( Option
          { optHelp = "Port to listen on.",
            optMetaVar = "INT",
            optLong = "port",
            optShort = 'p',
            optEnvKey = "BINPLZ_PORT",
            optReadM = Opt.auto,
            optDefault = 8081,
            optShow = show
          }
      )

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  cfg <- getConfig
  mainWithConfig cfg
