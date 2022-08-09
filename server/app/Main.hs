import Lib
import Options (Option (..), runOptionParser)
import qualified Options.Applicative as Opt

getConfig :: IO ServerConfig
getConfig = runOptionParser $ \envOption ->
  ServerConfig
    <$> envOption
      ( Option
          { optHelp = "Path to the nix program database.",
            optMetaVar = "PATH",
            optLong = "program-db",
            optShort = 'd',
            optEnvKey = "NIX_PROGRAM_DB",
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
            optEnvKey = "APP_DB",
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
            optEnvKey = "PORT",
            optReadM = Opt.auto,
            optDefault = 8081,
            optShow = show
          }
      )

main :: IO ()
main = getConfig >>= mainWithConfig
