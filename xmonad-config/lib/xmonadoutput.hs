import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List (elemIndex)
import Data.Maybe
import Eww
import System.Environment
import System.IO
import System.Posix.IO
import Text.Read

main = pure ()

data WindowSubCommand = GetName {index :: Int} | GetCurrentStatus {index :: Int} | Title {index :: Int} | List

data Action
    = WindowCommand WindowSubCommand
    | Layout
data Config = Config {backupFile :: String, action :: Action}

defaultConfig :: Config
defaultConfig = Config{backupFile = "/tmp/xmonad-command-backup", action = WindowCommand List}

parseWorkspaceAction :: [String] -> Maybe WindowSubCommand
parseWorkspaceAction ("current" : x : s) = GetCurrentStatus <$> readMaybe x
parseWorkspaceAction ("title" : x : xs) = Title <$> readMaybe x
parseWorkspaceAction ("name" : x : xs) = GetName <$> readMaybe x
parseWorkspaceAction _ = Nothing

parseArgs :: [String] -> Maybe Config
parseArgs ("--backup" : x : xs) = (\cf -> cf{backupFile = x}) <$> parseArgs xs
parseArgs ("ws" : xs) = do
    cf <- parseArgs xs
    wsAction <- parseWorkspaceAction xs
    pure $ cf{action = WindowCommand wsAction}
parseArgs ("workspace" : xs) = parseArgs ("ws" : xs)
parseArgs ("layout" : _) = Just $ defaultConfig{action = Layout}
parseArgs [] = Just defaultConfig
parseArgs _ = Nothing

getConfig :: FilePath -> IO (Maybe OutputState)
getConfig path = do
    handle <- openFile "/tmp/xmonad-status-json" ReadMode
    d <- B.hGetContents handle
    ata <- maybe (B.readFile path listToMaybe) (pure)
    pure $ decode ata
