{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Language.Haskell.Tools.Parser.ProgramOptions where

import           Options.Applicative

data Options =
  Options
    { optCommand :: ParseConfig
    }

data ASTRefactors = FunctionDependency | LetRefactoring
  deriving (Show)

data ParseConfig = Refact ASTParseConfig | SplitAndWrite WriteFileConfig

data ASTParseConfig =
  ASTParseConfig
    { modulePath      :: String
    , moduleName      :: String
    , functionality   :: ASTRefactors
    }
  deriving (Show)


data WriteFileConfig =
  WriteFileConfig
    { funDeps      :: String
    , clusters     :: String
    , modPath      :: String
    , newModName   :: String
    , modName      :: String
    }
  deriving (Show)


opts :: Parser ParseConfig
opts =
      subparser (command "getFunDeps" $ (info (parserConfig FunctionDependency) (progDesc "Get the function dependency graph of a module")))
  <|> subparser (command "letRefactor" $ (info (parserConfig FunctionDependency) (progDesc "Multiple Let Refactoring")))
  <|> subparser (command "writeModSplits" $ (info writeFileConfig (progDesc "Split the grouped functions into modules")))

parserConfig :: ASTRefactors -> Parser ParseConfig
parserConfig refactor =
      Refact
  <$> (ASTParseConfig
  <$> (strOption (long "module-path" <> short 'p' <> help "Path of module to be split"))
  <*> (strOption (long "module-name" <> short 'o' <> help "Module Name"))
  <*> (pure refactor))

writeFileConfig :: Parser ParseConfig
writeFileConfig =
  SplitAndWrite
  <$> (WriteFileConfig
  <$> (strOption (long "fun-deps" <> short 'l' <> help "List of function dependencies"))
  <*> (strOption (long "groups" <> short 'g' <> help "Grouped Functions list"))
  <*> (strOption (long "module-path" <> short 'p' <> help "Path of module to be split"))
  <*> (strOption (long "new-die" <> short 'o' <> help "New directory name for the module"))
  <*> (strOption (long "module-name" <> short 'o' <> help "Module Name")))
