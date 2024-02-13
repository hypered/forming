{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
module Forming.Command
  ( Command(..)
  , Command'(..)
  , Run(..)
  , ExprOrFilePath(..)
  , parserInfo
  , parserRunInfo
  , parserInfo'
  , parserRun'
  ) where

import Control.Applicative ( many, optional, some, (<**>), ( <|>) )
import qualified Options.Applicative as A


--------------------------------------------------------------------------------
-- | Describes the command available from the @forming@ command-line tool.
data Command =
    Tokenize String
  | TokenizeFile FilePath
  | SExpr String
  | SExprFile FilePath
  | Serve String
  | ServeFile FilePath
  | Servant String -- ^ Servant version of Serve.
  | ServantFile FilePath -- ^ Servant version of ServeFile.
  | Run Run ExprOrFilePath
    -- ^ Mode, and an expression or file to evaluate.
  deriving Show

-- | Describes the command available from the `defaultMain` function, i.e.
-- accepting multiple computations.
data Command' =
    RunInfo'
  | Run' String Run
    -- ^ Slug of the computation to run, and a mode.
  | Serve'
  | Servant'
  deriving Show

-- | Defines a mode to run or query a computation.
data Run =
    RunInfo
  | RunHtml
  | RunList
  | RunUnset (Maybe String)            -- optional specific rule to evaluate
  | RunJsonInput String (Maybe String) -- optional specific rule to evaluate
  | RunInputs [(String, String)] (Maybe String)
    -- key/value pairs, optional specific rule to evaluate
  deriving (Eq, Show)

data ExprOrFilePath = ExprString String | ExprFilePath FilePath
  deriving Show

--------------------------------------------------------------------------------
-- | The command-line parser used for the concrete expressions evaluation (i.e.
-- the @forming@ command-line tool).
parserInfo :: A.ParserInfo Command
parserInfo =
  A.info (parser <**> A.helper)
    $  A.fullDesc
    <> A.header "forming - parse, run, and serve Forming expressions"
    <> A.progDesc
         "Forming is tool to define simple computations, forms and APIs."

-- | The command-line parser used with `defaultMainOne`.
parserRunInfo :: A.ParserInfo Run
parserRunInfo =
  A.info (parserRun' <**> A.helper)
    $  A.fullDesc
    <> A.header "forming - run a Forming expression"
    <> A.progDesc
         "Forming is tool to define simple computations, forms and APIs."

-- | The command-line parser used with `defaultMain`.
parserInfo' :: A.ParserInfo Command'
parserInfo' =
  A.info (parser' <**> A.helper)
    $  A.fullDesc
    <> A.header "forming - run Forming expressions"
    <> A.progDesc
         "Forming is tool to define simple computations, forms and APIs."

parser :: A.Parser Command
parser =
  A.subparser
    (  A.command
        "tokenize"
        ( A.info (parserTokenize <**> A.helper)
        $ A.progDesc "Apply lexing to an input expression"
        )
    <> A.command
        "sexpr"
        ( A.info (parserSExpr <**> A.helper)
        $ A.progDesc "Apply sexpr parsing to an input expression"
        )
    <> A.command
        "serve"
        ( A.info (parserServe <**> A.helper)
        $ A.progDesc "Serve an expression"
        )
    <> A.command
        "servant"
        ( A.info (parserServant <**> A.helper)
        $ A.progDesc "Serve an expression, using Servant"
        )
    <> A.command
        "run"
        ( A.info (parserRun <**> A.helper)
        $ A.progDesc "Run an expression"
        )
    )

parser' :: A.Parser Command'
parser' =
  (A.flag' RunInfo' $ A.long "info" <> A.help
    "Display information about an expression, in particular possible user inputs"
  )
  <|>
  A.subparser
    (  A.command
        "serve"
        ( A.info ((pure Serve') <**> A.helper)
        $ A.progDesc "Serve an expression"
        )
    <> A.command
        "servant"
        ( A.info ((pure Servant') <**> A.helper)
        $ A.progDesc "Serve an expression, using Servant"
        )
    <> A.command
        "run"
        ( A.info ((Run' <$> (A.argument A.str (A.metavar "SLUG" <> A.help "The name of a computation"))
                        <*> parserRun'
        ) <**> A.helper)
        $ A.progDesc "Run an expression"
        )
    )

parserTokenize :: A.Parser Command
parserTokenize =
  ( do
      fp <- A.strOption
              $  A.long "file"
              <> A.help "A file to parse"
              <> A.metavar "FILEPATH"
      pure $ TokenizeFile fp
  )
  <|>
  ( do
      s <- A.argument A.str (A.metavar "EXPR" <> A.help "An expression to parse")
      pure $ Tokenize s
  )

parserSExpr :: A.Parser Command
parserSExpr =
  ( do
      fp <- A.strOption
              $  A.long "file"
              <> A.help "A file to parse"
              <> A.metavar "FILEPATH"
      pure $ SExprFile fp
  )
  <|>
  ( do
      s <- A.argument A.str (A.metavar "EXPR" <> A.help "An expression to parse")
      pure $ SExpr s
  )

parserServe :: A.Parser Command
parserServe =
  ( do
      fp <- A.strOption
              $  A.long "file"
              <> A.help "A file to serve"
              <> A.metavar "FILEPATH"
      pure $ ServeFile fp
  )
  <|>
  ( do
      s <- A.argument A.str (A.metavar "EXPR" <> A.help "An expression to serve")
      pure $ Serve s
  )

parserServant :: A.Parser Command
parserServant =
  ( do
      fp <- A.strOption
              $  A.long "file"
              <> A.help "A file to serve"
              <> A.metavar "FILEPATH"
      pure $ ServantFile fp
  )
  <|>
  ( do
      s <- A.argument A.str (A.metavar "EXPR" <> A.help "An expression to serve")
      pure $ Servant s
  )

parseExrpOrFilePath =
  (ExprFilePath <$> (A.strOption
     $  A.long "file"
     <> A.help "A file to run"
     <> A.metavar "FILEPATH"))
  <|>
  (ExprString <$> A.argument A.str (A.metavar "EXPR" <> A.help "An expression to run"))
  
parserRun :: A.Parser Command
parserRun = do
  e <- parseExrpOrFilePath
  r <- parserRun'
  pure $ Run r e

parserRun' :: A.Parser Run
parserRun' =
  (A.flag' RunInfo $ A.long "info" <> A.help
    "Display information about the expression, in particular possible user inputs"
  )
  <|>
  (A.flag' RunHtml $ A.long "html" <> A.help
    "Generate an HTML page with technical comments for the expression"
  )
  <|>
  (A.flag' RunList $ A.long "list" <> A.help
    "List all rules for the expression"
  )
  <|>
  (do
     f <- A.flag' RunUnset $ A.long "unset" <> A.help
           "List unset names, and their types if possible"
     mname <- optional $ A.argument A.str (A.metavar "NAME" <> A.help "A name to evaluate")
     pure $ f mname
  )
  <|> RunJsonInput
    <$> (  A.strOption
      $  A.long "json"
      <> A.help "Parse inputs given as JSON and evaluate one rule"
      <> A.metavar "JSON"
      )
    <*>
     (optional $ A.argument A.str (A.metavar "NAME" <> A.help "A name to evaluate"))
  <|> RunInputs <$> parserInputs
     <*> (optional $ A.argument A.str (A.metavar "NAME" <> A.help "A name to evaluate"))

parserInputs = do
  many $ A.option
    A.auto
    (A.long "set" <> A.help "Set an input" <> A.metavar
      "PAIR"
    )
