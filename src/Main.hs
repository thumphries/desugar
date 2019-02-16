module Main where

import Control.Applicative
import Data.Data
import Data.Foldable
import Data.Generics.Aliases
import Data.Generics.Schemes
import Language.Haskell.Exts.Simple.Parser
import Language.Haskell.Exts.Simple.Pretty
import Language.Haskell.Exts.Simple.Syntax
import System.IO

main :: IO ()
main = do
  mdl <-  parseModule <$> getContents
  case prettyPrint . Main.traverse <$> mdl of
    ParseOk string -> putStrLn string
    err            -> hPrint stderr err

traverse :: Data a => a -> a
traverse = everywhere (mkT desugar)

desugar :: Exp -> Exp
desugar (Do ls) = desugarDo ls
desugar e = e

desugarDo :: [Stmt] -> Exp
desugarDo xs = foldr' foldFun (Main.traverse ret) rest
  where (Qualifier ret) = last xs
        rest            = init xs

foldFun :: Stmt -> Exp -> Exp
foldFun (Generator var fun) expr =
  InfixApp fun bind (Lambda [var] expr)
foldFun (Qualifier e1) e2 =
  InfixApp e1 bind (Lambda [PWildCard] e2)
foldFun (LetStmt bnds) e = Let (Main.traverse bnds) e
-- No idea how to desugar these arrow expressions.
foldFun (RecStmt _stmts) _e = error "-XArrows not supported."

bind :: QOp
bind = QVarOp (UnQual (Symbol ">>="))

