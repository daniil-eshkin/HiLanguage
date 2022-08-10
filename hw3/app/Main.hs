module Main where

import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )
import HW3.Pretty ( prettyValue )
import HW3.Action (HiPermission(..), HIO (runHIO))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Set (Set, fromList)
import HW3.Parser ( parse )
import HW3.Evaluator ( eval )
import Prettyprinter ( viaShow )

permitions :: Set HiPermission
permitions = fromList [AllowRead, AllowWrite, AllowTime]

main :: IO ()
main = runInputT defaultSettings loop
    where
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "hi> "
            case minput of
                Nothing -> return ()
                Just "" -> return ()
                Just input -> do 
                    (liftIO (runHIO 
                        (let
                            prettyExpr e = let
                                expr = parse e

                                evalExpr (Left err) = return $ viaShow err
                                evalExpr (Right ex) = let
                                    res = eval ex

                                    getPretty (Left err) = viaShow err
                                    getPretty (Right r) = prettyValue r
                                    in
                                        fmap getPretty res
                                in
                                    evalExpr expr
                        in prettyExpr input) 
                        permitions) >>= outputStrLn . show)
                    loop
