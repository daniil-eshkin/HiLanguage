module HW3.Action (
    HiPermission (..),
    PermissionException (..),
    HIO (..)
) where
    
import Data.Set (Set, member)
import Control.Exception (Exception, throwIO)
import HW3.Base (HiMonad (runAction), HiAction (..), HiValue (..))
import Control.Monad.Identity ((>=>))
import System.Directory (getCurrentDirectory, createDirectory, setCurrentDirectory, doesDirectoryExist, doesFileExist, listDirectory)
import qualified Data.Text as T
import qualified Data.Sequence as S
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock (getCurrentTime)
import System.Random ( getStdRandom, Random(randomR) )

data HiPermission =
    AllowRead
    | AllowWrite
    | AllowTime
    deriving (Eq, Ord, Show, Enum, Bounded)

data PermissionException =
    PermissionRequired HiPermission
    deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
    fmap f (HIO m) = HIO (m >=> (return . f))

instance Applicative HIO where
    pure x = HIO (\_ -> return x) 
    m1 <*> m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))

instance Monad HIO where
    HIO m >>= k = HIO $ \p -> (m p) >>= (\a -> runHIO (k a) p)

instance HiMonad HIO where
    runAction (HiActionRead p) = HIO $ \perm -> if member AllowRead perm 
        then do
            fileEx <- doesFileExist p 
            dirEx <- doesDirectoryExist p
            if fileEx then B.readFile p >>= \s -> 
                let
                    res = decodeUtf8' s
                in
                    case res of
                        Left _ -> return $ HiValueBytes s
                        Right t -> return $ HiValueString $ t
            else if dirEx then fmap (HiValueList . S.fromList . (map (HiValueString . T.pack))) $ listDirectory p
                else return HiValueNull
        else throwIO $ PermissionRequired AllowRead
    runAction (HiActionWrite p s) = HIO $ \perm -> if member AllowWrite perm 
        then HiValueNull <$ B.writeFile p s
        else throwIO $ PermissionRequired AllowWrite
    runAction (HiActionMkDir p) = HIO $ \perm -> if member AllowWrite perm 
        then HiValueNull <$ createDirectory p
        else throwIO $ PermissionRequired AllowWrite
    runAction (HiActionChDir p) = HIO $ \perm -> if member AllowRead perm 
        then HiValueNull <$ setCurrentDirectory p
        else throwIO $ PermissionRequired AllowRead
    runAction HiActionCwd = HIO $ \perm -> if member AllowRead perm 
        then fmap (HiValueString . T.pack) getCurrentDirectory
        else throwIO $ PermissionRequired AllowRead
    runAction HiActionNow = HIO $ \perm -> if member AllowTime perm
        then fmap HiValueTime getCurrentTime
        else throwIO $ PermissionRequired AllowTime
    runAction (HiActionRand a b) = HIO $ \_ -> do
        fmap (HiValueNumber . toRational) (getStdRandom $ randomR (a, b))
    runAction (HiActionEcho t) = HIO $ \perm -> if member AllowWrite perm 
        then HiValueNull <$ (putStrLn $ T.unpack t)
        else throwIO $ PermissionRequired AllowWrite
        