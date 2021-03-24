{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HaskellWorks.Data.Uri.IO.Lazy
  ( readResource
  , readFirstAvailableResource
  , resourceExists
  , firstExistingResource
  , headS3Uri
  , writeResource
  , writeResourceWithParent
  , writeResource'
  , createLocalDirectoryIfMissing
  , linkOrCopyResource
  , readHttpUri
  , removePathRecursive
  , listResourcePrefix
  , deleteResource
  ) where

import Antiope.Core
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Data.Either                    (isRight)
import Data.Generics.Product.Any
import HaskellWorks.Data.Uri.Location (Location (..))
import HaskellWorks.Data.Uri.Show
import HaskellWorks.Data.Uri.Status
import HaskellWorks.Data.Uri.UriError

import qualified Antiope.S3                       as AWS
import qualified Antiope.S3.Lazy                  as AWSL
import qualified Control.Concurrent               as IO
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.DList                       as DL
import qualified Data.Text                        as T
import qualified HaskellWorks.Data.Uri.IO.Console as CIO
import qualified HaskellWorks.Data.Uri.IO.File    as FIO
import qualified HaskellWorks.Data.Uri.Location   as URI
import qualified Network.AWS                      as AWS
import qualified Network.AWS.S3.CopyObject        as AWS
import qualified Network.AWS.S3.HeadObject        as AWS
import qualified Network.AWS.S3.PutObject         as AWS
import qualified Network.HTTP.Client              as HTTP
import qualified System.Directory                 as IO
import qualified System.FilePath.Posix            as FP
import qualified System.IO                        as IO
import qualified System.IO.Error                  as IO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

handleAwsError :: MonadCatch m => m a -> m (Either UriError a)
handleAwsError f = catch (Right <$> f) $ \(e :: AWS.Error) ->
  case e of
    (AWS.ServiceError (AWS.ServiceError' _ s _ _ _ _)) -> return (Left (AwsUriError (fromHttpStatus s)))
    _                                                  -> throwM e

handleHttpError :: MonadCatch m => m a -> m (Either UriError a)
handleHttpError f = catch (Right <$> f) $ \(e :: HTTP.HttpException) ->
  case e of
    (HTTP.HttpExceptionRequest _ e') -> case e' of
      HTTP.StatusCodeException resp _ -> return (Left (HttpUriError (fromHttpStatus (resp & HTTP.responseStatus))))
      _                               -> return (Left (GenericUriError (tshow e')))
    _                                 -> throwM e

getS3Uri :: (MonadResource m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> m (Either UriError LBS.ByteString)
getS3Uri envAws (AWS.S3Uri b k) = handleAwsError $ runAws envAws $ AWSL.unsafeDownload b k

readResource :: (MonadResource m, MonadCatch m) => AWS.Env -> Location -> m (Either UriError LBS.ByteString)
readResource envAws = \case
  S3 s3Uri        -> getS3Uri envAws s3Uri
  Local path      -> do
    fileExists <- liftIO $ IO.doesFileExist path
    if fileExists
      then do
        h <- liftIO $ IO.openFile path IO.ReadMode
        void $ register (IO.hClose h)
        lbs <- liftIO $ LBS.hGetContents h
        return (Right lbs)
      else pure (Left NotFound)
  HttpUri httpUri -> liftIO $ readHttpUri httpUri

readFirstAvailableResource :: (MonadResource m, MonadCatch m) => AWS.Env -> [Location] -> m (Either UriError (LBS.ByteString, Location))
readFirstAvailableResource _ [] = return (Left (GenericUriError "No resources specified in read"))
readFirstAvailableResource envAws (a:as) = do
  result <- readResource envAws a
  case result of
    Right lbs -> return $ Right (lbs, a)
    Left e -> if null as
      then return $ Left e
      else readFirstAvailableResource envAws as

safePathIsSymbolLink :: FilePath -> IO Bool
safePathIsSymbolLink filePath = catch (IO.pathIsSymbolicLink filePath) handler
  where handler :: IOError -> IO Bool
        handler e = if IO.isDoesNotExistError e
          then return False
          else return True

resourceExists :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> Location -> m Bool
resourceExists envAws = \case
  S3 s3Uri        -> isRight <$> runResourceT (headS3Uri envAws s3Uri)
  HttpUri httpUri -> isRight <$> headHttpUri httpUri
  Local path  -> do
    fileExists <- liftIO $ IO.doesFileExist path
    if fileExists
      then return True
      else do
        symbolicLinkExists <- liftIO $ safePathIsSymbolLink path
        if symbolicLinkExists
          then do
            target <- liftIO $ IO.getSymbolicLinkTarget path
            resourceExists envAws (Local target)
          else return False

firstExistingResource :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> [Location] -> m (Maybe Location)
firstExistingResource _ [] = return Nothing
firstExistingResource envAws (a:as) = do
  exists <- resourceExists envAws a
  if exists
    then return (Just a)
    else firstExistingResource envAws as

headS3Uri :: (MonadResource m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> m (Either UriError AWS.HeadObjectResponse)
headS3Uri envAws (AWS.S3Uri b k) = handleAwsError $ runAws envAws $ AWS.send $ AWS.headObject b k

uploadToS3 :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> LBS.ByteString -> m (Either UriError (Maybe AWS.ETag))
uploadToS3 envAws (AWS.S3Uri b k) lbs = do
  let req = AWS.toBody lbs
  let po  = AWS.putObject b k req
  handleAwsError $ runResAws envAws $ view AWS.porsETag <$> AWS.send po

uploadToS3' :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> FilePath -> m (Either UriError (Maybe AWS.ETag))
uploadToS3' envAws uri fn =
  handleAwsError $ runResAws envAws $ AWS.putFile' uri fn

-- | Write a lazy bytestring to a location
writeResource :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> Location -> LBS.ByteString -> ExceptT UriError m (Maybe AWS.ETag)
writeResource envAws loc lbs = ExceptT $ case loc of
  S3 s3Uri   -> uploadToS3 envAws s3Uri lbs
  Local path -> liftIO (LBS.writeFile path lbs) >> return (Right Nothing)
  HttpUri _  -> return (Left (GenericUriError "HTTP PUT method not supported"))

-- | Write a lazy bytestring to a location, creating the parent directory if necessary.
writeResourceWithParent :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> Location -> LBS.ByteString -> ExceptT UriError m (Maybe AWS.ETag)
writeResourceWithParent envAws location lbs = do
  let parent = URI.dirname location
  createLocalDirectoryIfMissing parent
  writeResource envAws location lbs

writeResource' :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> Location -> FilePath -> ExceptT UriError m (Maybe AWS.ETag)
writeResource' envAws loc fn = ExceptT $ case loc of
  S3 s3Uri   -> uploadToS3' envAws s3Uri fn
  Local path -> liftIO (LBS.readFile fn >>= LBS.writeFile path) >> return (Right Nothing)
  HttpUri _  -> return (Left (GenericUriError "HTTP PUT method not supported"))

createLocalDirectoryIfMissing :: (MonadCatch m, MonadIO m) => Location -> m ()
createLocalDirectoryIfMissing = \case
  S3 _        -> return ()
  Local path  -> liftIO $ IO.createDirectoryIfMissing True path
  HttpUri _   -> return ()

copyS3Uri :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> AWS.S3Uri -> ExceptT UriError m ()
copyS3Uri envAws (AWS.S3Uri sourceBucket sourceObjectKey) (AWS.S3Uri targetBucket targetObjectKey) = ExceptT $ do
  responseResult <- runResourceT $
    handleAwsError $ runAws envAws $ AWS.send (AWS.copyObject targetBucket (toText sourceBucket <> "/" <> toText sourceObjectKey) targetObjectKey)
  case responseResult of
    Right response -> do
      let responseCode = response ^. AWS.corsResponseStatus
      if 200 <= responseCode && responseCode < 300
        then return (Right ())
        else do
          liftIO $ CIO.hPutStrLn IO.stderr $ "Error in S3 copy: " <> tshow response
          return (Left RetriesFailedUriError)
    Left msg -> return (Left msg)

retry :: (Show e, MonadIO m) => Int -> ExceptT e m () -> ExceptT e m ()
retry = retryWhen (const True)

retryWhen :: (Show e, MonadIO m) => (e -> Bool) -> Int -> ExceptT e m () -> ExceptT e m ()
retryWhen p n f = catchError f $ \exception -> if n > 0
  then do
    liftIO $ CIO.hPutStrLn IO.stderr $ "WARNING: " <> tshow exception <> " (retrying)"
    liftIO $ IO.threadDelay 1000000
    if p exception
      then retry (n - 1) f
      else throwError exception
  else throwError exception

retryUnless :: (Show e, MonadIO m) => (e -> Bool) -> Int -> ExceptT e m () -> ExceptT e m ()
retryUnless p = retryWhen (not . p)

linkOrCopyResource :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> Location -> Location -> ExceptT UriError m ()
linkOrCopyResource envAws source target = case source of
  S3 sourceS3Uri -> case target of
    S3 targetS3Uri -> retryUnless ((== Just 301) . uriErrorStatus) 3 (copyS3Uri envAws sourceS3Uri targetS3Uri)
    Local _        -> throwError "Can't copy between different file backends"
    HttpUri _      -> throwError "Link and copy unsupported for http backend"
  Local sourcePath -> case target of
    S3 _             -> throwError "Can't copy between different file backends"
    Local targetPath -> do
      liftIO $ IO.createDirectoryIfMissing True (FP.takeDirectory targetPath)
      targetPathExists <- liftIO $ IO.doesFileExist targetPath
      unless targetPathExists $ liftIO $ IO.createFileLink sourcePath targetPath
    HttpUri _      -> throwError "Link and copy unsupported for http backend"
  HttpUri _ -> throwError "HTTP PUT method not supported"

readHttpUri :: (MonadIO m, MonadCatch m) => Text -> m (Either UriError LBS.ByteString)
readHttpUri httpUri = handleHttpError $ do
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  request <- liftIO $ HTTP.parseUrlThrow (T.unpack ("GET " <> httpUri))
  response <- liftIO $ HTTP.httpLbs request manager

  return $ HTTP.responseBody response

headHttpUri :: (MonadIO m, MonadCatch m) => Text -> m (Either UriError LBS.ByteString)
headHttpUri httpUri = handleHttpError $ do
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  request <- liftIO $ HTTP.parseUrlThrow (T.unpack ("HEAD " <> httpUri))
  response <- liftIO $ HTTP.httpLbs request manager

  return $ HTTP.responseBody response

removePathRecursive :: (MonadIO m, MonadCatch m) => FilePath -> m (Either UriError ())
removePathRecursive pkgStorePath = catch action handler
  where action :: MonadIO m => m (Either UriError ())
        action = Right <$> liftIO (IO.removeDirectoryRecursive pkgStorePath)
        handler :: MonadIO m => IOError -> m (Either UriError ())
        handler e = do
          CIO.hPutStrLn IO.stderr $ "Warning: Caught " <> tshow e
          return (Left (GenericUriError (tshow e)))

listResourcePrefix :: (MonadUnliftIO m, MonadResource m) => AWS.Env -> Location -> ExceptT UriError m [Location]
listResourcePrefix envAws location = case location of
  S3 s3Uri   -> fmap S3 . DL.toList <$> runAws envAws (AWSL.dlistS3Uris (AWSL.s3UriToListObjectsV2 s3Uri))
  Local path -> fmap Local <$> lift (FIO.listFilesRecursiveWithPrefix path)
  HttpUri _  -> throwError "HTTP method not supported"

deleteResource :: MonadResource m => AWS.Env -> Location -> ExceptT UriError m ()
deleteResource envAws location = case location of
  S3 s3Uri        -> do
    result <- runAws envAws $ AWS.deleteFiles (s3Uri ^. the @"bucket") [s3Uri ^. the @"objectKey"]
    when (result /= [s3Uri]) $ throwError $ DeleteFailed (tshow location)
  Local path      -> liftIO $ IO.removeFile path
  HttpUri _ -> throwError "HTTP method not supported"
