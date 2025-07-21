module Utils where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, forkFinally, Chan, writeChan)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))

getFirstJust :: Monad m => [m (Maybe a)] -> m (Maybe a)
getFirstJust [] = pure Nothing
getFirstJust (x:xs) = x >>= \case
  Nothing -> getFirstJust xs
  Just b  -> pure (Just b)

forkThread :: IO a -> IO (MVar ())
forkThread proc = do
  handle <- newEmptyMVar
  forkFinally proc (\err -> either print (const $ pure ()) err >> putMVar handle ())
  return handle

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left  _) = Nothing
rightToMaybe (Right b) = Just b

traverseAndPushInto :: MonadIO m => Chan b -> (a -> m b) -> [a] -> m ()
traverseAndPushInto chan f as =
  forM_ as $ \a -> do
    b <- f a
    liftIO $ writeChan chan b

