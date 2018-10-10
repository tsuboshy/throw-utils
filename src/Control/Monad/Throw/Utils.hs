module Control.Monad.Throw.Utils where
import Control.Exception.Safe (throwM, MonadThrow, Exception)


-- | throw exception if nothing.
--    
-- >>> import Control.Exception.Safe
-- >>> data MyException = ME !String deriving (Show, Eq)
-- >>> instance Exception MyException
-- >>> (Just 1) `ifNothingThrow` ME "value is nothing" :: IO Int
-- 1
-- >>> Nothing `ifNothingThrow` ME "value is nothing" :: IO Int
-- *** Exception: ME "value is nothing"
-- 
-- >>> (Just 1) `ifNothingThrow` ME "value is nothing" :: Either SomeException Int
-- Right 1
-- >>> Nothing `ifNothingThrow` ME "value is nothing" :: Either SomeException Int
-- Left (ME "value is nothing")
ifNothingThrow :: (Exception e, Monad m, MonadThrow m) => Maybe a -> e -> m a
ifNothingThrow (Just a) _ = return a
ifNothingThrow Nothing  e = throwM e


-- | throw exception if monadic value is nothing.
--
-- >>> import Control.Exception.Safe
-- >>> data MyException = ME !String deriving (Show, Eq)
-- >>> instance Exception MyException
-- >>> return (Just 1) `ifNothingThrowM` ME "value is IO Nothing" :: IO Int
-- 1
-- >>> return Nothing `ifNothingThrowM` ME "value is IO Nothing" :: IO Int
-- *** Exception: ME "value is IO Nothing"
--
-- >>> Right (Just 1) `ifNothingThrowM` ME "value is nothing" :: Either SomeException Int
-- Right 1
-- >>> Right Nothing `ifNothingThrowM` ME "value is nothing" :: Either SomeException Int
-- Left (ME "value is nothing")
ifNothingThrowM :: (Exception e, Monad m, MonadThrow m) => m (Maybe a) -> e -> m a
ifNothingThrowM mmyb e = do
    myb <- mmyb
    ifNothingThrow myb e


-- | throw exception if value is Left.
--
-- >>> import Control.Exception.Safe
-- >>> import Text.Read
-- >>> data MyException = ME !String deriving (Show, Eq)
-- >>> instance Exception MyException
-- >>> (readEither "1" `ifLeftThrow` \errMsg -> ME ("value is Left: " ++ errMsg)) :: IO Int
-- 1
-- >>> (readEither "a" `ifLeftThrow` \errMsg -> ME ("value is Left: " ++ errMsg)) :: IO Int
-- *** Exception: ME "value is Left: Prelude.read: no parse"
ifLeftThrow :: (Exception e, Monad m, MonadThrow m) => Either l r -> (l -> e) -> m r
ifLeftThrow (Right a) _ = return a
ifLeftThrow (Left  b) f = throwM $ f b


-- | throw exception if value is Left.
--
-- >>> import Control.Exception.Safe
-- >>> import Text.Read
-- >>> data MyException = ME !String deriving (Show, Eq)
-- >>> instance Exception MyException
-- >>> readEither "1" `ifLeftThrow'`  ME "cannot read as Int"  :: IO Int
-- 1
-- >>> readEither "a" `ifLeftThrow'` ME "cannot read as Int"  :: IO Int
-- *** Exception: ME "cannot read as Int"
ifLeftThrow' :: (Exception e, Monad m, MonadThrow m) => Either l r -> e -> m r
ifLeftThrow' eith e = ifLeftThrow eith (const e)


-- | throw exception if monadic value is Left.
--
-- >>> import Control.Exception.Safe
-- >>> data MyException = ME !String deriving (Show, Eq)
-- >>> instance Exception MyException
-- >>> (return (Right 1) `ifLeftThrowM` \leftVal -> ME ("value is Left: " ++ show leftVal)) :: IO Int
-- 1
-- >>> (return (Left 1) `ifLeftThrowM` \leftVal -> ME ("value is Left: " ++ show leftVal)) :: IO Int
-- *** Exception: ME "value is Left: 1"
ifLeftThrowM :: (Exception e, Monad m, MonadThrow m) => m (Either l r) -> (l -> e) -> m r
ifLeftThrowM meith f = do
    eith <- meith
    ifLeftThrow eith f


-- | throw exception if monadic value is Left.
--
-- >>> import Control.Exception.Safe
-- >>> data MyException = ME !String deriving (Show, Eq)
-- >>> instance Exception MyException
-- >>> return (Right 1) `ifLeftThrowM'` ME "value is Left" :: IO Int
-- 1
-- >>> return (Left 1) `ifLeftThrowM'` ME "value is Left" :: IO Int
-- *** Exception: ME "value is Left"    
ifLeftThrowM' :: (Exception e, Monad m, MonadThrow m) => m (Either l r) -> e -> m r
ifLeftThrowM' meith e = do
    eith <- meith
    ifLeftThrow' eith e