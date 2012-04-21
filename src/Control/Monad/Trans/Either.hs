{-# OPTIONS -Wall -O2 -fno-warn-orphans #-}
{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances, TypeFamilies #-}

module Control.Monad.Trans.Either(EitherT(..), left) where

import Data.Monoid(Monoid(..))
import Control.Monad(liftM)
import Control.Monad.Trans(MonadTrans(..))
import Control.Monad.Instances()
import Control.Monad.Trans(MonadIO(..))
import Control.Applicative(Applicative(..), liftA2)
import Control.Monad.Base
import Control.Monad.Trans.Control

newtype EitherT l m a = EitherT { runEitherT :: m (Either l a) }
inEitherT0 :: m (Either l a) -> EitherT l m a
inEitherT0 = EitherT
inEitherT1 :: (m (Either l a) -> m (Either l b)) ->
              EitherT l m a -> EitherT l m b
inEitherT1 f = inEitherT0 . f . runEitherT
inEitherT2 :: (m (Either l a) -> m (Either l b) -> m (Either l c)) ->
              EitherT l m a -> EitherT l m b -> EitherT l m c
inEitherT2 f = inEitherT1 . f . runEitherT

left :: Monad m => l -> EitherT l m a
left = EitherT . return . Left

instance Monad m => Monad (EitherT l m) where
  -- We can't support "fail" because we don't have a
  -- (String -> l). But we can at least make it a Left, with the error inside
  -- it as a pure exception.
  fail = EitherT . return . Left . error
  return = EitherT . return . Right
  EitherT x >>= f = EitherT $ do
    res <- x
    case res of
      Right r -> runEitherT . f $ r
      Left l -> return (Left l)

instance MonadTrans (EitherT l) where
  lift = EitherT . liftM Right

instance Functor f => Functor (EitherT l f) where
  fmap = inEitherT1 . fmap . fmap
instance Applicative f => Applicative (EitherT l f) where
  pure = inEitherT0 . pure . pure
  (<*>) = inEitherT2 . liftA2 . liftA2 $ id

instance (MonadIO m) => MonadIO (EitherT l m) where
  liftIO = lift . liftIO

instance (Applicative m, Monoid a) => Monoid (EitherT l m a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance MonadBase b m => MonadBase b (EitherT l m) where
    liftBase = liftBaseDefault

instance MonadTransControl (EitherT l) where
    newtype StT (EitherT l) r = StEither {unStEither :: Either l r}
    liftWith f = EitherT $ liftM return $ f $ liftM StEither . runEitherT
    restoreT = EitherT . liftM unStEither
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}
{-
instance (MonadBaseControl b m) => MonadBaseControl b (Either l m) where
    newType StM (EitherT l m) a = StMEither { unStMEither :: ComposeSt (EitherT l) m a }
    liftBaseWith = defaultLiftBaseWith StMEither
    restoreM = defaultRestoreM unSTMEither
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}
-}

--Copied from monad-control-0.3.1.1.
#define BODY(T, ST, unST) {                              \
    newtype StM (T m) a = ST {unST :: ComposeSt (T) m a}; \
    liftBaseWith = defaultLiftBaseWith ST;               \
    restoreM     = defaultRestoreM   unST;               \
    {-# INLINE liftBaseWith #-};                         \
    {-# INLINE restoreM #-}}

--Copied from monad-control-0.3.1.1.
#define TRANS(         T, ST, unST) \
  instance (     MonadBaseControl b m) => MonadBaseControl b (T m) where BODY(T, ST, unST)

TRANS(EitherT l, StMEither, unStMEither)
