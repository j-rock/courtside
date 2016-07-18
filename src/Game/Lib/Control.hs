module Game.Lib.Control where


anyOfM :: (Monad m, Traversable t) => (a -> m Bool) -> t a -> m Bool
anyOfM p as = or <$> forM as p

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

forM :: (Monad m, Traversable t) => t a -> (a -> m b) -> m (t b)
forM = flip mapM

forM_ :: (Monad m, Traversable t) => t a -> (a -> m b) -> m ()
forM_ = flip mapM_

whenM :: Monad m => m Bool -> m a -> m ()
whenM mp ma =
    do b <- mp
       if b
       then ma >> return ()
       else return ()

unlessM :: Monad m => m Bool -> m a -> m ()
unlessM mp = whenM (not <$> mp)

bool :: Bool -> a -> a -> a
bool cnd cns alt = if cnd then cns else alt

boolM :: Monad m => m Bool -> m a -> m a -> m a
boolM mCnd cns alt =
    do cnd <- mCnd
       bool cnd cns alt

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM _ []     _      = return []
zipWithM _ _      []     = return []
zipWithM f (a:as) (b:bs) = do c <- f a b
                              (c:) <$> zipWithM f as bs
