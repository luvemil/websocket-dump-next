module Lib.Utils where

embedMaybe :: MonadFail m => String -> Maybe a -> m a
embedMaybe _ (Just x) = pure x
embedMaybe s Nothing = fail s