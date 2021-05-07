{-# LANGUAGE StandaloneDeriving #-}

module Lib.StringNum (
    unwrap,
) where

import Data.Aeson
import Data.Text as T
import Text.Read (readMaybe)

-- data StringNum a = Num a => StringNum { unwrap :: a }

data StringNum a where
    SNum :: Num a => a -> StringNum a

deriving instance (Num a, Read a) => Read (StringNum a)

unwrap :: StringNum a -> a
unwrap (SNum x) = x

instance forall a. (FromJSON a, Num a, Read a) => FromJSON (StringNum a) where
    parseJSON (String s) = do
        let wrapped :: Maybe a
            wrapped = readMaybe $ T.unpack s
        case wrapped of
            Just n -> return $ SNum n
            Nothing -> fail "Could not read number from string"
    parseJSON x@(Number _) = do
        parsed <- parseJSON x
        return $ SNum parsed
    parseJSON _ = fail "Not a string or number"

instance (ToJSON a) => ToJSON (StringNum a) where
    toJSON = toJSON . unwrap