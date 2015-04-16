
module Data.Aeson.BetterErrors.Utils where

import Control.Monad.Error.Class (MonadError(..))

import qualified Data.Aeson as A
import Data.Scientific (Scientific)
import Data.Text (Text, pack)

-----------------------
-- Various utilities

tshow :: Show a => a -> Text
tshow = pack . show

-- | A version of catchJust from "Control.Exception.Base", except for any
-- instance of 'MonadError'.
catchJust :: MonadError e m
  => (e -> Maybe b) -- ^ Predicate to select exceptions
  -> m a            -- ^ Computation to run
  -> (b -> m a)     -- ^ Handler
  -> m a
catchJust p act handler = catchError act handle
  where
  handle e =
    case p e of
      Nothing -> throwError e
      Just b -> handler b

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right y) = Right y

-- Value-level patterns for json values

patNull :: A.Value -> Maybe ()
patNull A.Null = Just ()
patNull _ = Nothing

patString :: A.Value -> Maybe Text
patString (A.String t) = Just t
patString _ = Nothing

patNumber :: A.Value -> Maybe Scientific
patNumber (A.Number x) = Just x
patNumber _ = Nothing

patBool :: A.Value -> Maybe Bool
patBool (A.Bool x) = Just x
patBool _ = Nothing

patObject :: A.Value -> Maybe A.Object
patObject (A.Object obj) = Just obj
patObject _ = Nothing

patArray :: A.Value -> Maybe A.Array
patArray (A.Array arr) = Just arr
patArray _ = Nothing
