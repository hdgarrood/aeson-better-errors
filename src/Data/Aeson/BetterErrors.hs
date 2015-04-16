{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A utility module for dealing with reading JSON, and generating good error
-- messages in the case of JSON with a bad schema.

module Data.Aeson.BetterErrors where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Error.Class (MonadError(..))

import qualified Data.Aeson as A
import Data.Vector ((!?))
import qualified Data.Vector as V
import Data.Scientific (Scientific)
import qualified Data.Scientific as S
import qualified Data.HashMap.Strict as HashMap

import qualified Data.DList as DList
import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson.BetterErrors.Internal
import Data.Aeson.BetterErrors.Utils

-- TODO Alternative?
-- use monoid, combine errors?
-- Simply take rightmost?
-- Or maybe take the one with the deepest path?

-- Good for reaching into a structure and plucking out a particular value.

-- convention: 
--   asFoo :: Parse err Foo
--   parseFoo' :: a -> Either err Foo
--   parseFoo :: ByteString -> Either (ParseError err) Foo 

-- | Lift any parsing function into the 'Parse' type.
liftParse :: (A.Value -> Either (ErrorSpecifics err) a) -> Parse err a
liftParse f =
  asks rdrValue
    >>= either badSchema return . f

-- | Aborts parsing, due to an error in the structure of the JSON - that is,
-- any error other than the JSON not actually being parseable into a 'A.Value'.
badSchema :: ErrorSpecifics err -> Parse err a
badSchema specifics = do
  path <- asks rdrPath
  throwError (BadSchema (DList.toList path) specifics)

as :: (A.Value -> Maybe a) -> JSONType -> Parse err a
as pat ty = liftParse $ \v ->
  maybe (Left (WrongType ty v)) Right (pat v)

-- | Parse a single JSON string as 'Text'.
asText :: Parse err Text
asText = as patString TyString

-- | Parse a single JSON string as a 'String'.
asString :: Parse err String
asString = T.unpack <$> asText

-- | Parse a single JSON number as a 'Scientific'.
asScientific :: Parse err Scientific
asScientific = as patNumber TyNumber

-- | Parse a single JSON number as any 'Integral' type.
asIntegral :: Integral a => Parse err a
asIntegral =
  S.floatingOrInteger <$> asScientific
    >>= either (badSchema . ExpectedIntegral) return

-- | Parse a single JSON number as any 'RealFloat' type.
asRealFloat :: forall a err. RealFloat a => Parse err a
asRealFloat =
  floatingOrInteger <$> asScientific
    >>= either return (return . fromIntegral)
  where
  -- This local declaration is just here to give GHC a hint as to which type
  -- should be used in the case of an Integral (here, we choose Integer, for
  -- safety).
  floatingOrInteger :: Scientific -> Either a Integer
  floatingOrInteger = S.floatingOrInteger

-- | Parse a single JSON boolean as a 'Bool'.
asBool :: Parse err Bool
asBool = as patBool TyBool

-- | Parse a JSON object, as an 'A.Object'.
asObject :: Parse err A.Object
asObject = as patObject TyObject

-- | Parse a JSON array, as an 'A.Array'.
asArray :: Parse err A.Array
asArray = as patArray TyArray

-- | Parse a single `null` value.
asNull :: Parse err ()
asNull = as patNull TyNull

-- | Take the value corresponding to a given key in the current object.
key :: Text -> Parse err a -> Parse err a
key k p = key' (badSchema (KeyMissing k)) k p

-- | Take the value corresponding to a given key in the current object, or
-- if no property exists with that key, use the supplied default.
keyOrDefault :: Text -> a -> Parse err a -> Parse err a
keyOrDefault k def p = key' (pure def) k p

-- | Take the value corresponding to a given key in the current object, or
-- if no property exists with that key, return Nothing .
keyMay :: Text -> Parse err a -> Parse err (Maybe a)
keyMay k p = keyOrDefault k Nothing (Just <$> p)

key' :: Parse err a -> Text -> Parse err a -> Parse err a
key' onMissing k p = do
  v <- asks rdrValue
  case v of
    A.Object obj ->
      case HashMap.lookup k obj of
        Just v' ->
          local (appendPath (ObjectKey k) . setValue v') p
        Nothing ->
          onMissing
    _ ->
      badSchema (WrongType TyObject v)

-- | Take the nth value of the current array.
nth :: Int -> Parse err a -> Parse err a
nth n p = nth' (badSchema (OutOfBounds n)) n p

-- | Take the nth value of the current array, or if no value exists with that
-- index, use the supplied default.
nthOrDefault :: Int -> a -> Parse err a -> Parse err a
nthOrDefault n def p =
  nth' (pure def) n p

-- | Take the nth value of the current array, or if no value exists with that
-- index, return Nothing.
nthMay :: Int -> Parse err a -> Parse err (Maybe a)
nthMay n p = nthOrDefault n Nothing (Just <$> p)

nth' :: Parse err a -> Int -> Parse err a -> Parse err a
nth' onMissing n p = do
  v <- asks rdrValue
  case v of
    A.Array vect ->
      case vect !? n of
        Just v' ->
          local (appendPath (ArrayIndex n) . setValue v') p
        Nothing ->
          onMissing
    _ ->
      badSchema (WrongType TyArray v)

-- | Attempt to parse each value in the array with the given parser, and
-- collect the results.
eachInArray :: Parse err a -> Parse err [a]
eachInArray p = do
  xs <- zip [0..] . V.toList <$> asArray
  forM xs $ \(i, x) ->
    local (appendPath (ArrayIndex i) . setValue x) p

-- | Attempt to parse each property value in the array with the given parser,
-- and collect the results.
eachInObject :: Parse err a -> Parse err [(Text, a)]
eachInObject p = do
  xs <- HashMap.toList <$> asObject
  forM xs $ \(k, x) ->
    (k,) <$> local (appendPath (ObjectKey k) . setValue x) p

-- | Lifts a function attempting to validate an arbitrary JSON value into a
-- parser. You should only use this if absolutely necessary; the other
-- functions in this module will generally give better error reporting.
withValue :: (A.Value -> Either err a) -> Parse err a
withValue f = liftParse (mapLeft CustomError . f)

liftEither :: Either err a -> Parse err a
liftEither = either (badSchema . CustomError) return

with :: Parse err a -> (a -> Either err b) -> Parse err b
with g f = g >>= liftEither . f

withText :: (Text -> Either err a) -> Parse err a
withText = with asText

withString :: (String -> Either err a) -> Parse err a
withString = with asString

withScientific :: (Scientific -> Either err a) -> Parse err a
withScientific = with asScientific

withIntegral :: Integral a => (a -> Either err b) -> Parse err b
withIntegral = with asIntegral

withRealFloat :: RealFloat a => (a -> Either err b) -> Parse err b
withRealFloat = with asRealFloat

withBool :: (Bool -> Either err a) -> Parse err a
withBool = with asBool

withObject :: (A.Object -> Either err a) -> Parse err a
withObject = with asObject

withArray :: (A.Array -> Either err a) -> Parse err a
withArray = with asArray
