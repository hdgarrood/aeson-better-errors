{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Aeson.BetterErrors.Internal where

import Control.Applicative
import Control.Arrow (left)
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Error.Class (MonadError(..))

import Data.Void
import Data.Foldable (foldMap)
import Data.Monoid
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Vector ((!?))
import qualified Data.Vector as V
import Data.Scientific (Scientific)
import qualified Data.Scientific as S
import qualified Data.HashMap.Strict as HashMap

import Data.Aeson.BetterErrors.Utils

-- | The type of parsers: things which consume JSON values and produce either
-- detailed errors or successfully parsed values (of other types).
--
-- The @err@ type parameter is for custom validation errors; for parsers that
-- don't produce any custom validation errors, I recommend you just stick a
-- type variable in for full generality:
--
-- @
--     asTuple :: Parse e (Int, Int)
--     asTuple = (,) \<$\> nth 0 asIntegral \<*\> nth 1 asIntegral
-- @
--
-- The @m@ parameter allows you to run the parser within an abitrary underlying Monad.
-- You may want to use 'Parse' in most cases instead, and all functions in this module work on either.
newtype ParseT err m a
  = ParseT (ReaderT ParseReader (ExceptT (ParseError err) m) a)
  deriving (Functor, Applicative, Monad,
            MonadReader ParseReader, MonadError (ParseError err))
-- | This is the standard version of 'ParseT' over the 'Identity' Monad, for running pure parsers.
type Parse err a = ParseT err Identity a

instance MonadTrans (ParseT err) where
  lift f = ParseT (lift (lift f))

runParseT :: ParseT err m a -> A.Value -> m (Either (ParseError err) a)
runParseT (ParseT p) v = runExceptT (runReaderT p (ParseReader DList.empty v))

runParse :: Parse err a -> A.Value -> Either (ParseError err) a
runParse p v = runIdentity (runParseT p v)

mapParseT :: (ReaderT ParseReader (ExceptT (ParseError err) m) a -> ReaderT ParseReader (ExceptT (ParseError err') m') a') -> ParseT err m a -> ParseT err' m' a'
mapParseT f (ParseT p) = ParseT (f p)

-- | Transform the error of a parser according to the given function.
mapError :: Functor m => (err -> err') -> ParseT err m a -> ParseT err' m a
mapError f = mapParseT (mapReaderT (withExceptT (fmap f)))

-- | An infix version of 'mapError'.
(.!) :: Functor m => ParseT err m a -> (err -> err') -> ParseT err' m a
(.!) = flip mapError

-- | The type of parsers which never produce custom validation errors.
type Parse' a = Parse Void a

runParserT :: Monad m =>
  (s -> Either String A.Value) ->
  ParseT err m a ->
  s ->
  m (Either (ParseError err) a)
runParserT decode p src =
  case decode src of
    Left err -> return $ Left (InvalidJSON err)
    Right val -> runParseT p val

runParser ::
  (s -> Either String A.Value) ->
  Parse err a ->
  s ->
  Either (ParseError err) a
runParser decode p src =
  runIdentity (runParserT decode p src)

-- | Like 'parse' but runs the parser on an arbitrary underlying Monad.
parseM :: Monad m => ParseT err m a -> BL.ByteString -> m (Either (ParseError err) a)
parseM = runParserT A.eitherDecode

-- | Run a parser with a lazy 'BL.ByteString' containing JSON data. Note that
-- the normal caveat applies: the JSON supplied must contain either an object
-- or an array for this to work.
parse :: Parse err a -> BL.ByteString -> Either (ParseError err) a
parse = runParser A.eitherDecode

-- | Like 'parseStrict' but runs the parser on an arbitrary underlying Monad.
parseStrictM :: Monad m => ParseT err m a -> B.ByteString -> m (Either (ParseError err) a)
parseStrictM = runParserT A.eitherDecodeStrict

-- | Run a parser with a strict 'B.ByteString' containing JSON data. Note that
-- the normal caveat applies: the JSON supplied must contain either an object
-- or an array for this to work.
parseStrict :: Parse err a -> B.ByteString -> Either (ParseError err) a
parseStrict = runParser A.eitherDecodeStrict

-- | Like 'parseValue' but runs the parser on an arbitrary underlying Monad.
parseValueM :: Monad m => ParseT err m a -> A.Value -> m (Either (ParseError err) a)
parseValueM = runParserT Right

-- | Run a parser with a pre-parsed JSON 'A.Value'.
parseValue :: Parse err a -> A.Value -> Either (ParseError err) a
parseValue = runParser Right

-- | This function is useful when you have a @'Parse' err a@ and you want to
-- obtain an instance for @'A.FromJSON' a@. Simply define:
--
-- @
--    parseJSON = toAesonParser showMyCustomError myParser
-- @
toAesonParser :: (err -> Text) -> Parse err a -> A.Value -> A.Parser a
toAesonParser showCustom p val =
  case parseValue p val of
    Right x -> return x
    Left err -> fail (unlines (map T.unpack (displayError showCustom err)))

-- | Take a parser which never produces custom validation errors and turn
-- it into an Aeson parser. Note that in this case, there is no need to provide
-- a display function.
toAesonParser' :: Parse' a -> A.Value -> A.Parser a
toAesonParser' = toAesonParser absurd

fromAesonResult :: (Functor m, Monad m) => (A.Value -> A.Result a) -> ParseT e m a
fromAesonResult f = liftParse $ \v ->
  case f v of
    A.Success x -> Right x
    A.Error err -> Left (FromAeson err)

-- | Create a parser for any type, using its FromJSON instance.  Generally, you
-- should prefer to write parsers using the other functions in this module;
-- 'key', 'value', etc, since they will usually generate better error
-- messages. However this function is also useful occasionally.
fromAesonParser :: (Functor m, Monad m) => A.FromJSON a => ParseT e m a
fromAesonParser = fromAesonResult A.fromJSON

-- | Data used internally by the 'Parse' type.
data ParseReader = ParseReader
  { rdrPath  :: DList PathPiece
  , rdrValue :: A.Value
  }

appendPath :: PathPiece -> ParseReader -> ParseReader
appendPath p r = r { rdrPath = DList.snoc (rdrPath r) p }

setValue :: A.Value -> ParseReader -> ParseReader
setValue v r = r { rdrValue = v }

-- | A piece of a path leading to a specific part of the JSON data.
-- Internally, a list of these is maintained as the parser traverses the JSON
-- data. This list is included in the error if one occurs.
data PathPiece
  = ObjectKey Text
  | ArrayIndex Int
  deriving (Show, Eq, Ord)

-- | A value indicating that the JSON could not be decoded successfully.
data ParseError err
  = InvalidJSON String
    -- ^ Indicates a syntax error in the JSON string. Unfortunately, in this
    -- case, Aeson's errors are not very helpful.
  | BadSchema [PathPiece] (ErrorSpecifics err)
    -- ^ Indicates a decoding error; the input was parsed as JSON successfully,
    -- but a value of the required type could not be constructed, perhaps
    -- because of a missing key or type mismatch.
  deriving (Show, Eq, Functor)

-- | The type of parse errors which never involve custom validation
-- errors.
type ParseError' = ParseError Void

-- | Detailed information in the case where a value could be parsed as JSON,
-- but a value of the required type could not be constructed from it, for some
-- reason.
data ErrorSpecifics err
  = KeyMissing Text
  | OutOfBounds Int
  | WrongType JSONType A.Value -- ^ Expected type, actual value
  | ExpectedIntegral Double
  | FromAeson String -- ^ An error arising inside a 'A.FromJSON' instance.
  | CustomError err
  deriving (Show, Eq, Functor)

-- | The type of error specifics which never involve custom validation
-- errors.
type ErrorSpecifics' = ErrorSpecifics Void

-- | An enumeration of the different types that JSON values may take.
data JSONType
  = TyObject
  | TyArray
  | TyString
  | TyNumber
  | TyBool
  | TyNull
  deriving (Show, Eq, Ord)

displayJSONType :: JSONType -> Text
displayJSONType t = case t of
  TyObject -> "object"
  TyArray  -> "array"
  TyString -> "string"
  TyNumber -> "number"
  TyBool   -> "boolean"
  TyNull   -> "null"

-- | Turn a 'ParseError' into a human-readable list of 'Text' values.
-- They will be in a sensible order. For example, you can feed the result to
-- @mapM putStrLn@, or @unlines@.
displayError :: (err -> Text) -> ParseError err -> [Text]
displayError _ (InvalidJSON str) =
  [ "The input could not be parsed as JSON", "aeson said: " <> T.pack str ]
displayError f (BadSchema [] specs) =
  displaySpecifics f specs
displayError f (BadSchema path specs) =
  [ "At the path: " <> displayPath path ] <> displaySpecifics f specs

-- | A version of 'displayError' for parsers which do not produce custom
-- validation errors.
displayError' :: ParseError' -> [Text]
displayError' = displayError absurd

displayPath :: [PathPiece] -> Text
displayPath = foldMap showPiece
  where
  showPiece (ObjectKey t)  = "[" <> tshow t <> "]"
  showPiece (ArrayIndex i) = "[" <> tshow i <> "]"

displaySpecifics :: (err -> Text) -> ErrorSpecifics err -> [Text]
displaySpecifics _ (KeyMissing k) =
  [ "The required key " <> tshow k <> " is missing" ]
displaySpecifics _ (OutOfBounds i) =
  [ "The array index " <> tshow i <> " is out of bounds" ]
displaySpecifics _ (WrongType t val) =
  [ "Type mismatch:"
  , "Expected a value of type " <> displayJSONType t
  , "Got: " <> decodeUtf8 (B.concat (BL.toChunks (A.encode val)))
  ]
displaySpecifics _ (ExpectedIntegral x) =
  [ "Expected an integral value, got " <> tshow x ]
displaySpecifics _ (FromAeson str) =
  [ "Arising from an Aeson FromJSON instance:"
  , T.pack str
  ]
displaySpecifics f (CustomError err) =
  [ f err ]

-- | A version of `displaySpecifics` for parsers which do not produce
-- custom validation errors.
displaySpecifics' :: ErrorSpecifics' -> [Text]
displaySpecifics' = displaySpecifics absurd

-- | Get the type of a JSON value.
jsonTypeOf :: A.Value -> JSONType
jsonTypeOf (A.Object _) = TyObject
jsonTypeOf (A.Array _)  = TyArray
jsonTypeOf (A.String _) = TyString
jsonTypeOf (A.Number _) = TyNumber
jsonTypeOf (A.Bool _)   = TyBool
jsonTypeOf A.Null       = TyNull

liftParseT :: (Functor m, Monad m) => (A.Value -> ExceptT (ErrorSpecifics err) m a) -> ParseT err m a
liftParseT f = ParseT $ ReaderT $ \(ParseReader path val) ->
  withExceptT (BadSchema (DList.toList path)) (f val)

liftParseM :: (Functor m, Monad m) => (A.Value -> m (Either (ErrorSpecifics err) a)) -> ParseT err m a
liftParseM f = liftParseT (ExceptT . f)

-- | Lift any parsing function into the 'Parse' type.
liftParse :: (Functor m, Monad m) => (A.Value -> Either (ErrorSpecifics err) a) -> ParseT err m a
liftParse f = liftParseM (return . f)

-- | Aborts parsing, due to an error in the structure of the JSON - that is,
-- any error other than the JSON not actually being parseable into a 'A.Value'.
badSchema :: (Functor m, Monad m) => ErrorSpecifics err -> ParseT err m a
badSchema = liftParse . const . Left

class A.FromJSON a => FromJSONBetterErrors a where
  -- | The default implementation just uses 'fromAesonParser', but this will not produce useful errors.
  value :: (Functor m, Monad m) => ParseT err m a
  value = fromAesonParser

instance FromJSONBetterErrors A.Value where
  value = asks rdrValue

as :: (Functor m, Monad m) => (A.Value -> Maybe a) -> JSONType -> ParseT err m a
as pat ty = liftParse $ \v ->
  maybe (Left (WrongType ty v)) Right (pat v)

-- | Parse a single JSON string as 'Text'.
instance FromJSONBetterErrors Text where
  value = as patString TyString

instance FromJSONBetterErrors TL.Text where
  value = TL.fromStrict <$> value

-- | Parse a single JSON string as a 'String'.
instance FromJSONBetterErrors String where
  value = T.unpack <$> value

-- | Parse a single JSON number as a 'Scientific'.
instance FromJSONBetterErrors Scientific where
  value = as patNumber TyNumber

-- | Parse a single JSON number as any 'Integral' type.
asIntegral :: (Functor m, Monad m, Integral a) => ParseT err m a
asIntegral =
  value
    >>= liftParse . const . left ExpectedIntegral . S.floatingOrInteger

instance FromJSONBetterErrors Integer where
  value = asIntegral
instance FromJSONBetterErrors Int where
  value = asIntegral

-- | Parse a single JSON number as any 'RealFloat' type.
asRealFloat :: (Functor m, Monad m, RealFloat a) => ParseT err m a
asRealFloat =
  either id fromInteger . S.floatingOrInteger <$> value

instance FromJSONBetterErrors Float where
  value = asRealFloat
instance FromJSONBetterErrors Double where
  value = asRealFloat

-- | Parse a single JSON boolean as a 'Bool'.
instance FromJSONBetterErrors Bool where
  value = as patBool TyBool

-- | Parse a JSON object, as an 'A.Object'. You should prefer functions like
-- 'eachInObject' where possible, since they will usually generate better
-- error messages.
instance FromJSONBetterErrors A.Object where
  value = as patObject TyObject

-- | Parse a JSON array, as an 'A.Array'. You should prefer functions like
-- 'eachInArray' where possible, since they will usually generate better
-- error messages.
instance FromJSONBetterErrors A.Array where
  value = as patArray TyArray

-- | Parse a single JSON null value. Useful if you want to throw an error in
-- the case where something is not null.
instance FromJSONBetterErrors () where
  value = as patNull TyNull

-- | Given a parser, transform it into a parser which returns @Nothing@ when
-- supplied with a JSON @null@, and otherwise, attempts to parse with the
-- original parser; if this succeeds, the result becomes a @Just@ value.
perhaps :: (Functor m, Monad m) => ParseT err m a -> ParseT err m (Maybe a)
perhaps p = do
  v <- asks rdrValue
  case v of
    A.Null -> return Nothing
    _      -> Just <$> p

instance FromJSONBetterErrors a => FromJSONBetterErrors (Maybe a) where
  value = perhaps value

-- | Take the value corresponding to a given key in the current object.
key :: (Functor m, Monad m) => Text -> ParseT err m a -> ParseT err m a
key k p = key' (badSchema (KeyMissing k)) k p

-- | Take the value corresponding to a given key in the current object, or
-- if no property exists with that key, use the supplied default.
keyOrDefault :: (Functor m, Monad m) => Text -> a -> ParseT err m a -> ParseT err m a
keyOrDefault k def p = key' (pure def) k p

-- | Take the value corresponding to a given key in the current object, or
-- if no property exists with that key, return Nothing .
keyMay :: (Functor m, Monad m) => Text -> ParseT err m a -> ParseT err m (Maybe a)
keyMay k p = keyOrDefault k Nothing (Just <$> p)

key' :: (Functor m, Monad m) => ParseT err m a -> Text -> ParseT err m a -> ParseT err m a
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
nth :: (Functor m, Monad m) => Int -> ParseT err m a -> ParseT err m a
nth n p = nth' (badSchema (OutOfBounds n)) n p

-- | Take the nth value of the current array, or if no value exists with that
-- index, use the supplied default.
nthOrDefault :: (Functor m, Monad m) => Int -> a -> ParseT err m a -> ParseT err m a
nthOrDefault n def p =
  nth' (pure def) n p

-- | Take the nth value of the current array, or if no value exists with that
-- index, return Nothing.
nthMay :: (Functor m, Monad m) => Int -> ParseT err m a -> ParseT err m (Maybe a)
nthMay n p = nthOrDefault n Nothing (Just <$> p)

nth' :: (Functor m, Monad m) => ParseT err m a -> Int -> ParseT err m a -> ParseT err m a
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
eachInArray :: (Functor m, Monad m) => ParseT err m a -> ParseT err m [a]
eachInArray p = do
  xs <- zip [0..] . V.toList <$> value
  forM xs $ \(i, x) ->
    local (appendPath (ArrayIndex i) . setValue x) p

-- | Parse each property in an object with the given parser, given the key as
-- an argument, and collect the results.
forEachInObject :: (Functor m, Monad m) => (Text -> ParseT err m a) -> ParseT err m [a]
forEachInObject p = do
  xs <- HashMap.toList <$> value
  forM xs $ \(k, x) ->
    local (appendPath (ObjectKey k) . setValue x) (p k)

-- | Attempt to parse each property value in the object with the given parser,
-- and collect the results.
eachInObject :: (Functor m, Monad m) => ParseT err m a -> ParseT err m [(Text, a)]
eachInObject = eachInObjectWithKey Right

-- | Attempt to parse each property in the object: parse the key with the
-- given validation function, parse the value with the given parser, and
-- collect the results.
eachInObjectWithKey :: (Functor m, Monad m) => (Text -> Either err k) -> ParseT err m a -> ParseT err m [(k, a)]
eachInObjectWithKey parseKey parseVal = forEachInObject $ \k ->
  (,) <$> liftEither (parseKey k) <*> parseVal

-- | Lifts a function attempting to validate an arbitrary JSON value into a
-- parser. You should only use this if absolutely necessary; the other
-- functions in this module will generally give better error reporting.
withValue :: (Functor m, Monad m) => (A.Value -> Either err a) -> ParseT err m a
withValue f = liftParse (left CustomError . f)

liftEither :: (Functor m, Monad m) => Either err a -> ParseT err m a
liftEither = withValue . const

withParserM :: (Functor m, Monad m) => ParseT err m a -> (a -> m (Either err b)) -> ParseT err m b
withParserM g f = g >>= lift . f >>= liftEither

withParser :: (Functor m, Monad m) => ParseT err m a -> (a -> Either err b) -> ParseT err m b
withParser g f = withParserM g (return . f)

withM :: (Functor m, Monad m, FromJSONBetterErrors a) => (a -> m (Either err b)) -> ParseT err m b
withM = withParserM value

with :: (Functor m, Monad m, FromJSONBetterErrors a) => (a -> Either err b) -> ParseT err m b
with f = withM (return . f)

withIntegralM :: (Functor m, Monad m, Integral a) => (a -> m (Either err b)) -> ParseT err m b
withIntegralM = withParserM asIntegral

withIntegral :: (Functor m, Monad m, Integral a) => (a -> Either err b) -> ParseT err m b
withIntegral = withParser asIntegral

withRealFloatM :: (Functor m, Monad m, RealFloat a) => (a -> m (Either err b)) -> ParseT err m b
withRealFloatM = withParserM asRealFloat

withRealFloat :: (Functor m, Monad m, RealFloat a) => (a -> Either err b) -> ParseT err m b
withRealFloat = withParser asRealFloat

-- | Throw a custom validation error.
throwCustomError :: (Functor m, Monad m) => err -> ParseT err m a
throwCustomError = liftEither . Left

liftCustomT :: (Functor m, Monad m) => ExceptT err m a -> ParseT err m a
liftCustomT f = lift (runExceptT f) >>= liftEither
