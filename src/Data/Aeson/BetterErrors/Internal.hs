{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.Aeson.BetterErrors.Internal where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Error.Class (MonadError(..))

import Data.Foldable (foldMap)
import Data.Monoid
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Text (Text)
import qualified Data.Text as T
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
-- The @err@ type parameter is for your own errors; if you don't need to use
-- any errors of your own, simply set it to @()@.
newtype Parse err a
  = Parse (ReaderT ParseReader (Except (ParseError err)) a)
  deriving (Functor, Applicative, Monad,
            MonadReader ParseReader, MonadError (ParseError err))

runParser ::
  (s -> Either String A.Value) ->
  Parse err a ->
  s ->
  Either (ParseError err) a
runParser decode (Parse p) src =
  case decode src of
    Left err -> Left (InvalidJSON err)
    Right value ->
      let initialReader = ParseReader DList.empty value
      in  runExcept (runReaderT p initialReader)

-- | Run a parser with a lazy 'BL.ByteString' containing JSON data. Note that
-- the normal caveat applies: the JSON supplied must contain either an object
-- or an array for this to work.
parse :: Parse err a -> BL.ByteString -> Either (ParseError err) a
parse = runParser A.eitherDecode

-- | Run a parser with a strict 'B.ByteString' containing JSON data. Note that
-- the normal caveat applies: the JSON supplied must contain either an object
-- or an array for this to work.
parseStrict :: Parse err a -> B.ByteString -> Either (ParseError err) a
parseStrict = runParser A.eitherDecodeStrict

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

-- | Data used internally by the 'Parse' type.
data ParseReader = ParseReader
  { rdrPath  :: DList PathPiece
  , rdrValue :: A.Value
  }

appendPath :: PathPiece -> ParseReader -> ParseReader
appendPath p r = r { rdrPath = DList.snoc (rdrPath r) p }

setValue :: A.Value -> ParseReader -> ParseReader
setValue v r = r { rdrValue = v }

-- | A piece of a path into a specific part of some JSON data. Internally, a
-- list of these is maintained as the parser traverses the JSON values, and
-- it is included in the error if one occurs.
data PathPiece
  = ObjectKey Text
  | ArrayIndex Int
  deriving (Show, Eq, Ord)

-- | A value indicating that the JSON could not be decoded successfully.
data ParseError err
  = InvalidJSON String
  | BadSchema [PathPiece] (ErrorSpecifics err)
  deriving (Show, Eq)

-- | Detailed information in the case where a value could be parsed as JSON,
-- but a value of the required type could not be constructed from it, for some
-- reason.
data ErrorSpecifics err
  = KeyMissing Text
  | OutOfBounds Int
  | WrongType JSONType A.Value -- ^ Expected type, actual value
  | ExpectedIntegral Double
  | CustomError err
  deriving (Show, Eq)

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

-- | Turn a 'ParseError' into a human-readable list of 'String' values.
-- They will be in a sensible order. For example, you can feed the result to
-- @'mapM' 'putStrLn'@, or 'unlines'.
displayError :: (err -> Text) -> ParseError err -> [Text]
displayError _ (InvalidJSON str) =
  [ "The input could not be parsed as JSON", "aeson said: " <> T.pack str ]
displayError f (BadSchema [] specs) =
  displaySpecifics f specs
displayError f (BadSchema path specs) =
  [ "At the path: " <> displayPath path ] <> displaySpecifics f specs

displayPath :: [PathPiece] -> Text
displayPath = foldMap showPiece
  where
  showPiece (ObjectKey t)  = "[" <> tshow t <> "]"
  showPiece (ArrayIndex i) = "[" <> tshow i <> "]"

displaySpecifics :: (err -> Text) -> ErrorSpecifics err -> [Text]
displaySpecifics _ (KeyMissing k) =
  [ "The required key " <> tshow k <> " is missing." ]
displaySpecifics _ (OutOfBounds i) =
  [ "The array index " <> tshow i <> " is out of bounds." ]
displaySpecifics _ (WrongType t val) =
  [ "Type mismatch:"
  , "Expected a value of type " <> displayJSONType t
  , "Got:" <> decodeUtf8 (BL.toStrict (A.encode val))
  ]
displaySpecifics _ (ExpectedIntegral x) =
  [ "Expected an integral value, got " <> tshow x ]
displaySpecifics f (CustomError err) =
  [ f err ]

-- | Get the type of a JSON value.
jsonTypeOf :: A.Value -> JSONType
jsonTypeOf (A.Object _) = TyObject
jsonTypeOf (A.Array _)  = TyArray
jsonTypeOf (A.String _) = TyString
jsonTypeOf (A.Number _) = TyNumber
jsonTypeOf (A.Bool _)   = TyBool
jsonTypeOf A.Null       = TyNull

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
asRealFloat :: RealFloat a => Parse err a
asRealFloat =
  floatingOrInteger <$> asScientific
    >>= either return (return . fromIntegral)
  where
  -- This local declaration is just here to give GHC a hint as to which type
  -- should be used in the case of an Integral (here, we choose Integer, for
  -- safety).
  floatingOrInteger :: RealFloat b => Scientific -> Either b Integer
  floatingOrInteger = S.floatingOrInteger

-- | Parse a single JSON boolean as a 'Bool'.
asBool :: Parse err Bool
asBool = as patBool TyBool

-- | Parse a JSON object, as an 'A.Object'. You should prefer functions like
-- 'eachInObject' where possible, since they will usually generate better
-- error messages.
asObject :: Parse err A.Object
asObject = as patObject TyObject

-- | Parse a JSON array, as an 'A.Array'. You should prefer functions like
-- 'eachInArray' where possible, since they will usually generate better
-- error messages.
asArray :: Parse err A.Array
asArray = as patArray TyArray

-- | Parse a single JSON null value. Useful if you want to throw an error in
-- the case where something is not null.
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

-- | Prefer to use functions like 'key or 'eachInObject' to this one where
-- possible, as they will generate better error messages.
withObject :: (A.Object -> Either err a) -> Parse err a
withObject = with asObject

-- | Prefer to use functions like 'nth' or 'eachInArray' to this one where
-- possible, as they will generate better error messages.
withArray :: (A.Array -> Either err a) -> Parse err a
withArray = with asArray
