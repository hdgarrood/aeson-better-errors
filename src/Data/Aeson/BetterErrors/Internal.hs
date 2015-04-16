{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aeson.BetterErrors.Internal where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Trans.Except

import qualified Data.Aeson as A

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL

newtype Parse err a
  = Parse { unParse :: ReaderT ParseReader (Except (ParseError err)) a }
  deriving (Functor, Applicative, Monad,
            MonadReader ParseReader, MonadError (ParseError err))

runParse :: Parse err a -> BL.ByteString -> Either (ParseError err) a
runParse (Parse p) str =
  case A.eitherDecode str of
    Left err -> Left (InvalidJSON err)
    Right value ->
      let initialReader = ParseReader DList.empty value
      in  runExcept (runReaderT p initialReader)

data ParseReader = ParseReader
  { rdrPath  :: DList PathPiece
  , rdrValue :: A.Value
  }

-- helper functions for ParseReader
appendPath :: PathPiece -> ParseReader -> ParseReader
appendPath p r = r { rdrPath = DList.snoc (rdrPath r) p }

setValue :: A.Value -> ParseReader -> ParseReader
setValue v r = r { rdrValue = v }

data PathPiece
  = ObjectKey Text
  | ArrayIndex Int
  deriving (Show, Eq, Ord)

data ParseError err
  = InvalidJSON String
  | BadSchema [PathPiece] (ErrorSpecifics err)
  deriving (Show, Eq)

data ErrorSpecifics err
  = KeyMissing Text
  | OutOfBounds Int
  | WrongType JSONType A.Value -- ^ Expected type, actual value
  | ExpectedIntegral Double
  | CustomError err
  deriving (Show, Eq)

data JSONType
  = TyObject
  | TyArray
  | TyString
  | TyNumber
  | TyBool
  | TyNull
  deriving (Show, Eq, Ord)

jsonTypeOf :: A.Value -> JSONType
jsonTypeOf (A.Object _) = TyObject
jsonTypeOf (A.Array _)  = TyArray
jsonTypeOf (A.String _) = TyString
jsonTypeOf (A.Number _) = TyNumber
jsonTypeOf (A.Bool _)   = TyBool
jsonTypeOf A.Null       = TyNull
