{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | A utility module for dealing with reading JSON, and generating good better
-- messages. Note, however, that this package only deals with generating good
-- error messages /after/ the JSON has been parsed into a 'Data.Aeson.Value' -
-- unfortunately, invalid JSON will still produce poor error messages.

module Data.Aeson.BetterErrors
  ( -- * The Parser type, and how to run it
    Parse
  , parse
  , parseStrict
  , parseValue
  -- * Basic parsers
  , asText
  , asString
  , asScientific
  , asIntegral
  , asRealFloat
  , asBool
  , asNull
  , asObject
  , asArray

  -- * Traversing JSON
  , key
  , keyOrDefault
  , keyMay

  , nth
  , nthOrDefault
  , nthMay

  , eachInArray
  , eachInObject

  -- * Custom validations
  , withText
  , withString
  , withScientific
  , withIntegral
  , withRealFloat
  , withBool
  , withObject
  , withArray

  -- * Errors
  , ParseError(..)
  , PathPiece(..)
  , ErrorSpecifics(..)
  , displayError
  , displayPath
  , displaySpecifics

  -- * Miscellaneous
  , toAesonParser
  , JSONType(..)
  , jsonTypeOf
  ) where

import Data.Aeson (Value) -- for haddock
import Data.Aeson.BetterErrors.Internal

-- TODO Alternative?
-- use monoid, combine errors?
-- Simply take rightmost?
-- Or maybe take the one with the deepest path?
--
-- Or, export our own (<|>) that uses a semigroup (since errors are
-- 'nonempty') - that is, we don't want to allow failing with no error.
--
-- TODO
-- Interop with FromJSON? Can we make a FromJSON instance from a Parse err a?

-- Good for reaching into a structure and plucking out a particular value.
--
-- TODO
-- Display error

-- convention:
--   asFoo :: Parse err Foo
--   parseFoo' :: a -> Either err Foo
--   parseFoo :: ByteString -> Either (ParseError err) Foo
--
