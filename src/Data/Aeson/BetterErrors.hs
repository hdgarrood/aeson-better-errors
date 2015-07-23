{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | A module for decoding JSON, and generating good error messages. Note,
-- however, that this package only deals with generating good error messages
-- /after/ the JSON has been parsed into a 'Data.Aeson.Value' - unfortunately,
-- invalid JSON will still produce poor error messages.
--
-- See <http://harry.garrood.me/blog/aeson-better-errors/> for a tutorial.
--
-- Any kind of feedback is very welcome: suggestions for a better designed API,
-- bug reports, whatever - the best place for it is probably the GitHub issue
-- tracker: <https://github.com/hdgarrood/aeson-better-errors/issues>.

module Data.Aeson.BetterErrors
  ( -- * The Parser type
    ParseT
  , Parse
  , Parse'
  , mapError
  , (.!)

  -- * Basic parsers
  , FromJSONBetterErrors(..)
  , asIntegral
  , asRealFloat

  -- * Traversing JSON
  , perhaps

  , key
  , keyOrDefault
  , keyMay

  , nth
  , nthOrDefault
  , nthMay

  , eachInArray
  , forEachInObject
  , eachInObject
  , eachInObjectWithKey

  -- * Custom validations
  , with
  , withIntegral
  , withRealFloat
  , throwCustomError

  -- ** Monadic validators
  , withM
  , withIntegralM
  , withRealFloatM

  -- * Running parsers
  , parse
  , parseStrict
  , parseValue

  -- ** Monadic parsers
  , parseM
  , parseStrictM
  , parseValueM

  -- * Errors
  , ParseError(..)
  , ParseError'
  , PathPiece(..)
  , ErrorSpecifics(..)
  , ErrorSpecifics'
  , displayError
  , displayError'
  , displayPath
  , displaySpecifics
  , displaySpecifics'

  -- * Miscellaneous
  , toAesonParser
  , toAesonParser'
  , fromAesonParser
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
