
-- | A utility module for dealing with reading JSON, and generating good error
-- messages in the case of JSON with a bad schema.

module Data.Aeson.BetterErrors
  ( -- * The Parser type
    Parse
  , runParse
  -- * Basic parsers
  , PathPiece(..)
  , ParseError(..)
  , ErrorSpecifics(..)
  , JSONType(..)
  , jsonTypeOf
  ) where

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

-- convention:
--   asFoo :: Parse err Foo
--   parseFoo' :: a -> Either err Foo
--   parseFoo :: ByteString -> Either (ParseError err) Foo
--
