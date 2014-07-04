{-# LANGUAGE RankNTypes #-}

-- | Quickpull - a simple testing framework to reduce boilerplating
--
-- Quickpull is a testing framework that helps you test multiple
-- QuickCheck properties with a minimum of boilerplating and
-- redundancy.  It consists of two parts: first, an executable file
-- which reads Haskell modules and extracts properties from them; and
-- second, this library, which runs the tests.
--
-- The executable, named @quickpull@ by default, is a preprocessor.
-- You have to run this executable to get the QuickCheck properties
-- from your modules.  It fetches single properties, which are values
-- that have type @'Testable' a@, as well as trees of properties,
-- which have type 'TestTree'.  For example, to get your properties if
-- they are in modules in a directory named @tests@, run this from
-- your project's root directory:
--
-- > $ quickpull tests
--
-- The results will be printed to standard output.  Inspect this
-- output, then redirect the output to a module named @Decrees@.
--
-- @quickpull@ does not know or parse Haskell.  It simply reads the
-- file and, following a few simple rules, copies the names of
-- properties and trees, along with metadata about them.  All single
-- properties must be named beginning with @prop_@, and trees must be
-- named beginning with @proptree_@.  The name of the property or tree
-- must be the first thing on the line, not preceded by spaces on the
-- line (which is probably how you write modules anyway if you are
-- using layout.)
--
-- @quickpull@ only gets tests from files ending in @.hs@, and the
-- property name must be the very first thing on the line; therefore,
-- literate Haskell files won't work.
--
-- You then need a program that actually runs your tests.  To get
-- started, you need just a short simple program, like this:
--
-- @ 
--    module Main where
--
--    import Decrees
--    import Quickpull
--
--    main = defaultMain decrees
-- @
--
-- When run, this program will run the tests, print the results to
-- standard output, and exit with an appropriate error code.
--
-- If your needs are more complicated, study this module.  For
-- example, you can:
--
-- * change the arguments you pass to the QuickCheck driver
--
-- * inspect each 'Decree' separately to change the way the test is
-- run for different 'Decree's
--
-- * filter the list of 'Decree' so that you run only some tests
-- rather than all of them
--
-- Advantages of Quickpull:
--
-- * reduces boilerplate and frees you from the tedious task of
-- assembling many tests from one module, and of assembling tests from
-- different modules into another module
--
-- * compiles quickly; no heavyweight dependencies.
--
-- * No Template Haskell.
--
-- Disadvantages:
--
-- * Requires you to run a preprocessor to get the tests
--
-- * No way to select at runtime which tests to run or the QuickCheck
-- parameters (at least, not out of the box; you could add this.)
--
-- * The preprocessor is dumb and inelegant.
--
-- * No support for Smallcheck or HUnit.
--
-- * Currently no way to test polymorphic properties; you will have to
-- monomorphise these yourself.

module Quickpull
  ( -- * Trees of 'Testable'
    TestTree
  , group
  , test

  -- * Test 'Article', 'Meta'data
  , ModDesc(..)
  , Meta(..)
  , Article(..)
  , Decree(..)

  -- * Runners
  , runTests
  , defaultMain
  , defaultMainWith
  ) where

import Test.QuickCheck (Testable)
import Quickpull.Types
import Quickpull.Runners

