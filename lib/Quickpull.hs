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
-- One problem with this method is that code that is commented out
-- using the block commenting (with braces) will still be read; at
-- present a simple workaround is to comment out blocks of code using
-- per-line commenting instead.
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
-- If your needs are more complicated, study the "Quickpull.Types" and
-- "Quickpull.Runners" modules.  For example, you can:
--
-- * change the arguments you pass to the QuickCheck driver
--
-- * inspect each 'Decree' separately to change the way the test is
-- run for different 'Decree's
--
-- * filter the list of 'Decree' so that you run only some tests
-- rather than all of them
--
-- There is also the "Quickpull.Laws" module, which helps you test
-- common laws, such as the Monad laws and the Monoid laws.
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
--
-- For examples of usage, see the tests that are bundled with
-- Quickpull in the @tests/@ directory.  These tests use Quickpull to
-- test some common laws, such as the Monad and Monoid laws, on types
-- that come in the standard library.  There is a similar test for the
-- QuickCheck 'Test.QuickCheck.Gen' monad in the @quickcheck-tests/@
-- directory; interestingly enough, Gen does not satisfy the monad
-- laws.  To compile and run the @tests/@ tests, run cabal with the
-- @--enable-tests@ option; it will produce a binary named
-- @quickpull-tests@.  For the @quickcheck-tests/@ directory, run
-- Cabal with the @-fbuild-test-gen@ option; it will produce a binary
-- named @quickpull-test-gen@.
--
-- To see how to integrate Quickpull into a development workflow, see
-- the Quickpull source on Github at
--
-- <http://www.github.com/massysett/quickpull>
--
-- It has a @generate@ script that runs the @quickpull@ binary to
-- generate the appropriate @Decrees@ modules; after that, @cabal@ can
-- be used as usual to build the package, executables, and
-- distribution tarball.  That way, although your test suite will need
-- Quickpull listed as a build dependency so the tests will build,
-- your users will not need to do any preprocessing to run the tests.

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

  -- | Basic functions to run tests; for more complex needs, see
  -- "Quickpull.Runners".
  , quickCheckTree
  , defaultMain
  , defaultMainWith
  ) where

import Quickpull.Types
import Quickpull.Runners

