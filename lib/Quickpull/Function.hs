-- | Helpers to generate random functions.
module Quickpull.Function where

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe

-- | Generate a random function.  This is simply a duplicate of what's
-- in the QuickCheck 'Arbitrary' instance for functions; having it
-- here saves you from having to define 'Arbitrary' and 'CoArbitrary'
-- instances for your types (which can require either orphan instances
-- or a lot of annoying wrapping and unwrapping.)
function
  :: (a -> Gen b -> Gen b)
  -> Gen b
  -> Gen (a -> b)
function perturb gen = promote (`perturb` gen)
