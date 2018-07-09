# these-skinny

The `these` haskell package has some serious bloat. In particular, it depends on 'QuickCheck'
(just to provide 'Arbitrary' typeclass instances), aeson, and has a large, extraneous
API that in my opinion should be another package. This package cuts `these` down to just `Data.These`.
