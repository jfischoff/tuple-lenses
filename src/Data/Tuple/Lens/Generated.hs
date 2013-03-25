{-# LANGUAGE TemplateHaskell, QuasiQuotes, NoMonomorphismRestriction, 
    ExtendedDefaultRules, NoDatatypeContexts, NondecreasingIndentation #-}
module Data.Tuple.Lens.Generated where
import Data.Tuple.Lens.TH
import Data.List (subsequences)

makeManyTuples . filter ((1<) . length) . subsequences $ [1..9]



