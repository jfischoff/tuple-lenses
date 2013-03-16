{-# LANGUAGE TemplateHaskell, QuasiQuotes, NoMonomorphismRestriction, 
    ExtendedDefaultRules, NoDatatypeContexts, NondecreasingIndentation #-}
module Data.Tuple.Lens (
    -- *** Template function for generating custom disjoint lens expressions
    tl,
    -- ** Generated combos
    module Data.Tuple.Lens.Generated
) where
import Data.Tuple.Lens.Generated
import Data.Tuple.Lens.TH (tl)
