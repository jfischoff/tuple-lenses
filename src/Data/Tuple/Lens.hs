-- | This package provides some stock lenses for manipulating multiple elements
--   of a tuple at once. For instance:
-- 
--   >>> (1,2,3)^._1_3
--   (1, 3)
--
--   The package provide all lenses that are in increasing order, for FieldN classes 1-9. If you 
--   want to swap the order or premute the elements, you need to make your own.
--   
--   Custom combos are provided by a Template Haskell function.
--  
--   >>> ('a','b','c','d') ^. $(tl [4,1,2,3])
--   ('d','a','b','c')

{-# LANGUAGE TemplateHaskell, QuasiQuotes, NoMonomorphismRestriction, 
    ExtendedDefaultRules, NoDatatypeContexts, NondecreasingIndentation #-}
module Data.Tuple.Lens (
    -- *** Template function for generating custom disjoint lens expressions
    declareLens,
    tl,
    -- ** Generated combos
    module Data.Tuple.Lens.Generated
) where
import Data.Tuple.Lens.Generated
import Data.Tuple.Lens.TH (tl, declareLens)
