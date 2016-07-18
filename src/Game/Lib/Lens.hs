{-# LANGUAGE TemplateHaskell #-}

module Game.Lib.Lens
  ( module Lens.Micro.Platform
  , makeAllLenses
  ) where

import Lens.Micro.Platform
import Language.Haskell.TH

makeAllLenses :: [Name] -> DecsQ
makeAllLenses ns = concat <$> mapM makeLenses ns
