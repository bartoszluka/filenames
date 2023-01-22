{-# LANGUAGE ImportQualifiedPost #-}

module FilteredDropdown (filteredDropdown) where

import Data.Default (def)
import Monomer
import Monomer.Widgets.Single (Single (singleRender), createSingle)
import Relude
import Prelude qualified

data FilteredDropdownState = FilteredDropdownState
    { inputField :: Text
    }

type FilteredDropdownCfg = ()

filteredDropdown selectedFieldLens listLens = defaultWidgetNode "filteredDropdown" newWidget
  where
    newWidget =
        createSingle
            ()
            def
                { singleRender =
                    \wenv node renderer -> do
                        renderLine renderer (Point 0 0) (Point 100 100)
                }
