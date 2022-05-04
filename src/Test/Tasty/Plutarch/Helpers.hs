module Test.Tasty.Plutarch.Helpers (
    ourStyle,
) where

import Text.PrettyPrint (Style, lineLength, style)

ourStyle :: Style
ourStyle = style{lineLength = 80}
