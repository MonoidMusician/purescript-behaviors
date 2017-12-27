module FRP.Behavior.Keyboard
  ( keys
  , modifiers
  , key
  ) where

import Prelude

import Data.Array (fromFoldable)
import Data.Filterable (filterMap)
import Data.Set as Set
import FRP.Behavior (Behavior, behavior)
import FRP.Event.Keyboard (withKeys)
import Web.UIEvents.Key (Key, toModifier)
import Web.UIEvents.Key.Internal.Modifier (Modifier)

-- | A `Behavior` which reports the keys which are currently pressed.
keys :: Behavior (Set.Set Key)
keys = behavior \e -> map (\{ value, keys: ks } -> value (Set.fromFoldable ks)) (withKeys e)

-- | A `Behavior` which reports currently pressed modifier keys.
modifiers :: Behavior (Set.Set Modifier)
modifiers = keys <#> filterMapSet toModifier
  where
    filterMapSet f = Set.fromFoldable <<< filterMap f <<< fromFoldable

-- | A `Behavior` which reports whether a specific key is currently pressed.
key :: Key -> Behavior Boolean
key k = Set.member k <$> keys
