module FRP.Event.Keyboard
  ( down
  , up
  , withKeys
  ) where

import FRP.Event (Event, filterMap)
import Prelude ((<#>))
import Web.UIEvents.Key (Key, parse)

-- | Create an `Event` which fires when a key is pressed
down :: Event Key
down = filterMap parse downImpl

foreign import downImpl :: Event String

-- | Create an `Event` which fires when a key is released
up :: Event Key
up = filterMap parse upImpl

foreign import upImpl :: Event String

-- | Create an event which also returns the current pressed keycodes.
withKeys :: forall a. Event a -> Event { value :: a, keys :: Array Key }
withKeys e = withKeysImpl e <#> \r -> r { keys = filterMap parse r.keys }

foreign import withKeysImpl :: forall a. Event a -> Event { value :: a, keys :: Array String }
