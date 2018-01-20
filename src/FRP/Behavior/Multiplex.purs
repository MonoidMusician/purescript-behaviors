module FRP.Behavior.Multiplex where

import Data.Record (delete, get, insert)
import Data.Record.Builder (Builder)
import Data.Record.Builder as B
import Data.Symbol (class IsSymbol, SProxy(..))
import FRP.Behavior (ABehavior, Behavior)
import FRP.Event (Event)
import Prelude (compose, id, map, pure, (<$>), (<*>), (<<<), (<@>), (>>>))
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(..), kind RowList)

class Multiplex (behaviors :: # Type) (values :: # Type) where
  mux :: Record behaviors -> Behavior (Record values)
  demux :: Behavior (Record values) -> Record behaviors

instance multiplex ::
  ( RowToList behaviors behaviorRL
  , RowToList values valueRL
  , MultiplexRL behaviorRL valueRL behaviors values
  ) => Multiplex behaviors values
  where
    mux = muxRL (RLProxy :: RLProxy valueRL) >>> map (B.build <@> {})
    demux = demuxRL (RLProxy :: RLProxy valueRL)

class MultiplexRL
  (behaviorRL :: RowList) (valueRL :: RowList)
  (behaviors :: # Type) (values :: # Type)
  | behaviorRL -> behaviors, valueRL -> values
  , behaviorRL -> valueRL
  , valueRL -> behaviorRL
  where
    muxRL :: RLProxy valueRL -> Record behaviors -> Behavior (Builder {} (Record values))
    demuxRL :: RLProxy valueRL -> Behavior (Record values) -> Record behaviors

instance multiplexNil :: MultiplexRL Nil Nil () () where
  muxRL _ _ = pure id
  demuxRL _ _ = {}

instance multiplexCons ::
  ( IsSymbol l
  , MultiplexRL behaviorRL valueRL behaviors' values'
  , RowLacks l values'
  , RowLacks l behaviors'
  , RowCons l (Behavior v) behaviors' behaviors
  , RowCons l v values' values
  , Union values' bleh values
  ) => MultiplexRL (Cons l (ABehavior Event v) behaviorRL) (Cons r v valueRL) behaviors values where
  muxRL _ bs = compose <<< B.insert l <$> get l bs <*> b
    where
      l = SProxy :: SProxy l
      rl = RLProxy :: RLProxy valueRL
      b = muxRL rl (delete l bs)
  demuxRL _ b = insert l (get l <$> b) otherBs
    where
      l = SProxy :: SProxy l
      rl = RLProxy :: RLProxy valueRL
      otherBs :: Record behaviors'
      otherBs = demuxRL rl (map (delete l) b)
