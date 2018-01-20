module FRP.Event.Multiplex where

import Control.Alternative (empty, (<|>))
import Data.Maybe (Maybe(..))
import Data.Record (delete, get, insert)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Variant (Variant, expand, inj, on, prj)
import FRP.Event (Event, filterMap, fix)
import Prelude (const, ($), (<$>), (>>>))
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(..), kind RowList)

fixMany :: forall events values o. Multiplex events values =>
  (Record events -> { inputs :: Record events, output :: Event o }) -> Event o
fixMany f = fix $ (demux :: Event (Variant values) -> Record events) >>>
  f >>> \{ inputs, output } -> { input: mux inputs, output }

class Multiplex (events :: # Type) (values :: # Type) where
  mux :: Record events -> Event (Variant values)
  demux :: Event (Variant values) -> Record events

instance multiplex ::
  ( RowToList events eventRL
  , RowToList values valueRL
  , MultiplexRL eventRL valueRL events values
  ) => Multiplex events values
  where
    mux = muxRL (RLProxy :: RLProxy valueRL)
    demux = demuxRL (RLProxy :: RLProxy valueRL)

class MultiplexRL
  (eventRL :: RowList) (valueRL :: RowList)
  (events :: # Type) (values :: # Type)
  | eventRL -> events, valueRL -> values
  , eventRL -> valueRL
  , valueRL -> eventRL
  where
    muxRL :: RLProxy valueRL -> Record events -> Event (Variant values)
    demuxRL :: RLProxy valueRL -> Event (Variant values) -> Record events

instance multiplexNil :: MultiplexRL Nil Nil () () where
  muxRL _ _ = empty
  demuxRL _ _ = {}

instance multiplexCons ::
  ( IsSymbol l
  , MultiplexRL eventRL valueRL events' values'
  , RowLacks l values'
  , RowLacks l events'
  , RowCons l (Event v) events' events
  , RowCons l v values' values
  , Union values' bleh values
  ) => MultiplexRL (Cons l (Event v) eventRL) (Cons r v valueRL) events values where
  muxRL _ es = expand <$> e <|> inj l <$> get l es
    where
      l = SProxy :: SProxy l
      rl = RLProxy :: RLProxy valueRL
      e :: Event (Variant values')
      e = muxRL rl (delete l es)
  demuxRL _ e = insert l thisE otherEs
    where
      l = SProxy :: SProxy l
      rl = RLProxy :: RLProxy valueRL
      rej = on l (const Nothing) Just
      thisE = filterMap (prj l) e
      otherEs :: Record events'
      otherEs = demuxRL rl (filterMap rej e)
