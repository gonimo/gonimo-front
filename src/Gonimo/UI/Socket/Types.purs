module Gonimo.UI.Socket.Types where


import Prelude
import Data.Map as Map
import Gonimo.UI.Socket.Channel.Types as ChannelC
import Control.Monad.Reader (runReader)
import Control.Monad.Reader.Class (class MonadReader)
import Data.Foldable (foldl)
import Data.Lens (lens, LensP)
import Data.Lens.At (at)
import Data.Map (Map)
import Data.Maybe (maybe, fromMaybe, Maybe(Just, Nothing))
import Data.Monoid (mempty)
import Data.Tuple (uncurry, snd, Tuple(Tuple))
import Gonimo.Client.Types (GonimoError, class ReportErrorAction, Settings)
import Gonimo.Pux (noEffects)
import Gonimo.Server.DbEntities (Family(Family), Device(Device))
import Gonimo.Server.Types (DeviceType(Baby, NoBaby))
import Gonimo.Types (Secret(Secret), Key(Key))
import Gonimo.UI.Socket.Message (Message)
import Gonimo.WebAPI.Subscriber (receiveSocketByFamilyIdByToDevice, receiveSocketByFamilyIdByFromDeviceByToDeviceByChannelId)
import Gonimo.WebAPI.Types (AuthData(AuthData))
import Servant.Subscriber (Subscriptions)
import WebRTC.MediaStream (MediaStreamConstraints(MediaStreamConstraints), MediaStream)
import WebRTC.RTC (newRTCPeerConnection, RTCPeerConnection)

type Props ps = { settings :: Settings | ps }

type State =
  { authData :: AuthData
  , currentFamily :: Maybe (Key Family)
  , channels :: Map ChannelId ChannelC.State
  , onlineStatus :: Maybe BabyStation
  }

type BabyStation = { babyName  :: String
                   , mediaStream :: MediaStream
                   }

toDeviceType :: Maybe BabyStation -> DeviceType
toDeviceType Nothing = NoBaby
toDeviceType (Just station) = Baby station.babyName

data ChannelId = ChannelId (Key Device) Secret

makeChannelId :: Key Device -> Secret -> ChannelId
makeChannelId = ChannelId

toTheirId :: ChannelId -> Key Device
toTheirId (ChannelId theirId' _) = theirId'

toCSecret :: ChannelId -> Secret
toCSecret (ChannelId _ cSecret') = cSecret'

instance eqChannelId :: Eq ChannelId where
  eq (ChannelId theirId1 channel1) (ChannelId theirId2 channel2) = eq theirId1 theirId2
                                                                   && eq channel1 channel2

instance ordChannelId :: Ord ChannelId where
  compare (ChannelId theirId1 channel1) (ChannelId theirId2 channel2) =
    -- Not using the semigroup instance of Ordering, because we are not Haskell, we are not lazy.
    -- Using the semigroup instance would run the channel compare everytime!
    case compare theirId1 theirId2 of
      EQ -> compare channel1 channel2
      GT -> GT
      LT -> LT


data Action = AcceptConnection ChannelId
            | AddChannel ChannelId ChannelC.State
            | ConnectToBaby (Key Device)
            | CloseChannel ChannelId
            | CloseBabyChannel ChannelId -- Close channel if it is acting as a baby station
              -- Channel actions for a given sender and channel id
            | ChannelA ChannelId ChannelC.Action
            | SwitchFamily (Key Family)
            | ServerFamilyGoOffline (Key Family) -- | A bit of a hack - for reliably switching families
            | SetAuthData AuthData
            | StartBabyStation String MediaStreamConstraints
            | StopBabyStation
            | ReportError GonimoError
            | Nop

instance reportErrorAction :: ReportErrorAction Action where
  reportError = ReportError

channels :: LensP State (Map ChannelId ChannelC.State)
channels = lens _.channels (_ { channels = _ })

channel :: ChannelId -> LensP State (Maybe ChannelC.State)
channel channelId' = channels <<< at channelId'
