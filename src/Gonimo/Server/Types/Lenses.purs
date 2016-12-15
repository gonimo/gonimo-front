module Gonimo.Server.Types.Lenses where

import Gonimo.Server.Types
import Data.Either as Either
import Data.Lens as Lens
import Prelude as Prelude
import Gonimo.Types (Secret(Secret))


_GonimoSecret :: Lens.PrismP AuthToken Secret
_GonimoSecret = Lens.prism GonimoSecret unwrap
  where
    unwrap (GonimoSecret x) = Either.Right x
    unwrap y = Either.Left y

_Tea :: Lens.PrismP Coffee Prelude.Unit
_Tea = Lens.prism (Prelude.const Tea) unwrap
  where
    unwrap Tea = Either.Right Prelude.unit
    unwrap y = Either.Left y

_EmailInvitation :: Lens.PrismP InvitationDelivery String
_EmailInvitation = Lens.prism EmailInvitation unwrap
  where
    unwrap (EmailInvitation x) = Either.Right x
    unwrap y = Either.Left y

_OtherDelivery :: Lens.PrismP InvitationDelivery Prelude.Unit
_OtherDelivery = Lens.prism (Prelude.const OtherDelivery) unwrap
  where
    unwrap OtherDelivery = Either.Right Prelude.unit
    unwrap y = Either.Left y

_NoBaby :: Lens.PrismP DeviceType Prelude.Unit
_NoBaby = Lens.prism (Prelude.const NoBaby) unwrap
  where
    unwrap NoBaby = Either.Right Prelude.unit
    unwrap y = Either.Left y

_Baby :: Lens.PrismP DeviceType String
_Baby = Lens.prism Baby unwrap
  where
    unwrap (Baby x) = Either.Right x
    unwrap y = Either.Left y

_FamilyName :: Lens.PrismP FamilyName { familyMemberName :: String, familyName :: String }
_FamilyName = Lens.prism FamilyName unwrap
  where
    unwrap (FamilyName r) = Either.Right r
    unwrap y = Either.Left y

familyName :: forall a b r. Lens.LensP FamilyName String
familyName = Lens.lens unwrap wrap
  where
    unwrap (FamilyName n) = n.familyName
    wrap (FamilyName f) n = FamilyName (f { familyName = n })
