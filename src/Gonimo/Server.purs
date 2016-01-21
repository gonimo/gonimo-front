module Gonimo.Server where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Aff.Console (log)
import qualified Control.Monad.Aff as Aff
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Data.Generic
import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Data.Argonaut.Encode


-- For gEncodeJson:
import Data.Argonaut.Core (Json(), jsonNull, fromBoolean, fromNumber, fromString, fromArray, fromObject)
import Data.Either (Either(), either)
import Data.Foldable (foldr)
import Data.Generic (Generic, GenericSpine(..), toSpine, GenericSignature(..), DataConstructor())
import Data.Int (toNumber)
import Data.List (List(..), fromList)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.String (fromChar, lastIndexOf, drop)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Data.Array (null, concatMap, filter)
import Type.Proxy (Proxy(..))
-- import Partial.Unsafe (unsafeCrashWith)


type FamilyId = Int
type EmailAddress = String

data Invitation = EmailInvitation EmailAddress

derive instance genericInvitation :: Generic Invitation

-- instance isForeignInvitation :: IsForeign Invitation where
--  read = readGeneric defaultOptions

sendInvitation :: forall e . FamilyId -> Invitation -> Affjax e String
sendInvitation fid inv = post "http://localhost:8081/families/10/invitations" (gAesonEncodeJson inv)

gAesonEncodeJson :: forall a. (Generic a) => a -> Json
gAesonEncodeJson = gAesonEncodeJson' sign <<< toSpine
  where sign = toSignature (Proxy :: Proxy a)

-- | Encode `GenericSpine` into `Json`.
gAesonEncodeJson' :: GenericSignature -> GenericSpine -> Json
gAesonEncodeJson' sign spine = case spine of
 SInt x            -> fromNumber $ toNumber x
 SString x         -> fromString x
 SChar x           -> fromString $ fromChar x
 SNumber x         -> fromNumber x
 SBoolean x        -> fromBoolean x
 SArray thunks     -> fromArray (gAesonEncodeJson' sign <<< (unit #) <$> thunks)
 SProd constr args -> case sign of
                        SigProd _ constrSigns -> gAesonEncodeProdJson' constrSigns constr args
                      --  _ -> unsafeCrashWith "Signature does not match value, please don't do that!" -- Not yet supported, waiting for purescript 0.8
 SRecord fields    -> fromObject $ foldr addField SM.empty fields
   where addField field = SM.insert field.recLabel
                                    (gEncodeJson' $ field.recValue unit)

gAesonEncodeProdJson' :: Array DataConstructor -> String -> Array (Unit -> GenericSpine) -> Json
gAesonEncodeProdJson' constrSigns constr args = fromObject
                                          $ SM.insert "tag" (encodeJson fixedConstr)
                                          $ SM.singleton "contents" flattenedArgs
  where
    fixedConstr      = case lastIndexOf "." constr of
                        Nothing -> constr
                        Just i -> drop (i+1) constr
    allConstrNullary = foldr (&&) true <<< map (\c -> null c.sigValues) $ constrSigns
    contents         = if allConstrNullary -- If no constructor has any values - serialize as string ...
                       then fromString constr
                       else flattenedArgs
    encodedArgs      = gAesonEncodeProdArgs constrSigns constr args
    flattenedArgs    = case encodedArgs of
                        [a] -> a
                        as  -> encodeJson as

gAesonEncodeProdArgs :: Array DataConstructor -> String -> Array (Unit -> GenericSpine) -> Array (Json)
gAesonEncodeProdArgs constrSigns constr args = gAesonEncodeJson' <$> sigValues <*> values
  where
   lSigValues = concatMap (\c -> c.sigValues)
                   <<< filter (\c -> c.sigConstructor == constr) $ constrSigns
   sigValues = (unit #) <$> lSigValues
   values = (unit #) <$> args
