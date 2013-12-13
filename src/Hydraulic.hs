{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Hydraulic where
import Control.Lens
import Control.Lens.TH
import Control.Monad.Cont
import Control.Monad.Trans
import Control.Monad.RWS.Strict
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Monoid (mempty)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Void
import Pipes
import Network.HTTP.Types
import Network.HTTP.Types.Status
import Network.Wai

data ResourceState c = ResourceState
  { _resourceConfig :: c
  , _resourceRequest :: Request
  -- , _halt :: Response -> ContT Response IO () -- IO Response -> ContT Response IO Response
  }

type ResourceHandler c = RWST Request () (ResourceState c) (ContT Response IO)
type HaltFlag c = ResourceHandler c Bool

data AuthResult
  = Authorized
  | Unauthorized
  | Authenticate ByteString

class Haltable h r | h -> r where
  checkHalt :: Status -> h -> Either Response r

data HaltStatus a
  = Success a
  | Error Text
  | Halt Response

instance Haltable (HaltStatus a) a where
  checkHalt s h = case h of
    Success x -> Right x
    Error issue -> Left undefined
    Halt resp -> Left resp

instance Haltable Bool Bool where
  checkHalt s h = if h
    then Left undefined
    else Right h

data Resource c a = Resource
  { _rResourceExists :: HaltFlag c
  , _rIsServiceAvailable :: HaltFlag c
  , _rIsAuthRequired :: HaltFlag c
  , _rIsAuthorized :: ResourceHandler c (HaltStatus AuthResult)
  , _rIsForbidden :: HaltFlag c
  , _rAllowMissingPost :: HaltFlag c
  , _rIsMalformedRequest :: HaltFlag c
  , _rIsUriTooLong :: HaltFlag c
  , _rIsKnownContentType :: HaltFlag c
  , _rAreContentHeadersValid :: HaltFlag c
  , _rIsEntityLengthValid :: HaltFlag c
  , _rOptions :: ResourceHandler c [Header]
  , _rAllowedMethods :: ResourceHandler c [Method]
  , _rKnownMethods :: ResourceHandler c [Method]
  , _rIsDeletionSuccessful :: HaltFlag c
  , _rIsDeletionCompleted :: HaltFlag c
  , _rPostIsCreate :: ResourceHandler c Bool
  , _rCreatePath :: ResourceHandler c ByteString
  , _rProcessPost :: HaltFlag c
  , _rContentTypesProvided :: ResourceHandler c (HashMap ByteString (a -> ByteString))
  , _rContentTypesAccepted :: ResourceHandler c (HashMap ByteString (ByteString -> Either Text a))
  , _rCharsetsProvided :: ResourceHandler c (HashMap ByteString (ByteString -> ByteString))
  , _rEncodingsProvided :: ResourceHandler c (HashMap ByteString (ByteString -> ByteString))
  , _rVariances :: ResourceHandler c [Header]
  , _rIsConflict :: ResourceHandler c Bool
  , _rHasMultipleChoices :: HaltFlag c
  , _rPreviouslyExisted :: HaltFlag c
  , _rMovedPermanently :: ResourceHandler c (HaltStatus (Maybe ByteString))
  , _rMovedTemporarily :: ResourceHandler c (HaltStatus (Maybe ByteString))
  , _rLastModified :: ResourceHandler c (Maybe UTCTime)
  , _rExpires :: ResourceHandler c (Maybe UTCTime)
  , _rGenerateETag :: ResourceHandler c (Maybe ByteString)
  , _rFinishRequest :: ResourceHandler c Bool
  }

makeFieldsWith defaultFieldRules ''Resource
	
defaultResource :: Resource c a
defaultResource = Resource
  { _rIsServiceAvailable = return True
  , _rResourceExists = return True
  , _rIsAuthRequired = return True
  , _rIsAuthorized = return $ Success Authorized
  , _rIsForbidden = return False
  , _rAllowMissingPost = return False
  , _rIsMalformedRequest = return False
  , _rIsUriTooLong = return False
  -- known_content_type: true
  -- valid_content_headers: true
  , _rIsEntityLengthValid = return True
  , _rOptions = return []
  , _rAllowedMethods = return [methodGet, methodHead]
  , _rKnownMethods = return [methodGet, methodHead, methodPost, methodPut, methodDelete, methodPatch, methodTrace, methodConnect, methodOptions]
  , _rContentTypesProvided = return mempty
  , _rContentTypesAccepted = return mempty
  }

checkEarlyTerminators :: Haltable h r => (Status -> h -> ResourceHandler c r) -> ResourceHandler c ()
checkEarlyTerminators onHalt = do
  rsrc <- ask
  -- B13: Available?
  onHalt serviceUnavailable503 =<< rsrc ^. isServiceAvailable
  -- B12: Known method?
  req <- fmap _resourceRequest get
  onHalt notImplemented501 =<< fmap (elem $ requestMethod req) (rsrc ^. knownMethods)
  -- B11: URI too long?
  onHalt requestURITooLong414 =<< rsrc ^. isUriTooLong
  -- B10: Is method allowed?
  req <- fmap _resourceRequest get
  onHalt methodNotAllowed405 =<< fmap (elem $ requestMethod req) (rsrc ^. allowedMethods)
  -- B9: Malformed?
  onHalt badRequest400 =<< rsrc ^. isMalformedRequest
  -- B8: Authorized?
  onHalt unauthorized401 =<< rsrc ^. isAuthorized
  -- B7: Forbidden?
  onHalt forbidden403 =<< rsrc ^. isForbidden
  -- B6: Unknown or unsupported content header?
  -- TODO: supported content header?
  -- B5: Unknown Content-Type?
  onHalt unsupportedMediaType415 =<< fmap not (rsrc ^. isKnownContentType)
  -- B4: Request entity too large?
  onHalt requestEntityTooLarge413 =<< fmap not (rsrc ^. isEntityLengthValid)
  -- B3: OPTIONS?
  -- TODO: options
  return ()

checkAcceptHeaders :: Haltable h r => (Status -> h -> ResourceHandler c r) -> ResourceHandler c ()
checkAcceptHeaders onHalt = do
  let unacceptable = onHalt notAcceptable406
  -- C3: Accept exists?
  -- TODO: acceptExists
    -- True => C4: Acceptable media type available?
    -- False => 
      -- D4: Accept language exists?
        -- True => D5: Acceptable language available?
        -- False =>
  -- D5
  -- E5
  -- E6
  -- F6
  -- F7
  return ()

runHydraulic :: c -> Resource c a -> Request -> IO Response
runHydraulic c rsrc req = (flip runContT) return $ callCC $ \halt -> do
  (r, ()) <- (flip evalRWST) req (ResourceState c req) $ do
    let
      onHalt :: Haltable h r => Status -> h -> ResourceHandler c r
      onHalt s h = case checkHalt s h of
        -- since halt exits the computation, we can restrict its type to `Response -> ResourceHandler c Void`
        -- and then use vacuousM to work around monomorphism restrictions for its return type
        Left resp -> vacuousM $ lift $ halt resp
        Right x -> return x
    checkEarlyTerminators onHalt
    checkAcceptHeaders onHalt
    return undefined
  return r
