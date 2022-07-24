{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType          #-}

module Network.TypedProtocol.ReqResp.Codec where

import           Data.Singletons

import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.PingPong.Codec (decodeTerminatedFrame)
import           Network.TypedProtocol.ReqResp.Type
import           Text.Read (readMaybe)


codecReqResp ::
    forall req resp m
  . (Monad m, Show req, Show resp, Read req, Read resp)
  => Codec (ReqResp req resp) CodecFailure m String
codecReqResp =
    Codec{encode, decode}
  where
    encode :: forall req' resp'
                     (st  :: ReqResp req' resp')
                     (st' :: ReqResp req' resp')
           .  ( Show (Message (ReqResp req' resp') st st') )
           => Message (ReqResp req' resp') st st'
           -> String
    encode msg = show msg ++ "\n"

    decode :: forall req' resp' m'
                     (st :: ReqResp req' resp')
           .  (Monad m', Read req', Read resp', ActiveState st)
           => Sing st
           -> m' (DecodeStep String CodecFailure m' (SomeMessage st))
    decode stok =
      decodeTerminatedFrame '\n' $ \str trailing ->
        case (stok, break (==' ') str) of
          (SingIdle, ("MsgReq", str'))
             | Just resp <- readMaybe str'
            -> DecodeDone (SomeMessage (MsgReq resp)) trailing
          (SingIdle, ("MsgDone", ""))
            -> DecodeDone (SomeMessage MsgDone) trailing
          (SingBusy, ("MsgResp", str'))
             | Just resp <- readMaybe str'
            -> DecodeDone (SomeMessage (MsgResp resp)) trailing

          (_       , _     ) -> DecodeFail failure
            where failure = CodecFailure ("unexpected server message: " ++ str)


codecReqRespId ::
    forall req resp m
  . (Monad m, Show req, Show resp)
  => Codec (ReqResp req resp) CodecFailure m (AnyMessage (ReqResp req resp))
codecReqRespId = idCodec
