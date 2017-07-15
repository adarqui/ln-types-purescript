module LN.T.Templates where
import LN.T.Resource
import LN.T.Leuron
import LN.T.LeuronTraining
import LN.T.Bucket


import Data.Argonaut.Core               (jsonEmptyObject)
import Data.Argonaut.Decode             (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode             (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Argonaut.Printer            (printJson)
import Data.Date.Helpers                (Date)
import Data.Either                      (Either(..))
import Data.Foreign                     (ForeignError(..), fail)
import Data.Foreign.NullOrUndefined     (unNullOrUndefined)
import Data.Foreign.Class               (class IsForeign, read, readProp)
import Data.Maybe                       (Maybe(..))
import Data.Tuple                       (Tuple(..))
import Purescript.Api.Helpers           (class QueryParam, qp)
import Network.HTTP.Affjax.Request      (class Requestable, toRequest)
import Network.HTTP.Affjax.Response     (class Respondable, ResponseType(..))
import Optic.Core                       ((^.), (..))
import Optic.Types                      (Lens, Lens')
import Prelude                          (class Show, show, class Eq, eq, pure, bind, ($), (<>), (<$>), (<*>), (==), (&&))
import Data.Default

import Purescript.Api.Helpers

newtype Templates = Templates {
  resourceRequest :: ResourceRequest,
  leuronRequest :: LeuronRequest,
  leuronTrainingRequest :: LeuronTrainingRequest,
  bucketRequest :: BucketRequest
}


type TemplatesR = {
  resourceRequest :: ResourceRequest,
  leuronRequest :: LeuronRequest,
  leuronTrainingRequest :: LeuronTrainingRequest,
  bucketRequest :: BucketRequest
}


_Templates :: Lens' Templates {
  resourceRequest :: ResourceRequest,
  leuronRequest :: LeuronRequest,
  leuronTrainingRequest :: LeuronTrainingRequest,
  bucketRequest :: BucketRequest
}
_Templates f (Templates o) = Templates <$> f o


mkTemplates :: ResourceRequest -> LeuronRequest -> LeuronTrainingRequest -> BucketRequest -> Templates
mkTemplates resourceRequest leuronRequest leuronTrainingRequest bucketRequest =
  Templates{resourceRequest, leuronRequest, leuronTrainingRequest, bucketRequest}


unwrapTemplates :: Templates -> {
  resourceRequest :: ResourceRequest,
  leuronRequest :: LeuronRequest,
  leuronTrainingRequest :: LeuronTrainingRequest,
  bucketRequest :: BucketRequest
}
unwrapTemplates (Templates r) = r

instance templatesEncodeJson :: EncodeJson Templates where
  encodeJson (Templates o) =
       "tag" := "Templates"
    ~> "resource_request" := o.resourceRequest
    ~> "leuron_request" := o.leuronRequest
    ~> "leuron_training_request" := o.leuronTrainingRequest
    ~> "bucket_request" := o.bucketRequest
    ~> jsonEmptyObject


instance templatesDecodeJson :: DecodeJson Templates where
  decodeJson o = do
    obj <- decodeJson o
    resourceRequest <- obj .? "resource_request"
    leuronRequest <- obj .? "leuron_request"
    leuronTrainingRequest <- obj .? "leuron_training_request"
    bucketRequest <- obj .? "bucket_request"
    pure $ Templates {
      resourceRequest,
      leuronRequest,
      leuronTrainingRequest,
      bucketRequest
    }


instance templatesRequestable :: Requestable Templates where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance templatesRespondable :: Respondable Templates where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkTemplates
      <$> readProp "resource_request" json
      <*> readProp "leuron_request" json
      <*> readProp "leuron_training_request" json
      <*> readProp "bucket_request" json


instance templatesIsForeign :: IsForeign Templates where
  read json =
      mkTemplates
      <$> readProp "resource_request" json
      <*> readProp "leuron_request" json
      <*> readProp "leuron_training_request" json
      <*> readProp "bucket_request" json

-- footer