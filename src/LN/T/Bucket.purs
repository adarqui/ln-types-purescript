module LN.T.Bucket where



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

newtype BucketRequest = BucketRequest {
  displayName :: String,
  description :: (Maybe String),
  scoreLo :: Int,
  scoreHi :: Int,
  leurons :: (Array Int),
  resources :: (Array Int),
  categories :: (Array String),
  filters :: (Array Int),
  guard :: Int
}


type BucketRequestR = {
  displayName :: String,
  description :: (Maybe String),
  scoreLo :: Int,
  scoreHi :: Int,
  leurons :: (Array Int),
  resources :: (Array Int),
  categories :: (Array String),
  filters :: (Array Int),
  guard :: Int
}


_BucketRequest :: Lens' BucketRequest {
  displayName :: String,
  description :: (Maybe String),
  scoreLo :: Int,
  scoreHi :: Int,
  leurons :: (Array Int),
  resources :: (Array Int),
  categories :: (Array String),
  filters :: (Array Int),
  guard :: Int
}
_BucketRequest f (BucketRequest o) = BucketRequest <$> f o


mkBucketRequest :: String -> (Maybe String) -> Int -> Int -> (Array Int) -> (Array Int) -> (Array String) -> (Array Int) -> Int -> BucketRequest
mkBucketRequest displayName description scoreLo scoreHi leurons resources categories filters guard =
  BucketRequest{displayName, description, scoreLo, scoreHi, leurons, resources, categories, filters, guard}


unwrapBucketRequest :: BucketRequest -> {
  displayName :: String,
  description :: (Maybe String),
  scoreLo :: Int,
  scoreHi :: Int,
  leurons :: (Array Int),
  resources :: (Array Int),
  categories :: (Array String),
  filters :: (Array Int),
  guard :: Int
}
unwrapBucketRequest (BucketRequest r) = r

instance bucketRequestEncodeJson :: EncodeJson BucketRequest where
  encodeJson (BucketRequest o) =
       "tag" := "BucketRequest"
    ~> "display_name" := o.displayName
    ~> "description" := o.description
    ~> "score_lo" := o.scoreLo
    ~> "score_hi" := o.scoreHi
    ~> "leurons" := o.leurons
    ~> "resources" := o.resources
    ~> "categories" := o.categories
    ~> "filters" := o.filters
    ~> "guard" := o.guard
    ~> jsonEmptyObject


instance bucketRequestDecodeJson :: DecodeJson BucketRequest where
  decodeJson o = do
    obj <- decodeJson o
    displayName <- obj .? "display_name"
    description <- obj .? "description"
    scoreLo <- obj .? "score_lo"
    scoreHi <- obj .? "score_hi"
    leurons <- obj .? "leurons"
    resources <- obj .? "resources"
    categories <- obj .? "categories"
    filters <- obj .? "filters"
    guard <- obj .? "guard"
    pure $ BucketRequest {
      displayName,
      description,
      scoreLo,
      scoreHi,
      leurons,
      resources,
      categories,
      filters,
      guard
    }


instance bucketRequestRequestable :: Requestable BucketRequest where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance bucketRequestRespondable :: Respondable BucketRequest where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkBucketRequest
      <$> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "score_lo" json
      <*> readProp "score_hi" json
      <*> readProp "leurons" json
      <*> readProp "resources" json
      <*> readProp "categories" json
      <*> readProp "filters" json
      <*> readProp "guard" json


instance bucketRequestIsForeign :: IsForeign BucketRequest where
  read json =
      mkBucketRequest
      <$> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "score_lo" json
      <*> readProp "score_hi" json
      <*> readProp "leurons" json
      <*> readProp "resources" json
      <*> readProp "categories" json
      <*> readProp "filters" json
      <*> readProp "guard" json


newtype BucketResponse = BucketResponse {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  scoreLo :: Int,
  scoreHi :: Int,
  leurons :: (Array Int),
  resources :: (Array Int),
  categories :: (Array String),
  filters :: (Array Int),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


type BucketResponseR = {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  scoreLo :: Int,
  scoreHi :: Int,
  leurons :: (Array Int),
  resources :: (Array Int),
  categories :: (Array String),
  filters :: (Array Int),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}


_BucketResponse :: Lens' BucketResponse {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  scoreLo :: Int,
  scoreHi :: Int,
  leurons :: (Array Int),
  resources :: (Array Int),
  categories :: (Array String),
  filters :: (Array Int),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
_BucketResponse f (BucketResponse o) = BucketResponse <$> f o


mkBucketResponse :: Int -> Int -> String -> String -> (Maybe String) -> Int -> Int -> (Array Int) -> (Array Int) -> (Array String) -> (Array Int) -> Boolean -> Int -> (Maybe Date) -> (Maybe Date) -> (Maybe Date) -> BucketResponse
mkBucketResponse id userId name displayName description scoreLo scoreHi leurons resources categories filters active guard createdAt modifiedAt activityAt =
  BucketResponse{id, userId, name, displayName, description, scoreLo, scoreHi, leurons, resources, categories, filters, active, guard, createdAt, modifiedAt, activityAt}


unwrapBucketResponse :: BucketResponse -> {
  id :: Int,
  userId :: Int,
  name :: String,
  displayName :: String,
  description :: (Maybe String),
  scoreLo :: Int,
  scoreHi :: Int,
  leurons :: (Array Int),
  resources :: (Array Int),
  categories :: (Array String),
  filters :: (Array Int),
  active :: Boolean,
  guard :: Int,
  createdAt :: (Maybe Date),
  modifiedAt :: (Maybe Date),
  activityAt :: (Maybe Date)
}
unwrapBucketResponse (BucketResponse r) = r

instance bucketResponseEncodeJson :: EncodeJson BucketResponse where
  encodeJson (BucketResponse o) =
       "tag" := "BucketResponse"
    ~> "id" := o.id
    ~> "user_id" := o.userId
    ~> "name" := o.name
    ~> "display_name" := o.displayName
    ~> "description" := o.description
    ~> "score_lo" := o.scoreLo
    ~> "score_hi" := o.scoreHi
    ~> "leurons" := o.leurons
    ~> "resources" := o.resources
    ~> "categories" := o.categories
    ~> "filters" := o.filters
    ~> "active" := o.active
    ~> "guard" := o.guard
    ~> "created_at" := o.createdAt
    ~> "modified_at" := o.modifiedAt
    ~> "activity_at" := o.activityAt
    ~> jsonEmptyObject


instance bucketResponseDecodeJson :: DecodeJson BucketResponse where
  decodeJson o = do
    obj <- decodeJson o
    id <- obj .? "id"
    userId <- obj .? "user_id"
    name <- obj .? "name"
    displayName <- obj .? "display_name"
    description <- obj .? "description"
    scoreLo <- obj .? "score_lo"
    scoreHi <- obj .? "score_hi"
    leurons <- obj .? "leurons"
    resources <- obj .? "resources"
    categories <- obj .? "categories"
    filters <- obj .? "filters"
    active <- obj .? "active"
    guard <- obj .? "guard"
    createdAt <- obj .? "created_at"
    modifiedAt <- obj .? "modified_at"
    activityAt <- obj .? "activity_at"
    pure $ BucketResponse {
      id,
      userId,
      name,
      displayName,
      description,
      scoreLo,
      scoreHi,
      leurons,
      resources,
      categories,
      filters,
      active,
      guard,
      createdAt,
      modifiedAt,
      activityAt
    }


instance bucketResponseRequestable :: Requestable BucketResponse where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance bucketResponseRespondable :: Respondable BucketResponse where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkBucketResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "score_lo" json
      <*> readProp "score_hi" json
      <*> readProp "leurons" json
      <*> readProp "resources" json
      <*> readProp "categories" json
      <*> readProp "filters" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


instance bucketResponseIsForeign :: IsForeign BucketResponse where
  read json =
      mkBucketResponse
      <$> readProp "id" json
      <*> readProp "user_id" json
      <*> readProp "name" json
      <*> readProp "display_name" json
      <*> (unNullOrUndefined <$> readProp "description" json)
      <*> readProp "score_lo" json
      <*> readProp "score_hi" json
      <*> readProp "leurons" json
      <*> readProp "resources" json
      <*> readProp "categories" json
      <*> readProp "filters" json
      <*> readProp "active" json
      <*> readProp "guard" json
      <*> (unNullOrUndefined <$> readProp "created_at" json)
      <*> (unNullOrUndefined <$> readProp "modified_at" json)
      <*> (unNullOrUndefined <$> readProp "activity_at" json)


newtype BucketResponses = BucketResponses {
  bucketResponses :: (Array BucketResponse)
}


type BucketResponsesR = {
  bucketResponses :: (Array BucketResponse)
}


_BucketResponses :: Lens' BucketResponses {
  bucketResponses :: (Array BucketResponse)
}
_BucketResponses f (BucketResponses o) = BucketResponses <$> f o


mkBucketResponses :: (Array BucketResponse) -> BucketResponses
mkBucketResponses bucketResponses =
  BucketResponses{bucketResponses}


unwrapBucketResponses :: BucketResponses -> {
  bucketResponses :: (Array BucketResponse)
}
unwrapBucketResponses (BucketResponses r) = r

instance bucketResponsesEncodeJson :: EncodeJson BucketResponses where
  encodeJson (BucketResponses o) =
       "tag" := "BucketResponses"
    ~> "bucket_responses" := o.bucketResponses
    ~> jsonEmptyObject


instance bucketResponsesDecodeJson :: DecodeJson BucketResponses where
  decodeJson o = do
    obj <- decodeJson o
    bucketResponses <- obj .? "bucket_responses"
    pure $ BucketResponses {
      bucketResponses
    }


instance bucketResponsesRequestable :: Requestable BucketResponses where
  toRequest s =
    let str = printJson (encodeJson s) :: String
    in toRequest str


instance bucketResponsesRespondable :: Respondable BucketResponses where
  responseType =
    Tuple Nothing JSONResponse
  fromResponse json =
      mkBucketResponses
      <$> readProp "bucket_responses" json


instance bucketResponsesIsForeign :: IsForeign BucketResponses where
  read json =
      mkBucketResponses
      <$> readProp "bucket_responses" json

-- footer