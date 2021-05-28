module Lib (
  merge,
  mergeMany,
  combineFiles,
) where

import Data.Aeson (Value (..))
import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Yaml as Yaml
import System.Environment (getArgs)

merge :: Value -> Value -> Value
merge valueA valueB =
  case (valueA, valueB) of
    (Array arrA, Array arrB) -> Array $ arrA <> arrB
    (Object objA, Object objB) -> Object $ HashMap.unionWith merge objA objB
    _ -> valueA

mergeMany :: Foldable f => f Value -> Value
mergeMany = foldr merge $ Object HashMap.empty

combineFiles :: IO ()
combineFiles =
  let readFiles = traverse Yaml.decodeFileThrow
      outputYaml = ByteString.putStr . Yaml.encode
   in (getArgs >>= readFiles) >>= outputYaml . mergeMany
