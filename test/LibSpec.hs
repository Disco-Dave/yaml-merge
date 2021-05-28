{-# LANGUAGE QuasiQuotes #-}

module LibSpec where

import Data.Aeson.QQ.Simple (aesonQQ)
import Lib (merge, mergeMany)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "merge" $ do
    it "combines two objects with no conflicts" $
      let first = [aesonQQ| { "a": 1, "b": 2 } |]
          second = [aesonQQ| { "c": 3, "d": 4 } |]
          expected = [aesonQQ| { "a": 1, "b": 2 , "c": 3, "d": 4 } |]
       in merge first second `shouldBe` expected

    it "combines two objects and replaces the rhs with the lhs if there is a conflict" $
      let first = [aesonQQ| { "a": 1, "b": 2, "d": 10 } |]
          second = [aesonQQ| { "c": 3, "d": 4 } |]
          expected = [aesonQQ| { "a": 1, "b": 2 , "c": 3, "d": 10 } |]
       in merge first second `shouldBe` expected

    it "combines arrays" $
      let first = [aesonQQ| [1,2,3,4] |]
          second = [aesonQQ| [5,6,7,8] |]
          expected = [aesonQQ| [1,2,3,4,5,6,7,8] |]
       in merge first second `shouldBe` expected

    it "strings are overwritten" $
      let first = [aesonQQ| "first" |]
          second = [aesonQQ| "second" |]
          expected = [aesonQQ| "first" |]
       in merge first second `shouldBe` expected

    it "numbers are overwritten" $
      let first = [aesonQQ| 1.23 |]
          second = [aesonQQ| 22 |]
          expected = [aesonQQ| 1.23 |]
       in merge first second `shouldBe` expected

    it "bools are overwritten" $
      let first = [aesonQQ| true |]
          second = [aesonQQ| false |]
          expected = [aesonQQ| true |]
       in merge first second `shouldBe` expected

    it "rhs is overwritten with the lhs even when the types don't match" $ do
      let first = [aesonQQ| "Ay" |]
          second = [aesonQQ| false |]
          expected = [aesonQQ| "Ay" |]
       in merge first second `shouldBe` expected

      let first = [aesonQQ| [1,2,3] |]
          second = [aesonQQ| false |]
          expected = [aesonQQ| [1,2,3] |]
       in merge first second `shouldBe` expected

      let first = [aesonQQ| { "name": "Nibbler"} |]
          second = [aesonQQ| "No" |]
          expected = [aesonQQ| { "name": "Nibbler"} |]
       in merge first second `shouldBe` expected

    it "can handle nested json" $
      let first =
            [aesonQQ| { 
              "name": "Nibbler",
              "title": "Commander",
              "nested": {
                "foo": "bar",
                "other": [1,2],
                "size": 12
              }
            }|]
          second =
            [aesonQQ| { 
              "name": "Fry",
              "age": 25,
              "nested": {
                "baz": false,
                "other": [3,4],
                "size": 20
              }
            }|]
          expected =
            [aesonQQ| { 
              "name": "Nibbler",
              "title": "Commander",
              "age": 25,
              "nested": {
                "foo": "bar",
                "baz": false,
                "other": [1,2,3,4],
                "size": 12
              }
            } |]
       in merge first second `shouldBe` expected

  describe "mergeMany" $ do
    it "an empty list is equal to any empty object" $
      mergeMany [] `shouldBe` [aesonQQ| {} |]

    it "the left most values take precendence" $
      let first =
            [aesonQQ| {
              "arr": [1],
              "first": "first",
              "obj": {
                "first": "first"
              }
            } |]
          second =
            [aesonQQ| {
              "arr": [2],
              "second": "second",
              "obj": {
                "second": "second"
              }
            } |]
          third =
            [aesonQQ| {
              "arr": [3],
              "third": "third",
              "obj": {
                "third": "third"
              }
            } |]
          fourth =
            [aesonQQ| {
              "arr": [4],
              "fourth": "fourth",
              "obj": {
                "first": "not first",
                "second": "not second",
                "third": "not third",
                "fourth": "fourth"
              }
            } |]
          expected =
            [aesonQQ| {
              "arr": [1,2,3,4],
              "first": "first",
              "second": "second",
              "third": "third",
              "fourth": "fourth",
              "obj": {
                "first": "first",
                "second": "second",
                "third": "third",
                "fourth": "fourth"
              }
            } |]
       in mergeMany [first, second, third, fourth] `shouldBe` expected
