module Interpolation where

import Regex exposing (..)
import Html exposing (div, text)
import List exposing (map, foldl)
import String exposing (toFloat)
import Result exposing (withDefault)

numberRegex = regex "[-]?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?"

replaceStr : (String, String) -> String -> String

replaceStr (a, b) = replace All (regex <| escape a) (\_ -> b)

sanitizeProperties : String -> String
sanitizeProperties = replaceStr ("3d", "THREE_D") >> replaceStr("2d", "TWO_D")

unsanitizeProperties : String -> String
unsanitizeProperties = replaceStr ("THREE_D", "3d") >> replaceStr("TWO_D", "2d")

placeholdNumbers : String -> String
placeholdNumbers = sanitizeProperties >> replace All numberRegex (\_ -> "$")

extractNumbers : String -> List Float
extractNumbers string =
  sanitizeProperties string
    |> find All numberRegex
    |> map .match
    |> map (withDefault 0 << String.toFloat)

interpolateNumbers : String -> List Float -> String
interpolateNumbers string numbers =
  unsanitizeProperties <| foldl (\number _ -> replaceStr ("$", toString number) string) string numbers
