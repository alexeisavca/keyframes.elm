module Easing where

{-|
Easing functions for keyframes animation.
Stolen from Robert Penner's easing formulas http://gizma.com/easing/
#Easing
@docs Easing
#ease
@docs ease
#linear
@docs linear
#easeInQuad
@docs easeInQuad
#easeOutQuad
@docs easeOutQuad
#easeInOutQuad
@docs easeInOutQuad
#easeInCubic
@docs easeInCubic
#easeOutCubic
@docs easeOutCubic
#easeInOutCubic
@docs easeInOutCubic
#easeInQuart
@docs easeInQuart
#easeOutQuart
@docs easeOutQuart
#easeInOutQuart
@docs easeInOutQuart
#easeInQuint
@docs easeInQuint
#easeOutQuint
@docs easeOutQuint
#easeInOutQuint
@docs easeInOutQuint
#easeInSine
@docs easeInSine
#easeOutSine
@docs easeOutSine
#easeInOutSine
@docs easeInOutSine
#easeInExpo
@docs easeInExpo
#easeOutExpo
@docs easeOutExpo
#easeInOutExpo
@docs easeInOutExpo
#easeInCirc
@docs easeInCirc
#easeOutCirc
@docs easeOutCirc
#easeInOutCirc
@docs easeInOutCirc
-}

import Interpolation exposing (..)
import Keyframes exposing (Time, Animation)
import Dict exposing (map, fromList, toList, get)
import List exposing (map2)
import Maybe exposing (withDefault)

{-|
Represents an easing function. An easing function takes 5 arguments:

currentTime - a float from 0 to 1(inclusive) representing the current progress of the animation

totalTime - total time of an animation, always 1

progressRatio - same as currentTime

value - the initial value to be eased

change - the diff between initial value and the value at currentTime
-}
type alias Easing = Time -> Time -> Float -> Float -> Float -> Float

{-|Takes an easing function and an animation and returns the eased animation-}
ease : Easing -> Animation -> Animation
ease func tween t =
  let
    from = tween 0 |> fromList
    to = tween t |> fromList
    tValues property value =
      let
        numbersPlaceholder = placeholdNumbers value
        fromNumbers = extractNumbers value
        toNumbers = get property to |> withDefault value |> extractNumbers
        tValue a b = func t 1 t a (b-a)
      in map2 tValue fromNumbers toNumbers |> interpolateNumbers numbersPlaceholder
  in Dict.map tValues from |> toList

{-|Linear easing, i.e. no easing. This is in truth an identity function.-}
linear : Animation -> Animation
linear tween = tween

{-|Ease in quadratically

    transition "width" "1px" "100px" |> easeInQuad
    --returns an animation of width from 1px to 100px that is eased in quadratically
-}
easeInQuad : Animation -> Animation
easeInQuad = ease (\currentTime totalTime progressRatio value change -> change * progressRatio ^ 2 + value)

{-|Ease out quadratically-}
easeOutQuad : Animation -> Animation
easeOutQuad = ease (\currentTime totalTime progressRatio value change -> (negate change) * progressRatio * (progressRatio - 2) + value)

{-|Ease in and out quadratically-}
easeInOutQuad : Animation -> Animation
easeInOutQuad =
    let easing currentTime totalTime progressRatio value change =
        let t = currentTime / (totalTime / 2)
            t' = t - 1
        in if t < 1
            then change / 2 * t * t + value
            else change / 2 * (t' * (t' - 2) - 1) + value
    in ease easing

{-|Ease in cubically-}
easeInCubic : Animation -> Animation
easeInCubic = ease (\currentTime totalTime progressRatio value change -> change * progressRatio ^ 3 + value)

{-|Ease out cubically-}
easeOutCubic : Animation -> Animation
easeOutCubic = ease (\currentTime totalTime progressRatio value change -> change * ( ( progressRatio - 1 ) ^ 3 + 1 ) + value)

{-|Ease in and out cubically-}
easeInOutCubic : Animation -> Animation
easeInOutCubic =
    let easing currentTime totalTime progressRatio value change =
        let t = currentTime / ( totalTime / 2 )
            t' = t - 2
        in if t < 1
            then change / 2 * t ^ 3 + value
            else change / 2 * (t' ^ 3 + 2 ) + value
    in ease easing

{-|Ease in quartically-}
easeInQuart : Animation -> Animation
easeInQuart = ease (\currentTime totalTime progressRatio value change ->
    change * progressRatio ^ 4 + value)

{-|Ease out quartically-}
easeOutQuart : Animation -> Animation
easeOutQuart = ease (\currentTime totalTime progressRatio value change ->
    change * ( ( progressRatio - 1 ) ^ 4 - 1 ) + value )

{-|Ease in and out quartically-}
easeInOutQuart : Animation -> Animation
easeInOutQuart =
    let easing currentTime totalTime progressRatio value change =
        let t = currentTime / ( totalTime / 2 )
            t' = t - 2
        in if t < 1
            then change / 2 * t ^ 4 + value
            else change / 2 * (t' ^ 4 + 2 ) + value
    in ease easing

{-|Ease in quintically-}
easeInQuint : Animation -> Animation
easeInQuint = ease (\currentTime totalTime progressRatio value change ->
    change * progressRatio ^ 5 + value)

{-|Ease out quintically-}
easeOutQuint : Animation -> Animation
easeOutQuint = ease (\currentTime totalTime progressRatio value change ->
    change * ( ( progressRatio - 1 ) ^ 5 + 1 ) + value)

{-|Ease in and out quintically-}
easeInOutQuint : Animation -> Animation
easeInOutQuint =
    let easing currentTime totalTime progressRatio value change =
        let t = currentTime / ( totalTime / 2 )
            t' = t - 2
        in if t < 1
            then change / 2 * t ^ 5 + value
            else change / 2 * (t' ^ 5 + 2 ) + value
    in ease easing

{-|Ease in sinusoidally-}
easeInSine : Animation -> Animation
easeInSine = ease (\currentTime totalTime progressRatio value change ->
    (negate change ) * (cos (progressRatio * ( pi / 2 ) ) ) + change + value )

{-|Ease out sinusoidally-}
easeOutSine : Animation -> Animation
easeOutSine = ease (\currentTime totalTime progressRatio value change ->
    (negate change ) * (sin (progressRatio * ( pi / 2 ) ) ) + value )

{-|Ease in and out sinusoidally-}
easeInOutSine : Animation -> Animation
easeInOutSine = ease (\currentTime totalTime progressRatio value change ->
    (negate change ) / 2 * ( cos ( pi * progressRatio ) ) - 1 + value)

{-|Ease in exponentially-}
easeInExpo : Animation -> Animation
easeInExpo = ease (\currentTime totalTime progressRatio value change ->
    change * 2 ^ ( 10 * ( progressRatio - 1 ) ) + value)

{-|Ease out exponentially-}
easeOutExpo : Animation -> Animation
easeOutExpo = ease (\currentTime totalTime progressRatio value change ->
    change * 2 ^ (negate ( -10 * ( progressRatio + 1 ) ) ) + value)

{-|Ease in and out exponentially-}
easeInOutExpo : Animation -> Animation
easeInOutExpo =
    let easing currentTime totalTime progressRatio value change =
        let t = currentTime / ( totalTime / 2 )
            t' = t - 1
        in if t < 1
            then change / 2 * (2 ^ (10 * t') ) + value
            else change / 2 * (negate (2 ^ (-10 * t')) + 2) + value
    in ease easing

{-|Ease in circularly-}
easeInCirc : Animation -> Animation
easeInCirc = ease (\currentTime totalTime progressRatio value change ->
    (negate change) * ((sqrt (1 - progressRatio ^ 2)) - 1) + value)

{-|Ease in circularly-}
easeOutCirc : Animation -> Animation
easeOutCirc = ease (\currentTime totalTime progressRatio value change ->
    change * (sqrt (1 - (progressRatio - 1) ^ 2)) + value)

{-|Ease in and out circularly-}
easeInOutCirc : Animation -> Animation
easeInOutCirc =
    let easing currentTime totalTime progressRatio value change =
        let t = currentTime / ( totalTime / 2 )
            t' = t - 2
        in if t < 1
            then (negate change) / 2 * ((sqrt (1 - t ^ 2)) - 1) + value
            else change / 2 * ((sqrt (1 - t' ^ 2)) + 1) + value
    in ease easing
