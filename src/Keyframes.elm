module Keyframes where

{-|
Keyframes is a truly functional animation library.

It treats animations as functions of time over state,
allowing for complex yet easy combinations
of those via higher order functions.

We define time as range of floats [0, 1], zero being the beginning of the animation and 1
its end. This way, we can abstract animations from physical time and apply them for any duration.

We define state as a list of (String, String) tuples, first member being an identifier,
and second member being its value. For example:

     myStyle = [("width", "0"), ("opacity", "0")]
     myOtherStyle = [("marioX", "0", ("marioY", "0")]

And an animation is just a function that takes time(floats from 0 to 1)
and for any given value returns a state, thus, by consecutively calling
an animation with a certain step we can animate virtually anything.

#State
@docs State
#Time
@docs Time
#Animation
@docs Animation
#Timestamp
@docs Timestamp
#tween
@docs tween
#transition
@docs transition
#ensure
@docs ensure
#ensureProperty
@docs ensureProperty
#reverse
@docs reverse
#chain
@docs chain
#linger
@docs linger
#foreshadow
@docs foreshadow
#imposePresence
@docs imposePresence
#toAndFrom
@docs toAndFrom
#repeat
@docs repeat
#merge
@docs merge
#play
@docs play
#playOnce
@docs playOnce
#playInfinitely
@docs playInfinitely
-}

import Interpolation exposing (..)
import Dict exposing (fromList, toList, map, get, keys, empty)
import List exposing (map, filter, maximum, minimum)
import Maybe exposing (withDefault)
import Time exposing (fpsWhen, timestamp)
import Signal exposing (map, constant)

{-|A list of (String, String) tuples, first member being an identifier,
and second member being its value. For example:

    myStyle = [("width", "0"), ("opacity", "0")]
    myOtherStyle = [("marioX", "0", ("marioY", "0")]
-}
type alias State = List (String, String)
{-|A float from 0 to 1, inclusive, 0 representing the beginning of an animation an 1 its end-}
type alias Time = Float
{-|A function of Time over State-}
type alias Animation = Time -> State
{-|A tuple (Time, Animation), used for chaining-}
type alias Timestamp = (Time, Animation)

{-|Given two states, will return an animation that bridges them

    myAnimation = tween [("opacity", "0")] [("opacity", "1")]
-}
tween : State -> State -> Animation
tween from to t =
  let
    from' = fromList from
    to' = fromList to
    tValues = \property value ->
      let
        numbersPlaceholder = placeholdNumbers value
        fromNumbers = extractNumbers value
        toNumbers = get property to' |> withDefault value |> extractNumbers
        tValue a b = a + (b - a) * t
      in List.map2 tValue fromNumbers toNumbers |> interpolateNumbers numbersPlaceholder
  in Dict.map tValues from' |> toList

{-|Given a property name and two values for it, will return an animation
that bridges those two values

    transition "width" "1px" "100px"
    --same as tween [("width", "1px")] ["width", "100px"])
-}
transition : String -> String -> String -> Animation
transition property from to = tween [(property, from)] [(property, to)]

{-|Given a state, will create an animation that always returns that state.
Useful for stuff like _transformOrigin_

    ensure [("transformOrigin", "(0, 0)")]
    --same as transition "transformOrigin" "(0, 0)" "(0, 0)"
    --or tween [("transformOrigin", "(0, 0)")] [("transformOrigin", "(0, 0)")]
-}
ensure : State -> Animation
ensure state t = state

{-|Give a property name and its value, will return an animation that always returns (property, value)

    ensureProperty "width" "100px"
    --same as ensure[("width", "100px")]
-}
ensureProperty : String -> String -> Animation
ensureProperty property value = ensure [(property, value)]

{-|Reverses an animation

    fadeIn = transition "opacity" "0" "1"
    fadeOut = reverse fadeIn

-}
reverse : Animation -> Animation
reverse animation t = animation (1.0 - t)

{-|Chains animations, given their time of start in the new animation

    fadeIn : Animation
    fadeOut : Animation
    blink = chain[
            (0, fadeIn),--fadeIn will start at 0
            (0.5, fadeOut),--fadeOut will end at .5, and from here fadeOut will start
        ]
-}
chain : List Timestamp -> Animation
chain animations t =
  let
    animations' = Dict.fromList animations
    beforeT = (>=) t
    afterT = (<) t
    currentAnimationIndex = keys animations' |> filter beforeT |> maximum |> withDefault 0
    currentAnimation = get currentAnimationIndex animations'
    animationDuration = keys animations' |> filter afterT |> minimum |> withDefault 1.0 |> \min -> min - currentAnimationIndex
    relativeT = (t - currentAnimationIndex) / animationDuration
  in case currentAnimation of
    Just animation -> animation relativeT
    Nothing -> []

{-|Persists the last state of an animation for a given time

    fadeOut = transition "opacity" "1" "0"
    fadeOutAndStayThatWay = linger 0.5 animation
    --the target will fade until the middle of the animation, that it will retain its latest opacity until the end

-}
linger : Time -> Animation -> Animation
linger t animation = chain [
    (0, animation),
    (t, ensure <| animation 1.0)
  ]

{-|Persists the initial state of an animation for a given time before running the actual animation-}
foreshadow : Time -> Animation -> Animation
foreshadow t animation = chain [
    (0, ensure <| animation 0),
    (t, animation)
  ]

{-|Given the time of start and end of an animation(relative to the resulting animation),
persists the initial and final state of the argument animation

    blur = transition "opacity" "1" "0.5"
    blurWithDelay = imposePresence 0.25 0.75 blur
    --the target will stay opaque until one quarter of the animation
    --then, it will fade to 0.5 opacity until three quarters of animation's duration
    --after that, it will state at 0.5 opacity until the end of the animation
-}
imposePresence : Time -> Time -> Animation -> Animation
imposePresence from to animation = chain [
    (0, ensure <| animation 0),
    (from, animation),
    (to, ensure <| animation 1)
  ]

{-|A combination of _chain_ and _reverse._

    fadeOut = transition "opacity" "1" "0"
    blink = toAndFrom fadeOut
-}
toAndFrom : Animation -> Animation
toAndFrom animation = chain [
    (0, animation),
    (0.5, reverse animation)
  ]

{-|Repeats an animation

    blink : Animation
    blink4times = repeat 4 blink

-}
repeat : Int -> Animation -> Animation
repeat times animation =
  let
    times' = toFloat times
    time time' = if time' >= times' then [] else (1.0 - time'/times', animation) :: (time (time' + 1.0))
  in time 0.0 |> chain

{-|Merges animations

    collapseWidth = transition "width" "100%" "1%"
    collapseHeight = transition "height" "100%" "1%"
    shrink = merge [collapseWidth, collapseHeight]
    --same as tween [("width", "100%"), ("height", "100%")] [("width", "1%"), ("height", "1%")]

-}
merge : List Animation -> Animation
merge animations t =
    List.map (\animation -> animation t) animations
        |> List.map fromList
        |> List.foldl Dict.union Dict.empty
        |> toList

{-|Plays an animation in real time

    --play totalTime animation currentTime

    main =
        let
            animation = transition "opacity" "1" "0" |> play 500.0 --play for 500ms
            div' time = div [animation time] [text "testing"]
        in Signal.constant True |> fpsWhen 60 |> Signal.foldp (+) 0 |> Signal.map div'

-}
play: Time -> Animation -> Time -> State
play duration animation currentTime = currentTime / duration |> animation

{-|Same as _play_, but clamps _currentTime_ between _0_ and _1_(inclusive)

    playOnce totalTime animation currentTime
-}
playOnce: Time -> Animation -> Time -> State
playOnce duration animation currentTime =
  let currentTime' = clamp 0.0 duration currentTime
  in play duration animation currentTime'

{-|Plays an animation infinitely, in loops

    playInfinitely loopDuration animation timeSinceStart
-}
playInfinitely: Time -> Animation -> Time -> State
playInfinitely duration animation currentTime =
  let
    duration' = round duration
    currentTime' = round currentTime
    modulo = currentTime' % duration' |> toFloat
  in play duration animation modulo