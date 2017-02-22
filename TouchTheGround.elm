import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window
import Graphics.Element exposing (..)
import Mouse

-- DEFINING THE MODEL

(courtWidth, courtHeight) = (1000,600)

type State = Play | Pause

type alias Ball =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  }

type alias Player =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , lives : Int
  }

player : Float -> Player
player y =
  Player 0 y 0 0 0

type alias Game =
  { state    : State
  , ball     : Ball
  , gameUser : Player
  }

defaultGame : Game
defaultGame =
  { state = Pause
  , ball  = Ball 0 0 500 400
  , gameUser = player (-285)
  }

type alias Input =
  { space  : Bool
  , dir : Int
  , delta  : Time
  }

-- SETTING UPDATES

update : Input -> Game -> Game
update {space,dir,delta} ({state,ball,gameUser} as game) =
  let
    death = ball.y < -(courtHeight/2 - 5)
    stillAlive = if not death && state == Play then 1 else 0

    newState =
      if space then
        Play
      else if death then
        Pause
      else
        state

    newBall =
      if state == Pause then
        ball
      else
        updateBall delta ball gameUser
  in
    { game |
        state = newState,
        ball  = newBall,
        gameUser = updatePlayer delta dir stillAlive gameUser
    }

updateBall : Time -> Ball -> Player -> Ball
updateBall dt ball paddle =
  if not (near 0 (courtHeight/2) ball.y) then
    { ball | x = 0, y = 0 }
  else
    definedUpdate dt
      { ball |
          vx = stepV ball.vx (ball.x < 25 - courtWidth/2) (ball.x > courtWidth/2 - 25),
          vy = stepV ball.vy (within paddle ball) (ball.y > courtHeight/2 - 25)
      }

updatePlayer : Time -> Int -> Int -> Player -> Player
updatePlayer dt dir stillAlive player =
  let
    movedPlayer =
      definedUpdate dt { player | vx = toFloat dir * 500 }
  in
    { movedPlayer |
      x = clamp (51 - courtWidth/2) (courtWidth/2 - 51) movedPlayer.x,
      lives = player.lives + stillAlive
    }

definedUpdate dt obj =
  { obj |
      x = obj.x + obj.vx * dt,
      y = obj.y + obj.vy * dt
  }

near k c n =
  n >= k-c && n <= k+c

within paddle ball =
  near paddle.x 51 ball.x && near paddle.y 10 ball.y

stepV v westSide eastSide =
  if westSide then
    abs v
  else if eastSide  then
    abs v
  else
    v

-- BUILDING THE VIEW

view : (Int, Int) -> Game -> Element
view (w,h) game =
  let
    lives =
      txt (Text.height 100) (toString game.gameUser.lives)
  in
      container w h middle <|
        collage courtWidth courtHeight
        [ rect courtWidth courtHeight
            |> filled courtCanvas
        , oval 15 15
            |> make game.ball
        , rect 100 20
            |> make game.gameUser
        , toForm lives
            |> move (0, courtHeight/2 - 60)
        , toForm (if game.state == Play then spacer 1 1 else txt identity msg)
          |> move (0, 60 - courtHeight/2)
        ]

courtCanvas =
  rgb 41 128 185
contentColor =
  rgb 236 240 241

txt f string =
  Text.fromString string
    |> Text.color contentColor
    |> Text.monospace
    |> Text.height 24
    |> f
    |> leftAligned

msg = "Press SPACE to start, &larr; and &rarr; to move"

make obj shape =
  shape
    |> filled contentColor
    |> move (obj.x, obj.y)

-- DEFINE SIGNALS

main =
  Signal.map2 view Window.dimensions gameState

gameState : Signal Game
gameState =
  Signal.foldp update defaultGame input

delta =
  Signal.map inSeconds (fps 35)
  -- using the fps is an intuitive function that gets as close
  -- to the desired FPS as possible so long as the browser can
  -- keep up.  If it can't keep up then it slows down gracefully

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map3 Input
      Keyboard.space
      (Signal.map .x Keyboard.arrows)
      delta
