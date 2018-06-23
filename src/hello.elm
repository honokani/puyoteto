import Maybe
import List            as L
import Random          as Rnd
import Array                  exposing (Array)

import Html            as H
import Html                   exposing (Html,text,div,node)
import Html.Attributes        exposing (class,attribute)

import Time
import Keyboard        as Kbd

-- stylesheet setting for elm-reactor
stylesheet =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      "obj.css"
            ]
        children = []
    in
        node tag attrs children




type Colors = Red
            | Green
            | Blue
            | Purple

showCol : Colors -> String
showCol c = case c of
    Red -> "red"
    _   -> "black"

type Cell a = Wall
            | Empty
            | FilledBy a

type alias Block = Cell (Maybe Colors)

type alias Field = Array (Array Block)

type alias VisibleField = Array (Array (Html Msg))


-- main
type alias Model = { gState  : GState
                   , field   : Field
                   , curr    : Field
                   , next    : Field
                   , time    : Time
                   , rndSeed : Seed
                   }

type GState = Ready
            | Playing

type Msg = NoOp
         | StartGame
         | SpendTime

main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }


-- init
init : ( Model, Cmd Msg )
init = ( { gState = Ready
         , field  = initField
         , curr   = initBlock
         , next   = initBlock
         }
       , Cmd.none
       )

hight = 16
width = 8
initField : Field
initField = make2D width hight
            |> Array.indexedMap (\i ar ->
                    if i==0 || i==width-1 then
                        Array.repeat hight Wall
                    else
                        Array.set (hight-1) Wall ar
               )
initBlock : Field
initBlock = Array.repeat 2 <| Array.repeat 2 <| FilledBy <| Just Red


-- update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    StartGame ->
        (model, Cmd.none)
    SpendTime ->
        (model, Cmd.none)
    _         ->
        (model, Cmd.none)


-- view
view : Model -> Html Msg
view ({ field, next }) =
    let
        visibeF = Array.toList <| Array.map (\ar ->
                    (div []) <| Array.toList <| Array.map watchCell ar
                  )
                  field
        inner = div [class "field"] visibeF
    in
        div [class "outer"] [stylesheet, inner]








tetrisBlocks : List (Array (Array Bool))
tetrisBlocks = L.map (\bs -> Array.fromList <| List.map Array.fromList bs)
               [ [ [False, False, False, False]
                 , [False, True , True , False]
                 , [False, True , False, False]
                 , [False, True , False, False]
                 ]
               , [ [False, True , False, False]
                 , [False, True , False, False]
                 , [False, True , False, False]
                 , [False, True , False, False]
                 ]
               ]

encolorBlocks : Array (Array Bool) -> Field
encolorBlocks bss = Array.map (\bs ->
                        Array.map (\b -> case b of
                            True -> FilledBy <| Just Red
                            _    -> Empty
                        ) bs
                    ) bss


watchCell : Block -> Html Msg
watchCell cell = case cell of
    Wall        -> div [class "cell-wall"] [text "w"]
    FilledBy mc -> case mc of
        Just col -> div [class "cell-filled"] [div [class ("block_" ++ showCol col)] []]
        _        -> div [class "cell-empty"] [text "e"]
    _          -> div [class "cell-empty"] [text "e"]

make2D : Int -> Int -> Field
make2D h w = Array.repeat h <| Array.repeat w Empty





