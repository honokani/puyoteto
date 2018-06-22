import Html            as H
import Html                 exposing (Html,text,div,node)
import Html.Attributes      exposing (class,attribute)
import Array                exposing (Array)
import Maybe

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



type Cell a = Wall
            | Empty
            | FilledBy a

type alias Block = Maybe String

type alias Field = Array (Array (Cell Block))
type alias VisibleField = Array (Array (Html Msg))

type alias Model = { field : Field
                   , next  : Array (Array Block)
                   }



-- main
type Msg = SpendTime
         | NoOp

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
init = ( { field = initField, next = initBlock }
       , Cmd.none
       )

hight = 16
width = 8
initField = make2D width hight
            |> Array.indexedMap (\i ar ->
                    if i==0 || i==width-1 then
                        Array.repeat hight Wall
                    else
                        Array.set (hight-1) Wall ar
               )
initBlock = Array.repeat 2 <| Array.repeat 2 (Just "red")

-- update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = (model, Cmd.none)

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




watchCell : Cell Block -> Html Msg
watchCell cell = case cell of
    Wall        -> div [class "cell-wall"] [text "w"]
    FilledBy mc -> case mc of
        Just col -> div [class "cell-filled"] [div [class ("block_"++col)] []]
        _        -> div [class "cell-empty"] [text "e"]
    _          -> div [class "cell-empty"] [text "e"]

make2D : Int -> Int -> Array (Array (Cell Block))
make2D h w = Array.repeat h <| Array.repeat w Empty

