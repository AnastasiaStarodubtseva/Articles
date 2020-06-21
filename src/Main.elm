module Main exposing (main)

import List.Extra exposing (groupsOf)
import Http
import Html exposing (..)
import Browser
import Json.Decode exposing (..)
import Html.Attributes exposing (class)

type alias Article =
  { title : String
  , content : String
  }

type alias Model =
  { articles : List Article
  }

type alias Flags = {}

type Msg
  = FetchArticlesDone (Result Http.Error (List Article))
  
decodeArticle : Decoder Article
decodeArticle = 
  map2 Article 
    (field "title" string)
    (field "body" string)

articlesDecoder : Decoder (List Article)
articlesDecoder = list decodeArticle

fetchArticles : Cmd Msg 
fetchArticles = 
  Http.get 
    { url = "https://jsonplaceholder.typicode.com/posts"
    , expect = Http.expectJson FetchArticlesDone articlesDecoder
    }

init : Flags -> (Model, Cmd Msg)
init flags = ({ articles = [] }, fetchArticles)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  FetchArticlesDone result ->
    case result of
      Err _ ->
        let _ = Debug.log "result" result
         in (model, Cmd.none)
      Ok newArticles ->
        ({ model | articles = newArticles }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch []

articleRowView : List Article -> Html a
articleRowView articles = 
  div [ class "row" ] (List.map articleView articles)

articleView : Article -> Html a
articleView article =
  div
    [ class "article"
    , class "four"
    , class "columns"
    ] [text article.title]

view : Model -> Html a
view model =
  let
      rows = groupsOf 3 model.articles
  in
      div []
        [ div [ class "container" ] (List.map articleRowView rows)
        ]

main : Program Flags Model Msg
main = Browser.element
  { init = init
  , subscriptions = subscriptions
  , update = update
  , view = view
  }
