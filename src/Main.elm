module Main exposing (main)

import Maybe.Extra exposing (values)
import List.Extra exposing (groupsOf)
import Http
import Html exposing (..)
import Browser
import Json.Decode exposing (..)
import Html.Attributes exposing (class, placeholder)
import Html.Events exposing (onInput)

type alias Flags = {}

type Msg
  = FetchArticlesDone (Result Http.Error (List Article))
  | FetchAuthorsDone (Result Http.Error (List Author))
  | SetQuery String

type alias Article =
  { title : String
  , content : String
  , userId : Int
  }

type alias Author =
  { name : String
  , id : Int
  }

type alias Model =
  { articles : List Article
  , authors : List Author
  , query : String
  }

init : Flags -> (Model, Cmd Msg)
init flags =
  ( { articles = []
    , authors = []
    , query = ""
    }
  , Cmd.batch [ fetchArticles, fetchAuthors ]
  )

decodeArticle : Decoder Article
decodeArticle =
  map3 Article
    (field "title" string)
    (field "body" string)
    (field "userId" int)

articlesDecoder : Decoder (List Article)
articlesDecoder = list decodeArticle

fetchArticles : Cmd Msg
fetchArticles =
  Http.get
    { url = "https://jsonplaceholder.typicode.com/posts"
    , expect = Http.expectJson FetchArticlesDone articlesDecoder
    }

decodeAuthor : Decoder Author
decodeAuthor =
  map2 Author
    (field "name" string)
    (field "id" int)

authorsDecoder : Decoder (List Author)
authorsDecoder = list decodeAuthor

fetchAuthors : Cmd Msg
fetchAuthors =
  Http.get
    { url = "https://jsonplaceholder.typicode.com/users"
    , expect = Http.expectJson FetchAuthorsDone authorsDecoder
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
  FetchArticlesDone result ->
    case result of
      Err _ ->
        let _ = Debug.log "result" result
         in (model, Cmd.none)
      Ok newArticles ->
        ({ model | articles = newArticles }, Cmd.none)
  FetchAuthorsDone result ->
    case result of
      Err _ ->
        let _ = Debug.log "result" result
          in (model, Cmd.none)
      Ok newAuthors ->
        ({ model | authors = newAuthors }, Cmd.none)
  SetQuery newQuery ->
    ({ model | query = newQuery }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch []

articlesWithAuthors : List Article -> List Author -> List (Article, Author)
articlesWithAuthors articles authors =
  let f art =
        authors
          |> List.filter (\auth -> auth.id == art.userId)
          |> List.head
          |> Maybe.map (\author -> (art, author))
   in values <| List.map f articles

articleRowView : List (Article, Author) -> Html a
articleRowView  articles =
  div [ class "row" ] (List.map articleView articles)

articleView : (Article, Author) -> Html a
articleView (article, author) =
  div
    [ class "article"
    , class "four"
    , class "columns"
    ] [ p [class "title"] [text article.title]
      , p [class "content"] [text article.content]
      , p [class "author"] [text author.name]
      ]

view : Model -> Html Msg
view model =
  let
      articles =
        if String.length model.query > 0
        then
          articlesWithAuthors model.articles model.authors
            |> List.filter (\(article, author) -> String.contains model.query author.name)
        else
          articlesWithAuthors model.articles model.authors

      rows =
        groupsOf 3 articles
  in
      div []
        [ div [] [input [placeholder "Filter by author...", onInput SetQuery] []]
        , div [ class "container" ] (List.map articleRowView rows)
        ]

main : Program Flags Model Msg
main = Browser.element
  { init = init
  , subscriptions = subscriptions
  , update = update
  , view = view
  }
