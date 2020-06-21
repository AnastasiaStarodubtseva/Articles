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
  , userId : Int
  }

type alias Author =
  { name : String
  , id : Int
  }

type alias Model =
  { articles : List Article
  , authors : List Author
  }

type alias Flags = {}

type Msg
  = FetchArticlesDone (Result Http.Error (List Article))
  | FetchAuthorsDone (Result Http.Error (List Author))

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

init : Flags -> (Model, Cmd Msg)
init flags =
  ( { articles = []
    , authors = []
    }
  , Cmd.batch [ fetchArticles, fetchAuthors ]
  )

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

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch []

articleRowView : Model -> List Article -> Html a
articleRowView model articles =
  div [ class "row" ] (List.map (articleView model) articles)

articleView : Model -> Article -> Html a
articleView model article =
  let
      mAuthor =
        List.head (List.filter (\a -> a.id == article.userId) model.authors)
  in
      div
        [ class "article"
        , class "four"
        , class "columns"
        ] [ p [class "title"] [text article.title]
          , p [class "content"] [text article.content]
          , case mAuthor of 
              Nothing ->  text ""
              Just author -> 
                p [class "author"] [text author.name]
          ]

view : Model -> Html a
view model =
  let
      rows = groupsOf 3 model.articles
  in
      div []
        [ div [ class "container" ] (List.map (articleRowView model) rows)
        ]

main : Program Flags Model Msg
main = Browser.element
  { init = init
  , subscriptions = subscriptions
  , update = update
  , view = view
  }
