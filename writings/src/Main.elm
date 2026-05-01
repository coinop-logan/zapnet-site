module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Markdown.Parser
import Markdown.Renderer
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



-- ARTICLES


type alias Article =
    { slug : String
    , title : String
    , description : String
    , author : String
    , date : String
    , body : String
    }


articles : List Article
articles =
    [ { slug = "zapnet-a-cure-for-slop"
      , title = "Zapnet: A Cure for Slop"
      , description = "Our Internet feeds are broken. A better kind of content platform is possible."
      , author = "Schalk"
      , date = "2026"
      , body = articleCureForSlop
      }
    ]


articleCureForSlop : String
articleCureForSlop =
    """Our Internet feeds are broken. Content platforms ignore value and optimize for engagement via clickbait, outrage, and bias confirmation—leaving us zombified. Despite our efforts to fix this with more sophisticated algorithms, more moderation, or more rules, our content keeps getting worse.

So far, our efforts have only been aimed at the symptoms. We haven't been focusing on the root cancer: the ad revenue engine at the heart of our content platforms.

The ad revenue engine wants impressions or views to glue eyeballs to screens—regardless of the cost to the individual or society. This incentive exists at the macro and micro level. Both content platforms and creators want to get clicks and views, and will do anything to get viewers to "stay to the end".

> Imagine a piece of content so thought-provoking you launch out of your chair, go on a walk, and think about it. Your life may have changed, but the platform and the creator both consider it a loss when eyeballs look away.

A better kind of content platform is possible. The ad revenue engine can be replaced with something we call the *zapnet*.

The first half already exists in the form of Nostr, a decentralized content platform. On Nostr, a user can "zap" content they value. This sends a Bitcoin lightning micropayment directly from the user to the content author.

The second half, yet to be built, goes deeper than treating zaps as global votes in a popularity contest. Instead, it recognizes zaps as local, personal threads in a web of expressed value, which can be followed to build a content feed.

The core idea is simple. Start with the content the user has zapped. Consider the authors of that content. What have *they* zapped? These second-order zaps point to content that is novel to the user, valued by others, and probably aligned with the user's interests. Follow the flow of zaps further out into the zapnet, and you'll keep finding good content. You're simply asking, as many times as you like: "and *then* where did the zaps flow?"

The quality of content discovered this way will be drastically higher than in traditional feeds. On traditional feeds, passively tolerated slop is successful content—the ad revenue engine rewards it, and the platform propagates it. On the zapnet, it doesn't matter how many people simply consume the content, or how long they stay engaged. Content only propagates via zaps: deliberate actions that reflect *genuine appreciation*.

**On traditional feeds, tolerated slop is successful. On the zapnet, valued content is all there is.**

Every zap has three immediate, natural consequences. The first is what we've already described: zap by zap, content that resonates spreads. Even a humble two-cent zap is a respectable part of the content's growing momentum.

The second consequence is that the author is rewarded financially as this momentum grows—possibly into a tsunami of revenue.

The third consequence is profound. If every user's feed is built by following trails of zaps outward from the user, then every zap is a new pathway of content discovery—a modification of the very structure of the zapnet, deepening and diversifying the content delivered not just to the user who zapped, but to anyone upstream of them. **To zap is to shape the zapnet.**

---

On the zapnet, we are no longer being fed by an algorithm that wins when we sit zombified in a chair. Zaps flow only when we recognize true value; this flow shapes the network; and the network populates our feeds with content that resonates.

No ads, no spam. No engagement farming. No centralized algorithmic manipulation. Just people paying for what they value, and an app that knows how to follow that trail.

Half the infrastructure already exists. The missing piece is an application that follows the flow of zaps and builds a feed from that flow.

If this vision calls to you like it calls to us, come say hi at [zapnet.info](https://zapnet.info). Let's bring this thing to life."""



-- MODEL


type Page
    = Index
    | ArticlePage String


type alias Model =
    { key : Nav.Key
    , page : Page
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , page = hashToPage url
      }
    , Cmd.none
    )


hashToPage : Url -> Page
hashToPage url =
    case url.fragment of
        Nothing ->
            Index

        Just "" ->
            Index

        Just frag ->
            ArticlePage frag



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        UrlRequested (Browser.External href) ->
            ( model, Nav.load href )

        UrlChanged url ->
            ( { model | page = hashToPage url }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title =
        case model.page of
            Index ->
                "Writings — Zapnet"

            ArticlePage slug ->
                case findArticle slug of
                    Just article ->
                        article.title ++ " — Zapnet"

                    Nothing ->
                        "Not Found — Zapnet"
    , body =
        [ div [ class "page" ]
            [ viewHeader
            , case model.page of
                Index ->
                    viewIndex

                ArticlePage slug ->
                    case findArticle slug of
                        Just article ->
                            viewArticle article

                        Nothing ->
                            viewNotFound
            ]
        ]
    }


viewHeader : Html Msg
viewHeader =
    header [ class "site-header" ]
        [ a [ class "site-title", href "/writings/" ]
            [ text "zapnet" ]
        , span [ class "site-subtitle" ] [ text "writings" ]
        ]


viewIndex : Html Msg
viewIndex =
    div [ class "content" ]
        (List.map viewArticleCard articles)


viewArticleCard : Article -> Html Msg
viewArticleCard article =
    a [ class "article-card", href ("#" ++ article.slug) ]
        [ h2 [ class "article-card-title" ] [ text article.title ]
        , p [ class "article-card-desc" ] [ text article.description ]
        , div [ class "article-card-meta" ]
            [ span [] [ text article.author ]
            , span [ class "meta-sep" ] [ text "·" ]
            , span [] [ text article.date ]
            ]
        ]


viewArticle : Article -> Html Msg
viewArticle article =
    div [ class "content article" ]
        [ a [ class "back-link", href "/writings/" ] [ text "\u{2190} back" ]
        , h1 [ class "article-title" ] [ text article.title ]
        , div [ class "article-meta" ]
            [ span [] [ text article.author ]
            , span [ class "meta-sep" ] [ text "\u{00B7}" ]
            , span [] [ text article.date ]
            ]
        , div [ class "article-body" ]
            (renderMarkdown article.body)
        ]


viewNotFound : Html Msg
viewNotFound =
    div [ class "content" ]
        [ h1 [] [ text "Not found" ]
        , p [] [ text "This article doesn't exist." ]
        , a [ href "/writings/" ] [ text "\u{2190} back to writings" ]
        ]


findArticle : String -> Maybe Article
findArticle slug =
    List.head (List.filter (\a -> a.slug == slug) articles)


renderMarkdown : String -> List (Html Msg)
renderMarkdown markdown =
    case
        markdown
            |> Markdown.Parser.parse
            |> Result.mapError (\_ -> "Parse error")
            |> Result.andThen (Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer)
    of
        Ok rendered ->
            rendered

        Err _ ->
            [ pre [] [ text markdown ] ]
