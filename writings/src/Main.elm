module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Html.Attributes
import Markdown.Block
import Markdown.Html
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



-- COLORS


colors =
    { bg = rgb255 8 8 13
    , surface = rgb255 15 15 22
    , border = rgb255 26 26 37
    , text = rgb255 216 215 207
    , textDim = rgb255 138 137 146
    , textBright = rgb255 240 239 232
    , zap = rgb255 247 147 26
    }



-- FONTS


newsreader : Attribute msg
newsreader =
    Font.family
        [ Font.typeface "Newsreader"
        , Font.serif
        ]


inter : Attribute msg
inter =
    Font.family
        [ Font.typeface "Inter"
        , Font.sansSerif
        ]



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
    """Our Internet feeds are broken. Content platforms ignore value and optimize for engagement via clickbait, outrage, and bias confirmation\u{2014}leaving us zombified. Despite our efforts to fix this with more sophisticated algorithms, more moderation, or more rules, our content keeps getting worse.

So far, our efforts have only been aimed at the symptoms. We haven't been focusing on the root cancer: the ad revenue engine at the heart of our content platforms.

The ad revenue engine wants impressions or views to glue eyeballs to screens\u{2014}regardless of the cost to the individual or society. This incentive exists at the macro and micro level. Both content platforms and creators want to get clicks and views, and will do anything to get viewers to \u{201C}stay to the end\u{201D}.

> Imagine a piece of content so thought-provoking you launch out of your chair, go on a walk, and think about it. Your life may have changed, but the platform and the creator both consider it a loss when eyeballs look away.

A better kind of content platform is possible. The ad revenue engine can be replaced with something we call the *zapnet*.

The first half already exists in the form of Nostr, a decentralized content platform. On Nostr, a user can \u{201C}zap\u{201D} content they value. This sends a Bitcoin lightning micropayment directly from the user to the content author.

The second half, yet to be built, goes deeper than treating zaps as global votes in a popularity contest. Instead, it recognizes zaps as local, personal threads in a web of expressed value, which can be followed to build a content feed.

The core idea is simple. Start with the content the user has zapped. Consider the authors of that content. What have *they* zapped? These second-order zaps point to content that is novel to the user, valued by others, and probably aligned with the user's interests. Follow the flow of zaps further out into the zapnet, and you'll keep finding good content. You're simply asking, as many times as you like: \u{201C}and *then* where did the zaps flow?\u{201D}

The quality of content discovered this way will be drastically higher than in traditional feeds. On traditional feeds, passively tolerated slop is successful content\u{2014}the ad revenue engine rewards it, and the platform propagates it. On the zapnet, it doesn't matter how many people simply consume the content, or how long they stay engaged. Content only propagates via zaps: deliberate actions that reflect *genuine appreciation*.

**On traditional feeds, tolerated slop is successful. On the zapnet, valued content is all there is.**

Every zap has three immediate, natural consequences. The first is what we've already described: zap by zap, content that resonates spreads. Even a humble two-cent zap is a respectable part of the content's growing momentum.

The second consequence is that the author is rewarded financially as this momentum grows\u{2014}possibly into a tsunami of revenue.

The third consequence is profound. If every user's feed is built by following trails of zaps outward from the user, then every zap is a new pathway of content discovery\u{2014}a modification of the very structure of the zapnet, deepening and diversifying the content delivered not just to the user who zapped, but to anyone upstream of them. **To zap is to shape the zapnet.**

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
                "Writings \u{2014} Zapnet"

            ArticlePage slug ->
                case findArticle slug of
                    Just article ->
                        article.title ++ " \u{2014} Zapnet"

                    Nothing ->
                        "Not Found \u{2014} Zapnet"
    , body =
        [ Element.layout
            [ Background.color colors.bg
            , Font.color colors.text
            , inter
            , Font.size 16
            , padding 0
            ]
            (column
                [ width (fill |> maximum 680)
                , centerX
                , paddingXY 24 0
                ]
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
            )
        ]
    }


viewHeader : Element Msg
viewHeader =
    row
        [ width fill
        , paddingEach { top = 40, bottom = 32, left = 0, right = 0 }
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color colors.border
        , spacing 12
        ]
        [ link [ Font.bold, Font.size 20, Font.color colors.zap ]
            { url = "/writings/"
            , label = text "zapnet"
            }
        , el [ Font.size 15, Font.color colors.textDim ] (text "writings")
        ]


viewIndex : Element Msg
viewIndex =
    column
        [ width fill
        , paddingEach { top = 40, bottom = 64, left = 0, right = 0 }
        , spacing 16
        ]
        (List.map viewArticleCard articles)


viewArticleCard : Article -> Element Msg
viewArticleCard article =
    link
        [ width fill
        , padding 24
        , Border.width 1
        , Border.color colors.border
        , Border.rounded 8
        , mouseOver
            [ Border.color (rgba255 247 147 26 0.3)
            , Background.color colors.surface
            ]
        ]
        { url = "#" ++ article.slug
        , label =
            column [ spacing 8, width fill ]
                [ el
                    [ Font.size 20
                    , Font.semiBold
                    , Font.color colors.textBright
                    ]
                    (text article.title)
                , paragraph
                    [ Font.size 15
                    , Font.color colors.textDim
                    ]
                    [ text article.description ]
                , row
                    [ Font.size 13
                    , Font.color colors.textDim
                    , spacing 8
                    ]
                    [ text article.author
                    , el [ alpha 0.4 ] (text "\u{00B7}")
                    , text article.date
                    ]
                ]
        }


viewArticle : Article -> Element Msg
viewArticle article =
    column
        [ width fill
        , paddingEach { top = 32, bottom = 64, left = 0, right = 0 }
        , spacing 0
        ]
        [ link
            [ Font.size 14
            , Font.color colors.textDim
            , mouseOver [ Font.color colors.zap ]
            , paddingEach { bottom = 32, top = 0, left = 0, right = 0 }
            ]
            { url = "/writings/"
            , label = text "\u{2190} back"
            }
        , el
            [ newsreader
            , Font.size 32
            , Font.medium
            , Font.color colors.textBright
            , Region.heading 1
            , paddingEach { bottom = 12, top = 0, left = 0, right = 0 }
            ]
            (text article.title)
        , row
            [ Font.size 14
            , Font.color colors.textDim
            , spacing 8
            , paddingEach { bottom = 24, top = 0, left = 0, right = 0 }
            , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
            , Border.color colors.border
            , width fill
            ]
            [ text article.author
            , el [ alpha 0.4 ] (text "\u{00B7}")
            , text article.date
            ]
        , column
            [ width fill
            , paddingEach { top = 32, bottom = 0, left = 0, right = 0 }
            , newsreader
            , Font.size 18
            , spacing 20
            ]
            (renderMarkdownElmUi article.body)
        ]


viewNotFound : Element Msg
viewNotFound =
    column
        [ width fill
        , paddingEach { top = 40, bottom = 64, left = 0, right = 0 }
        , spacing 16
        ]
        [ el [ Font.size 24, Font.bold ] (text "Not found")
        , paragraph [] [ text "This article doesn't exist." ]
        , link [ Font.color colors.zap ]
            { url = "/writings/"
            , label = text "\u{2190} back to writings"
            }
        ]


findArticle : String -> Maybe Article
findArticle slug =
    List.head (List.filter (\a -> a.slug == slug) articles)



-- MARKDOWN RENDERING


renderMarkdownElmUi : String -> List (Element Msg)
renderMarkdownElmUi markdown =
    case
        markdown
            |> Markdown.Parser.parse
            |> Result.mapError (\_ -> "Parse error")
            |> Result.andThen (Markdown.Renderer.render elmUiRenderer)
    of
        Ok rendered ->
            rendered

        Err _ ->
            [ paragraph [] [ text markdown ] ]


elmUiRenderer : Markdown.Renderer.Renderer (Element Msg)
elmUiRenderer =
    { heading = renderHeading
    , paragraph = \children -> paragraph [ spacing 8 ] children
    , blockQuote =
        \children ->
            column
                [ Border.widthEach { left = 3, right = 0, top = 0, bottom = 0 }
                , Border.color colors.zap
                , paddingEach { left = 20, right = 0, top = 0, bottom = 0 }
                , Font.color colors.textDim
                , Font.italic
                , spacing 12
                ]
                children
    , html = Markdown.Html.oneOf []
    , text = \s -> el [] (text s)
    , codeSpan =
        \code ->
            el
                [ Font.family [ Font.monospace ]
                , Font.size 15
                , Background.color colors.surface
                , Border.width 1
                , Border.color colors.border
                , Border.rounded 4
                , paddingXY 4 2
                ]
                (text code)
    , strong = \children -> row [ Font.bold, Font.color colors.textBright ] children
    , emphasis = \children -> row [ Font.italic ] children
    , strikethrough = \children -> row [ Font.strike ] children
    , hardLineBreak = Html.br [] [] |> html
    , link =
        \{ destination } children ->
            newTabLink
                [ Font.color colors.zap
                , Font.underline
                , mouseOver [ alpha 0.8 ]
                ]
                { url = destination
                , label = row [] children
                }
    , image =
        \{ src, alt } ->
            image [ width fill ]
                { src = src
                , description = alt
                }
    , unorderedList =
        \items ->
            column [ spacing 8, paddingEach { left = 24, right = 0, top = 0, bottom = 0 } ]
                (List.map
                    (\(Markdown.Block.ListItem _ children) ->
                        row [ spacing 8, width fill ]
                            [ el [ alignTop ] (text "\u{2022}")
                            , paragraph [] children
                            ]
                    )
                    items
                )
    , orderedList =
        \startIndex items ->
            column [ spacing 8, paddingEach { left = 24, right = 0, top = 0, bottom = 0 } ]
                (List.indexedMap
                    (\i children ->
                        row [ spacing 8, width fill ]
                            [ el [ alignTop ] (text (String.fromInt (startIndex + i) ++ "."))
                            , paragraph [] children
                            ]
                    )
                    items
                )
    , codeBlock =
        \{ body } ->
            el
                [ Background.color colors.surface
                , Border.width 1
                , Border.color colors.border
                , Border.rounded 6
                , padding 16
                , width fill
                , Font.family [ Font.monospace ]
                , Font.size 14
                , scrollbarX
                ]
                (Html.pre [] [ Html.text body ] |> html)
    , thematicBreak =
        el
            [ width fill
            , Border.widthEach { top = 1, bottom = 0, left = 0, right = 0 }
            , Border.color colors.border
            , paddingEach { top = 16, bottom = 16, left = 0, right = 0 }
            ]
            none
    , table = \children -> column [ width fill, spacing 0 ] children
    , tableHeader = \children -> column [ Font.bold, width fill ] children
    , tableBody = \children -> column [ width fill ] children
    , tableRow = \children -> row [ width fill, spacing 16 ] children
    , tableCell = \_ children -> paragraph [ padding 8 ] children
    , tableHeaderCell = \_ children -> paragraph [ padding 8, Font.bold ] children
    }


renderHeading :
    { level : Markdown.Block.HeadingLevel
    , rawText : String
    , children : List (Element Msg)
    }
    -> Element Msg
renderHeading { level, children } =
    let
        attrs =
            case level of
                Markdown.Block.H1 ->
                    [ Font.size 24, Font.bold, Font.color colors.textBright ]

                Markdown.Block.H2 ->
                    [ Font.size 21, Font.semiBold, Font.color colors.textBright ]

                Markdown.Block.H3 ->
                    [ Font.size 19, Font.medium, Font.color colors.textBright ]

                _ ->
                    [ Font.size 18, Font.color colors.textBright ]
    in
    paragraph (attrs ++ [ paddingEach { top = 16, bottom = 0, left = 0, right = 0 } ]) children
