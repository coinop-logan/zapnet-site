module Main exposing (main)

import Browser
import Browser.Events
import Dict exposing (Dict)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- CONFIG


config =
    { width = 1200
    , height = 800
    , zapIntervalMs = 400
    , newNodeChance = 0.75
    , horizontalSpread = 180
    , verticalDrop = 60
    , verticalVariance = 30
    , lerpSpeed = 0.25
    , lineColor = "#f7931a"
    , lineOpacity = 0.4
    , lineWidth = "2"
    , nodeColor = "#f7931a"
    , nodeCoreColor = "#fff"
    , bgColor = "#08080d"
    , fadeStartMs = 5000
    , fadeDurationMs = 3000
    , reachDurationMs = 150
    , fieldSpawnRadius = 300
    , zapBrightColor = "#ffe066"
    , zapBrightWidth = "3"
    }



-- MODEL


type alias NodeId =
    Int


type alias Node =
    { id : NodeId
    , x : Float
    , y : Float
    , targetX : Float
    , targetY : Float
    , parent : Maybe NodeId
    , depth : Int
    , bornAt : Float
    , lastActiveAt : Float
    , settled : Bool
    }


type alias Edge =
    { from : NodeId
    , to : NodeId
    }


type alias ActiveZap =
    { from : NodeId
    , to : NodeId
    , startedAt : Float
    , treeTargetX : Float
    , treeTargetY : Float
    }


type alias Model =
    { nodes : Dict NodeId Node
    , edges : List Edge
    , activeZaps : List ActiveZap
    , nextId : NodeId
    , elapsed : Float
    , lastZapTime : Float
    , seed : Random.Seed
    }


init : () -> ( Model, Cmd Msg )
init () =
    let
        rootNode =
            { id = 0
            , x = config.width / 2
            , y = 40
            , targetX = config.width / 2
            , targetY = 40
            , parent = Nothing
            , depth = 0
            , bornAt = 0
            , lastActiveAt = 0
            , settled = True
            }
    in
    ( { nodes = Dict.singleton 0 rootNode
      , edges = []
      , activeZaps = []
      , nextId = 1
      , elapsed = 0
      , lastZapTime = 0
      , seed = Random.initialSeed 42
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            let
                newElapsed =
                    model.elapsed + delta

                modelWithTime =
                    { model | elapsed = newElapsed }

                timeSinceLastZap =
                    newElapsed - model.lastZapTime

                ( zappedModel, newLastZapTime ) =
                    if timeSinceLastZap >= config.zapIntervalMs then
                        ( initiateZap modelWithTime, newElapsed )

                    else
                        ( modelWithTime, model.lastZapTime )

                -- Process active zaps: check if any have completed reaching
                resolvedModel =
                    resolveCompletedZaps zappedModel

                lerpedModel =
                    lerpAllNodes resolvedModel

                cleanedModel =
                    removeFullyFaded lerpedModel
            in
            ( { cleanedModel
                | lastZapTime = newLastZapTime
              }
            , Cmd.none
            )


zapProgress : Float -> ActiveZap -> Float
zapProgress now zap =
    let
        elapsed =
            now - zap.startedAt
    in
    Basics.min 1.0 (elapsed / config.reachDurationMs)


resolveCompletedZaps : Model -> Model
resolveCompletedZaps model =
    let
        ( completed, ongoing ) =
            List.partition (\z -> zapProgress model.elapsed z >= 1.0) model.activeZaps

        modelWithEdgesAndTargets =
            List.foldl completeZap model completed
    in
    { modelWithEdgesAndTargets | activeZaps = ongoing }


completeZap : ActiveZap -> Model -> Model
completeZap zap model =
    let
        newEdge =
            { from = zap.from, to = zap.to }

        -- Set the node's target to its tree position (pulling it in)
        updatedNodes =
            Dict.update zap.to
                (Maybe.map
                    (\n ->
                        { n
                            | targetX = zap.treeTargetX
                            , targetY = zap.treeTargetY
                            , lastActiveAt = model.elapsed
                            , settled = True
                        }
                    )
                )
                model.nodes
                |> touchNode model.elapsed zap.from
    in
    { model
        | nodes = updatedNodes
        , edges = newEdge :: model.edges
    }


nodeOpacity : Float -> Node -> Float
nodeOpacity now node =
    if node.id == 0 then
        1.0

    else
        let
            timeSinceActive =
                now - node.lastActiveAt
        in
        if timeSinceActive < config.fadeStartMs then
            1.0

        else
            Basics.max 0 (1.0 - (timeSinceActive - config.fadeStartMs) / config.fadeDurationMs)


removeFullyFaded : Model -> Model
removeFullyFaded model =
    let
        fadedIds =
            Dict.values model.nodes
                |> List.filter (\n -> n.id /= 0 && nodeOpacity model.elapsed n <= 0)
                |> List.map .id

        fadedSet =
            List.foldl (\id acc -> Dict.insert id True acc) Dict.empty fadedIds

        reparentedNodes =
            List.foldl
                (\removedId acc ->
                    case Dict.get removedId model.nodes of
                        Nothing ->
                            acc

                        Just removedNode ->
                            Dict.map
                                (\_ n ->
                                    if n.parent == Just removedId then
                                        { n | parent = removedNode.parent }

                                    else
                                        n
                                )
                                acc
                )
                model.nodes
                fadedIds

        cleanedNodes =
            Dict.filter (\id _ -> not (Dict.member id fadedSet)) reparentedNodes

        cleanedEdges =
            List.filter
                (\e -> not (Dict.member e.from fadedSet) && not (Dict.member e.to fadedSet))
                model.edges

        reparentEdges =
            Dict.values cleanedNodes
                |> List.filterMap
                    (\n ->
                        case n.parent of
                            Just pid ->
                                if Dict.member pid cleanedNodes && not (List.any (\e -> e.to == n.id) cleanedEdges) then
                                    Just { from = pid, to = n.id }

                                else
                                    Nothing

                            Nothing ->
                                Nothing
                    )

        -- Also remove any active zaps involving faded nodes
        cleanedZaps =
            List.filter
                (\z -> not (Dict.member z.from fadedSet) && not (Dict.member z.to fadedSet))
                model.activeZaps
    in
    { model
        | nodes = cleanedNodes
        , edges = cleanedEdges ++ reparentEdges
        , activeZaps = cleanedZaps
    }


initiateZap : Model -> Model
initiateZap model =
    let
        ( roll, seed1 ) =
            Random.step (Random.float 0 1) model.seed

        isNewNode =
            roll < config.newNodeChance || Dict.size model.nodes < 3
    in
    if isNewNode then
        initiateNewNodeZap { model | seed = seed1 }

    else
        initiateExistingNodeZap { model | seed = seed1 }


touchNode : Float -> NodeId -> Dict NodeId Node -> Dict NodeId Node
touchNode now nodeId nodes =
    Dict.update nodeId
        (Maybe.map (\n -> { n | lastActiveAt = now }))
        nodes


initiateNewNodeZap : Model -> Model
initiateNewNodeZap model =
    let
        nodeList =
            Dict.values model.nodes

        ( parentIndex, seed1 ) =
            Random.step (Random.int 0 (List.length nodeList - 1)) model.seed

        maybeParent =
            List.head (List.drop parentIndex nodeList)
    in
    case maybeParent of
        Nothing ->
            model

        Just parent ->
            let
                -- Tree position (where it will end up)
                ( xOffset, seed2 ) =
                    Random.step (Random.float -config.horizontalSpread config.horizontalSpread) seed1

                ( yVariance, seed3 ) =
                    Random.step (Random.float 0 config.verticalVariance) seed2

                treeX =
                    clamp 40 (config.width - 40) (parent.targetX + xOffset)

                treeY =
                    parent.targetY + config.verticalDrop + yVariance

                -- Field position (where it spawns, out in the network)
                -- Always below or at the same level as tree target
                ( fieldAngle, seed4 ) =
                    Random.step (Random.float (pi * 0.1) (pi * 0.9)) seed3

                ( fieldDist, seed5 ) =
                    Random.step (Random.float (config.fieldSpawnRadius * 0.5) config.fieldSpawnRadius) seed4

                fieldX =
                    clamp 20 (config.width - 20) (treeX + fieldDist * cos fieldAngle)

                fieldY =
                    clamp (treeY + 20) (config.height - 20) (treeY + fieldDist * sin fieldAngle)

                newNode =
                    { id = model.nextId
                    , x = fieldX
                    , y = fieldY
                    , targetX = fieldX
                    , targetY = fieldY
                    , parent = Just parent.id
                    , depth = parent.depth + 1
                    , bornAt = model.elapsed
                    , lastActiveAt = model.elapsed
                    , settled = False
                    }

                newZap =
                    { from = parent.id
                    , to = model.nextId
                    , startedAt = model.elapsed
                    , treeTargetX = treeX
                    , treeTargetY = treeY
                    }
            in
            { model
                | nodes = Dict.insert model.nextId newNode model.nodes
                , activeZaps = newZap :: model.activeZaps
                , nextId = model.nextId + 1
                , seed = seed5
            }


initiateExistingNodeZap : Model -> Model
initiateExistingNodeZap model =
    let
        nodeList =
            Dict.values model.nodes

        ( zapperIndex, seed1 ) =
            Random.step (Random.int 0 (List.length nodeList - 1)) model.seed

        maybeZapper =
            List.head (List.drop zapperIndex nodeList)
    in
    case maybeZapper of
        Nothing ->
            { model | seed = seed1 }

        Just zapper ->
            let
                nodesBelow =
                    List.filter
                        (\n ->
                            n.depth
                                > zapper.depth
                                + 1
                                && n.id
                                /= zapper.id
                                && n.settled
                        )
                        nodeList
            in
            if List.isEmpty nodesBelow then
                initiateNewNodeZap { model | seed = seed1 }

            else
                let
                    ( targetIndex, seed2 ) =
                        Random.step (Random.int 0 (List.length nodesBelow - 1)) seed1

                    maybeTarget =
                        List.head (List.drop targetIndex nodesBelow)
                in
                case maybeTarget of
                    Nothing ->
                        { model | seed = seed2 }

                    Just target ->
                        let
                            ( xOffset, seed3 ) =
                                Random.step (Random.float -config.horizontalSpread config.horizontalSpread) seed2

                            newTargetX =
                                clamp 40 (config.width - 40) (zapper.targetX + xOffset)

                            newTargetY =
                                zapper.targetY + config.verticalDrop

                            newDepth =
                                zapper.depth + 1

                            -- Remove old edge to this target
                            filteredEdges =
                                List.filter (\e -> e.to /= target.id) model.edges

                            -- Update parent and depth now, but don't move yet
                            updatedTarget =
                                { target
                                    | parent = Just zapper.id
                                    , depth = newDepth
                                    , settled = False
                                }

                            updatedNodes =
                                Dict.insert target.id updatedTarget model.nodes

                            finalNodes =
                                updateDescendantDepths target.id newDepth updatedNodes

                            newZap =
                                { from = zapper.id
                                , to = target.id
                                , startedAt = model.elapsed
                                , treeTargetX = newTargetX
                                , treeTargetY = newTargetY
                                }
                        in
                        { model
                            | nodes = finalNodes
                            , edges = filteredEdges
                            , activeZaps = newZap :: model.activeZaps
                            , seed = seed3
                        }


updateDescendantDepths : NodeId -> Int -> Dict NodeId Node -> Dict NodeId Node
updateDescendantDepths parentId parentDepth nodes =
    let
        children =
            Dict.values nodes
                |> List.filter (\n -> n.parent == Just parentId)

        updateChild child acc =
            let
                newDepth =
                    parentDepth + 1

                yDelta =
                    config.verticalDrop + config.verticalVariance / 2

                updatedChild =
                    { child
                        | depth = newDepth
                        , targetY = (parentDepth + 1 |> toFloat) * yDelta + 40
                    }

                withChild =
                    Dict.insert child.id updatedChild acc
            in
            updateDescendantDepths child.id newDepth withChild
    in
    List.foldl updateChild nodes children


lerpAllNodes : Model -> Model
lerpAllNodes model =
    let
        lerpNode _ node =
            { node
                | x = node.x + (node.targetX - node.x) * config.lerpSpeed
                , y = node.y + (node.targetY - node.y) * config.lerpSpeed
            }
    in
    { model | nodes = Dict.map lerpNode model.nodes }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Tick



-- VIEW


view : Model -> Svg.Svg Msg
view model =
    let
        w =
            String.fromFloat config.width

        h =
            String.fromFloat config.height

        visibleNodes =
            Dict.values model.nodes
                |> List.filter (\n -> nodeOpacity model.elapsed n > 0)
    in
    svg
        [ viewBox ("0 0 " ++ w ++ " " ++ h)
        , width w
        , height h
        , Svg.Attributes.style ("background:" ++ config.bgColor)
        ]
        (-- Settled edges
         List.map (viewEdge model.elapsed model.nodes) model.edges
            -- Active zap lines (bright, reaching)
            ++ List.map (viewActiveZap model.elapsed model.nodes) model.activeZaps
            -- Nodes
            ++ List.map (viewNodeGlow model.elapsed) visibleNodes
            ++ List.map (viewNode model.elapsed) visibleNodes
            ++ List.map (viewNodeCore model.elapsed) visibleNodes
        )


viewEdge : Float -> Dict NodeId Node -> Edge -> Svg.Svg Msg
viewEdge now nodes edge =
    case ( Dict.get edge.from nodes, Dict.get edge.to nodes ) of
        ( Just fromNode, Just toNode ) ->
            let
                opacity =
                    Basics.min (nodeOpacity now fromNode) (nodeOpacity now toNode)
            in
            if opacity <= 0 then
                text ""

            else
                line
                    [ x1 (String.fromFloat fromNode.x)
                    , y1 (String.fromFloat fromNode.y)
                    , x2 (String.fromFloat toNode.x)
                    , y2 (String.fromFloat toNode.y)
                    , stroke config.lineColor
                    , strokeOpacity (String.fromFloat (opacity * config.lineOpacity))
                    , strokeWidth config.lineWidth
                    , strokeLinecap "round"
                    ]
                    []

        _ ->
            text ""


viewActiveZap : Float -> Dict NodeId Node -> ActiveZap -> Svg.Svg Msg
viewActiveZap now nodes zap =
    case ( Dict.get zap.from nodes, Dict.get zap.to nodes ) of
        ( Just fromNode, Just toNode ) ->
            let
                progress =
                    zapProgress now zap

                -- Head position (leading edge of the comet)
                headX =
                    fromNode.x + (toNode.x - fromNode.x) * progress

                headY =
                    fromNode.y + (toNode.y - fromNode.y) * progress

                -- Tail position (trails behind the head)
                tailProgress =
                    Basics.max 0 (progress - 0.35)

                tailX =
                    fromNode.x + (toNode.x - fromNode.x) * tailProgress

                tailY =
                    fromNode.y + (toNode.y - fromNode.y) * tailProgress

                -- The burned trail left behind (from source to tail)
                trailEndProgress =
                    Basics.max 0 (progress - 0.15)

                trailEndX =
                    fromNode.x + (toNode.x - fromNode.x) * trailEndProgress

                trailEndY =
                    fromNode.y + (toNode.y - fromNode.y) * trailEndProgress
            in
            Svg.g []
                [ -- Burned trail (dimmer, left behind by the comet)
                  if trailEndProgress > 0.01 then
                    line
                        [ x1 (String.fromFloat fromNode.x)
                        , y1 (String.fromFloat fromNode.y)
                        , x2 (String.fromFloat trailEndX)
                        , y2 (String.fromFloat trailEndY)
                        , stroke config.lineColor
                        , strokeOpacity (String.fromFloat config.lineOpacity)
                        , strokeWidth config.lineWidth
                        , strokeLinecap "round"
                        ]
                        []

                  else
                    text ""
                , -- Comet body (bright, from tail to head)
                  line
                    [ x1 (String.fromFloat tailX)
                    , y1 (String.fromFloat tailY)
                    , x2 (String.fromFloat headX)
                    , y2 (String.fromFloat headY)
                    , stroke config.zapBrightColor
                    , strokeOpacity "0.7"
                    , strokeWidth "4"
                    , strokeLinecap "round"
                    ]
                    []
                , -- Comet head (brightest point)
                  circle
                    [ cx (String.fromFloat headX)
                    , cy (String.fromFloat headY)
                    , r "4"
                    , fill "#fff"
                    , fillOpacity "0.9"
                    ]
                    []
                , -- Head glow
                  circle
                    [ cx (String.fromFloat headX)
                    , cy (String.fromFloat headY)
                    , r "8"
                    , fill config.zapBrightColor
                    , fillOpacity "0.3"
                    ]
                    []
                ]

        _ ->
            text ""


viewNodeGlow : Float -> Node -> Svg.Svg Msg
viewNodeGlow now node =
    let
        opacity =
            nodeOpacity now node
    in
    circle
        [ cx (String.fromFloat node.x)
        , cy (String.fromFloat node.y)
        , r "12"
        , fill config.nodeColor
        , fillOpacity (String.fromFloat (opacity * 0.15))
        ]
        []


viewNode : Float -> Node -> Svg.Svg Msg
viewNode now node =
    let
        opacity =
            nodeOpacity now node
    in
    circle
        [ cx (String.fromFloat node.x)
        , cy (String.fromFloat node.y)
        , r "4"
        , fill config.nodeColor
        , fillOpacity (String.fromFloat opacity)
        ]
        []


viewNodeCore : Float -> Node -> Svg.Svg Msg
viewNodeCore now node =
    let
        opacity =
            nodeOpacity now node
    in
    circle
        [ cx (String.fromFloat node.x)
        , cy (String.fromFloat node.y)
        , r "1.5"
        , fill config.nodeCoreColor
        , fillOpacity (String.fromFloat (opacity * 0.9))
        ]
        []
