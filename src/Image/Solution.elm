module Image.Solution exposing (checksum, partitionImage, renderImage)


type alias Layer =
    List (List Int)


type alias Image =
    List Layer


chunk : Int -> List a -> List (List a)
chunk count list =
    case list of
        [] ->
            []

        _ ->
            List.take count list :: chunk count (List.drop count list)


partitionImage : ( Int, Int ) -> List Int -> Image
partitionImage ( width, height ) =
    chunk width >> chunk height


checksum : Image -> Maybe ( Int, Int )
checksum image =
    let
        multiplyNonzeroes digits =
            List.partition ((==) 1) digits
                |> Tuple.mapBoth List.length List.length
                |> (\( x, y ) -> x * y)
    in
    image
        |> List.map
            (List.concat
                >> List.partition ((==) 0)
                >> Tuple.mapBoth List.length multiplyNonzeroes
            )
        |> List.sortBy Tuple.first
        |> List.head


overlayPixels : Int -> Int -> Int
overlayPixels bottom top =
    if top < 2 then
        top

    else
        bottom


detectDimensions : Image -> ( Int, Int )
detectDimensions image =
    let
        layer =
            List.head image

        row =
            Maybe.andThen List.head layer

        length =
            Maybe.map List.length >> Maybe.withDefault 0
    in
    ( length row, length layer )


overlayLayers : Image -> Layer
overlayLayers image =
    let
        ( width, height ) =
            detectDimensions image

        emptyLayer =
            List.repeat width 2
                |> List.repeat height
    in
    List.foldl
        (List.map2 (List.map2 overlayPixels))
        emptyLayer
        image


renderPixel : Int -> Char
renderPixel pixel =
    case pixel of
        1 ->
            '#'

        _ ->
            ' '


renderLayer : Layer -> String
renderLayer layer =
    List.map (List.map renderPixel) layer
        |> List.intersperse [ '\n' ]
        |> List.concat
        |> String.fromList


renderImage : Image -> String
renderImage =
    overlayLayers >> renderLayer
