module Image.Solution exposing (checksum, partitionImage)


type alias Image =
    List (List (List Int))


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
