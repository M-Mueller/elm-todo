module Uuid4 exposing (join, toHex, uuid4)

import Random


toHex : Int -> Char
toHex number =
    if number < 10 then
        Char.fromCode (Char.toCode '0' + number)

    else
        Char.fromCode (Char.toCode 'a' + (number - 10))


join : List Char -> String
join digits =
    ""
        ++ (List.take 8 digits |> String.fromList)
        ++ "-"
        ++ (List.drop 8 digits |> List.take 4 |> String.fromList)
        ++ "-"
        ++ (List.drop 12 digits |> List.take 3 |> (::) '4' |> String.fromList)
        ++ "-"
        ++ (List.drop 15 digits |> List.take 3 |> (::) '8' |> String.fromList)
        ++ "-"
        ++ (List.drop 18 digits |> String.fromList)


uuid4 : Random.Generator String
uuid4 =
    Random.list 30 (Random.int 0 15)
        |> Random.map (List.map toHex)
        |> Random.map join
