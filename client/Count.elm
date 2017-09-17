module Count exposing (..)

import Dict exposing (Dict)


type Count key comparable
    = Count
        { dict : Dict comparable Int
        , toComparable : key -> comparable
        , fromComparable : comparable -> Maybe key
        }


count : (key -> comparable) -> (comparable -> Maybe key) -> Dict comparable Int -> Count key comparable
count toComparable fromComparable dict =
    Count
        { dict = dict
        , toComparable = toComparable
        , fromComparable = fromComparable
        }


setDict : Dict comparable Int -> Count key comparable -> Count key comparable
setDict newDict (Count c) =
    Count { c | dict = newDict }


subtract : Count key comparable -> Count key comparable -> Count key comparable
subtract (Count opt1) (Count opt2) =
    let
        merger key n m diffDict =
            if n == m then
                diffDict
            else
                Dict.insert key (n - m) diffDict
    in
        Dict.merge
            (\k n dict -> merger k n 0 dict)
            (\k n m dict -> merger k n m dict)
            (\k m dict -> merger k 0 m dict)
            opt1.dict
            opt2.dict
            Dict.empty
            |> count opt1.toComparable opt1.fromComparable


sum : Count key comparable -> Int
sum (Count { dict }) =
    Dict.foldl (\_ count sum -> sum + abs count) 0 dict


toList : Count key comparable -> List ( key, Int )
toList (Count { dict, fromComparable }) =
    let
        parsePair ( comp, count ) =
            Maybe.map (flip (,) count) (fromComparable comp)
    in
        Dict.toList dict
            |> List.filterMap parsePair


fromList : (key -> comparable) -> (comparable -> Maybe key) -> List ( key, Int ) -> Count key comparable
fromList toComparable fromComparable lst =
    tally Tuple.first Tuple.second toComparable fromComparable lst


fromKeyList : (key -> comparable) -> (comparable -> Maybe key) -> List ( comparable, Int ) -> Count key comparable
fromKeyList toComparable fromComparable lst =
    tallyKeys Tuple.first Tuple.second toComparable fromComparable lst


tally : (a -> key) -> (a -> Int) -> (key -> comparable) -> (comparable -> Maybe key) -> List a -> Count key comparable
tally aToKey aToValue toComparable fromComparable lst =
    tallyKeys (aToKey >> toComparable) aToValue toComparable fromComparable lst


tallyKeys : (a -> comparable) -> (a -> Int) -> (key -> comparable) -> (comparable -> Maybe key) -> List a -> Count key comparable
tallyKeys aToComparable aToValue toComparable fromComparable lst =
    List.foldl
        (\x ->
            Dict.update
                (aToComparable x)
                (Maybe.withDefault 0 >> (+) (aToValue x) >> Just)
        )
        Dict.empty
        lst
        |> count toComparable fromComparable


empty : (key -> comparable) -> (comparable -> Maybe key) -> Count key comparable
empty toComparable fromComparable =
    count toComparable fromComparable Dict.empty


get : key -> Count key comparable -> Int
get key (Count { dict, toComparable }) =
    Dict.get (toComparable key) dict |> Maybe.withDefault 0


set : key -> Int -> Count key comparable -> Count key comparable
set key value ((Count { dict, toComparable }) as c) =
    Dict.insert (toComparable key) value dict
        |> flip setDict c


add : key -> Int -> Count key comparable -> ( Count key comparable, Int )
add key delta count =
    case ( get key count, delta > 0 ) of
        ( 0, True ) ->
            ( set key delta count, delta )

        ( 0, False ) ->
            ( count, 0 )

        ( val, False ) ->
            if val + delta < 0 then
                ( count, val )
            else
                ( set key (val + delta) count, val + delta )

        ( val, True ) ->
            ( set key (val + delta) count, val + delta )
