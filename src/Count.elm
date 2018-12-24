module Count exposing
    ( Count(..)
    , add
    , empty
    , fromKeyList
    , fromList
    , get
    , join
    , set
    , splitValuesModulo
    , toList
    )

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


setDict : Count key comparable -> Dict comparable Int -> Count key comparable
setDict (Count c) newDict =
    Count { c | dict = newDict }


mapValues : (Int -> Int) -> Count key comparable -> Count key comparable
mapValues fn ((Count { dict }) as c) =
    dict
        |> Dict.map (always fn)
        |> setDict c


filterValues : (Int -> Bool) -> Count key comparable -> Count key comparable
filterValues fn ((Count { dict }) as c) =
    dict |> Dict.filter (always fn) |> setDict c


splitValues : (Int -> ( Int, Int )) -> Count key comparable -> ( Count key comparable, Count key comparable )
splitValues splitFn (Count c) =
    c.dict
        |> Dict.map (always splitFn)
        |> Dict.foldl (\k -> tupleMap2 (Dict.insert k)) ( Dict.empty, Dict.empty )
        |> tupleMap (count c.toComparable c.fromComparable)


splitValuesModulo : Int -> Count key comparable -> ( Count key comparable, Count key comparable )
splitValuesModulo rounder =
    splitValues (\n -> ( n // rounder * rounder, modBy rounder n ))


tupleMap : (a -> b) -> ( a, a ) -> ( b, b )
tupleMap fn ( a1, a2 ) =
    ( fn a1, fn a2 )


tupleMap2 : (a -> b -> c) -> ( a, a ) -> ( b, b ) -> ( c, c )
tupleMap2 fn ( a1, a2 ) ( b1, b2 ) =
    ( fn a1 b1, fn a2 b2 )


map2 : (Int -> Int -> Int) -> Count key comparable -> Count key comparable -> Count key comparable
map2 op (Count opt1) (Count opt2) =
    let
        merger key n m newDict =
            let
                val =
                    op n m
            in
            if val == 0 then
                newDict

            else
                Dict.insert key val newDict
    in
    Dict.merge
        (\k n dict -> merger k n 0 dict)
        (\k n m dict -> merger k n m dict)
        (\k m dict -> merger k 0 m dict)
        opt1.dict
        opt2.dict
        Dict.empty
        |> count opt1.toComparable opt1.fromComparable


join : Count key comparable -> Count key comparable -> Count key comparable
join =
    map2 (+)


sum : Count key comparable -> Int
sum (Count { dict }) =
    Dict.foldl (\_ cnt partialSum -> partialSum + abs cnt) 0 dict


toList : Count key comparable -> List ( key, Int )
toList (Count { dict, fromComparable }) =
    let
        parsePair ( comp, cnt ) =
            Maybe.map (\k -> ( k, cnt )) (fromComparable comp)
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
        |> setDict c


add : key -> Int -> Count key comparable -> ( Count key comparable, Int )
add key delta cnt =
    case ( get key cnt, delta > 0 ) of
        ( 0, True ) ->
            ( set key delta cnt, delta )

        ( 0, False ) ->
            ( cnt, 0 )

        ( val, False ) ->
            if val + delta < 0 then
                ( cnt, val )

            else
                ( set key (val + delta) cnt, val + delta )

        ( val, True ) ->
            ( set key (val + delta) cnt, val + delta )
