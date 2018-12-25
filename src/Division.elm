module Division exposing (Division, makePies)

import Config
import Count exposing (Count)
import Topping exposing (Pair, Topping)


type alias Division =
    { pies : List Pie
    , remaining : Topping.Count
    , leftovers : Topping.Count
    }


type alias Pie =
    List Pair


emptyDivision : Division
emptyDivision =
    { pies = []
    , remaining = Topping.emptyCount
    , leftovers = Topping.emptyCount
    }



--


map : (Topping.Count -> Division) -> Division -> Division
map remainingParser { pies, remaining, leftovers } =
    let
        sortRemaining =
            remainingParser remaining
    in
    { sortRemaining
        | pies = pies ++ sortRemaining.pies
        , leftovers = Count.join leftovers sortRemaining.leftovers
    }


{-| First, if there's a pair with more than slicesPerPie slices,
make a pie with just that topping.

Then, put aside the slices which are less than slicesPerPart,
because those can't fill pies anyway.

Then the main part happens. More on that in attemptToFill.

-}
makePies : Config.Slices -> Count Topping Topping.Key -> Division
makePies config count =
    { emptyDivision | remaining = count }
        |> map (extractWholePies config)
        |> map (extractLeftovers config)
        |> map (maybeFill config)



--


splitToPies : Int -> Pair -> List Pie
splitToPies slicesPerPie ( topping, count ) =
    List.repeat (count // slicesPerPie) [ ( topping, slicesPerPie ) ]


extractWholePies : Config.Slices -> Topping.Count -> Division
extractWholePies config count =
    let
        slicesPerPie =
            config.slicesPerPart * config.partsPerPie

        ( wholePies, remaining ) =
            Count.splitValuesModulo slicesPerPie count
    in
    { emptyDivision
        | pies =
            wholePies
                |> Count.toList
                |> List.concatMap (splitToPies slicesPerPie)
        , remaining = remaining
    }



--


extractLeftovers : Config.Slices -> Topping.Count -> Division
extractLeftovers { slicesPerPart } count =
    let
        ( rounded, leftovers ) =
            Count.splitValuesModulo slicesPerPart count
    in
    { emptyDivision
        | remaining = rounded
        , leftovers = leftovers
    }



--


maybeFill : Config.Slices -> Topping.Count -> Division
maybeFill config count =
    case attemptToFill config (List.sortBy sortKey <| Count.toList count) [] of
        Nothing ->
            emptyDivision

        Just semipies ->
            let
                slicesPerPie =
                    config.slicesPerPart * config.partsPerPie

                ( pies, rest ) =
                    List.partition
                        (\semipie -> sliceCount semipie == slicesPerPie)
                        semipies
            in
            { emptyDivision
                | pies = pies
                , remaining = rest |> List.concat |> Topping.countFromList
            }


{-| This is the main part. It's a recursive function whose job is to fill pies.

It can return a list of pies, or Nothing.
The end condition is an empty `countSortedBigToSmall`, meaning there are no
more pairs to organize.
When this happens, there are two options:

1.  All pies except for at most one are full = success
2.  There's more than one partial pie = failure

The recursion step has multiple options and when one results in a failure,
it moves to the next option.
Take the largest unorganized pair.

1.  If it can fit in an existing pie, put it there (`tryFittingIn`). Otherwise,
2.  Start a new pie with that pair (`tryAsNewPie`). If this fails,
3.  Split the pair into two (`trySplitting`) and try again.

I didn't check whether this algorithm always succeeds,
but that's what fuzz tests are for :D

-}
attemptToFill : Config.Slices -> List Pair -> List Pie -> Maybe (List Pie)
attemptToFill config countSortedBigToSmall semipies =
    let
        pieSizes =
            List.map sliceCount semipies

        slicesPerPie =
            config.slicesPerPart * config.partsPerPie
    in
    case countSortedBigToSmall of
        [] ->
            if
                pieSizes
                    |> List.filter (\count -> count < slicesPerPie)
                    |> List.length
                    |> (\unfilledPies -> unfilledPies <= 1)
            then
                Just semipies

            else
                Nothing

        biggestPair :: rest ->
            Nothing
                |> orTry (\_ -> tryFittingIn config biggestPair rest semipies)
                |> orTry (\_ -> tryAsNewPie config biggestPair rest semipies)
                |> orTry (\_ -> trySplitting config biggestPair rest semipies)


orTry : (() -> Maybe a) -> Maybe a -> Maybe a
orTry newTry previousTry =
    if previousTry == Nothing then
        newTry ()

    else
        previousTry


maybeFitInPies : Int -> Pair -> List Pie -> Maybe (List Pie)
maybeFitInPies slicesPerPie ( topping, count ) sortedSemipies =
    case sortedSemipies of
        [] ->
            Nothing

        biggestPie :: rest ->
            if sliceCount biggestPie + count > slicesPerPie then
                maybeFitInPies slicesPerPie ( topping, count ) rest
                    |> Maybe.map ((::) biggestPie)

            else
                (( topping, count ) :: biggestPie) :: rest |> Just


tryFittingIn : Config.Slices -> Pair -> List Pair -> List Pie -> Maybe (List Pie)
tryFittingIn config pair rest semipies =
    maybeFitInPies (config.slicesPerPart * config.partsPerPie)
        pair
        (semipies |> List.sortBy (negate << sliceCount))
        |> Maybe.andThen (attemptToFill config rest)


tryAsNewPie : Config.Slices -> Pair -> List Pair -> List Pie -> Maybe (List Pie)
tryAsNewPie config pair rest semipies =
    attemptToFill config rest ([ pair ] :: semipies)


trySplitting : Config.Slices -> Pair -> List Pair -> List Pie -> Maybe (List Pie)
trySplitting config ( topping, count ) rest semipies =
    if count > config.slicesPerPart then
        let
            newPairs =
                rest
                    |> insertToSorted ( topping, config.slicesPerPart )
                    |> insertToSorted ( topping, count - config.slicesPerPart )
        in
        attemptToFill config newPairs semipies

    else
        Nothing



--


isEmpty : Pair -> Bool
isEmpty ( _, n ) =
    n == 0


sortKey : Pair -> Int
sortKey ( _, count ) =
    -count


sliceCount : List Pair -> Int
sliceCount =
    List.map Tuple.second >> List.sum


insertToSorted : Pair -> List Pair -> List Pair
insertToSorted pair pairs =
    case pairs of
        [] ->
            [ pair ]

        hd :: tl ->
            if sortKey pair < sortKey hd then
                pair :: pairs

            else
                hd :: insertToSorted pair tl
