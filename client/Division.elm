module Division exposing (Division, makePies)

import Count exposing (Count)
import Topping exposing (Topping, Pair)
import Config exposing (Config)


type alias Division =
    { pies : List Pie, remaining : Topping.Count, leftovers : Topping.Count }


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


{-|
First, if there's a pair with more than slicesPerPie slices,
make a pie with just that topping.

Then, put aside the slices which are less than slicesPerPart,
because those can't fill pies anyway.

Then the main part happens. More on that in attemptToFill.
-}
makePies : Config -> Count Topping Topping.Key -> Division
makePies config count =
    { emptyDivision | remaining = count }
        |> map (extractWholePies config)
        |> map (extractLeftovers config)
        |> map (maybeFill config)



--


splitToPies : Int -> Pair -> List Pie
splitToPies slicesPerPie ( topping, count ) =
    List.repeat (count // slicesPerPie) [ ( topping, slicesPerPie ) ]


extractWholePies : Config -> Topping.Count -> Division
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


extractLeftovers : Config -> Topping.Count -> Division
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


maybeFill : Config -> Topping.Count -> Division
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


{-|
This is the main part. It's a recursive function whose job is to fill pies.

It can return a list of pies, or Nothing.
The end condition is an empty `countsSortedBigToSmall`, meaning there are no
more pairs to organize.
When this happens, there are two options:
1. All pies except for at most one are full = success
2. There's more than one partial pie = failure

The recursion step has multiple options and when one results in a failure,
it moves to the next option.
Take the largest unorganized pair.
1. If it can fit inside an existing partial pie, put it there. Otherwise,
2. Start a new pie with that pair. If this fails,
3. Split the pair into two and try again.

I didn't check whether this algorithm always succeeds, but that's what fuzz tests
are for :D.
-}
attemptToFill : Config -> List Pair -> List Pie -> Maybe (List Pie)
attemptToFill config countsSortedBigToSmall semipies =
    let
        pieCounts =
            List.map sliceCount semipies

        slicesPerPie =
            config.slicesPerPart * config.partsPerPie
    in
        case countsSortedBigToSmall of
            [] ->
                if
                    pieCounts
                        |> List.filter (\count -> count < slicesPerPie)
                        |> List.length
                        |> \unfilledPies -> unfilledPies <= 1
                then
                    Just semipies
                else
                    Nothing

            ( topping, count ) :: rest ->
                case
                    fitPairInPies slicesPerPie
                        ( topping, count )
                        (List.sortBy (negate << sliceCount) semipies)
                of
                    Just newPies ->
                        attemptToFill config rest newPies

                    Nothing ->
                        case attemptToFill config rest ([ ( topping, count ) ] :: semipies) of
                            Just asNewPie ->
                                Just asNewPie

                            Nothing ->
                                if count > config.slicesPerPart then
                                    let
                                        sortedCountsWithSplit =
                                            rest
                                                |> insertToSorted ( topping, config.slicesPerPart )
                                                |> insertToSorted ( topping, count - config.slicesPerPart )
                                    in
                                        attemptToFill config sortedCountsWithSplit semipies
                                else
                                    Nothing


fitPairInPies : Int -> Pair -> List (List Pair) -> Maybe (List (List Pair))
fitPairInPies slicesPerPie ( topping, count ) sortedSemipies =
    case sortedSemipies of
        [] ->
            Nothing

        biggestPie :: rest ->
            if sliceCount biggestPie + count > slicesPerPie then
                fitPairInPies slicesPerPie ( topping, count ) rest
                    |> Maybe.map ((::) biggestPie)
            else
                (( topping, count ) :: biggestPie) :: rest |> Just



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
