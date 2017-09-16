module Division exposing (Division, makePies)

import ToppingCount exposing (ToppingCount, Pair)
import Topping exposing (Topping)
import Config exposing (Config)


type alias Division =
    { pies : List (List Pair), remaining : List Pair, leftovers : List Pair }



--


map : (List Pair -> Division) -> Division -> Division
map remainingParser { pies, remaining, leftovers } =
    let
        sortUnsorted =
            remainingParser remaining
    in
        { sortUnsorted
            | pies = pies ++ sortUnsorted.pies
            , leftovers = leftovers ++ sortUnsorted.leftovers
        }


{-|
First, if there's a pair with more than slicesPerPie slices,
make a pie with just that topping.

Then, put aside the slices which are less than slicesPerPart,
because those can't fill pies anyway.

Then the main part happens. More on that in attemptToFill.
-}
makePies : Config -> ToppingCount -> Division
makePies config toppingCount =
    { remaining = toppingCount |> ToppingCount.toList
    , pies = []
    , leftovers = []
    }
        |> map (extractWholePies config)
        |> map (extractLeftovers config)
        |> map (maybeFill config)



--


splitToPies : Int -> Pair -> List Pair
splitToPies slicesPerPie ( topping, count ) =
    if count > slicesPerPie then
        ( topping, slicesPerPie )
            :: splitToPies slicesPerPie ( topping, count - slicesPerPie )
    else
        [ ( topping, count ) ]


extractWholePies : Config -> List Pair -> Division
extractWholePies config toppingCounts =
    let
        slicesPerPie =
            config.slicesPerPart * config.partsPerPie

        ( wholePies, lessThanWholePies ) =
            toppingCounts
                |> List.concatMap (splitToPies slicesPerPie)
                |> List.partition (\( _, count ) -> count == slicesPerPie)
    in
        { pies = List.map List.singleton wholePies
        , remaining = lessThanWholePies
        , leftovers = []
        }



--


separatePartsAndLeftovers : Int -> Pair -> ( Topping, Int, Int )
separatePartsAndLeftovers slicesPerPart ( topping, count ) =
    ( topping
    , (count // slicesPerPart) * slicesPerPart
    , count % slicesPerPart
    )


extractLeftovers : Config -> List Pair -> Division
extractLeftovers { slicesPerPart } toppingCounts =
    let
        split ( topping, partCount, leftoverCount ) =
            ( ( topping, partCount ), ( topping, leftoverCount ) )

        ( parts, leftovers ) =
            toppingCounts
                |> List.map (separatePartsAndLeftovers slicesPerPart)
                |> List.map split
                |> List.unzip
    in
        { pies = []
        , remaining = parts |> List.filter (not << isEmpty)
        , leftovers = leftovers |> List.filter (not << isEmpty)
        }



--


maybeFill : Config -> List Pair -> Division
maybeFill config counts =
    case attemptToFill config (List.sortBy sortKey counts) [] of
        Nothing ->
            Division [] counts []

        Just semipies ->
            let
                slicesPerPie =
                    config.slicesPerPart * config.partsPerPie

                ( pies, rest ) =
                    List.partition (\semipie -> sliceCount semipie == slicesPerPie) semipies
            in
                Division pies (List.concat rest) []


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
attemptToFill : Config -> List Pair -> List (List Pair) -> Maybe (List (List Pair))
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
                                                |> insertToSorted ( topping, config.slicesPerPart ) sortKey
                                                |> insertToSorted ( topping, count - config.slicesPerPart ) sortKey
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
                fitPairInPies slicesPerPie ( topping, count ) rest |> Maybe.map ((::) biggestPie)
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


insertToSorted : a -> (a -> comparable) -> List a -> List a
insertToSorted x key xs =
    case xs of
        [] ->
            [ x ]

        hd :: tl ->
            if key x < key hd then
                x :: xs
            else
                hd :: insertToSorted x key tl
