module Stage exposing
    ( Data(..)
    , Stage(..)
    , applyResult
    , canEdit
    , canSubmit
    , data
    , error
    , update
    , waiting
    )


type Stage input output
    = Editing input
    | Waiting input
    | Failure input String
    | Success output


type Data input output
    = In input
    | Out output


data : Stage input output -> Data input output
data stage =
    case stage of
        Editing in_ ->
            In in_

        Waiting in_ ->
            In in_

        Failure in_ _ ->
            In in_

        Success out ->
            Out out


error : Stage input output -> Maybe String
error stage =
    case stage of
        Editing _ ->
            Nothing

        Waiting _ ->
            Nothing

        Failure _ error_ ->
            Just error_

        Success _ ->
            Nothing


waiting : Stage input output -> Bool
waiting stage =
    case stage of
        Editing _ ->
            False

        Waiting _ ->
            True

        Failure _ _ ->
            False

        Success _ ->
            False


canEdit : Stage input output -> Bool
canEdit stage =
    case stage of
        Editing _ ->
            True

        Waiting _ ->
            False

        Failure _ _ ->
            True

        Success _ ->
            False


canSubmit : Stage input output -> Bool
canSubmit stage =
    case stage of
        Editing _ ->
            True

        Waiting _ ->
            False

        Failure _ _ ->
            False

        Success _ ->
            False


setInput : input -> Stage input output -> Stage input output
setInput newInput stage =
    case stage of
        Editing _ ->
            Editing newInput

        Failure _ _ ->
            Editing newInput

        _ ->
            Failure newInput
                "Tried to set input when the stage wasn't editing or failing"


update : (a -> input -> input) -> a -> Stage input output -> Stage input output
update fn a stage =
    case data stage of
        Out output ->
            Debug.todo <|
                "Tried to udpdate "
                    ++ Debug.toString stage
                    ++ " with "
                    ++ Debug.toString a

        In input ->
            setInput (fn a input) stage


succeed : (input -> output) -> Stage input output -> Stage input output
succeed fn stage =
    case stage of
        Waiting input ->
            Success (fn input)

        _ ->
            Debug.todo <|
                "Tried to succeed with "
                    ++ Debug.toString stage


applyResult : (extra -> input -> output) -> Stage input output -> Result String extra -> Stage input output
applyResult joiningFn stage result =
    case stage of
        Waiting input ->
            case result of
                Ok extra ->
                    Success <| joiningFn extra input

                Err error_ ->
                    Failure input error_

        _ ->
            Debug.todo <|
                "Tried to use result "
                    ++ Debug.toString result
                    ++ " with "
                    ++ Debug.toString stage
