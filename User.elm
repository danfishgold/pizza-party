module User exposing (User, Key, key)


type alias User =
    { name : String }


type alias Key =
    String


key : User -> Key
key { name } =
    name
