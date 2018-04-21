module JsonSchema.Form exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode exposing (Value)
import Json.Encode
import Json.Patch as Patch
import Json.Pointer exposing (Pointer)
import JsonSchema
import JsonSchema.Decoder
import JsonSchema.Encoder
import JsonSchema.Model
import JsonSchema.Validator as Validator


type alias Model =
    { schema : Schema
    , json : Json
    }


type Json
    = ValidJson Value (List Validator.Error)
    | InvalidJson InvalidText


type Schema
    = ValidSchema JsonSchema.Schema
    | InvalidSchema InvalidText


type alias InvalidText =
    { intendedText : String
    , errorMessage : String
    }


stringToJson : JsonSchema.Schema -> String -> Json
stringToJson schema string =
    let
        toJson result =
            case result of
                Ok value ->
                    let
                        ( validatedValue, errors ) =
                            validateJson schema value
                    in
                    ValidJson validatedValue errors

                Err errorMessage ->
                    InvalidJson { intendedText = string, errorMessage = errorMessage }
    in
    string
        |> Json.Decode.decodeString Json.Decode.value
        |> toJson


validateJson : JsonSchema.Schema -> Value -> ( Value, List Validator.Error )
validateJson schema value =
    ( value
    , Validator.validate schema value
    )


init : JsonSchema.Schema -> String -> Model
init schema jsonString =
    Model (ValidSchema schema) (stringToJson schema jsonString)


type Msg
    = SchemaChange String
    | DirectJsonChange String
    | PatchJson Pointer String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SchemaChange newSchemaString ->
            let
                newSchemaResult =
                    Json.Decode.decodeString
                        JsonSchema.Decoder.decoder
                        newSchemaString

                newSchema =
                    case newSchemaResult of
                        Ok schema ->
                            ValidSchema schema

                        Err err ->
                            InvalidSchema
                                { intendedText = newSchemaString
                                , errorMessage = err
                                }
            in
            { model | schema = newSchema } ! []

        DirectJsonChange newJsonString ->
            case model.schema of
                ValidSchema schema ->
                    { model | json = stringToJson schema newJsonString } ! []

                InvalidSchema _ ->
                    model ! []

        PatchJson pointer newString ->
            let
                newJson =
                    case model.json of
                        ValidJson validJson errors ->
                            let
                                patchResult =
                                    Patch.apply
                                        [ newString
                                            |> Json.Encode.string
                                            |> Patch.Replace pointer
                                        ]
                                        validJson
                            in
                            case patchResult of
                                Ok result ->
                                    Json.Encode.encode 2 result

                                Err err ->
                                    Json.Encode.encode 2 validJson

                        InvalidJson invalid ->
                            invalid.intendedText
            in
            update (DirectJsonChange newJson) model

        NoOp ->
            model ! []


rootPointer : Pointer
rootPointer =
    []


view : Model -> Html Msg
view model =
    Html.div []
        [ viewForm rootPointer model.schema model.json
        , viewSchemaTextarea model.schema
        , viewJsonOutput model.json
        ]


viewSchemaTextarea : Schema -> Html Msg
viewSchemaTextarea schema =
    case schema of
        ValidSchema schema ->
            Html.div []
                [ Html.textarea
                    [ Html.Events.onInput SchemaChange
                    , Html.Attributes.value <| JsonSchema.Encoder.encode schema
                    , textareaSize
                    ]
                    []
                ]

        InvalidSchema { intendedText, errorMessage } ->
            Html.div []
                [ Html.div [] [ Html.text "This is total garbage" ]
                , Html.textarea
                    [ Html.Events.onInput SchemaChange
                    , Html.Attributes.value intendedText
                    , textareaSize
                    ]
                    []
                ]


viewJsonOutput : Json -> Html Msg
viewJsonOutput json =
    case json of
        ValidJson json errors ->
            Html.div []
                [ Html.textarea
                    [ Html.Events.onInput DirectJsonChange
                    , Html.Attributes.value <| Json.Encode.encode 2 json
                    , textareaSize
                    ]
                    []
                ]

        InvalidJson { intendedText, errorMessage } ->
            Html.div []
                [ Html.div [] [ Html.text "This is bad json" ]
                , Html.div [] [ Html.text errorMessage ]
                , Html.textarea
                    [ Html.Events.onInput DirectJsonChange
                    , Html.Attributes.value <| intendedText
                    , textareaSize
                    ]
                    []
                ]


textareaSize : Html.Attribute msg
textareaSize =
    Html.Attributes.style
        [ ( "height", "300px" )
        , ( "width", "200px" )
        ]


viewForm : Pointer -> Schema -> Json -> Html Msg
viewForm pointer schema json =
    case ( schema, json ) of
        ( ValidSchema validSchema, ValidJson validJson errors ) ->
            viewFormValid pointer validSchema errors validJson

        _ ->
            Html.text "Something is wrong"


viewFormValid : Pointer -> JsonSchema.Schema -> List Validator.Error -> Value -> Html Msg
viewFormValid pointer schema errors value =
    let
        validatedValue =
            Validator.getValidatedValue pointer schema value
    in
    case ( validatedValue, schema ) of
        ( Validator.StringValue errors string, JsonSchema.Model.String stringSchema ) ->
            stringFormView pointer errors stringSchema string

        ( _, JsonSchema.Model.String stringSchema ) ->
            stringFormView pointer errors stringSchema ""

        _ ->
            Html.text "workin on it"


stringFormView : Pointer -> List Validator.Error -> JsonSchema.Model.StringSchema -> String -> Html Msg
stringFormView pointer errors stringSchema string =
    Html.div []
        [ Html.div []
            [ viewJust label stringSchema.title
            , Html.input
                [ Html.Attributes.value string
                , Html.Events.onInput (PatchJson pointer)
                ]
                []
            ]
        , Html.div [] (List.map (\a -> Html.div [] [ Html.text <| toString a ]) errors)
        ]


label : String -> Html Msg
label string =
    Html.label [] [ Html.text string ]


viewJust fn a =
    case a of
        Just a_ ->
            fn a_

        Nothing ->
            Html.span [] []
