module JsonSchema.Form exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode exposing (Value)
import Json.Encode
import Json.Pointer
import JsonSchema
import JsonSchema.Decoder
import JsonSchema.Encoder
import JsonSchema.Model
import JsonSchema.Validator


type alias Model =
    { schema : Schema
    , json : Json
    }


type Json
    = ValidJson Value (List JsonSchema.Validator.Error)
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
                    ValidJson value <| JsonSchema.Validator.validate schema value

                Err errorMessage ->
                    InvalidJson { intendedText = string, errorMessage = errorMessage }
    in
    string
        |> Json.Decode.decodeString Json.Decode.value
        |> toJson


init : JsonSchema.Schema -> String -> Model
init schema jsonString =
    Model (ValidSchema schema) (stringToJson schema jsonString)


type Msg
    = SchemaChange String
    | DirectJsonChange String
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

        NoOp ->
            model ! []


rootPointer : Json.Pointer.Pointer
rootPointer =
    []


view : Model -> Html Msg
view model =
    Html.div []
        [ schemaView rootPointer model.schema model.json
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
                    ]
                    []
                ]

        InvalidSchema { intendedText, errorMessage } ->
            Html.div []
                [ Html.div [] [ Html.text "This is total garbage" ]
                , Html.textarea
                    [ Html.Events.onInput SchemaChange
                    , Html.Attributes.value intendedText
                    ]
                    []
                ]


viewJsonOutput : Json -> Html Msg
viewJsonOutput json =
    case json of
        ValidJson json errors ->
            Html.div []
                [ Html.div [] [ Html.text <| toString errors ]
                , Html.textarea
                    [ Html.Events.onInput DirectJsonChange
                    , Html.Attributes.value <| Json.Encode.encode 2 json
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
                    ]
                    []
                ]


schemaView : Json.Pointer.Pointer -> Schema -> Json -> Html msg
schemaView pointer schema json =
    Html.text "THis will be the form"



-- let
--     schema =
--         case probablySchema of
--             ValidSchema s ->
--                 s
--             Invalid s ->
--                 --TODO THis is obv garbage
--                 JsonSchema.string [ JsonSchema.title "sup" ]
-- in
-- case ( JsonSchema.Validator.getValidatedValue [] schema value, schema ) of
--     ( JsonSchema.Validator.Invalid errorList, _ ) ->
--         Html.text "notImplemented"
--     ( JsonSchema.Validator.StringValue string, JsonSchema.Model.String stringSchema ) ->
--         stringView stringSchema string pointer
--     ( JsonSchema.Validator.StringValue string, _ ) ->
--         Html.text "WTF?"
--     ( JsonSchema.Validator.ArrayValue arrayOfValues, _ ) ->
--         Html.text "notImplemented"
--     ( JsonSchema.Validator.TupleValue arrayOfValues, _ ) ->
--         Html.text "notImplemented"
--     ( JsonSchema.Validator.ObjectValue objectOfValues, _ ) ->
--         Html.text "notImplemented"
--     ( JsonSchema.Validator.BoolValue bool, _ ) ->
--         Html.text "notImplemented"
--     ( JsonSchema.Validator.FloatValue float, _ ) ->
--         Html.text "notImplemented"
--     ( JsonSchema.Validator.IntValue int, _ ) ->
--         Html.text "notImplemented"
--     ( JsonSchema.Validator.LazyValue lazy, _ ) ->
--         Html.text "notImplemented"
--     ( JsonSchema.Validator.Valid _, _ ) ->
--         Html.text "notImplemented"


stringView : JsonSchema.Model.StringSchema -> String -> Json.Pointer.Pointer -> Html msg
stringView stringSchema string pointer =
    Html.div []
        [ viewJust label stringSchema.title
        , Html.input [ Html.Attributes.value string ] []
        ]


label : String -> Html msg
label string =
    Html.label [] [ Html.text string ]


viewJust fn a =
    case a of
        Just a_ ->
            fn a_

        Nothing ->
            Html.span [] []
