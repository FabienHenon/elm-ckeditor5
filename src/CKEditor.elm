module CKEditor exposing
    ( view
    , config, editor
    , onChange
    , Config, defaultConfig, withCustom, withLanguage, withPlugins, withPluginsRemoved, withToolbar
    )

{-|


# HTML Element

@docs view


# Attributes

@docs config, editor


# Events

@docs onChange


# Config

@docs Config, defaultConfig, withCustom, withLanguage, withPlugins, withPluginsRemoved, withToolbar

-}

import Dict exposing (Dict)
import Html exposing (Attribute, Html, node)
import Html.Attributes exposing (attribute)
import Html.Events exposing (on)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value, encode)


{-| Configuration of CKEditor
-}
type Config
    = Config Config_


type alias Config_ =
    { language : Maybe String
    , plugins : Maybe (List String)
    , removePlugins : Maybe (List String)
    , toolbar : Maybe (List String)
    , custom : Dict String Value
    }


{-| Renders a CKEditor instance

view
    [ config defaultConfig
    , onCKEditorChange CKEditorChanged
    ]
    [ text "Initial text"]

-}
view : List (Attribute msg) -> List (Html msg) -> Html msg
view =
    node "x-ckeditor"


{-| Sets the registerd editor build to use

_See [ckeditor5-webcomponent](https://github.com/FabienHenon/ckeditor5-webcomponent) for more information_

-}
editor : String -> Attribute msg
editor =
    attribute "editor"


{-| Add the specified configuration to the editor.
This will reload the component

view
    [ config (defaultConfig |> withLanguage "fr") ]
    []

-}
config : Config -> Attribute msg
config (Config config_) =
    attribute "config" (encodeConfig config_)


{-| Event fired when the CKEditor content changed. This event will not
necessarily fire on every single input action.

type Msg
    = CKEditorChanged String

onCKEditorChange CKEditorChanged

-}
onChange : (String -> msg) -> Attribute msg
onChange msg =
    on "ckeditorchange" (Decode.map msg (Decode.field "detail" Decode.string))


{-| Default CKEditor config
-}
defaultConfig : Config
defaultConfig =
    Config
        { language = Nothing
        , plugins = Nothing
        , removePlugins = Nothing
        , toolbar = Nothing
        , custom = Dict.empty
        }


{-| Set the language to use in the configuration

defaultConfig
    |> withLanguage "fr"

_See [reference](https://ckeditor.com/docs/ckeditor5/latest/api/module_core_editor_editorconfig-EditorConfig.html#member-language)_

-}
withLanguage : String -> Config -> Config
withLanguage language (Config config_) =
    Config { config_ | language = Just language }


{-| Set the plugins to use in the configuration

defaultConfig
    |> withPlugins ["Bold", "Italic"]

_See [reference](https://ckeditor.com/docs/ckeditor5/latest/api/module_core_editor_editorconfig-EditorConfig.html#member-plugins)_

-}
withPlugins : List String -> Config -> Config
withPlugins plugins (Config config_) =
    Config { config_ | plugins = Just plugins }


{-| Set the plugins that should not be loaded in the configuration

defaultConfig
    |> withPluginsRemoved ["Bold", "Italic"]

_See [reference](https://ckeditor.com/docs/ckeditor5/latest/api/module_core_editor_editorconfig-EditorConfig.html#member-removePlugins)_

-}
withPluginsRemoved : List String -> Config -> Config
withPluginsRemoved removePlugins (Config config_) =
    Config { config_ | removePlugins = Just removePlugins }


{-| Set the toolbar to use in the configuration

defaultConfig
    |> withToolbar ["bold", "italic"]

_See [reference](https://ckeditor.com/docs/ckeditor5/latest/api/module_core_editor_editorconfig-EditorConfig.html#member-toolbar)_

-}
withToolbar : List String -> Config -> Config
withToolbar toolbar (Config config_) =
    Config { config_ | toolbar = Just toolbar }


{-| Set any other configuration

defaultConfig
    |> withCustom "mediaEmbed" mediaEmbedEncoder

**The name must be a valid configuration property name**

-}
withCustom : String -> Value -> Config -> Config
withCustom name value (Config config_) =
    Config { config_ | custom = Dict.insert name value config_.custom }



-- INTERNAL


encodeConfig : Config_ -> String
encodeConfig config_ =
    Encode.object
        (Dict.toList config_.custom
            |> addEncode config_.language "language" Encode.string
            |> addEncode config_.plugins "plugins" (Encode.list Encode.string)
            |> addEncode config_.removePlugins "removePlugins" (Encode.list Encode.string)
            |> addEncode config_.toolbar "toolbar" (Encode.list Encode.string)
        )
        |> encode 0


addEncode : Maybe a -> String -> (a -> Value) -> List ( String, Value ) -> List ( String, Value )
addEncode value name encoder props =
    Maybe.map (encoder >> addToProps props name) value
        |> Maybe.withDefault props


addToProps : List ( String, Value ) -> String -> Value -> List ( String, Value )
addToProps props name value =
    ( name, value ) :: props
