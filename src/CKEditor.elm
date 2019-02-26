module CKEditor exposing
    ( view
    , config, editor, content
    , onChange
    , isContentEmpty, trimContent
    , Config, defaultConfig, withCustom, withLanguage, withPlugins, withPluginsRemoved, withToolbar
    )

{-|


# HTML Element

@docs view


# Attributes

@docs config, editor, content


# Events

@docs onChange


# Helpers

@docs isContentEmpty, trimContent


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
        [ text "Initial text" ]

-}
view : List (Attribute msg) -> List (Html msg) -> Html msg
view =
    node "x-ckeditor"


{-| Sets the registered editor build to use

_See [ckeditor5-webcomponent](https://github.com/FabienHenon/ckeditor5-webcomponent) for more information_

-}
editor : String -> Attribute msg
editor =
    attribute "editor"


{-| Sets the content of the editor

_See [ckeditor5-webcomponent](https://github.com/FabienHenon/ckeditor5-webcomponent) for more information_

-}
content : String -> Attribute msg
content =
    attribute "content"


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

    view
        [ onChange CKEditorChanged ]
        []

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

See [reference](https://ckeditor.com/docs/ckeditor5/latest/api/module_core_editor_editorconfig-EditorConfig.html#member-language) for more information

-}
withLanguage : String -> Config -> Config
withLanguage language (Config config_) =
    Config { config_ | language = Just language }


{-| Set the plugins to use in the configuration

    defaultConfig
        |> withPlugins [ "Bold", "Italic" ]

See [reference](https://ckeditor.com/docs/ckeditor5/latest/api/module_core_editor_editorconfig-EditorConfig.html#member-plugins) for more information

-}
withPlugins : List String -> Config -> Config
withPlugins plugins (Config config_) =
    Config { config_ | plugins = Just plugins }


{-| Set the plugins that should not be loaded in the configuration

    defaultConfig
        |> withPluginsRemoved [ "Bold", "Italic" ]

See [reference](https://ckeditor.com/docs/ckeditor5/latest/api/module_core_editor_editorconfig-EditorConfig.html#member-removePlugins) for more information

-}
withPluginsRemoved : List String -> Config -> Config
withPluginsRemoved removePlugins (Config config_) =
    Config { config_ | removePlugins = Just removePlugins }


{-| Set the toolbar to use in the configuration

    defaultConfig
        |> withToolbar [ "bold", "italic" ]

See [reference](https://ckeditor.com/docs/ckeditor5/latest/api/module_core_editor_editorconfig-EditorConfig.html#member-toolbar) for more information

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


{-| Check if the content you received from the editor is empty.

Such a check can be necessary because when you empty the editor the final data
still contains `<p>&nbsp;</p>`.
This function checks if the trimmed content is empty or is equal to `<p>&nbsp;</p>`

-}
isContentEmpty : String -> Bool
isContentEmpty content_ =
    (content_ |> String.trim |> String.isEmpty)
        || (content_ |> String.trim |> String.toLower |> (==) "<p>&nbsp;</p>")


{-| Trim the content, removing white spaces at the beginning and the end of the data.
If the data is considered as empty (see [isContentEmpty](#isContentEmpty)) this function will
return an empty string
-}
trimContent : String -> String
trimContent =
    String.trim >> clearEmptyContent



-- INTERNAL


clearEmptyContent : String -> String
clearEmptyContent content_ =
    if isContentEmpty content_ then
        ""

    else
        content_


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
