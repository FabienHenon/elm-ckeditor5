# elm-ckeditor5

Elm wrapper for [ckeditor 5](https://ckeditor.com/ckeditor-5/).

**It requires [ckeditor5-webcomponent](https://github.com/FabienHenon/ckeditor5-webcomponent) and thus [webcomponent polyfills](https://github.com/WebComponents/webcomponentsjs) and [ckeditor 5](https://ckeditor.com/docs/ckeditor5/latest/framework/guides/quick-start.html) in order to work**

## Install

```
$ elm install FabienHenon/elm-ckeditor5
```

## Usage

_Please follow these [instructions to initialize ckeditor](https://github.com/FabienHenon/ckeditor5-webcomponent#usage) if you want more information about using ckeditor._

First you have to use an existing editor build or create a new one and register it in your Javascript code.

`index.js`
```js
import ClassicEditor from '@ckeditor/ckeditor5-build-classic';
import EditorManager from 'ckeditor5-webcomponent';

// We register the ClassicEditor under the name 'classic'
EditorManager.register('classic', ClassicEditor);
```

Then, you can use the editor in your elm code:

```elm
import CKEditor

type Msg
    = OnChange String

view initialContent =
    CKEditor.view
        [ CKEditor.editor "classic"
        , CKEditor.onChange OnChange
        , CKEditor.config
            (CKEditor.defaultConfig
                |> CKEditor.withLanguage "fr"
            )
        ]
        [ text initialContent ]
```