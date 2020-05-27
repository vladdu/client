module Translation exposing (Language(..), TranslationId(..), langFromString, languageDecoder, timeDistInWords, tr)

import Json.Decode as Json exposing (..)
import Time
import Time.Distance as TimeDistance
import Time.Distance.I18n as I18n


type TranslationId
    = Cancel
    | HomeBlank
    | HomeImportJSON
    | HomeJSONFrom
    | RecentDocuments
    | LastOpened
    | OpenOtherDocuments
    | RemoveFromList
    | NeverSaved
    | UnsavedChanges
    | SavedInternally
    | ChangesSaved
    | DatabaseError
    | LastSaved
    | LastEdit
    | KeyboardHelp
    | RestoreThisVersion
    | EnterKey
    | EnterAction
    | EditCardTitle
    | ArrowsAction
    | AddChildAction
    | SplitChildAction
    | InsertChildTitle
    | AddBelowAction
    | SplitBelowAction
    | MergeDownAction
    | InsertBelowTitle
    | AddAboveAction
    | SplitUpwardAction
    | MergeUpAction
    | InsertAboveTitle
    | ArrowKeys
    | MoveAction
    | Backspace
    | DeleteAction
    | DeleteCardTitle
    | FormattingGuide
    | ForBold
    | ForItalic
    | ToSaveChanges
    | SaveChangesTitle
    | EscKey
    | ToCancelChanges
    | PressToSearch
    | HeadingFont
    | ContentFont
    | EditingFont
    | WordCountSession Int
    | WordCountTotal Int
    | WordCountCard Int
    | WordCountSubtree Int
    | WordCountGroup Int
    | WordCountColumn Int


type Language
    = En
    | Zh
    | Es
    | Fr


tr : Language -> TranslationId -> String
tr lang trans =
    let
        pluralize n str =
            if n == 1 then
                str

            else
                str ++ "s"

        translationSet =
            case trans of
                Cancel ->
                    { en = "Cancel"
                    , zh = "取消"
                    , es = "Cancelar"
                    , fr = ""
                    }

                HomeBlank ->
                    { en = "Blank"
                    , zh = "空白"
                    , es = "Vacío"
                    , fr = ""
                    }

                HomeImportJSON ->
                    { en = "Import JSON"
                    , zh = "导入JSON"
                    , es = "Importar JSON"
                    , fr = ""
                    }

                HomeJSONFrom ->
                    { en = "From Desktop or Online"
                    , zh = "从本地或线上"
                    , es = "Del App Escritorio o Web"
                    , fr = ""
                    }

                RecentDocuments ->
                    { en = "Recent Documents"
                    , zh = "最近的文档"
                    , es = "Documentos Recientes"
                    , fr = ""
                    }

                LastOpened ->
                    { en = "Last Opened"
                    , zh = "上一个打开"
                    , es = "Última Apertura"
                    , fr = ""
                    }

                OpenOtherDocuments ->
                    { en = "Open Other Documents"
                    , zh = "打开另一个文档"
                    , es = "Abrir Otros Documentos"
                    , fr = ""
                    }

                RemoveFromList ->
                    { en = "Remove From List"
                    , zh = "从列表中移除"
                    , es = "Elminiar de la Lista"
                    , fr = ""
                    }

                NeverSaved ->
                    { en = "New Document..."
                    , zh = "新的文档"
                    , es = "Documento Nuevo..."
                    , fr = ""
                    }

                UnsavedChanges ->
                    { en = "Unsaved Changes..."
                    , zh = "未保存的更改"
                    , es = "Cambios No Guardados..."
                    , fr = ""
                    }

                SavedInternally ->
                    { en = "Backup Saved"
                    , zh = "保存备份"
                    , es = "Copia De Seguridad Guardada"
                    , fr = ""
                    }

                ChangesSaved ->
                    { en = "Saved"
                    , zh = "保存"
                    , es = "Cambios Guardados"
                    , fr = ""
                    }

                DatabaseError ->
                    { en = "Database Error..."
                    , zh = "数据库错误"
                    , es = "Error de Database..."
                    , fr = ""
                    }

                LastSaved ->
                    { en = "Last saved"
                    , zh = "上一个保存"
                    , es = "Guardado"
                    , fr = ""
                    }

                LastEdit ->
                    { en = "Last edit"
                    , zh = "上一次编辑 "
                    , es = "Último cambio"
                    , fr = ""
                    }

                KeyboardHelp ->
                    { en = "Keyboard Shortcuts Help"
                    , zh = "快捷键帮助"
                    , es = "Atajos de Teclado"
                    , fr = ""
                    }

                RestoreThisVersion ->
                    { en = "Restore this Version"
                    , zh = "恢复此版本"
                    , es = "Restaurar esta Versión"
                    , fr = ""
                    }

                EnterKey ->
                    { en = "Enter"
                    , zh = "回车"
                    , es = "Enter"
                    , fr = ""
                    }

                EnterAction ->
                    { en = "to Edit"
                    , zh = "编辑"
                    , es = "para Editar"
                    , fr = ""
                    }

                EditCardTitle ->
                    { en = "Edit Card (Enter)"
                    , zh = "编辑卡片（回车）"
                    , es = "Editar Tarjeta (Enter)"
                    , fr = ""
                    }

                ArrowsAction ->
                    { en = "to Navigate"
                    , zh = "导航"
                    , es = "para Navegar"
                    , fr = ""
                    }

                AddChildAction ->
                    { en = "to Add Child"
                    , zh = "添加子卡片"
                    , es = "para Agregar un Hijo"
                    , fr = ""
                    }

                SplitChildAction ->
                    { en = "to Split Card to the Right"
                    , zh = "向右拆分卡"
                    , es = "para Dividir la Tarjeta hacia la Derecha"
                    , fr = ""
                    }

                InsertChildTitle ->
                    { en = "Insert Child (Ctrl+L)"
                    , zh = "插入子卡片"
                    , es = "Agregar un Hijo (Ctrl+L)"
                    , fr = ""
                    }

                AddBelowAction ->
                    { en = "to Add Below"
                    , zh = "在下方添加"
                    , es = "para Agregar Abajo"
                    , fr = ""
                    }

                SplitBelowAction ->
                    { en = "to Split Card Down"
                    , zh = "向下拆分卡"
                    , es = "para Dividir la Tarjeta hacia la Abajo"
                    , fr = ""
                    }

                MergeDownAction ->
                    { en = "to Merge into Next"
                    , zh = "合并到下一个"
                    , es = "para Combinar la Tarjeta a la Siguiente"
                    , fr = ""
                    }

                InsertBelowTitle ->
                    { en = "Insert Below (Ctrl+J)"
                    , zh = "在下方插入"
                    , es = "Agregar Abajo (Ctrl+J)"
                    , fr = ""
                    }

                AddAboveAction ->
                    { en = "to Add Above"
                    , zh = "在上方添加"
                    , es = "para Agregar Arriba"
                    , fr = ""
                    }

                SplitUpwardAction ->
                    { en = "to Split Card Upward"
                    , zh = "向上拆分卡"
                    , es = "para Dividir la Tarjeta hacia la Arriba"
                    , fr = ""
                    }

                MergeUpAction ->
                    { en = "to Merge into Previous"
                    , zh = "合并到上一个"
                    , es = "para Combinar la Tarjeta a la Anterior"
                    , fr = ""
                    }

                InsertAboveTitle ->
                    { en = "Insert Above (Ctrl+K)"
                    , zh = "在上方插入"
                    , es = "Agregar Arriba (Ctrl+K)"
                    , fr = ""
                    }

                ArrowKeys ->
                    { en = "(arrows)"
                    , zh = "（箭头）"
                    , es = "(flechas)"
                    , fr = ""
                    }

                MoveAction ->
                    { en = "to Move"
                    , zh = "移动"
                    , es = "para Mover"
                    , fr = ""
                    }

                Backspace ->
                    { en = "Backspace"
                    , zh = "退格键"
                    , es = "Backspace"
                    , fr = ""
                    }

                DeleteAction ->
                    { en = "to Delete"
                    , zh = "删除"
                    , es = "para Eliminar"
                    , fr = ""
                    }

                DeleteCardTitle ->
                    { en = "Delete Card (Ctrl+Backspace)"
                    , zh = "删除卡片"
                    , es = "Eliminar Tarjeta (Ctrl+Backspace)"
                    , fr = ""
                    }

                FormattingGuide ->
                    { en = "Formatting Syntax Guide"
                    , zh = "格式指南"
                    , es = "Guía de Sintaxis para Formato"
                    , fr = ""
                    }

                ForBold ->
                    { en = "for Bold"
                    , zh = "粗体"
                    , es = "para Negrita"
                    , fr = ""
                    }

                ForItalic ->
                    { en = "for Italic"
                    , zh = "斜体"
                    , es = "para Itálica"
                    , fr = ""
                    }

                ToSaveChanges ->
                    { en = "to Save Changes"
                    , zh = "保存更改"
                    , es = "para Guardar Cambios"
                    , fr = ""
                    }

                SaveChangesTitle ->
                    { en = "Save Changes (Ctrl+Enter)"
                    , zh = "保存更改 (Ctrl+Enter)"
                    , es = "Guardar Cambios (Ctrl+Enter)"
                    , fr = ""
                    }

                EscKey ->
                    { en = "Esc"
                    , zh = "退出"
                    , es = "Esc"
                    , fr = ""
                    }

                ToCancelChanges ->
                    { en = "to Cancel Changes"
                    , zh = "取消更改"
                    , es = "para Cancelar Cambios"
                    , fr = ""
                    }

                PressToSearch ->
                    { en = "Press '/' to search"
                    , zh = "输入“/”进行搜索"
                    , es = "Presiona '/' para buscar"
                    , fr = ""
                    }

                HeadingFont ->
                    { en = "Heading Font"
                    , zh = "标题字体"
                    , es = "Fuente de Titulos"
                    , fr = ""
                    }

                ContentFont ->
                    { en = "Content Font"
                    , zh = "内容字体"
                    , es = "Fuente de Contenido"
                    , fr = ""
                    }

                EditingFont ->
                    { en = "Editing/Monospace Font"
                    , zh = "编辑/等宽字体"
                    , es = "Fuente de Edición"
                    , fr = ""
                    }

                WordCountSession n ->
                    { en = "Session: " ++ String.fromInt n ++ pluralize n " word"
                    , zh = "本节：" ++ String.fromInt n ++ "个字"
                    , es = "Sesión: " ++ String.fromInt n ++ pluralize n " palabra"
                    , fr = ""
                    }

                WordCountTotal n ->
                    { en = "Total : " ++ String.fromInt n ++ pluralize n " word"
                    , zh = "总共：" ++ String.fromInt n ++ "个字"
                    , es = "Total : " ++ String.fromInt n ++ pluralize n " palabra"
                    , fr = ""
                    }

                WordCountCard n ->
                    { en = "Card : " ++ String.fromInt n ++ pluralize n " word"
                    , zh = "卡片：" ++ String.fromInt n ++ "个字"
                    , es = "Tarjeta : " ++ String.fromInt n ++ pluralize n " palabra"
                    , fr = ""
                    }

                WordCountSubtree n ->
                    { en = "Subtree : " ++ String.fromInt n ++ pluralize n " word"
                    , zh = "树节点下：" ++ String.fromInt n ++ "个字"
                    , es = "Subárbol : " ++ String.fromInt n ++ pluralize n " palabra"
                    , fr = ""
                    }

                WordCountGroup n ->
                    { en = "Group : " ++ String.fromInt n ++ pluralize n " word"
                    , zh = "组：" ++ String.fromInt n ++ "个字"
                    , es = "Grupo : " ++ String.fromInt n ++ pluralize n " palabra"
                    , fr = ""
                    }

                WordCountColumn n ->
                    { en = "Column : " ++ String.fromInt n ++ pluralize n " word"
                    , zh = "列：" ++ String.fromInt n ++ "个字"
                    , es = "Columna : " ++ String.fromInt n ++ pluralize n " palabra"
                    , fr = ""
                    }
    in
    case lang of
        En ->
            .en translationSet

        Zh ->
            .zh translationSet

        Es ->
            .es translationSet

        Fr ->
            .fr translationSet


timeDistInWords : Language -> Time.Posix -> Time.Posix -> String
timeDistInWords lang t1 t2 =
    case lang of
        En ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Zh ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Es ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.es t1 t2

        Fr ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.fr t1 t2


languageDecoder : Decoder Language
languageDecoder =
    Json.map langFromString string


langFromString : String -> Language
langFromString str =
    case str of
        "en" ->
            En

        "zh" ->
            Zh

        "es" ->
            Es

        "fr" ->
            Fr

        _ ->
            En
