// Editor Package - In-game code editor for Slate

export {
  type TokenCategory,
  type HighlightToken,
  highlightSlate,
  highlightToHtml,
  SLATE_HIGHLIGHT_CSS,
  SLATE_HIGHLIGHT_CSS_DARK,
  SLATE_HIGHLIGHT_CSS_LIGHT,
} from "./syntax";

export {
  type EditorState,
  type Diagnostic,
  type EditorEvents,
  type EditorOptions,
  SlateEditor,
  createEditor,
} from "./editor";
