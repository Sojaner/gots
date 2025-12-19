package lsp

type CompletionItem struct {
	Label         string `json:"label"`
	Kind          int    `json:"kind,omitempty"`
	Detail        string `json:"detail,omitempty"`
	InsertText    string `json:"insertText,omitempty"`
	InsertTextFormat int `json:"insertTextFormat,omitempty"`
}

const (
	CompletionItemKindKeyword  = 14
	CompletionItemKindFunction = 3
	InsertTextFormatPlain      = 1
)

var keywordCompletions = []CompletionItem{
	{Label: "function", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "type", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "interface", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "let", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "const", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "export", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "if", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "else", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "for", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "select", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "switch", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "case", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "default", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "fallthrough", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "go", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "spawn", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "send", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "await", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "throw", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "try", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "catch", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "defer", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "break", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "continue", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "return", Kind: CompletionItemKindKeyword, Detail: "keyword"},
	{Label: "number", Kind: CompletionItemKindKeyword, Detail: "type"},
	{Label: "float", Kind: CompletionItemKindKeyword, Detail: "type"},
	{Label: "string", Kind: CompletionItemKindKeyword, Detail: "type"},
	{Label: "boolean", Kind: CompletionItemKindKeyword, Detail: "type"},
	{Label: "any", Kind: CompletionItemKindKeyword, Detail: "type"},
	{Label: "error", Kind: CompletionItemKindKeyword, Detail: "type"},
	{Label: "map", Kind: CompletionItemKindKeyword, Detail: "type"},
	{Label: "chan", Kind: CompletionItemKindKeyword, Detail: "type"},
}

func defaultCompletions() []CompletionItem {
	out := make([]CompletionItem, len(keywordCompletions))
	copy(out, keywordCompletions)
	return out
}
