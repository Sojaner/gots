package lsp

import (
	"strings"
)

type Position struct {
	Line      int `json:"line"`
	Character int `json:"character"`
}

type Range struct {
	Start Position `json:"start"`
	End   Position `json:"end"`
}

type TextDocumentContentChangeEvent struct {
	Range *Range `json:"range,omitempty"`
	Text  string `json:"text"`
}

// Document mirrors the state the server keeps for an open text document.
type Document struct {
	URI     string
	Text    string
	Version int
}

type Workspace struct {
	docs map[string]*Document
}

func NewWorkspace() *Workspace {
	return &Workspace{docs: map[string]*Document{}}
}

func (w *Workspace) Open(doc Document) {
	copy := doc
	w.docs[doc.URI] = &copy
}

func (w *Workspace) Update(uri string, version int, changes []TextDocumentContentChangeEvent) string {
	doc, ok := w.docs[uri]
	if !ok {
		doc = &Document{URI: uri}
		w.docs[uri] = doc
	}
	text := doc.Text
	for _, change := range changes {
		text = applyChange(text, change)
	}
	doc.Text = text
	doc.Version = version
	return text
}

func (w *Workspace) Get(uri string) (*Document, bool) {
	doc, ok := w.docs[uri]
	return doc, ok
}

func applyChange(text string, change TextDocumentContentChangeEvent) string {
	if change.Range == nil {
		return change.Text
	}
	start := offsetAtPosition(text, change.Range.Start)
	end := offsetAtPosition(text, change.Range.End)
	if start < 0 || end < start || end > len(text) {
		return text
	}
	var sb strings.Builder
	sb.WriteString(text[:start])
	sb.WriteString(change.Text)
	sb.WriteString(text[end:])
	return sb.String()
}

// offsetAtPosition converts a zero-based LSP position into a byte offset within text.
func offsetAtPosition(text string, pos Position) int {
	if pos.Line == 0 && pos.Character == 0 {
		return 0
	}
	line := 0
	col := 0
	for i, r := range text {
		if line == pos.Line && col == pos.Character {
			return i
		}
		if r == '\n' {
			line++
			col = 0
			if line > pos.Line {
				return i
			}
			continue
		}
		col++
	}
	if line == pos.Line && col == pos.Character {
		return len(text)
	}
	return len(text)
}
