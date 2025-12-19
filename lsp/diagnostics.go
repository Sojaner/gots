package lsp

import "gots/lang"

const (
	SeverityError   = 1
	SeverityWarning = 2
)

type Diagnostic struct {
	Range    Range  `json:"range"`
	Severity int    `json:"severity,omitempty"`
	Source   string `json:"source,omitempty"`
	Message  string `json:"message"`
}

func diagnosticsFrom(diags []lang.Diagnostic) []Diagnostic {
	out := make([]Diagnostic, 0, len(diags))
	for _, d := range diags {
		start := Position{Line: max0(d.Pos.Line - 1), Character: max0(d.Pos.Column - 1)}
		end := Position{Line: start.Line, Character: start.Character + 1}
		out = append(out, Diagnostic{
			Range:    Range{Start: start, End: end},
			Severity: SeverityError,
			Source:   "gots",
			Message:  d.Message,
		})
	}
	return out
}

func max0(v int) int {
	if v < 0 {
		return 0
	}
	return v
}
