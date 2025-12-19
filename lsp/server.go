package lsp

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"net/url"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"gots/lang"
	"gots/translator"
)

type rpcMessage struct {
	JSONRPC string           `json:"jsonrpc"`
	ID      *json.RawMessage `json:"id,omitempty"`
	Method  string           `json:"method,omitempty"`
	Params  json.RawMessage  `json:"params,omitempty"`
}

type rpcResponse struct {
	JSONRPC string           `json:"jsonrpc"`
	ID      *json.RawMessage `json:"id,omitempty"`
	Result  interface{}      `json:"result,omitempty"`
	Error   *rpcError        `json:"error,omitempty"`
}

type rpcError struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
}

type InitializeParams struct {
	RootURI string `json:"rootUri"`
}

type InitializeResult struct {
	Capabilities ServerCapabilities `json:"capabilities"`
}

type ServerCapabilities struct {
	TextDocumentSync       int                 `json:"textDocumentSync"`
	CompletionProvider     CompletionOptions   `json:"completionProvider"`
	ExecuteCommandProvider ExecuteCommandOptions `json:"executeCommandProvider"`
}

type CompletionOptions struct {
	TriggerCharacters []string `json:"triggerCharacters,omitempty"`
}

type ExecuteCommandOptions struct {
	Commands []string `json:"commands"`
}

type TextDocumentIdentifier struct {
	URI string `json:"uri"`
}

type VersionedTextDocumentIdentifier struct {
	URI     string `json:"uri"`
	Version int    `json:"version"`
}

type TextDocumentItem struct {
	URI        string `json:"uri"`
	LanguageID string `json:"languageId"`
	Version    int    `json:"version"`
	Text       string `json:"text"`
}

type DidOpenTextDocumentParams struct {
	TextDocument TextDocumentItem `json:"textDocument"`
}

type DidChangeTextDocumentParams struct {
	TextDocument   VersionedTextDocumentIdentifier `json:"textDocument"`
	ContentChanges []TextDocumentContentChangeEvent `json:"contentChanges"`
}

type DidSaveTextDocumentParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
	Text         string                 `json:"text,omitempty"`
}

type CompletionParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
	Position     Position               `json:"position"`
}

type CompletionList struct {
	IsIncomplete bool             `json:"isIncomplete"`
	Items        []CompletionItem `json:"items"`
}

type PublishDiagnosticsParams struct {
	URI         string        `json:"uri"`
	Diagnostics []Diagnostic  `json:"diagnostics"`
}

type ExecuteCommandParams struct {
	Command   string        `json:"command"`
	Arguments []interface{} `json:"arguments"`
}

// Server implements a tiny subset of the LSP for .gots files.
type Server struct {
	reader    *bufio.Reader
	out       io.Writer
	workspace *Workspace
}

func NewServer(in io.Reader, out io.Writer) *Server {
	return &Server{
		reader:    bufio.NewReader(in),
		out:       out,
		workspace: NewWorkspace(),
	}
}

func (s *Server) Run() error {
	for {
		msg, err := s.readMessage()
		if err != nil {
			return err
		}
		if msg.Method == "" {
			continue
		}
		switch msg.Method {
		case "initialize":
			s.handleInitialize(msg)
		case "initialized":
			// no-op
		case "shutdown":
			s.sendResponse(msg.ID, nil, nil)
		case "exit":
			return nil
		case "textDocument/didOpen":
			s.handleDidOpen(msg)
		case "textDocument/didChange":
			s.handleDidChange(msg)
		case "textDocument/didSave":
			s.handleDidSave(msg)
		case "textDocument/completion":
			s.handleCompletion(msg)
		case "workspace/executeCommand":
			s.handleExecuteCommand(msg)
		default:
			s.sendResponse(msg.ID, nil, &rpcError{Code: -32601, Message: "method not found"})
		}
	}
}

func (s *Server) readMessage() (rpcMessage, error) {
	headers := map[string]string{}
	for {
		line, err := s.reader.ReadString('\n')
		if err != nil {
			return rpcMessage{}, err
		}
		line = strings.TrimRight(line, "\r\n")
		if line == "" {
			break
		}
		parts := strings.SplitN(line, ":", 2)
		if len(parts) == 2 {
			headers[strings.TrimSpace(parts[0])] = strings.TrimSpace(parts[1])
		}
	}

	lengthStr, ok := headers["Content-Length"]
	if !ok {
		return rpcMessage{}, fmt.Errorf("missing Content-Length header")
	}
	length, err := strconv.Atoi(lengthStr)
	if err != nil {
		return rpcMessage{}, err
	}
	body := make([]byte, length)
	if _, err := io.ReadFull(s.reader, body); err != nil {
		return rpcMessage{}, err
	}
	var msg rpcMessage
	if err := json.Unmarshal(body, &msg); err != nil {
		return rpcMessage{}, err
	}
	return msg, nil
}

func (s *Server) sendResponse(id *json.RawMessage, result interface{}, errObj *rpcError) {
	if id == nil {
		return
	}
	resp := rpcResponse{
		JSONRPC: "2.0",
		ID:      id,
		Result:  result,
		Error:   errObj,
	}
	s.writeMessage(resp)
}

func (s *Server) sendNotification(method string, params interface{}) {
	notify := rpcMessage{
		JSONRPC: "2.0",
		Method:  method,
		Params:  marshalRaw(params),
	}
	s.writeMessage(notify)
}

func (s *Server) writeMessage(payload interface{}) {
	data, err := json.Marshal(payload)
	if err != nil {
		return
	}
	fmt.Fprintf(s.out, "Content-Length: %d\r\n\r\n", len(data))
	s.out.Write(data)
}

func (s *Server) handleInitialize(msg rpcMessage) {
	caps := ServerCapabilities{
		TextDocumentSync: 1, // Full sync
		CompletionProvider: CompletionOptions{
			TriggerCharacters: []string{"."},
		},
		ExecuteCommandProvider: ExecuteCommandOptions{
			Commands: []string{"gots.translate"},
		},
	}
	s.sendResponse(msg.ID, InitializeResult{Capabilities: caps}, nil)
}

func (s *Server) handleDidOpen(msg rpcMessage) {
	var params DidOpenTextDocumentParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return
	}
	s.workspace.Open(Document{
		URI:     params.TextDocument.URI,
		Text:    params.TextDocument.Text,
		Version: params.TextDocument.Version,
	})
	s.publishDiagnostics(params.TextDocument.URI, params.TextDocument.Text)
}

func (s *Server) handleDidChange(msg rpcMessage) {
	var params DidChangeTextDocumentParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return
	}
	text := s.workspace.Update(params.TextDocument.URI, params.TextDocument.Version, params.ContentChanges)
	s.publishDiagnostics(params.TextDocument.URI, text)
}

func (s *Server) handleDidSave(msg rpcMessage) {
	var params DidSaveTextDocumentParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return
	}
	text := params.Text
	if text == "" {
		if doc, ok := s.workspace.Get(params.TextDocument.URI); ok {
			text = doc.Text
		}
	}
	if text != "" {
		s.publishDiagnostics(params.TextDocument.URI, text)
	}
}

func (s *Server) handleCompletion(msg rpcMessage) {
	var params CompletionParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		return
	}
	items := defaultCompletions()
	s.sendResponse(msg.ID, CompletionList{
		IsIncomplete: false,
		Items:        items,
	}, nil)
}

func (s *Server) handleExecuteCommand(msg rpcMessage) {
	var params ExecuteCommandParams
	if err := json.Unmarshal(msg.Params, &params); err != nil {
		s.sendResponse(msg.ID, nil, &rpcError{Code: -32602, Message: "invalid executeCommand params"})
		return
	}
	switch params.Command {
	case "gots.translate":
		if len(params.Arguments) == 0 {
			s.sendResponse(msg.ID, nil, &rpcError{Code: -32602, Message: "expected file URI argument"})
			return
		}
		uri, ok := params.Arguments[0].(string)
		if !ok {
			s.sendResponse(msg.ID, nil, &rpcError{Code: -32602, Message: "argument must be a URI string"})
			return
		}
		if err := s.translateFile(uri); err != nil {
			s.sendResponse(msg.ID, nil, &rpcError{Code: -32603, Message: err.Error()})
			return
		}
		s.sendResponse(msg.ID, map[string]string{"status": "ok"}, nil)
	default:
		s.sendResponse(msg.ID, nil, &rpcError{Code: -32601, Message: "unsupported command"})
	}
}

func (s *Server) publishDiagnostics(uri string, text string) {
	prog, parseDiags := lang.Parse(text)
	_, transDiags := translator.Translate(prog)
	all := append(parseDiags, transDiags...)
	params := PublishDiagnosticsParams{
		URI:         uri,
		Diagnostics: diagnosticsFrom(all),
	}
	s.sendNotification("textDocument/publishDiagnostics", params)
}

func (s *Server) translateFile(uri string) error {
	var text string
	if doc, ok := s.workspace.Get(uri); ok {
		text = doc.Text
	}
	if text == "" {
		path := uriToPath(uri)
		data, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		text = string(data)
	}

	prog, diags := lang.Parse(text)
	if len(diags) > 0 {
		return fmt.Errorf("translation blocked; fix reported diagnostics first")
	}
	code, transDiags := translator.Translate(prog)
	if len(transDiags) > 0 {
		return fmt.Errorf("translation blocked; fix reported diagnostics first")
	}
	path := uriToPath(uri)
	if path == "" {
		return fmt.Errorf("could not resolve path for URI %s", uri)
	}
	outPath := strings.TrimSuffix(path, filepath.Ext(path)) + ".go"
	if err := os.WriteFile(outPath, []byte(code), 0644); err != nil {
		return err
	}
	return nil
}

func uriToPath(uri string) string {
	if strings.HasPrefix(uri, "file://") {
		rest := strings.TrimPrefix(uri, "file://")
		unescaped, err := url.PathUnescape(rest)
		if err == nil {
			return unescaped
		}
		return rest
	}
	return uri
}

func marshalRaw(v interface{}) json.RawMessage {
	if v == nil {
		return nil
	}
	data, err := json.Marshal(v)
	if err != nil {
		return nil
	}
	return data
}
