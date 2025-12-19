# gots

`gots` lets you write Go programs in a small TypeScript-flavoured syntax that only allows constructs with a clean Go mapping. It provides:

- a parser/translator that turns `.gots` files into Go source
- a CLI to translate or run via the Go toolchain
- a lightweight Language Server (LSP) for diagnostics and keyword completions

## Language subset

The surface syntax borrows heavily from TypeScript. Where we mirror TS exactly:

- `package`/`import` statements match TS module imports (with a `from` clause).
- `type` aliases, object shapes `{ field: Type }`, and `interface { Method(): Type }` declarations follow TS syntax.
- Generics use TS angle-bracket type parameters: `function f<T>(x: T): T { ... }` and `type Box<T> = { value: T }`.
- Function signatures, parameters with type annotations, optional type parameters, and return types use TS forms: `function foo(a: number, b: string): boolean {}`.
- Variable declarations use TS `let`/`const` with optional type annotations: `let x: number = 1`.
- Arrow functions use TS syntax: `(x: number, y: number): number => x + y`.
- Type assertions use TS `expr as Type`.

Extensions beyond TS (Go-flavoured but TS-adjacent):

- `package` keyword (Go-style) and file-scoped packages.
- Channel types and operations: `chan<T>`, directional `chan<in T>` / `chan<out T>`, `send ch => v`, `await ch` / `<-ch`.
- Go control flow: `select { case ... }`, `switch type (x)` (type switch), `defer`, `go`/`spawn`, `fallthrough`, labels/goto.
- `try expr catch err { ... }` sugar for `(T, error)` calls; `throw` translates to `panic`.
- Multi-value returns and tuple bindings: `function f(): (number, error)`, `let v, ok = m[key]`.
- Pointers and casts: pointer types `*T`, unary `&`/`*`, and C#-style casts `(Type)expr`.
- Built-in-like helpers permitted as identifiers (`len`, `cap`, `make`, `new`, `close`) but currently lowered as regular calls.
- Bitwise/compound operators and `++/--`, which align with Go but not typical TS usage.

- Packages/imports: `package <name>`; `import fmt from "fmt"`.
- Types: structs (`type Person = { name: string, age: number }`) and interfaces (`type Reader<T> = interface { read(): T }`), with optional type parameters.
- Functions: `function greet<T>(name: string): string { ... }` (optional `export` to uppercase the Go name); multiple return values with parentheses `function load(): (string, error)`. Methods use `function Person.greet(...) {}`; variadics with TS rest-style `function log(...args: string[]): void`.
- Variables: `let`/`const` with optional type annotation. Inside functions, `let` becomes `:=` when possible; top-level uses `var`/`const`.
- Statements: expression statements, assignments, channel send (`send ch => v` or `ch <- v`), `go`/`spawn` to start goroutines, `throw`→`panic`, `if/else`, `for (init; cond; post)`, range loops `for (let v of xs)` or `for (let k, v of m)`, `switch/case/default` plus type switches via `switch type (x)`, `select { case ... }`, `defer`, `break`/`continue`/`fallthrough`, `return`.
- Labels/goto are supported with Go syntax (`label:` / `goto label`).
- Expressions: identifiers, number/string/bool/nil literals, unary `!`/`-`/`<-`/`await`/`&`/`*`, binary `+ - * / % == != < <= > >= && || & | ^ << >>` (with compound assignments and `++/--`), member access (`fmt.Println`), calls (including variadics), indexing/slicing (`xs[i]`, `xs[a:b]`), arrow function literals `(x: number) => x + 1`, array literals `[1,2]`, map/object literals `{ key: value }`, `expr as Type` assertions or casts `(Type)expr`, and a restricted `try expr catch err { ... }` sugar for Go-style `(T, error)` calls.
- Types supported: `number`→`int`, `float`→`float64`, `string`, `boolean`→`bool`, `any`→`interface{}`, `error`, `void` (only as return), generics (`Box<string>`) with constraints (`T extends Reader`), channels (`chan<number>` requires a type argument) plus directional `chan<in T>` / `chan<out T>`, `map<K,V>`, function types `(x: number) => string`, pointer types `*T`, array types via `name[]`.

Anything outside this subset is flagged as a diagnostic by the parser/translator.

Error handling sugar: `try` assumes a call returns `(T, error)` and the enclosing function returns `error` last. `let v = try read(path)` lowers to `v, err := read(path); if err != nil { return err }`. You can optionally add `catch err { ... }` to handle the error instead of auto-returning.

Note: channel creation currently assumes you obtain a channel from Go helpers or as a parameter; once you have one, you can send (`send ch => v` or `ch <- v`) and receive (`let v = await ch` or `let v = <-ch`). Helpers like `makeChannel<T>(size)` / `makeIntChannel(size)` live in Go.

## CLI

```
gots translate examples/hello/main.gots -o out.go
gots run examples/hello/main.gots
gots lsp   # start language server on stdio
```

`translate` and `run` stop if diagnostics are present. `run` writes a temporary `main.go` then invokes `go run` on it.
You need the Go toolchain available on your `PATH` for `run` and for compiling the generated code.

## Language Server

Start with `gots lsp`. Capabilities:

- full document sync, parses on open/change/save and publishes diagnostics
- keyword/type completions
- `workspace/executeCommand` `gots.translate` to write the corresponding `.go` file for the given document URI

## Example

`examples/hello/main.gots`:

```ts
package main

import fmt from "fmt"

function main(): void {
	let name: string = "gots"
	fmt.Println("hello from", name)
}
```

More samples:

- `examples/generics/main.gots`: generic structs/functions, float support.
- `examples/interfaces/main.gots`: interface definition/usage plus `throw`; paired with `examples/interfaces/native.go` to supply a concrete `Greeter`.
- `examples/concurrency/main.gots`: goroutines + channels. Uses Go helpers (`examples/concurrency/native.go`) such as `makeIntChannel(size)` (built on a generic `makeChannel[T]`) to create buffered channels.
- `examples/controlflow/main.gots`: switch/type-switch, range loops over slice/map provided by Go helpers, select, lambdas, and `try/catch` sugar for `(T, error)` calls.

Some samples rely on small native Go helpers to bridge features like channel creation or concrete interface implementations; see the accompanying `native.go` files.

## Full syntax example

```ts
package main

import fmt from "fmt"

type Result<T> = { value: T, err: error }
type Worker = interface { Work(): Result<number> }

function wrap<T>(v: T): Result<T> {
	return { value: v, err: nil }
}

function worker(name: string): Result<number> {
	if name == "" {
		throw "missing name"
	}
	return wrap<number>(42)
}

function producer(out: chan<number>, scale: float): void {
	let r = worker("bob")
	if r.err != nil {
		throw r.err
	}
	send out => r.value + int(scale)
}

function main(): void {
let ch: chan<number> = makeIntChannel(4) // provided by a Go helper
spawn producer(ch, 2.5)
let v = await ch
fmt.Println("got", v)
}
```
