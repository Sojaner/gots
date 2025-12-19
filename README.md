# gots

`gots` lets you write Go programs in a small TypeScript-flavoured syntax that only allows constructs with a clean Go mapping. It provides:

- a parser/translator that turns `.gots` files into Go source
- a CLI to translate or run via the Go toolchain
- a lightweight Language Server (LSP) for diagnostics and keyword completions

## Language subset

- Packages/imports: `package <name>`; `import fmt from "fmt"`.
- Types: structs (`type Person = { name: string, age: number }`) and interfaces (`type Reader<T> = interface { read(): T }`), with optional type parameters.
- Functions: `function greet<T>(name: string): string { ... }` (optional `export` to uppercase the Go name).
- Variables: `let`/`const` with optional type annotation. Inside functions, `let` becomes `:=` when possible; top-level uses `var`/`const`.
- Statements: expression statements, assignments, channel send (`send ch => v` or `ch <- v`), `go`/`spawn` to start goroutines, `throw`→`panic`, `if/else`, `for (init; cond; post)`, `return`.
- Expressions: identifiers, number/string/bool/nil literals, unary `!`/`-`/`<-`/`await` (channel receive), binary `+ - * / % == != < <= > >= && ||`, member access (`fmt.Println`), and calls.
- Types supported: `number`→`int`, `float`→`float64`, `string`, `boolean`→`bool`, `any`→`interface{}`, `error`, `void` (only as return), generics (`Box<string>`), channels (`chan<number>` requires a type argument), array types via `name[]`.

Anything outside this subset is flagged as a diagnostic by the parser/translator.

Note: channel creation currently assumes you obtain a channel from Go helpers or as a parameter; once you have one, you can send (`send ch => v` or `ch <- v`) and receive (`let v = await ch` or `let v = <-ch`).

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
- `examples/concurrency/main.gots`: goroutines + channels. Uses a Go helper (`examples/concurrency/native.go`) to provide `makeNumberChan()` via `make(chan int, 4)`.

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
	let ch: chan<number> = makeNumberChan() // provided by a Go helper
	spawn producer(ch, 2.5)
	let v = await ch
	fmt.Println("got", v)
}
```
