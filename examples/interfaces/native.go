package main

type greeter struct {
	name string
}

func (g greeter) Greet() string {
	return "hello " + g.name
}

func makeGreeter(name string) greeter {
	return greeter{name: name}
}
