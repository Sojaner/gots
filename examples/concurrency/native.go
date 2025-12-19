package main

// Generic helper so .gots can create channels without Go's make syntax.
func makeChannel[T any](size int) chan T {
	if size < 0 {
		size = 0
	}
	return make(chan T, size)
}

func makeIntChannel(size int) chan int       { return makeChannel[int](size) }
func makeFloatChannel(size int) chan float64 { return makeChannel[float64](size) }
func makeStringChannel(size int) chan string { return makeChannel[string](size) }
