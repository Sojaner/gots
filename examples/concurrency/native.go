package main

// makeNumberChan is implemented in Go so the .gots code can stay simple.
// Buffered to avoid deadlocks in the tiny demo.
func makeNumberChan() chan int {
	return make(chan int, 4)
}
