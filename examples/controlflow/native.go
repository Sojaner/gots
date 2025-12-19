package main

func numbers() []int {
	return []int{1, 2, 3}
}

func lookup() map[string]int {
	return map[string]int{
		"one": 1,
		"two": 2,
	}
}

func numbersChannel() chan int {
	ch := make(chan int, 1)
	return ch
}
