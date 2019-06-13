package main

import (
	"fmt"
	"github.com/dr2chase/dwarf-goodness/line_inputs"
	"os"
	"sort"
)

func main() {
	linesToInputs := line_inputs.ReadFile(os.Args[1])

	var lines []int
	for line := range linesToInputs {
		lines = append(lines, line)
	}
	sort.Ints(lines)
	for _, line := range lines {
		fmt.Printf("%d:", line)
		comma := ""
		for _, s := range linesToInputs[line] {
			fmt.Printf("%s %s", comma, s)
			comma = ","
		}
		fmt.Println()
	}
}
