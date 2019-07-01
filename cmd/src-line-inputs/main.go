package main

import (
	"fmt"
	"github.com/dr2chase/dwarf-goodness/line_inputs"
	"os"
)

func main() {
	linesToInputs := line_inputs.ReadFile(os.Args[1])

	lines := line_inputs.SortDomain(linesToInputs)
	for _, line := range lines {
		info := linesToInputs[line]
		hasCall := ""
		twoPart := ""
		if info.HasCall() {
			hasCall = "(has call)"
		}
		if info.HasTwoParts() {
			twoPart = "(two parts)"
		}
		fmt.Printf("%d%s%s:", line, hasCall, twoPart)
		comma := ""
		for _, s := range info.Inputs() {
			fmt.Printf("%s %s", comma, s)
			comma = ","
		}
		fmt.Println()
	}
}
