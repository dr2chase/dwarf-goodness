// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package line_inputs

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"math"
	"sort"
	"strings"
)

type StmtAndInputs struct {
	inputs      []string
	hasCall     bool
	hasTwoParts bool // for loops may have two parts if they have either test or increment.
	NumberOfPCs int  // record how many times a file/line combo appears in the generated code.
	file	    string
	line        int
}

func (s *StmtAndInputs) Inputs() []string {
	return s.inputs
}
func (s *StmtAndInputs) HasCall() bool {
	return s.hasCall
}
func (s *StmtAndInputs) HasTwoParts() bool {
	return s.hasTwoParts
}

func SortDomain(linesToInputs map[int]*StmtAndInputs) []int {
	var lines []int
	if linesToInputs == nil {
		return lines
	}
	for line := range linesToInputs {
		lines = append(lines, line)
	}
	sort.Ints(lines)
	return lines
}

func ReadFile(fileName string) (linesToInputs map[int]*StmtAndInputs) {
	if ! strings.HasSuffix(fileName, ".go") {
		return
	}
	fset := token.NewFileSet() // positions are relative to fset

	f, err := parser.ParseFile(fset, fileName, nil, parser.ParseComments)
	if err != nil {
		fmt.Println(err)
		return
	}
	var m map[string]*ast.File
	m = make(map[string]*ast.File)
	m[fileName] = f
	pack, err := ast.NewPackage(fset, m, nil, nil)
	if err != nil { // there are errors because of unresolved packages, ignore those
		// fmt.Println(err)
	}

	positionFor := func(n interface{}) token.Position {
		var p token.Position
		switch n := n.(type) {
		case *ast.ValueSpec:
			p = fset.Position(n.Pos())
		case *ast.Field:
			p = fset.Position(n.Pos())
		case *ast.AssignStmt:
			p = fset.Position(n.Pos())
		}
		return p
	}
	_ = positionFor

	linesToInputs = make(map[int]*StmtAndInputs)

	funcLow := math.MaxInt32
	funcHigh := 0

	var myFunc func(n ast.Node, isAssignmentContext bool) bool

	asValue := func(n ast.Node) bool {
		return myFunc(n, false)
	}
	asVariable := func(n ast.Node) bool {
		return myFunc(n, true)
	}

	myFunc = func(n ast.Node, isAssignmentContext bool) bool {
		if n == nil {
			return true
		}

	outerswitch:
		switch n := n.(type) {
		case *ast.Ident:
			if !isAssignmentContext {
				obj := n.Obj
				pos := fset.Position(n.Pos())
				if obj != nil && obj.Kind == ast.Var {
					declPos := positionFor(obj.Decl)
					//fmt.Printf("Ident, name=%s, file=%s, line=%d, decl line=%d, decl=%T\n",
					//	n.Name, pos.Filename, pos.Line, declPos.Line, obj.Decl)
					if funcLow <= declPos.Line && declPos.Line <= funcHigh {
						x := linesToInputs[pos.Line]
						if x == nil {
							x = &StmtAndInputs{file:fileName, line:pos.Line}
							linesToInputs[pos.Line] = x
						}
						for _, y := range x.inputs { // linear search, but lines are short(ish).
							if y == n.Name {
								break outerswitch
							}
						}
						x.inputs = append(x.inputs, n.Name)
					}
				}
			}

		case *ast.AssignStmt:
			for _, lhs := range n.Lhs {
				ast.Inspect(lhs, asVariable)
			}
			for _, rhs := range n.Rhs {
				ast.Inspect(rhs, asValue)
			}
			return false

		case *ast.RangeStmt:
			ast.Inspect(n.X, asValue)
			ast.Inspect(n.Body, asValue)
			return false

		case *ast.CallExpr:
			if fn, ok := n.Fun.(*ast.Ident); ok {
				// Exclude the not-real-calls
				switch fn.Name {
				case "len", "append", "make", "new", "panic":
					break outerswitch
				}
			}
			pos := fset.Position(n.Pos())
			x := linesToInputs[pos.Line]
			if x == nil {
				x = &StmtAndInputs{file:fileName, line:pos.Line}
				linesToInputs[pos.Line] = x
			}
			x.hasCall = true

		case *ast.ForStmt:
			pos := fset.Position(n.Pos())
			if n.Post != nil { // post stmt is a second statement, maybe
				x := linesToInputs[pos.Line]
				if x == nil {
					x = &StmtAndInputs{file:fileName, line:pos.Line}
					linesToInputs[pos.Line] = x
				}
				x.hasTwoParts = true
			}

		case *ast.ValueSpec:
			for _, rhs := range n.Values {
				ast.Inspect(rhs, asValue)
			}
			return false

		case *ast.IndexExpr:
			// Skip X if assignment context
			if isAssignmentContext {
				ast.Inspect(n.Index, asValue)
				return false

			}
		case *ast.SliceExpr:
			// Skip X if assignment context
			if isAssignmentContext {
				ast.Inspect(n.Low, asValue)
				ast.Inspect(n.High, asValue)
				ast.Inspect(n.Max, asValue)
				return false
			}
		case *ast.FuncDecl:
			// Skip parameters and returns.
			savedFL, savedFH := funcLow, funcHigh
			if n.Body != nil {
				funcLow, funcHigh = fset.Position(n.Pos()).Line, fset.Position(n.End()).Line
				ast.Inspect(n.Body, asValue)
				funcLow, funcHigh = savedFL, savedFH
			}
			return false
		case *ast.FuncLit:
			// Skip parameters and returns.
			ast.Inspect(n.Body, asValue)
			return false

		}
		return true
	}

	ast.Inspect(pack, asValue)
	return
}
