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
)

var notfset *token.FileSet

func ReadFile(fileName string) (linesToInputs map[int][]string) {
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

	var myFunc func(n ast.Node, isAssignmentContext bool) bool

	asValue := func(n ast.Node) bool {
		return myFunc(n, false)
	}
	asVariable := func(n ast.Node) bool {
		return myFunc(n, true)
	}

	linesToInputs = make(map[int][]string)

	funcLow := math.MaxInt32
	funcHigh := 0

	myFunc = func(n ast.Node, isAssignmentContext bool) bool {
		if n == nil {
			return true
		}

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
						i := linesToInputs[pos.Line]
						linesToInputs[pos.Line] = append(i, n.Name)
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
			return false

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
			funcLow, funcHigh = fset.Position(n.Pos()).Line, fset.Position(n.End()).Line
			ast.Inspect(n.Body, asValue)
			funcLow, funcHigh = savedFL, savedFH
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
