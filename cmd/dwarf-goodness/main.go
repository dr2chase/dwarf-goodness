// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

import (
	"debug/dwarf"
	"encoding/csv"
	"flag"
	"fmt"
	"os"
	"regexp"
	"runtime"
	"sort"
	"strings"
	"unsafe"

	"github.com/dr2chase/dwarf-goodness/line_inputs"
	"github.com/go-delve/delve/pkg/proc"
)

func must(err error) {
	if err != nil {
		panic(err)
	}
}

// unsafe-punned to get access to private data from delve.
type Function struct {
	Name       string
	Entry, End uint64 // same as DW_AT_lowpc and DW_AT_highpc
	offset     dwarf.Offset
	cu         uintptr
}

type pcln struct {
	pc       uint64
	line     int
	fileName string
	varName  string
	funcName string
	present  bool
}

type summaryKey struct {
	byFile string
	byFunc string
	byLine int
	byVar  string
}

type summaryValue struct {
	present int
	inputs  int
}

type summaryRecord struct {
	summaryKey
	summaryValue
}

type fileAndVar struct {
	file     string
	variable string
}

type sortedLineMap map[fileAndVar][]*pcln

var fileCache = make(map[string]sortedLineMap)

func getFile(bi *proc.BinaryInfo, path string, skipVar func(name string) bool, debug bool) sortedLineMap {
	if r, cached := fileCache[path]; cached {
		return r
	}
	line2inputs := line_inputs.ReadFile(path)
	sortedLines := line_inputs.SortDomain(line2inputs)
	line2pcs := bi.AllPCsForFileLines(path, sortedLines)

	input2Lines := make(map[fileAndVar][]*pcln)
	for _, line := range sortedLines {
		inputs := line2inputs[line]
		k := fileAndVar{file: path}
		for _, pc := range line2pcs[line] {
			for _, input := range inputs.Inputs() {
				if skipVar(input) {
					continue
				}
				k.variable = input
				pclns := input2Lines[k]
				x := pcln{fileName: path, line: line, varName: input, pc: pc}
				pclns = append(pclns, &x)
				input2Lines[k] = pclns
				if debug {
					_, _ = fmt.Fprintf(os.Stderr, "PCLN file=%s, var=%s, line=%d, pc=0x%x\n", x.fileName, x.varName, x.line, x.pc)
				}
			}
		}
	}
	// Sort the input locations by PC to allow faster checking later.
	for _, v := range input2Lines {
		sort.Slice(v, func(i, j int) bool { // less
			return v[i].pc < v[j].pc
		})
	}
	fileCache[path] = input2Lines
	return input2Lines
}

// For specifiying file, function, variables to report
type regexps []*regexp.Regexp

func (res *regexps) String() string {
	r := "["
	sep := ""
	for _, s := range *res {
		r = r + sep + s.String()
		sep = ", "
	}
	r += "]"
	return r
}
func (res *regexps) Set(s string) error {
	r, err := regexp.Compile(s)
	if err != nil {
		return err
	}
	*res = append(*res, r)
	return nil
}

func (res *regexps) IsBoolFlag() bool {
	return false
}

// For specifying split and ordering of fields in the output
type key struct {
	name         string
	compare      func(x, y *summaryRecord) int
	appendString func(x *summaryRecord, s []string) []string
	copyField    func(from, to *summaryKey)
}

func (x *key) String() string {
	return x.name
}

func compare(x, y int) int {
	if x < y {
		return -1
	}
	if x > y {
		return 1
	}
	return 0
}

func copyFieldNop(_, _ *summaryKey) {
}

var byFile = &key{
	name: "file",
	compare: func(x, y *summaryRecord) int {
		return strings.Compare(x.byFile, y.byFile)
	},
	appendString: func(x *summaryRecord, s []string) []string {
		return append(s, x.byFile)
	},
	copyField: func(from, to *summaryKey) {
		to.byFile = from.byFile
	},
}
var byFunc = &key{
	name: "func",
	compare: func(x, y *summaryRecord) int {
		return strings.Compare(x.byFunc, y.byFunc)
	},
	appendString: func(x *summaryRecord, s []string) []string {
		return append(s, x.byFunc)
	},
	copyField: func(from, to *summaryKey) {
		to.byFunc = from.byFunc
	},
}
var byVar = &key{
	name: "var",
	compare: func(x, y *summaryRecord) int {
		return strings.Compare(x.byVar, y.byVar)
	},
	appendString: func(x *summaryRecord, s []string) []string {
		return append(s, x.byVar)
	},
	copyField: func(from, to *summaryKey) {
		to.byVar = from.byVar
	},
}
var byLine = &key{
	name: "line",
	compare: func(x, y *summaryRecord) int {
		return compare(x.byLine, y.byLine)
	},
	appendString: func(x *summaryRecord, s []string) []string {
		return append(s, fmt.Sprintf("%d", x.byLine))
	},
	copyField: func(from, to *summaryKey) {
		to.byLine = from.byLine
	},
}

var byPresent = &key{
	name: "present",
	compare: func(x, y *summaryRecord) int {
		return compare(x.present, y.present)
	},
	appendString: func(x *summaryRecord, s []string) []string {
		return append(s, fmt.Sprintf("%d", x.present))
	},
	copyField: copyFieldNop,
}

var byInputs = &key{
	name: "inputs",
	compare: func(x, y *summaryRecord) int {
		return compare(x.inputs, y.inputs)
	},
	appendString: func(x *summaryRecord, s []string) []string {
		return append(s, fmt.Sprintf("%d", x.inputs))
	},
	copyField: copyFieldNop,
}

// Quality is present/inputs.
var byQuality = &key{
	name: "quality",
	compare: func(x, y *summaryRecord) int {
		if x.inputs == 0 && y.inputs != 0 {
			return -1
		}
		if y.inputs == 0 && x.inputs != 0 {
			return 1
		}
		if x.inputs == 0 && y.inputs == 0 {
			return 0
		}
		qx := float64(x.present) / float64(x.inputs)
		qy := float64(y.present) / float64(y.inputs)
		if qx < qy {
			return -1
		}
		if qx > qy {
			return 1
		}
		return 0
	},
	appendString: func(x *summaryRecord, s []string) []string {
		return append(s, fmt.Sprintf("%.2f", float64(x.present)/float64(x.inputs)))
	},
	copyField: copyFieldNop,
}

// Badness is inputs - present.
var byBadness = &key{
	name: "badness",
	compare: func(x, y *summaryRecord) int {
		qx := x.inputs - x.present
		qy := y.inputs - y.present
		if qx < qy {
			return -1
		}
		if qx > qy {
			return 1
		}
		return 0
	},
	appendString: func(x *summaryRecord, s []string) []string {
		return append(s, fmt.Sprintf("%d", x.inputs-x.present))
	},
	copyField: copyFieldNop,
}

var orderMap = map[string]*key{
	"badness": byBadness,
	"quality": byQuality,
	"inputs":  byInputs,
	"present": byPresent,
	"line":    byLine,
	"var":     byVar,
	"func":    byFunc,
	"file":    byFile,
}

type keySlice struct {
	expected string
	keys     []*key
}

func (ks *keySlice) String() string {
	r := "["
	sep := ""
	for _, s := range ks.keys {
		r = r + sep + s.String()
		sep = ", "
	}
	r += "]"
	return r

}
func (ks *keySlice) Set(s string) error {
	ks.keys = ks.keys[:0]
	ss := strings.Split(s, ",")
	for _, s := range ss {
		k, ok := orderMap[s]
		if !ok || !strings.Contains(ks.expected, s) {
			return fmt.Errorf("Expected some of {%s} but saw %s instead", ks.expected, s)
		}
		// Prevent duplicates (these lists are short, no need for cleverness)
		for _, kk := range ks.keys {
			if k == kk {
				k = nil
				break
			}
		}
		if k != nil {
			ks.keys = append(ks.keys, k)
		}
	}
	return nil
}

func (ks *keySlice) IsBoolFlag() bool {
	return false
}

func main() {
	debug := false

	var files []*regexp.Regexp
	var funcs []*regexp.Regexp
	var vars []*regexp.Regexp

	orders := &keySlice{expected: "file,func,line,var,inputs,present,quality,badness", keys: []*key{byFile, byVar}}
	splits := &keySlice{expected: "file,func,line,var"}

	flag.BoolVar(&debug, "debug", debug, "Emit (this program's) debugging output to stderr")

	flag.Var(orders, "order", "Keys for sorting output")
	flag.Var(splits, "split", "Separate output by these categories")
	flag.Var((*regexps)(&files), "file", "Limit scan to files (pathnames) matching regexp (may be repeated)")
	flag.Var((*regexps)(&funcs), "func", "Limit scan to functions matching regexp (may be repeated)")
	flag.Var((*regexps)(&vars), "var", "Limit scan to variables matching regexp (may be repeated)")

	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage of %s:\n", os.Args[0])
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, `
For a binary, %s reports the number of local-variable inputs to lines
that are present at that line's PC(s) in the debugging information,
and for each summary line prints 
   #inputs, #present, #present/#inputs,  #inputs-#present
The ratio is "quality", the difference is "badness"

With no -split option, it prints a single summary line, otherwise the
output is split and sorted as specified.

Example:
    ./dwarf-goodness \
    -file strings -file math \
    -order=badness,inputs,file,var \
    -split=file,var \
    $( which go $)

This will report, for the Go command binary, the files whose path names
contain "strings" or "math", and the variables in those files, sorted
by increasing badness (then by inputs, then by file, then by variable).

`, os.Args[0])
	}

	flag.Parse()
	if len(flag.Args()) == 0 {

		flag.Usage()
		fmt.Fprintf(os.Stderr, "\nNo input file was provided.\n")
		return
	}

	doit(flag.Arg(0), debug, splits.keys, orders.keys, files, funcs, vars)
}

// skip returns whether a name should be skipped, based on its membership in
// any regular expression in res.
func skip(name string, res []*regexp.Regexp) bool {
	if len(res) == 0 {
		return false
	}
	for _, re := range res {
		if re.MatchString(name) {
			return false
		}
	}
	return true
}

// doit analyzes binaryName for "DWARF quality" and prints a result in CSV to standard output.
// Analysis is limited to the files/funcs/vars matching at least one of their respective slices of regular expressions,
// and the report is split and sorted (or for results, just sorted) according to split and order.
// If debug is true, progress reports and perhaps debugging information appear on standard error.
func doit(binaryName string, debug bool, split []*key, order []*key, files, funcs, vars []*regexp.Regexp) {
	var file0 string

	bi := proc.NewBinaryInfo(runtime.GOOS, runtime.GOARCH)
	bi.LoadBinaryInfo(binaryName, 0, []string{})

	rdr := bi.Images[0].DwarfReader()
	rdr.Seek(0)

	skipVar := func(name string) bool {
		return skip(name, vars)
	}

	for _, fn := range bi.Functions {
		if fn.Entry == 0 {
			continue
		}
		if skip(fn.Name, funcs) {
			continue
		}

		file, _, _ := bi.PCToLine(fn.Entry)

		if file == "" || file == "<autogenerated>" {
			continue
		}

		if skip(file, files) {
			continue
		}

		if file != file0 && debug {
			_, _ = fmt.Fprintf(os.Stderr, "\n")
		}
		input2Lines := getFile(bi, file, skipVar, debug)
		if input2Lines == nil {
			_, _ = fmt.Fprintf(os.Stderr, "Couldn't read source file %s\n", file)
			continue
		}

		if file != file0 {
			if debug {
				_, _ = fmt.Fprintf(os.Stderr, "File %s: ", file)
			}
			file0 = file
		}

		if debug {
			_, _ = fmt.Fprintf(os.Stderr, ".")
		}

		_fn := (*Function)(unsafe.Pointer(&fn))
		rdr.Seek(_fn.offset)
		rdr.Next() // why?

		seen := make(map[string]bool)

		// Taken from optargorder
		for {
			e, err := rdr.Next()
			if err != nil {
				must(err)
			}
			if e == nil || e.Tag == 0 {
				break
			}
			rdr.SkipChildren()
			if e.Tag != dwarf.TagFormalParameter && e.Tag != dwarf.TagVariable {
				continue
			}

			if e.Val(dwarf.AttrName) == nil {
				continue
			}
			name := e.Val(dwarf.AttrName).(string)

			if seen[name] {
				continue
			}
			seen[name] = true

			pclns := input2Lines[fileAndVar{file: file, variable: name}]
			if len(pclns) == 0 {
				continue
			}

			pairs, err := bi.LocationCovers(e, dwarf.AttrLocation)
			if len(pairs) == 0 {
				continue
			}

			sort.Slice(pairs, func(i, j int) bool { // less
				return pairs[i][0] < pairs[j][0]
			})

			i, j := 0, 0
			pcln := pclns[i]
			p := pairs[j]

			for {
				if fn.Entry <= pcln.pc && pcln.pc <= fn.End {
					pcln.funcName = fn.Name
				}
				if pcln.pc > p[1] {
					j++
					if j >= len(pairs) {
						break
					}
					p = pairs[j]
					continue
				}
				if p[0] <= pcln.pc && pcln.pc <= p[1] {
					pcln.present = true
				}
				i++
				if i >= len(pclns) {
					break
				}
				pcln = pclns[i]
			}
		}
	}

	if debug {
		_, _ = fmt.Fprintf(os.Stderr, "\n")
		_, _ = fmt.Fprintf(os.Stderr, "\n")
	}

	summary := make(map[summaryKey]summaryValue)
	// Summarize the contents of the file Cache
	var total summaryRecord
	for fname, slmap := range fileCache {
		for vname, pclns := range slmap {
			for _, pcln := range pclns {
				key0 := &summaryKey{byVar: vname.variable, byFile: fname, byFunc: pcln.funcName, byLine: pcln.line}
				key := summaryKey{}
				for _, k := range split {
					k.copyField(key0, &key)
				}
				value := summary[key]
				value.inputs++
				total.inputs++
				if pcln.present {
					value.present++
					total.present++
				}
				summary[key] = value
			}
		}
	}

	// Now sort into a canonical order
	var ordered []summaryRecord
	for k, v := range summary {
		ordered = append(ordered, summaryRecord{k, v})
	}

	sort.Slice(ordered, func(i, j int) bool { // Less
		oi := &ordered[i]
		oj := &ordered[j]

		for _, k := range order {
			c := k.compare(oi, oj)
			if c != 0 {
				return c < 0
			}
		}
		return false // equal
	})

	csvw := csv.NewWriter(os.Stdout)
	fields := append(split, []*key{byInputs, byPresent, byQuality, byBadness}...)
	n := 0
	for _, record := range ordered {
		var s []string
		for _, k := range fields {
			s = k.appendString(&record, s)
		}
		n++
		must(csvw.Write(s))
	}
	csvw.Flush()
	if len(ordered) > 1 {
		commas := ",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,"
		commas = commas[0:len(split)]
		fmt.Printf("Total%s%d,%d,%.2f,%d\n", commas, total.inputs, total.present, float64(total.present)/float64(total.inputs), total.inputs-total.present)
	}

}
