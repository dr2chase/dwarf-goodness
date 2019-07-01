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

type Function struct {
	Name       string
	Entry, End uint64 // same as DW_AT_lowpc and DW_AT_highpc
	offset     dwarf.Offset
	cu         uintptr
}

type pcln struct {
	pc       uint64
	fileName string
	varName  string
	funcName string
	line     int
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
	total   int
}

type summaryRecord struct {
	summaryKey
	summaryValue
}

type i2lkey struct {
	file     string
	variable string
}

type sortedLineMap struct {
	m map[i2lkey][]*pcln
}

var fileCache = map[string]*sortedLineMap{}

func getFile(bi *proc.BinaryInfo, path string, skipVar func(name string) bool) *sortedLineMap {
	if r, cached := fileCache[path]; cached {
		return r
	}
	line2inputs := line_inputs.ReadFile(path)

	sortedLines := line_inputs.SortDomain(line2inputs)

	input2Lines := make(map[i2lkey][]*pcln)

	line2pcs := bi.AllPCsForFileLines(path, sortedLines)

	for _, line := range sortedLines {
		inputs := line2inputs[line]
		k := i2lkey{file: path}
		for _, pc := range line2pcs[line] {
			for _, input := range inputs.Inputs() {
				if skipVar(input) {
					continue
				}
				k.variable = input
				pclns := input2Lines[k]
				pclns = append(pclns, &pcln{fileName: path, line: line, varName: input, pc: pc})
				input2Lines[k] = pclns
			}
		}
	}
	// Sort the input locations by PC to allow faster checking later.
	for _, v := range input2Lines {
		sort.Slice(v, func(i, j int) bool { // less
			return v[i].pc < v[j].pc
		})
	}
	r := &sortedLineMap{m: input2Lines}
	fileCache[path] = r
	return r
}

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

type key struct {
	compare      func(x, y *summaryRecord) int
	appendString func(x *summaryRecord, s []string) []string
	copyField    func(from, to *summaryKey)
	name         string
	active       bool
}

func (x *key) IsBoolFlag() bool {
	return true
}

func (x *key) String() string {
	if x.active {
		return x.name
	}
	return "false"
}

var keys []*key

func (x *key) Set(s string) error {
	keys = append(keys, x)
	return nil
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
func appendStringNop(x *summaryRecord, s []string) []string {
	return s
}
func copyFieldNop(from, to *summaryKey) {
}

func main() {
	debug := false

	var files []*regexp.Regexp
	var funcs []*regexp.Regexp
	var vars []*regexp.Regexp

	var byFile = key{
		name: "by file",
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
	var byFunc = key{
		name: "by func",
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
	var byVar = key{
		name: "by var",
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
	var byLine = key{
		name: "by line",
		compare: func(x, y *summaryRecord) int {
			return compare(x.byLine, y.byLine)
		},
		appendString: func(x *summaryRecord, s []string) []string {
			return s
		},
		copyField: func(from, to *summaryKey) {
		},
	}

	var byPresent = key{
		name: "by present",
		compare: func(x, y *summaryRecord) int {
			return compare(x.present, y.present)
		},
		appendString: appendStringNop,
		copyField:    copyFieldNop,
	}

	var byTotal = key{
		name: "by total",
		compare: func(x, y *summaryRecord) int {
			return compare(x.total, y.total)
		},
		appendString: appendStringNop,
		copyField:    copyFieldNop,
	}

	// Quality is present/total.
	var byQuality = key{
		name: "by quality",
		compare: func(x, y *summaryRecord) int {
			if x.total == 0 && y.total != 0 {
				return -1
			}
			if y.total == 0 && x.total != 0 {
				return 1
			}
			if x.total == 0 && y.total == 0 {
				return 0
			}
			qx := float64(x.present) / float64(x.total)
			qy := float64(y.present) / float64(y.total)
			if qx < qy {
				return -1
			}
			if qx > qy {
				return 1
			}
			return 0
		},
		appendString: appendStringNop,
		copyField:    copyFieldNop,
	}

	// Badness is total - present.
	var byBadness = key{
		name: "by badness",
		compare: func(x, y *summaryRecord) int {
			qx := x.total - x.present
			qy := y.total - y.present
			if qx < qy {
				return -1
			}
			if qx > qy {
				return 1
			}
			return 0
		},
		appendString: appendStringNop,
		copyField:    copyFieldNop,
	}

	flag.BoolVar(&debug, "debug", debug, "Emit (this program's) debugging output to stderr")
	flag.Var(&byFile, "byfile", "Split summary and sort by file")
	flag.Var(&byFunc, "byfunc", "Split summary and sort by functions")
	flag.Var(&byVar, "byvar", "Split summary and sort by variables")
	flag.Var(&byLine, "byline", "Split summary and sort by lines")
	flag.Var(&byPresent, "bypresent", "Sort by how many inputs are present")
	flag.Var(&byTotal, "bytotal", "Sort by total number of inputs")
	flag.Var(&byQuality, "byquality", "Sort by present/total")
	flag.Var(&byBadness, "bybadness", "Sort by total-present")

	flag.Var((*regexps)(&files), "file", "Limit scan to files (pathnames) matching regexp (may be repeated)")
	flag.Var((*regexps)(&funcs), "func", "Limit scan to functions matching regexp (may be repeated)")
	flag.Var((*regexps)(&vars), "var", "Limit scan to variables matching regexp (may be repeated)")

	flag.Usage = func() {
		fmt.Fprintf(os.Stderr, "Usage of %s:\n", os.Args[0])
		flag.PrintDefaults()
		fmt.Fprintf(os.Stderr, `
For a binary, %s reports the number of local-variable inputs to lines
that are present at that line's PC(s) in the debugging information,
and for each summary line prints #inputs, #present, #present/#inputs.
With no -byX options, it prints a single summary line, otherwise the
output is split and sorted as specified (for sorting, parameter -byX
parameter order matters).

Example:
 go tool -n compile | xargs ./dwarf-goodness \
    -file=ssa/compile/internal/ssa \
    -bybadness -bytotal -byfile -byvar

This will report the files and variables in the ssa portion
of the Go compiler, sorted by increasing badness (then total,
then file, then variable).

(There are still bugs in this reporting.)
`, os.Args[0])
	}

	flag.Parse()

	doit(flag.Arg(0), debug, keys, files, funcs, vars)
}

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

// doit analyzes binaryName for "DWARF 	quality".
// analysis is limited to the files/funcs/varts matching at least one of their respective slices of regular expressions,
// and the report is split and sorted (or for results, just sorted) according to the ordered keys.
// If debug is true, progress reports and perhaps debugging information appear on standard error.
func doit(binaryName string, debug bool, keys []*key, files, funcs, vars []*regexp.Regexp) {
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
		input2Lines := getFile(bi, file, skipVar)
		if input2Lines.m == nil {
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

			pclns := input2Lines.m[i2lkey{file: file, variable: name}]
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

		for vname, pclns := range slmap.m {
			for _, pcln := range pclns {
				key0 := &summaryKey{byVar: vname.variable, byFile: fname, byFunc: pcln.funcName, byLine: pcln.line}
				key := summaryKey{}
				for _, k := range keys {
					k.copyField(key0, &key)
				}
				value := summary[key]
				value.total++
				total.total++
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

		for _, k := range keys {
			c := k.compare(oi, oj)
			if c != 0 {
				return c < 0
			}
		}
		return false // equal
	})

	csvw := csv.NewWriter(os.Stdout)

	describe := func(s []string, record summaryRecord) {
		s = append(s, fmt.Sprintf("%d", record.total))
		s = append(s, fmt.Sprintf("%d", record.present))
		s = append(s, fmt.Sprintf("%f", float64(record.present)/float64(record.total)))
		csvw.Write(s)
	}

	n := 0
	for _, record := range ordered {
		var s []string
		for _, k := range keys {
			s = k.appendString(&record, s)
		}
		n++
		describe(s, record)
	}
	if n > 1 {
		describe([]string{"total"}, total)
	}
	csvw.Flush()

}
