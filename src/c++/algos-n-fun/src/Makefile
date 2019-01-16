build-dir := ../build
work-dir := $(build-dir)/work
install-dir := $(build-dir)/install
bin-dir := $(install-dir)/bin
lib-dir := $(install-dir)/lib

objects = $(wildcard src/lib/*.cpp)
sources := $(wildcard src/*.cpp)
source-files := $(notdir $(sources))
target-files := $(source-files:.cpp=)
cpplint-targets := $(addprefix cpplint-,$(target-files))
bin-targets := $(addprefix $(bin-dir)/,$(target-files))
valgrind-targets := $(addprefix valgrind-,$(target-files))
run-targets := $(addprefix run-,$(target-files))
strace-targets := $(addprefix strace-,$(target-files))

CPPARGS := -Wall -Werror -Wextra -std=c++11 -ggdb3 -O3 -fpermissive
LDFLAGS :=

.PHONY: check clean cpplint valgrind-targets run-targets strace-targets cpplint-targets

all: $(bin-targets)

$(work-dir) $(bin-dir) $(lib-dir):
	mkdir -p $@

$(valgrind-targets): valgrind-%: $(bin-dir)/%
	valgrind --leak-check=full --error-exitcode=1 $^

$(strace-targets): strace-%: $(bin-dir)/%
	strace $^ > /tmp/out

$(run-targets): run-%: $(bin-dir)/%
	$^

cpplint: $(cpplint-targets)

$(cpplint-targets): cpplint-%: $(work-dir)/%.cpplint

$(work-dir)/%.cpplint: src/%.cpp | $(work-dir)
	@echo cpplint $^
	@cpplint --quiet $^ 2>&1 | tee $@

check: cpplint $(valgrind-targets)

clean:
	rm -r $(build-dir)

$(work-dir)/%.o: src/%.cpp | $(work-dir)
	g++ $(CPPARGS) -c -o $@ $(filter-out %.hpp, $^)

$(work-dir)/%.o: src/lib/%.cpp src/lib/%.hpp | $(work-dir)
	g++ $(CPPARGS) -c -o $@ $(filter-out %.hpp, $^)

## We assume each little test is a single source file
$(bin-dir)/%: $(work-dir)/%.o | $(bin-dir)
	g++ $(CPPARGS) -o $@ $^ $(LDFLAGS)

## Although we also support some rudimentary way of sharing objects
## by listing explicit dependencies here
##
## This is also used for any non-common linking ineeds
$(work-dir)/binary-tree-from-array-tests.o: src/lib/binary-tree.hpp
$(work-dir)/binary-tree-from-heap-tests.o: src/lib/binary-tree.hpp
$(work-dir)/binary-tree-from-order.o: src/lib/binary-tree.hpp
$(bin-dir)/digraph-tests: LDFLAGS := $(LDFLAGS) -lgraph
$(bin-dir)/graph-tests: LDFLAGS := $(LDFLAGS) -lgraph
$(bin-dir)/adjoin: LDFLAGS := $(LDFLAGS) -lgraph
$(bin-dir)/as-easy-as-abc: LDFLAGS := $(LDFLAGS) -lgraph
$(bin-dir)/union-find-test: $(work-dir)/union-find.o
$(bin-dir)/10-types-of-people: $(work-dir)/union-find.o
$(bin-dir)/rabin-karp-test: src/lib/rabin-karp.hpp
$(bin-dir)/text-editor: src/lib/rabin-karp.hpp
$(bin-dir)/dutch-flag-sort: src/lib/sort.hpp
$(bin-dir)/quicksort-test: src/lib/sort.hpp
$(bin-dir)/selection-test: src/lib/sort.hpp
$(bin-dir)/merge-contacts: LDFLAGS := $(LDFLAGS) -lgraph
$(bin-dir)/advent-2018-7-1: LDFLAGS := $(LDFLAGS) -lgraph
$(bin-dir)/advent-2018-7-2: LDFLAGS := $(LDFLAGS) -lgraph
$(bin-dir)/advent-2018-13: src/lib/circular-array.hpp src/lib/coord.hpp
$(bin-dir)/rapidcheck-test: LDFLAGS := $(LDFLAGS) -lrapidcheck
