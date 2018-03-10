build-dir := ../build
work-dir := $(build-dir)/work
install-dir := $(build-dir)/install
bin-dir := $(install-dir)/bin
lib-dir := $(install-dir)/lib

objects = $(wildcard src/lib/*.cpp)
sources := $(wildcard src/*.cpp)
source-files := $(notdir $(sources))
target-files := $(source-files:.cpp=)
bin-targets := $(addprefix $(bin-dir)/,$(target-files))
valgrind-targets := $(addprefix valgrind-,$(target-files))
run-targets := $(addprefix run-,$(target-files))
strace-targets := $(addprefix strace-,$(target-files))

CPPARGS := -Wall -Werror -Wextra -std=c++11 -ggdb3 -fno-exceptions -O0

.PHONY: check clean cpplint valgrind-targets run-targets strace-targets

all: $(bin-targets)

$(work-dir) $(bin-dir) $(lib-dir):
	mkdir -p $@

$(valgrind-targets): valgrind-%: $(bin-dir)/%
	valgrind --leak-check=full --error-exitcode=1 $^

$(strace-targets): strace-%: $(bin-dir)/%
	strace $^ > /tmp/out

$(run-targets): run-%: $(bin-dir)/%
	$^

cpplint: $(work-dir)/cpplint-report

$(work-dir)/cpplint-report: $(sources) | $(work-dir)
	cpplint $^ 2>&1 | tee $@

check: cpplint $(valgrind-targets)

clean:
	rm -r $(build-dir)

$(work-dir)/%.o: src/%.cpp | $(work-dir)
	g++ $(CPPARGS) -c -o $@ $(filter-out %.hpp, $^)

$(work-dir)/%.o: src/lib/%.cpp src/lib/%.hpp | $(work-dir)
	g++ $(CPPARGS) -c -o $@ $(filter-out %.hpp, $^)

## We assume each little test is a single source file
$(bin-dir)/%: $(work-dir)/%.o | $(bin-dir)
	g++ $(CPPARGS) -o $@ $^

## Although we also support some rudimentary way of sharing objects
## by listing explicit dependencies here
$(work-dir)/binary-tree-from-array-tests.o: src/lib/binary-tree.hpp
$(work-dir)/binary-tree-from-heap-tests.o: src/lib/binary-tree.hpp
$(work-dir)/binary-tree-from-order.o: src/lib/binary-tree.hpp
