build-dir := ../build
work-dir := $(build-dir)/work
install-dir := $(build-dir)/install
bin-dir := $(install-dir)/bin

sources := $(wildcard src/*.cpp)
source-files := $(notdir $(sources))
target-files := $(source-files:.cpp=)
bin-targets := $(addprefix $(bin-dir)/,$(target-files))
valgrind-targets := $(addprefix valgrind-,$(target-files))
run-targets := $(addprefix run-,$(target-files))
strace-targets := $(addprefix strace-,$(target-files))

CPPARGS := -Wall -Werror -Wextra -std=c++11 -ggdb3 -fno-exceptions -O0

.PHONY: check clean build-dirs cpplint valgrind-targets run-targets strace-targets

all: build-dirs $(bin-targets)

# FIXME: this is kind of ugly ...
build-dirs:
	mkdir -p $(bin-dir) $(work-dir)

$(valgrind-targets): valgrind-%: $(bin-dir)/%
	valgrind --leak-check=full --error-exitcode=1 $^

$(strace-targets): strace-%: $(bin-dir)/%
	strace $^ > /tmp/out

$(run-targets): run-%: $(bin-dir)/%
	$^

cpplint: $(work-dir)/cpplint-report

$(work-dir)/cpplint-report: $(sources)
	cpplint $^ 2>&1 | tee $@

check: cpplint $(valgrind-targets)

clean:
	find $(build-dir) -not -name .gitignore -and -type f -delete

$(work-dir)/%.o: src/%.cpp
	g++ $(CPPARGS) -c -o $@ $^

## We assume each little test is a single source file
$(bin-dir)/%: $(work-dir)/%.o
	g++ $(CPPARGS) -o $@ $^