# TODO: Update this to support multiple executables

build-dir := ../build
work-dir := $(build-dir)/work
install-dir := $(build-dir)/install
bin-dir := $(install-dir)/bin

sources := $(wildcard src/*.cpp)
source-files := $(notdir $(sources))
target-files := $(source-files:.cpp=)
targets := $(addprefix $(bin-dir)/,$(target-files))

CPPARGS := -Wall -Werror -Wextra -std=c++11 -ggdb3 -fno-exceptions -O0

.PHONY: check clean build-dirs valgrind run cpplint strace test

all: build-dirs $(targets)

# FIXME: this is kind of ugly ...
build-dirs:
	mkdir -p $(bin-dir) $(work-dir)

# valgrind: $(bin-dir)/reverse-string
# 	valgrind --leak-check=full --error-exitcode=1 $^ $(default-args)

# strace: $(bin-dir)/reverse-string
# 	strace $^ $(default-args) > /tmp/out

# run: $(bin-dir)/reverse-string
# 	$^ $(default-args)

# cpplint: $(work-dir)/cpplint-report

# $(work-dir)/cpplint-report: src/reverse-string.cpp
# 	cpplint $^ 2>&1 | tee $(work-dir)/cpplint-report

# test: $(bin-dir)/reverse-string
# 	test/test.sh $^

# check: cpplint valgrind test

clean:
	find $(build-dir) -not -name .gitignore -and -type f -delete

$(work-dir)/%.o: src/%.cpp
	g++ $(CPPARGS) -c -o $@ $^

## We assume each little test is a single source file
$(bin-dir)/%: $(work-dir)/%.o
	g++ $(CPPARGS) -o $@ $^