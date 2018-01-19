# TODO: Update this to support multiple executables

build-dir := ../build
work-dir := $(build-dir)/work
install-dir := $(build-dir)/install
bin-dir := $(install-dir)/bin
default-args := 

CPPARGS := -Wall -Werror -Wextra -std=c++11 -ggdb3 -fno-exceptions -O0

.PHONY: check clean build-dirs valgrind run cpplint strace test

all: build-dirs $(bin-dir)/mult-array

# FIXME: this is kind of ugly ...
build-dirs:
	mkdir -p $(bin-dir) $(work-dir)

$(work-dir)/mult-array.o: src/mult-array.cpp
	g++ $(CPPARGS) -c -o $@ $^

$(bin-dir)/mult-array: $(work-dir)/mult-array.o
	g++ -o $@ $^

valgrind: $(bin-dir)/mult-array
	valgrind --leak-check=full --error-exitcode=1 $^ $(default-args)

strace: $(bin-dir)/mult-array
	strace $^ $(default-args) > /tmp/out

run: $(bin-dir)/mult-array
	$^ $(default-args)

cpplint: $(work-dir)/cpplint-report

$(work-dir)/cpplint-report: src/mult-array.cpp
	cpplint $^ 2>&1 | tee $(work-dir)/cpplint-report

test: $(bin-dir)/mult-array
	test/test.sh $^

check: cpplint valgrind test

clean:
	find $(build-dir) -not -name .gitignore -and -type f -delete