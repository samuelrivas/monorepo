build-dir := ../build
work-dir := $(build-dir)/work
install-dir := $(build-dir)/install
bin-dir := $(install-dir)/bin
default-args := 10 < ./test/test-input

CPPARGS := -Wall -Wextra -Werror -std=c++11 -ggdb3 -fno-exceptions -O3
LDFLAGS := -lrnd

.PHONY: check clean build-dirs valgrind run cpplint strace test

all: build-dirs $(bin-dir)/reservoir

# FIXME: this is kind of ugly ...
build-dirs:
	mkdir -p $(bin-dir) $(work-dir)

$(work-dir)/reservoir.o: src/reservoir.cpp
	g++ $(CPPARGS) -c -o $@ $^

$(bin-dir)/reservoir: $(work-dir)/reservoir.o
	g++ -o $@ $^ $(LDFLAGS)

valgrind: $(bin-dir)/reservoir
	valgrind --leak-check=full --error-exitcode=1 $^ $(default-args)

strace: $(bin-dir)/reservoir
	strace $^ $(default-args) > /tmp/out

run: $(bin-dir)/reservoir
	$^ $(default-args)

cpplint: $(work-dir)/cpplint-report

$(work-dir)/cpplint-report: src/reservoir.cpp
	cpplint $^ 2>&1 | tee $(work-dir)/cpplint-report

test: $(bin-dir)/reservoir
	test/test.sh $^

check: cpplint valgrind test

clean:
	find $(build-dir) -not -name .gitignore -and -type f -delete
