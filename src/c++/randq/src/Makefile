build-dir := ../build
work-dir := $(build-dir)/work
install-dir := $(build-dir)/install
bin-dir := $(install-dir)/bin
default-args :=

debug-flags := -ggdb3 -O0
CPPARGS := -Wall -Wextra -Werror -std=c++11 -fno-exceptions

ifeq ($(DEBUG),true)
	CPPARGS += $(debug-flags)
endif

.PHONY: check clean build-dirs valgrind run cpplint strace

all: build-dirs $(bin-dir)/randq

# FIXME: this is kind of ugly ...
build-dirs:
	mkdir -p $(bin-dir) $(work-dir)

$(work-dir)/randq.o: src/randq.cpp
	g++ $(CPPARGS) -c -o $@ $^

$(bin-dir)/randq: $(work-dir)/randq.o
	g++ -o $@ $^

valgrind: $(bin-dir)/randq
	valgrind --leak-check=full --error-exitcode=1 $^ $(default-args)

strace: $(bin-dir)/randq
	strace $^ $(default-args) > /tmp/out

run: $(bin-dir)/randq
	$^ $(default-args)

cpplint: $(work-dir)/cpplint-report

$(work-dir)/cpplint-report: src/randq.cpp
	cpplint $^ 2>&1 | tee $(work-dir)/cpplint-report

check: cpplint valgrind test

clean:
	find $(build-dir) -not -name .gitignore -and -type f -delete