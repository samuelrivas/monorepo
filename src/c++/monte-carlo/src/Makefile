build-dir := ../build
work-dir := $(build-dir)/work
install-dir := $(build-dir)/install
bin-dir := $(install-dir)/bin
default-args := 

CPPARGS := -Wall -Wextra -Werror -std=c++11 -ggdb3 -fno-exceptions -O3

.PHONY: check clean build-dirs valgrind run cpplint strace test

all: build-dirs $(bin-dir)/appliance-repair

# FIXME: this is kind of ugly ...
build-dirs:
	mkdir -p $(bin-dir) $(work-dir)

$(work-dir)/appliance-repair.o: src/appliance-repair.cpp
	g++ $(CPPARGS) -c -o $@ $^

$(bin-dir)/appliance-repair: $(work-dir)/appliance-repair.o
	g++ -o $@ $^

valgrind: $(bin-dir)/appliance-repair
	valgrind --leak-check=full --error-exitcode=1 $^ $(default-args)

strace: $(bin-dir)/appliance-repair
	strace $^ $(default-args) > /tmp/out

run: $(bin-dir)/appliance-repair
	$^ $(default-args)

cpplint: $(work-dir)/cpplint-report

$(work-dir)/cpplint-report: src/appliance-repair.cpp
	cpplint $^ 2>&1 | tee $(work-dir)/cpplint-report

test: $(bin-dir)/appliance-repair
	test/test.sh $^

check: cpplint valgrind test

clean:
	find $(build-dir) -not -name .gitignore -and -type f -delete
