build-dir := ../build
work-dir := $(build-dir)/work
install-dir := $(build-dir)/install
bin-dir := $(install-dir)/bin
default-args :=

CPPARGS := -Wall -Wextra -Werror -std=c++11 -ggdb3 -fno-exceptions -O3
LDFLAGS :=

.PHONY: all
all: $(bin-dir)/scache

$(work-dir) $(install-dir) $(bin-dir):
	mkdir -p $@

$(work-dir)/scache.o: src/scache.cpp | $(work-dir)
	g++ $(CPPARGS) -c -o $@ $^

$(bin-dir)/scache: $(work-dir)/scache.o | $(bin-dir)
	g++ -o $@ $^ $(LDFLAGS)

.PHONY: valgrind
valgrind: $(bin-dir)/scache
	valgrind --leak-check=full --error-exitcode=1 $^ $(default-args)

.PHONY: strace
strace: $(bin-dir)/scache
	strace $^ $(default-args) > /tmp/out

.PHONY: run
run: $(bin-dir)/scache
	$^ $(default-args)

.PHONY: cpplint
cpplint: $(work-dir)/cpplint-report

$(work-dir)/cpplint-report: src/scache.cpp | $(work-dir)
	cpplint $^ 2>&1 | tee $(work-dir)/cpplint-report

.PHONY: test
test: $(bin-dir)/scache
	test/test.sh $^

.PHONY: check
check: cpplint valgrind test

.PHONY: clean
clean:
	rm -r $(build-dir)
