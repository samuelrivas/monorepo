build-dir := ../build
work-dir := $(build-dir)/work
install-dir := $(build-dir)/install
bin-dir := $(install-dir)/bin

sources := $(wildcard src/*.cpp)
source-files := $(notdir $(sources)) $(notdir $(test-sources))
target-files := $(source-files:.cpp=)
cpplint-targets := $(addprefix cpplint-,$(target-files))
bin-targets := $(addprefix $(bin-dir)/,$(target-files))
valgrind-targets := $(addprefix valgrind-,$(target-files))
run-targets := $(addprefix run-,$(target-files))
strace-targets := $(addprefix strace-,$(target-files))

CPPARGS := -Wall -Wextra -Werror -std=c++17 -ggdb3 -fno-exceptions -O3
LDFLAGS :=

.PHONY: all
all: $(bin-targets)

$(work-dir) $(bin-dir) $(lib-dir):
	mkdir -p $@

.PHONY: $(valgrind-targets)
$(valgrind-targets): valgrind-%: $(bin-dir)/%
	valgrind --leak-check=full --error-exitcode=1 $^

.PHONY: $(strace-targets)
$(strace-targets): strace-%: $(bin-dir)/%
	strace $^ > /tmp/out

.PHONY: $(run-targets)
$(run-targets): run-%: $(bin-dir)/%
	$^

.PHONY: $(cpplint-targets)
cpplint: $(cpplint-targets)

.PHONY: $(cpplint-targets)
$(cpplint-targets): cpplint-%: $(work-dir)/%.cpplint

$(work-dir)/%.cpplint: src/%.cpp | $(work-dir)
	@echo cpplint $^
	@cpplint --quiet $^ 2>&1 | tee $@

.PHONY: check
check: cpplint $(valgrind-targets)

.PHONY: clean
clean:
	rm -r $(build-dir)

$(work-dir)/%.o: src/%.cpp | $(work-dir)
	g++ $(CPPARGS) -c -o $@ $(filter-out %.hpp, $^)

$(work-dir)/%.o: src/lib/%.cpp src/lib/%.hpp | $(work-dir)
	g++ $(CPPARGS) -c -o $@ $(filter-out %.hpp, $^)

$(bin-dir)/%: $(work-dir)/%.o | $(bin-dir)
	g++ $(CPPARGS) -o $@ $^ $(LDFLAGS)

## Target specific flags
$(bin-dir)/scache-test: LDFLAGS := $(LDFLAGS) -lrapidcheck
$(bin-dir)/scache-test: CPPARGS := $(CPPARGS) -fexceptions
$(bin-dir)/http-server: LDFLAGS := $(LDFLAGS) -lPocoNet -lPocoUtil -lPocoFoundation
$(bin-dir)/http-server: CPPARGS := $(CPPARGS) -fexceptions
$(bin-dir)/shared-cache: CPPARGS := $(CPPARGS) -fexceptions
$(bin-dir)/shared-cache: LDFLAGS := $(LDFLAGS) -lPocoNet -lPocoUtil -lPocoFoundation -lpthread
