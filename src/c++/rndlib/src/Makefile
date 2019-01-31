build-dir := ../build
work-dir := $(build-dir)/work
install-dir := $(build-dir)/install
lib-dir := $(install-dir)/lib

sources := $(wildcard src/*.cpp)
binary := librnd
source-files := $(notdir $(sources))
target-files := $(source-files:.cpp=)
objects := $(addprefix $(work-dir)/,$(source-files:.cpp=.o))
cpplint-targets := $(addprefix cpplint-,$(target-files))

$(warning $(cpplint-targets))
CPPARGS := -Wall -Werror -Wextra -std=c++11 -ggdb3 -O3 -fpermissive
LDFLAGS :=

.PHONY: check clean cpplint valgrind-targets run-targets strace-targets cpplint-targets

all: $(lib-dir)/$(binary).a $(lib-dir)/$(binary).so

$(work-dir) $(lib-dir):
	mkdir -p $@

cpplint: $(cpplint-targets)

$(cpplint-targets): cpplint-%: $(work-dir)/%.cpplint

$(work-dir)/%.cpplint: src/%.cpp | $(work-dir)
	@echo cpplint $^
	@cpplint --quiet $^ 2>&1 | tee $@

check: cpplint

clean:
	rm -r $(build-dir)

$(work-dir)/%.o: src/%.cpp | $(work-dir)
	g++ $(CPPARGS) -c -o $@ $(filter-out %.hpp, $^)

$(lib-dir)/$(binary).a: $(objects) | $(lib-dir)
	ar -cvq $@ $^

$(lib-dir)/$(binary).so: $(objects) | $(lib-dir)
	g++ $(CPPARGS) -fPIC -shared -o $@ $^
