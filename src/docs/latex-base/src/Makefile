BUILD-DIR := ../build

.PHONY: all
all: $(BUILD-DIR)/hello.pdf

$(BUILD-DIR)/hello.pdf: hello.tex | $(BUILD-DIR)
	rubber --pdf --into $(BUILD-DIR) ../src/hello.tex

$(BUILD-DIR):
	mkdir -p $@

.PHONY: watch
watch:
	while inotifywait hello.tex; do \
		make all; \
	done

.PHONY: clean
clean:
	rm -rf $(BUILD-DIR)
