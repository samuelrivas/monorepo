BUILD-DIR := ../build

.PHONY: all
all: $(BUILD-DIR)/hello.pdf

$(BUILD-DIR)/hello.pdf: hello.tex | $(BUILD-DIR)
	rubber --pdf --into $(BUILD-DIR) hello.tex

$(BUILD-DIR):
	mkdir -p $@

.PHONY: clean
clean:
	rm -rf $(BUILD-DIR)
