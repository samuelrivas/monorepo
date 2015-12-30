EMACS := emacs

EFLAGS :=

BUILD_DIR := ../build

BATCH := $(EMACS) $(EFLAGS) --batch -Q -L .

ELFILES := $(wildcard *.el)

ELCFILES := $(addprefix $(BUILD_DIR)/,$(ELFILES:.el=.elc))

.PHONY: all compile clean

all: compile

compile: $(ELCFILES)

$(BUILD_DIR)/%.elc: %.el
	mkdir -p $(BUILD_DIR)
	$(BATCH) -f batch-byte-compile $*.el
	mv $*.elc $@

clean:
	rm -rf $(BUILD_DIR)
