SRC_DIR=$(shell pwd)
SRC_FILE=$(SRC_DIR)/README.org
DEST_FILE=$(SRC_DIR)/emacs.el
DEST_COMP_FILE=$(SRC_DIR)/emacs.elc
PACKAGES_DIR=$(SRC_DIR)/packages

.PHONY: build clean reset love

all: build

build: 
	emacs --batch \
		--eval "(require 'org)" \
		--eval "(org-babel-load-file \"$(SRC_FILE)\" t)"

clean:
	rm -f $(SRC_DIR)/*~ $(SRC_DIR)/.*~ $(DEST_FILE) $(DEST_COMP_FILE)

reset: clean
	rm -rf $(PACKAGES_DIR)

love: reset build
