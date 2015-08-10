BASE_DIR=$(shell pwd)
BASE_FILE=$(BASE_DIR)/emacs.el
VENDOR_DIR=$(BASE_DIR)/vendor

all: build

dep:
	mkdir $(VENDOR_DIR)
	git submodule update --recursive

build: dep
	emacs --batch -l $(BASE_FILE)

compile: build
	emacs --batch --eval '(byte-compile-file "$(BASE_FILE)")'

clean:
	rm -f $(BASE_DIR)/*~ $(BASE_DIR)/.*~ $(BASE_DIR)/*.elc

reset: clean
	rm -rf $(VENDOR_DIR)

love: reset compile
