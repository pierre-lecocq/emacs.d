BASE_DIR=$(shell pwd)
LISP_PATH=lisp
PACKAGES_DIR=packages
CUSTOM_FILE=custom.el
AUTO_SAVE_LIST=auto-save-list
EXTRAS=$(AUTO_SAVE_LIST) $(CUSTOM_FILE) elpa

all: my

my:
	$(info Generating config)
	emacs -batch -l init.el

compile:
	$(info Compiling generated config)
	emacs -batch -f batch-byte-compile init.el $(LISP_PATH)/*.el

clean:
	$(info Cleaning generated config)
	rm -rf *.elc $(LISP_PATH)/*.elc *.eld $(EXTRAS)

reset: clean
	$(info Removing packages sources)
	rm -rf $(EXTRAS) $(PACKAGES_DIR)

love: reset my
