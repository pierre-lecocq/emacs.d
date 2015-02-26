BASE_DIR=$(shell pwd)
LISP_PATH=lisp
PACKAGES_DIR=$(LISP_PATH)/vendor
CUSTOM_FILE=custom.el
AUTO_SAVE_LIST=auto-save-list
EXTRAS=$(AUTO_SAVE_LIST) $(CUSTOM_FILE) elpa tramp

all: my

my:
	$(info Generating config)
	emacs -batch -l init.el

clean:
	$(info Cleaning generated config)
	rm -rf *~ $(LISP_PATH)/config/*~ *.eld $(EXTRAS)

reset: clean
	$(info Removing packages sources)
	rm -rf $(EXTRAS) $(PACKAGES_DIR)

love: reset my
