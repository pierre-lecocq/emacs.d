BASE_DIR=$(shell pwd)
BASE_FILE=$(BASE_DIR)/emacs.el
LOADABLE_FILE=$(BASE_DIR)/emacs
VENDOR_DIR=$(BASE_DIR)/vendor

all: build

build:
        emacs --batch -l $(BASE_FILE)

compile:
        emacs --batch --eval '(byte-compile-file "$(BASE_FILE)")'

install: clean build compile
        $(info )
        $(info Save your current configuration, then execute:)
        $(info )
        $(info echo "(load \"$(LOADABLE_FILE)\")" > ~/.emacs)
        $(info )

clean:
        rm -f $(BASE_DIR)/*~ $(BASE_DIR)/*.elc

reset: clean
        rm -rf $(VENDOR_DIR)

love: reset build
