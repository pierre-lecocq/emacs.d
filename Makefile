BASE_DIR=$(shell pwd)
BASE_FILE=$(BASE_DIR)/emacs.el
YAK_DIR=$(BASE_DIR)/vendor/yak
PACKAGES_DIR=$(BASE_DIR)/vendor/packages

all: build

dep:
	git clone https://github.com/pierre-lecocq/yak $(YAK_DIR)

build: dep
	emacs --batch -l $(BASE_FILE)

compile: build
	emacs --batch --eval '(byte-compile-file "$(BASE_FILE)")'

clean:
	rm -f $(BASE_DIR)/*~ $(BASE_DIR)/.*~ $(BASE_DIR)/*.elc

reset: clean
	rm -rf $(PACKAGES_DIR)

love: reset compile
