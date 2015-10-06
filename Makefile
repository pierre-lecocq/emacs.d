#
# File: Makefile
# Time-stamp: <2015-10-06 10:26:48>
# Copyright (C) 2015 Pierre Lecocq
# Description: Makefile used to generate a lisp file from an org file
#

# Set configuration folder path
SRC_DIR=$(shell pwd)

# Source file (the org file)
SRC_FILE=$(SRC_DIR)/README.org

# Destination files (the emacs lisp file and its compiled version)
DEST_FILE=$(SRC_DIR)/emacs.el
DEST_COMP_FILE=$(SRC_DIR)/emacs.elc

# Packages folder
PACKAGES_DIR=$(SRC_DIR)/packages

# I love poneys
.PHONY: build clean reset love

# Main rule
all: build

# Generate lisp and compile it
build:
	emacs --batch \
		--eval "(require 'org)" \
		--eval "(org-babel-load-file \"$(SRC_FILE)\")" \
		--eval "(byte-compile-file \"$(DEST_FILE)\")"

# Housework
clean:
	rm -f $(SRC_DIR)/*~ $(SRC_DIR)/.*~ $(DEST_FILE) $(DEST_COMP_FILE)

# Housework on steroids
reset: clean
	rm -rf $(PACKAGES_DIR)

# Because "make love" is important
love: reset build
