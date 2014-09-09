all:
	emacs -batch -l init.el

compile:
	emacs -batch -f batch-byte-compile init.el config/elisp/*.el

clean:
	rm -rf *.elc config/elisp

reset: clean
	rm -rf el-get elpa auto-save-list
