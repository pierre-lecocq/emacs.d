all:
	emacs -batch -l init.el

compile:
	emacs -batch -f batch-byte-compile init.el config/*.el

clean:
	rm -f *.elc *~ config/*.el config/*.elc config/*~

reset: clean
	rm -rf el-get
