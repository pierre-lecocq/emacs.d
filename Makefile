all:
	emacs -batch -l init.el

compile:
	emacs -batch -f batch-byte-compile init.el config/*.el

clean:
	rm -f *.elc *~ config/*.el config/*.elc config/*~

reset: clean
	rm -rf el-get

eshell:
	test -s ~/.eshell/alias || (mkdir ~/.eshell; echo "alias l ls -lh" > ~/.eshell/alias && echo "alias la ls -lhA" >> ~/.eshell/alias)
#	test -s ~/.eshell/alias || (echo "Not exists" && mkdir ~/.eshell; alias | tr -d "'" |  sed  "s/^alias \(.*\)=/alias \1 /g" > ~/.eshell/alias)
