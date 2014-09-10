all:
	$(info Generating config)
	emacs -batch -l init.el

compile:
	$(info Compiling generated config)
	emacs -batch -f batch-byte-compile init.el config/elisp/*.el

clean:
	$(info Cleaning generated config)
	rm -rf *.elc config/elisp

reset: clean
	$(info Removing packages sources)
	rm -rf el-get elpa auto-save-list org-mode

org-mode:
	if ! [ -d "org-mode" ]; then \
		$(info Installing Orgmode from sources) \
		git clone git://orgmode.org/org-mode.git; \
		cd org-mode && make autoloads; \
	fi

