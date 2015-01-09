BASE_DIR=$(shell pwd)
CONFIG_ORG_PATH=config/org
CONFIG_ELISP_PATH=config/elisp
PACKAGES_DIR=elpa
CUSTOM_FILE=custom.el
EXTRAS=auto-save-list $(PACKAGES_DIR) $(CUSTOM_FILE)

all: my

my:
	$(info Generating config)
	emacs -batch -l init.el

compile:
	$(info Compiling generated config)
	emacs -batch -f batch-byte-compile init.el $(CONFIG_ELISP_PATH)/*.el

clean:
	$(info Cleaning generated config)
	rm -rf *.elc $(CONFIG_ELISP_PATH)

reset: clean
	$(info Removing packages sources)
	rm -rf $(EXTRAS)

prepare:
	$(info Preparing extra packages directories)
	touch $(CUSTOM_FILE); \
	[ -d $(PACKAGES_DIR) ] || mkdir $(PACKAGES_DIR);

org: prepare
	if ! [ -d $(PACKAGES_DIR)/org-mode ]; then \
		$(info Installing Orgmode from sources) \
		git clone git://orgmode.org/org-mode.git $(PACKAGES_DIR)/org-mode; \
		cd $(PACKAGES_DIR)/org-mode && make autoloads; \
		cd $(BASE_DIR); \
		echo "(add-to-list 'load-path \"$(BASE_DIR)/$(PACKAGES_DIR)/org-mode/lisp\" t)" >> $(CUSTOM_FILE); \
		echo "(add-to-list 'load-path \"$(BASE_DIR)/$(PACKAGES_DIR)/org-mode/contrib/lisp\" t)" >> $(CUSTOM_FILE); \
	fi

helm: prepare
	if ! [ -d $(PACKAGES_DIR)/helm ]; then \
		$(info Installing Helm from sources) \
		git clone https://github.com/emacs-helm/helm.git $(PACKAGES_DIR)/helm; \
	        git clone git@github.com:jwiegley/emacs-async.git $(PACKAGES_DIR)/async; \
		cd $(PACKAGES_DIR)/helm && make; \
		cd $(BASE_DIR); \
		echo "(add-to-list 'load-path \"$(BASE_DIR)/$(PACKAGES_DIR)/async\" t)" >> $(CUSTOM_FILE); \
		echo "(add-to-list 'load-path \"$(BASE_DIR)/$(PACKAGES_DIR)/helm\" t)" >> $(CUSTOM_FILE); \
		echo "(require 'helm-config)" >> $(CUSTOM_FILE); \
		echo "(helm-mode 1)" >> $(CUSTOM_FILE); \
		echo "(helm-autoresize-mode 1)" >> $(CUSTOM_FILE); \
		echo "(global-set-key (kbd \"M-x\") 'helm-M-x)" >> $(CUSTOM_FILE); \
	fi \

love: reset org my
