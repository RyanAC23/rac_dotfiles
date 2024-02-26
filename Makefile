MY_HOME := $(HOME)
EMACS_DIR := $(HOME)/.emacs.d
PWD := `pwd`

backgrounds:
	@echo "Feature not yet implemented."

cleanEmacs:
	rm -rf .emacs.d/elpa

linkEmacs:
	@if test -L $(EMACS_DIR); then \
	  echo "$(EMACS_DIR) exists... remove old symlink and create a new one."; \
	elif test -d $(EMACS_DIR); then \
	  echo "$(EMACS_DIR) is a real directory. If you would like to symlink the emacs files from this directory, please remove the directory from home after ensuring that the files are backed up, and try again."; \
	else \
	  echo "$(EMACS_DIR) symlink is not present. Creating..."; \
	  ln -s $(PWD)/.emacs.d $(EMACS_DIR); \
	fi
