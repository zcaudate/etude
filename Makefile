emacs ?= emacs

BASEDIR := $(shell pwd)

profile:
	$(emacs) -Q -l profile.el \
	--eval "(setq profile-dotemacs-file \
        (setq load-file-name \"$(abspath init.el)\"))" \
	-f profile-dotemacs