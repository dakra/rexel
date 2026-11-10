EMACS      ?= emacs
EMACSFLAGS ?=

XDG_CACHE_HOME ?= $(HOME)/.cache
GHOSTEL_DIR    ?= $(firstword $(wildcard $(HOME)/.emacs.d/lib/ghostel) $(XDG_CACHE_HOME)/ghostel)
WEBSOCKET_DIR  ?= $(firstword $(wildcard $(HOME)/.emacs.d/lib/websocket) $(XDG_CACHE_HOME)/emacs-websocket)
CONSULT_DIR    ?= $(firstword $(wildcard $(HOME)/.emacs.d/lib/consult) $(XDG_CACHE_HOME)/consult)
COMPAT_DIR     ?= $(firstword $(wildcard $(HOME)/.emacs.d/lib/compat) $(XDG_CACHE_HOME)/compat)
LINT_ELPA_DIR  ?= $(XDG_CACHE_HOME)/rexel-lint-elpa
LINT_DEPS_STAMP := $(LINT_ELPA_DIR)/.deps-installed

LOAD_PATH = -L "$(WEBSOCKET_DIR)" -L "$(GHOSTEL_DIR)/lisp" -L .
CONSULT_LOAD_PATH = $(LOAD_PATH) -L "$(COMPAT_DIR)" -L "$(CONSULT_DIR)" -L extensions/consult-rexel
CONSULT_REXEL = extensions/consult-rexel/consult-rexel

.PHONY: all byte-compile test test-consult lint package-lint checkdoc clean

all: byte-compile test test-consult lint

$(GHOSTEL_DIR):
	git clone --depth 1 https://github.com/dakra/ghostel.git "$@"

$(WEBSOCKET_DIR):
	git clone --depth 1 https://github.com/ahyatt/emacs-websocket.git "$@"

rexel.elc: rexel.el | $(GHOSTEL_DIR) $(WEBSOCKET_DIR)
	$(EMACS) --batch $(EMACSFLAGS) -Q $(LOAD_PATH) \
		--eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $<

rexel-project.elc: rexel-project.el rexel.elc
	$(EMACS) --batch $(EMACSFLAGS) -Q $(LOAD_PATH) \
		--eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $<

$(CONSULT_DIR):
	git clone --depth 1 https://github.com/minad/consult.git "$@"

$(COMPAT_DIR):
	git clone --depth 1 https://github.com/emacs-compat/compat.git "$@"

$(CONSULT_REXEL).elc: $(CONSULT_REXEL).el rexel.elc rexel-project.elc | $(CONSULT_DIR) $(COMPAT_DIR)
	$(EMACS) --batch $(EMACSFLAGS) -Q $(CONSULT_LOAD_PATH) \
		--eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $<

byte-compile: rexel.elc rexel-project.elc $(CONSULT_REXEL).elc

test: rexel.elc rexel-project.elc
	$(EMACS) --batch $(EMACSFLAGS) -Q $(LOAD_PATH) \
		-l ert -l test/rexel-test.el -l test/rexel-project-test.el \
		-f ert-run-tests-batch-and-exit

test-consult: $(CONSULT_REXEL).elc
	$(EMACS) --batch $(EMACSFLAGS) -Q $(CONSULT_LOAD_PATH) \
		-l ert -l test/consult-rexel-test.el -f ert-run-tests-batch-and-exit

lint: byte-compile package-lint checkdoc

$(LINT_DEPS_STAMP): Makefile | $(GHOSTEL_DIR)
	$(EMACS) --batch $(EMACSFLAGS) -Q \
		--eval "(setq package-user-dir \"$(LINT_ELPA_DIR)\")" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(package-install 'package-lint)" \
		--eval "(package-install 'websocket)" \
		--eval "(package-install 'consult)" \
		--eval "(package-install-file (expand-file-name \"$(GHOSTEL_DIR)/lisp/ghostel.el\"))" \
		--eval "(package-install-file (expand-file-name \"rexel.el\"))"
	@touch $@

package-lint: $(LINT_DEPS_STAMP) rexel.el rexel-project.el $(CONSULT_REXEL).el
	$(EMACS) --batch $(EMACSFLAGS) -Q \
		--eval "(setq package-user-dir \"$(LINT_ELPA_DIR)\")" \
		--eval "(package-initialize)" \
		--eval "(require 'package-lint)" \
		--eval "(setq package-lint-main-file \"rexel.el\")" \
		-f package-lint-batch-and-exit rexel.el rexel-project.el
	$(EMACS) --batch $(EMACSFLAGS) -Q \
		--eval "(setq package-user-dir \"$(LINT_ELPA_DIR)\")" \
		--eval "(package-initialize)" \
		--eval "(require 'package-lint)" \
		-f package-lint-batch-and-exit $(CONSULT_REXEL).el

checkdoc: rexel.el rexel-project.el $(CONSULT_REXEL).el
	$(EMACS) --batch $(EMACSFLAGS) -Q \
		--eval "(require 'checkdoc)" \
		--eval "(let ((sentence-end-double-space nil) \
		              (checkdoc-verb-check-experimental-flag nil) \
		              (ok t)) \
		  (ignore-errors (kill-buffer \"*Warnings*\")) \
		  (let ((inhibit-message t)) \
		    (checkdoc-file \"rexel.el\") \
		    (checkdoc-file \"rexel-project.el\") \
		    (checkdoc-file \"$(CONSULT_REXEL).el\")) \
		  (when (get-buffer \"*Warnings*\") \
		    (setq ok nil) \
		    (with-current-buffer \"*Warnings*\" (message \"%s\" (buffer-string)))) \
		  (unless ok (kill-emacs 1)))"

clean:
	rm -f rexel.elc rexel-project.elc $(CONSULT_REXEL).elc
