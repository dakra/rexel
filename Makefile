EMACS      ?= emacs
EMACSFLAGS ?=

XDG_CACHE_HOME ?= $(HOME)/.cache
GHOSTEL_DIR    ?= $(firstword $(wildcard $(HOME)/.emacs.d/lib/ghostel) $(XDG_CACHE_HOME)/ghostel)
WEBSOCKET_DIR  ?= $(firstword $(wildcard $(HOME)/.emacs.d/lib/websocket) $(XDG_CACHE_HOME)/emacs-websocket)
LINT_ELPA_DIR  ?= $(XDG_CACHE_HOME)/rexel-lint-elpa
LINT_DEPS_STAMP := $(LINT_ELPA_DIR)/.deps-installed

LOAD_PATH = -L "$(WEBSOCKET_DIR)" -L "$(GHOSTEL_DIR)/lisp" -L .

.PHONY: all byte-compile test lint package-lint checkdoc clean

all: byte-compile test lint

$(GHOSTEL_DIR):
	git clone --depth 1 https://github.com/dakra/ghostel.git "$@"

$(WEBSOCKET_DIR):
	git clone --depth 1 https://github.com/ahyatt/emacs-websocket.git "$@"

rexel.elc: rexel.el | $(GHOSTEL_DIR) $(WEBSOCKET_DIR)
	$(EMACS) --batch $(EMACSFLAGS) -Q $(LOAD_PATH) \
		--eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $<

byte-compile: rexel.elc

test: rexel.elc
	$(EMACS) --batch $(EMACSFLAGS) -Q $(LOAD_PATH) \
		-l ert -l test/rexel-test.el -f ert-run-tests-batch-and-exit

lint: byte-compile package-lint checkdoc

$(LINT_DEPS_STAMP): | $(GHOSTEL_DIR)
	$(EMACS) --batch $(EMACSFLAGS) -Q \
		--eval "(setq package-user-dir \"$(LINT_ELPA_DIR)\")" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(package-install 'package-lint)" \
		--eval "(package-install 'websocket)" \
		--eval "(package-install-file (expand-file-name \"$(GHOSTEL_DIR)/lisp/ghostel.el\"))"
	@touch $@

package-lint: $(LINT_DEPS_STAMP) rexel.el
	$(EMACS) --batch $(EMACSFLAGS) -Q \
		--eval "(setq package-user-dir \"$(LINT_ELPA_DIR)\")" \
		--eval "(package-initialize)" \
		--eval "(require 'package-lint)" \
		-f package-lint-batch-and-exit rexel.el

checkdoc: rexel.el
	$(EMACS) --batch $(EMACSFLAGS) -Q \
		--eval "(require 'checkdoc)" \
		--eval "(let ((sentence-end-double-space nil) \
		              (checkdoc-verb-check-experimental-flag nil) \
		              (ok t)) \
		  (ignore-errors (kill-buffer \"*Warnings*\")) \
		  (let ((inhibit-message t)) (checkdoc-file \"rexel.el\")) \
		  (when (get-buffer \"*Warnings*\") \
		    (setq ok nil) \
		    (with-current-buffer \"*Warnings*\" (message \"%s\" (buffer-string)))) \
		  (unless ok (kill-emacs 1)))"

clean:
	rm -f rexel.elc
