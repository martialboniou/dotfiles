.POSIX:
EMACS = emacs
BATCH   = $(EMACS) -Q --batch

.PHONY: all
all: compile extensions auto-compile-library

compile:
	$(BATCH) init.el -L . -l early-init -f batch-byte-compile

extensions:
	$(BATCH) --eval "(byte-recompile-directory \"./lisp/\" 0 t)"

auto-compile-library:
	$(BATCH) --eval "(byte-recompile-directory \"./auto-compile/\" 0 t)"

.PHONY: clean
clean:
	rm -vf init.elc auto-compile/*.elc lisp/*.elc
