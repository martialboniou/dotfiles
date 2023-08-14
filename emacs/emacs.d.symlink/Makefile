.POSIX:
EMACS = emacs
BATCH   = $(EMACS) --batch

.PHONY: all
all: auto-compile-library compile extensions

compile:
	$(BATCH) -L . -l early-init -f batch-byte-compile init.el

extensions:
	$(BATCH) -L . -l early-init --eval "(byte-recompile-directory \"./lisp/\" 0 t)"

auto-compile-library:
	$(BATCH) --eval "(byte-recompile-directory \"./auto-compile/\" 0 t)"

.PHONY: clean
clean:
	rm -vf init.elc auto-compile/*.elc lisp/*.elc
