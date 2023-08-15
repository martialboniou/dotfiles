.POSIX:
EMACS = @emacs
BATCH   = $(EMACS) --batch

.PHONY: all
all: auto-compile-library early-init init extensions

early-init:
	$(BATCH) -f batch-byte-compile early-init.el \
		&& echo "Done (early-init.el compiled)"

init:
	$(BATCH) -L . -l early-init -f batch-byte-compile init.el \
		&& echo "Done (init.el compiled)"

auto-compile-library:
	$(BATCH) --eval "(byte-recompile-directory \"./auto-compile/\" 0 t)"

extensions:
	$(BATCH) -L . -l early-init --eval "(byte-recompile-directory \"./lisp/\" 0 t)"

.PHONY: clean
clean:
	rm -vf early-init.elc init.elc auto-compile/*.elc lisp/*.elc
