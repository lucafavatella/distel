all: erlext.elc

erlext.elc: erlext.el
	emacs -batch -f batch-byte-compile $<

clean:
	rm *.elc
