all: erlext.elc

erlext.elc: erlext.el
#	NOTE: If you want to use XEmacs, then change the following
#	line to call xemacs instead
	emacs -batch -f batch-byte-compile $<

clean:
	-rm *.elc
