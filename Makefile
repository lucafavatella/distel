all:
	(cd c && ${MAKE} $@)
	(cd erl && ${MAKE} $@)
	(cd elisp && ${MAKE} $@)

clean:
	(cd c && ${MAKE} $@)
	(cd erl && ${MAKE} $@)
	(cd elisp && ${MAKE} $@)

