SOURCES := $(wildcard *.erl)
OBJS    := $(patsubst %.erl,%.beam,${SOURCES})

all: $(OBJS)

%.beam: %.erl
	erlc +debug_info $<

clean:
	rm *.beam
