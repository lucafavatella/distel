SOURCES := $(wildcard *.erl)
OBJS    := $(patsubst %.erl,%.beam,${SOURCES})

all: $(OBJS)

%.beam: %.erl
	erlc $<

clean:
	rm *.beam
