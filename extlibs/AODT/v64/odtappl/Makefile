SOURCES= odtappl.o

CFLAGS= -w -ansi -pedantic -fno-common -g
CC=gcc
LIB=-laodtapi -laodtmcidas -laodtlib -laodtext -laodtnav -lmcidas -laodtf2c -lm

naodt :$(SOURCES)
	$(CC) $(CFLAGS) -o $(@) $(SOURCES) -L../lib $(LIB)
	mv $(@) ..

clean :
	rm -f $(SOURCES)

