SOURCES= aodttest.o

CFLAGS= -w -ansi -pedantic -fno-common
CC=gcc
LIB=-laodtapi -laodtlib -laodtext -lm

naodttest :$(SOURCES)
	$(CC) $(CFLAGS) -o $(@) $(SOURCES) -L../lib $(LIB)
	mv $(@) ..

clean :
	rm -f $(SOURCES)

