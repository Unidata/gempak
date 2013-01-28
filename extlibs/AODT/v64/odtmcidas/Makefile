SOURCES= odtmcidas.o odtmcinput.o odtmcerror.o

CFLAGS= -w -ansi -pedantic -fno-common -g
CC=gcc
AR= ar
ARFLAGS= ruv

.SUFFIXES:	.c

c.o:
	$(CC) $(CFLAGS) -c $<
AODTLIB = libaodtmcidas.a
all     : src $(AODTLIB)
src     : $(SOURCES)
$(AODTLIB) : $(SOURCES)
	$(AR) $(ARFLAGS) $@ $?
clean :
	rm -f $(SOURCES)

