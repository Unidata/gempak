SOURCES= odtsst.o odtexterror.o odtgrid.o 

CFLAGS= -w -ansi -pedantic -fno-common -g
CC=gcc
AR= ar
ARFLAGS= ruv

.SUFFIXES:	.c

c.o:
	$(CC) $(CFLAGS) -c $<
AODTLIB = libaodtext.a
all     : src $(AODTLIB)
src     : $(SOURCES)
$(AODTLIB) : $(SOURCES)
	$(AR) $(ARFLAGS) $@ $?
clean :
	rm -f $(SOURCES)

