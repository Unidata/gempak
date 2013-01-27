#
#		Builds ldmlog.a library archive (C)
#		Unidata 11/99 Chiz
#
include $(CONFIGDIR)/$(MAKEINC)

ALIB = $(LDMLOG)

LIBINC =

FOBJS = 

COBJS = \
	$(ALIB)(ulog.o) 

OPTC = \
	inetutil.c

.PRECIOUS: $(ALIB)

CFLAGS = $(COPT) $(LDM_FLAGS)

all : $(ALIB)

$(ALIB): $(LOBJS) $(COBJS)
	@case $(OPSYS) in \
	   AIX|OSF1 ) \
		$(COMPILE.c) $(OPTC) \
	   ;; \
	   *) \
	   ;; \
	esac
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o

.c.a :
	$(CC) $(CFLAGS) -c $<

.f.a :
	$(FC) $(FFLAGS) -c $<



clean:
	$(RM) *.o 

install:

