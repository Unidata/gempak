LIBFSRCS =	asspl2.f \
		cfi.f \
		cfz.f \
		clit.f \
		ddest.f \
		edest.f \
		flalo.f \
		ftime.f \
		geolat.f \
		gms5_nav.f \
		gryscl.f \
		iftok.f \
		iround.f \
		kb1aaa.f \
		kb1gms.f \
		kb1gvar.f \
		kb1msat.f \
		kb1visr.f \
		kb2aaa.f \
		kb2gms.f \
		kb2gvar.f \
		kb2msat.f \
		kb2visr.f \
		kb3aaa.f \
		kb3gms.f \
		kb3gvar.f \
		kb3msat.f \
		kb3visr.f \
		isir.f \
		lit.f \
		llcart.f \
		m0itime.f \
		movcw.f \
		movwc.f \
		nllxyz.f \
		nv1goes.f \
		nv1gmsx.f \
		nv1gvar.f \
		nv1merc.f \
		nv1msat.f \
		nv1ps.f \
		nv1rect.f \
		nv2goes.f \
		nv2gmsx.f \
		nv2gvar.f \
		nv2merc.f \
		nv2msat.f \
		nv2ps.f \
		nv2rect.f \
		nv3goes.f \
		nv3gmsx.f \
		nv3gvar.f \
		nv3merc.f \
		nv3msat.f \
		nv3ps.f \
		nv3rect.f \
		raerac.f \
		sdest.f \
		solarp.f \
		timdif.f \
		arabox.c \
		araget.c \
		araopt.c \
		brksubs.c \
		clsara.c \
		coords.c \
		dblnint.c \
		fltcon.c \
		mpixel.c \
		opnara.c \
		readd.c \
		redara.c
LIBXSRCS =	asspl2.c \
		cfz.c \
		clit.c \
		ddest.c \
		edest.c \
		flalo.c \
		ftime.c \
		geolat.c \
		gms5_nav.c \
		gryscl.c \
		iftok.c \
		iround.c \
		isir.c \
		kb1aaa.c \
		kb1gms.c \
		kb1gvar.c \
		kb1msat.c \
		kb1visr.c \
		kb2aaa.c \
		kb2gms.c \
		kb2gvar.c \
		kb2msat.c \
		kb2visr.c \
		kb3aaa.c \
		kb3gms.c \
		kb3gvar.c \
		kb3msat.c \
		kb3visr.c \
		lit.c \
		llcart.c \
		m0itime.c \
		movcw.c \
		movwc.c \
		nllxyz.c \
		nv1goes.c \
		nv1gmsx.c \
		nv1gvar.c \
		nv1msat.c \
		nv1merc.c \
		nv1ps.c \
		nv1rect.c \
		nv2goes.c \
		nv2gmsx.c \
		nv2gvar.c \
		nv2merc.c \
		nv2msat.c \
		nv2ps.c \
		nv2rect.c \
		nv3goes.c \
		nv3gmsx.c \
		nv3gvar.c \
		nv3merc.c \
		nv3msat.c \
		nv3ps.c \
		nv3rect.c \
		raerac.c \
		sdest.c \
		solarp.c \
		timdif.c 
LIBCSRCS =	araget_.c \
		daytime.c \
		fbyte4_.c \
		ischar_.c \
		kbprep.c \
		lwfile_.c \
		lwi_.c \
		maaatb_.c \
		movb_.c \
		movc_.c \
		movw_.c \
		mpixtb_.c \
		mgvatb_.c \
		nvprep.c \
		swbyt4_.c \
		zeros_.c \
		syskey.c \
		arabox.c \
		araget.c \
		araopt.c \
		brksubs.c \
		clsara.c \
		coords.c \
		dblnint.c \
		fltcon.c \
		mpixel.c \
		opnara.c \
		readd.c \
		redara.c
F2C =		f2c -ARw8 -Nn802 -Nx400
CC =		gcc
PFLAGS =	-I../libf2c
#CFLAGS =	-ansi -pedantic -fno-common -O2 $(PFLAGS) 
#F2CFLAGS =	-ansi -pedantic -fno-common -O2 $(PFLAGS)
CFLAGS =	-ansi -pedantic -fno-common -g $(PFLAGS)
F2CFLAGS =	-ansi -pedantic -fno-common -g $(PFLAGS)
SHELL =		/bin/sh
AR =		ar

all:		libaodtnav.a

libaodtnav.a:	$(LIBCSRCS:.c=.o) $(LIBFSRCS:.f=.o)
		$(AR) $(ARFLAGS) $@ $?

.f.o:
		$(F2C) $<; \
		$(CC) $(F2CFLAGS) -c $*.c

clean:
		rm -f $(LIBCSRCS:.c=.o) $(LIBFSRCS:.f=.o) $(LIBFSRCS:.f=.c)
