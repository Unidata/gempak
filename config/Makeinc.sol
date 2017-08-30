OPSYS = SunOS

X11LIBDIR = -L/usr/openwin/lib
MOTIFLIBS = -L/usr/dt/lib -lXm
MOTIFINC  = -I/usr/dt/include
XWINCDIR  = -I/usr/openwin/include

COPT = -DUNDERSCORE -DNO_NANOSLEEP -D$(OPSYS) $(GEMINC) $(MOTIFINC) $(XWINCDIR) -O #-g
FOPT = $(GEMINC) -O2 # -g # (bad experience with -O aka -O3 and WS6.2)

NCOPT = "FC= " "CXX= "
JASPEROPT = 'CC=$(CC)' 'CFLAGS=-O' '--disable-libjpeg'
XML2OPT = 'CC=$(CC)' '--with-iconv'
PNGOPT = 'LD_LIBRARY_PATH=$(OS_LIB)'

XMLDEP = -lpthread -lsocket -lnsl

CPLUS_COMMENT = -xCC

CFLAGS = $(COPT) 
CFLAGS_DB = $(COPT)
FFLAGS = $(FOPT) 
LDFLAGS = -xildoff
LDM_FLAGS = -D_DEV_CONSLOG
export echo=echo


INSTALL = /usr/ucb/install -s
AUXINSTALL = /usr/ucb/install

SYSLIBS = -lm -lsocket -lnsl -lgen
#PURIFY = purify -best-effort
