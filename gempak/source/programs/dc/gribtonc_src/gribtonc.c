/*
 *	Copyright 1993, University Corporation for Atmospheric Research.
 */
/* $Id: gribtonc.c,v 1.12 1995/10/31 22:53:42 russ Exp $ */

/* 
 * decodes GRIB products into netCDF files
 */

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <unistd.h>
#include <limits.h>		/* _POSIX_PATH_MAX */
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "ulog.h"
#include "mkdirs_open.h"

#include "nc.h"
#include "centers.h"
#include "models.h"
#include "params.h"
#include "levels.h"
#include "timeunits.h"
#include "grib1.h"
#include "product_data.h"
#include "quasi.h"
#include "units.h"

#ifdef NO_ATEXIT
#include <atexit.h>
#endif

#ifdef __STDC__
static void cleanup(void);
static void signal_handler(int sig);
static void set_sigactions(void);
static void usage(char* av0);
static void print_floats(float* ff, int nn, int pp);
static int is_ixg(int id);
static int do_nc(FILE * ep, int to, quas*, char *cdlpath, char *ncpath);
static product_data* grib_decode(prod*, quas*);
int main(int ac, char** av);
#endif

unsigned long num_wmo_messages; /* for statistics on exit */
unsigned long num_gribs_decoded;
unsigned long num_gribs_written;
unsigned long num_degribbed;

/*
 * Timeout in seconds.  If no input is received for this interval, process
 * closes output file and exits.
 */
#define DEFAULT_TIMEOUT  600


/*
 * Called at exit.
 * This callback routine registered by atexit().
 */
static void
cleanup()
{
    uinfo("Exiting") ;
    uinfo("%lu WMO msgs, %lu GRIBs decoded, %lu written ",
	  num_wmo_messages, num_gribs_decoded, num_gribs_written);
    nccleanup();		/* close open netCDF files, if any */
    (void) closeulog();
}


/*
 * Called upon receipt of signals.
 * This callback routine registered in set_sigactions() .
 */
static void
signal_handler(sig)
     int sig ;
{
#ifdef SVR3SIGNALS
    /* 
     * Some systems reset handler to SIG_DFL upon entry to handler.
     * In that case, we reregister our handler.
     */
    (void) signal(sig, signal_handler) ;
#endif
    switch(sig) {
      case SIGHUP :
	udebug("SIGHUP") ;
	return ;
      case SIGINT :
	unotice("Interrupt") ;
	exit(0) ;
      case SIGTERM :
	udebug("SIGTERM") ;
	exit(0) ;
      case SIGUSR1 :
	udebug("SIGUSR1") ;
	return ;
      case SIGUSR2 :
	if (toggleulogpri(LOG_INFO))
	  unotice("Going verbose") ;
	else
	  unotice("Going silent") ;
	return ;
      case SIGPIPE :
	unotice("SIGPIPE") ;
	exit(0) ;
    }
    udebug("signal_handler: unhandled signal: %d", sig) ;
}


static void
set_sigactions()
{
#ifndef NO_POSIXSIGNALS
    struct sigaction sigact ;
    
    sigact.sa_handler = signal_handler;
    sigemptyset(&sigact.sa_mask) ;
    sigact.sa_flags = 0 ;
#ifdef SA_RESTART	/* SVR4, 4.3+ BSD */
    /* usually, restart system calls */
    sigact.sa_flags |= SA_RESTART ;
#endif
    
    (void) sigaction(SIGHUP, &sigact, NULL) ;
    (void) sigaction(SIGINT, &sigact, NULL) ;
    (void) sigaction(SIGTERM, &sigact, NULL) ;
    (void) sigaction(SIGUSR1, &sigact, NULL) ;
    (void) sigaction(SIGUSR2, &sigact, NULL) ;
    (void) sigaction(SIGPIPE, &sigact, NULL) ;
#else
    (void) signal(SIGHUP, signal_handler) ;
    (void) signal(SIGINT, signal_handler) ;
    (void) signal(SIGTERM, signal_handler) ;
    (void) signal(SIGUSR1, signal_handler) ;
    (void) signal(SIGUSR2, signal_handler) ;
    (void) signal(SIGPIPE, signal_handler) ;
#endif
}


static void
usage(av0)
     char *av0 ; /*  id string */
{
    (void)fprintf(stderr,
		  "Usage: %s [options] [files ...]\t\nOptions:\n", av0);
    (void)fprintf(stderr,
		  "\t-v           Verbose, report decoding steps\n") ;
    (void)fprintf(stderr,
		  "\t-l logfile   Send log info to file (default uses syslogd)\n") ;
    (void)fprintf(stderr,
		  "\t-t timeout   If no input, exit after \"timeout\" seconds (default %d)\n",
		  DEFAULT_TIMEOUT) ;
    (void)fprintf(stderr,
		  "\t-e errfile   Append bad GRIB products to this file\n") ;
    (void)fprintf(stderr,
		  "\t-q meth      Specs for expanding quasi-regular grids,\n\t\t\te.g. -q \"lin,dlat=2.5,dlon=5.0\"\n") ;
    (void)fprintf(stderr,
		  "\t[ CDL_file ] CDL template, when netCDF output file does not exist\n") ;
    (void)fprintf(stderr,
		  "\tnetCDF_file  netCDF output file\n") ;
    exit(1);
}


/*
 * Parse raw product bytes into product_data structure.  Returns 0 if
 * failed.  User should call free_product_data() on result when done with
 * it.  Also expands quasi-regular grids to full rectangular grids if qmeth
 * is non-null.
 */
static product_data *
grib_decode(prodp, quasp)
     prod *prodp ;		/* input raw product bytes */
     quas *quasp ;		/* if non-null, method used to expand
				   quasi-regular "grids" */
{
    grib1 *gp = new_grib1(prodp); /* overlay raw bits on raw grib1 structure */
    product_data *pdp;

    if (gp == 0)
	return 0;

    pdp = new_product_data(gp);	/* compute cooked product structure, with GDS
				   (manufactured, if necessary) and bytemap */
    
    free_grib1(gp);

    if (!pdp)
	return 0;

    if (pdp->gd->quasi && quasp) {
	int ret = expand_quasi(quasp, pdp) ; /* Changes *pdp */
	if (!ret) {
	    uerror("%s: can't expand quasi-regular grid", pdp->header);
	    return 0;
	}
    }
    return pdp;
}


static int
do_nc (ep, timeout, quasp, cdlname, ncname)
    FILE *ep;			/* if non-null, where to append bad GRIBs */
    int timeout;		/* exit if no data in this many seconds */
    quas *quasp;		/* if non-null, specification for how
				   quasi-regular "grids" are to be expanded */
    char *cdlname;		/* Pathname of CDL template file to be used to
				   create netCDF file, if it doesn't exist */
    char *ncname;		/* Pathname of netCDF output file */
{
    struct prod the_prod;	/* raw bits of GRIB message, length, id */
    struct product_data *gribp;	/* decoded GRIB product structure */
    FILE *fp = stdin;		/* input */
    ncfile *ncp;
    int ncid;
    
    ncid = cdl_netcdf(cdlname, ncname);	/* get netCDF file handle */
    setncid(ncid);		/* store ncid so can be closed if interrupt */

    num_wmo_messages = 0;
    num_gribs_decoded = 0;

    if (init_udunits() != 0) {
	uerror("can't initialize udunits library, exiting");
	exit(-1);
    }
    ncp = new_ncfile(ncname);
    if (!ncp) {
	uerror("can't create output netCDF file %s, exiting", ncname);
	exit(-1);
    }

    while(1) {			/* usual exit is timeout in get_prod() */
	int bytes = get_prod(fp, timeout, &the_prod);
	if (bytes == 0)
	  continue;
	else
	  num_wmo_messages++;
	
	/* decode it into a grib1 product structure */
	gribp = grib_decode(&the_prod, quasp);
	if (gribp == 0) {
	    if (ep) {		/* doesn't write WMO header, just product */
		if (fwrite(the_prod.bytes, the_prod.len, 1, ep) == 0) {
		    serror("writing bad GRIB to error file");
		}
	    }
	    continue;
	}
	num_gribs_decoded++;

	/* Write grib info to netCDF file */
	if (nc_write(gribp, ncp) == 0) {/* OK */
	    num_gribs_written++;
	}
	free_product_data(gribp);
    }
}


/*
 * Reads GRIB data from standard input.
 * GRIB data may be contained in WMO envelope or not.
 * Decodes GRIB data to in-memory structure.
 * Writes decoded data that conforms to one specified netCDF output file.
 * May be used as an in-line LDM decoder, invoked by pqact(1).
 */
int
main(ac,av)
     int ac ;
     char *av[] ;
{
    char *logfname = 0 ;	/* log file name, default uses syslogd */
    char *ofile = 0 ;		/* output netCDF file name */
    char *cdlfile = 0 ;		/* CDL template file name */
    FILE *ep = 0;		/* file handle for bad GRIBS output, when
				   -e badfname used */
    int timeo = DEFAULT_TIMEOUT ; /* timeout */
    quas *quasp = 0;		/* default, don't expand quasi-regular grids */

    /*
     * register exit handler
     */
    if(atexit(cleanup) != 0)
      {
	  serror("atexit") ;
	  exit(1) ;
      }
    
    /*
     * set up signal handlers
     */
    set_sigactions() ;
    
    {
	extern int optind;
	extern int opterr;
	extern char *optarg;
	int ch;
	int logmask = (LOG_MASK(LOG_ERR) | LOG_MASK(LOG_NOTICE)) ;
	int errflg = 0;
	
	opterr = 1;
	
	while ((ch = getopt(ac, av, "vxl:t:e:q:")) != EOF) {
	    switch (ch) {
	    case 'v':
		logmask |= LOG_MASK(LOG_INFO) ;
		break;
	    case 'x':
		logmask |= LOG_MASK(LOG_DEBUG) ;
		break;
	    case 'l':
		logfname = optarg ;
		break;
	    case 't':
		timeo = atoi(optarg) ;
		if(timeo < 1) {
		    fprintf(stderr, "%s: invalid timeout %s",
			    av[0], optarg) ;
		    errflg++;
		}
		break;
	    case 'e':
		ep = fopen(optarg, "w");
		if(!ep) {
		    serror("can't open %s", optarg);
		    errflg++;
		}
		break;
	    case 'q':
		quasp = qmeth_parse(optarg);
		if(!quasp) {
		    fprintf(stderr,
			    "%s: invalid quasi-regular expansion method %s",
			    av[0], optarg) ;
		    errflg++;
		}
		break;
	    case '?':
		errflg++;
		break;
	    }
	}
	
	switch (ac - optind) {	/* number of args left */
	  case 1:		/* must be existing netCDF file */
	    ofile = av[optind] ;
	    break;
	  case 2:
	    cdlfile = av[optind] ;
	    ofile = av[optind+1] ;
	    break;
	  default:
	    errflg++;
	}

	if(errflg)
	  usage(av[0]);	
	(void) setulogmask(logmask) ;
    }
        
    /*
     * initialize logger
     */
    (void) openulog(ubasename(av[0]),
		    (LOG_CONS|LOG_PID), LOG_LOCAL0, logfname) ;
    uinfo("Starting Up") ;

    do_nc(ep, timeo, quasp, cdlfile, ofile);
    
    return 0;
}

