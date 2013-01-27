/*
 *	Copyright 1993, University Corporation for Atmospheric Research.
 */
/* $Id: gribdump.c,v 1.20 1995/12/07 15:30:49 russ Exp $ */

/* 
 * decodes GRIB products into ASCII
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

#include "centers.h"
#include "models.h"
#include "params.h"
#include "levels.h"
#include "timeunits.h"
#include "grib1.h"
#include "product_data.h"
#include "quasi.h"

#ifdef NO_ATEXIT
#include <atexit.h>
#endif

#ifdef __STDC__
static void cleanup(void);
static void signal_handler(int sig);
static void set_sigactions(void);
static void usage(char* av0);
static void print_floats(float* ff, int cols, int rows, int pp);
static int is_ixg(int id);
static void print_grib(struct product_data* gp, int prec);
static void print_grib_line(struct product_data* gp);
static int do_dump(FILE * fp, FILE * ep, int timeout, int prec, quas*);
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
 * Default precision used for printing floats.
 */
#define DEFAULT_PRECISION	7

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
		  "\t-l logfile   Send log info to file (default uses stdout)\n") ;
    (void)fprintf(stderr,
		  "\t-h           Output headers only, no data values\n") ;
    (void)fprintf(stderr,
		  "\t-b           Brief output of headers data\n") ;
    (void)fprintf(stderr,
		  "\t-t timeout   If no input, exit after \"timeout\" seconds (default %d)\n",
		  DEFAULT_TIMEOUT) ;
    (void)fprintf(stderr,
		  "\t-e errfile   Append bad GRIB products to this file\n") ;
    (void)fprintf(stderr,
		  "\t-p prec      Precision used to print floats (default %d)\n",
		  DEFAULT_PRECISION) ;
    (void)fprintf(stderr,
		  "\t-q meth      Specs for expanding quasi-regular grids,\n\t\t\te.g. -q \"lin,dlat=2.5,dlon=5.0\"\n") ;
    exit(1);
}

int headers_only;
int brief_output;

static void
print_floats(ff, cols, rows, pp)
     float *ff;			/* array of floats to print */
     int cols; 			/* number of columns of data */
     int rows; 			/* number of rows of data */
     int pp;			/* precision to use in printing */
{
    int ii;
    int jj;
    int per_line = 8;
    static char format[] = "%.10g%s";
    
    sprintf(format, "%%.%dg%%s", pp);

    for(ii = 0; ii < rows; ii++) {
	printf("Row %d:\n\t",ii);

	for(jj = 0; jj < cols;jj++)
	    printf(format, ff[ii*cols+jj],
		   jj%per_line+1 == per_line ? "\n\t" : " ");

	if ((jj-1)%per_line+1 != per_line)
	    printf("\n");
    }
}


static int
is_ixg(id)			/* is an international exchange grid? */
     int id;
{
    switch (id) {
      case 21:
      case 22:
      case 23:
      case 24:
      case 25:
      case 26:
      case 37:
      case 38:
      case 39:
      case 40:
      case 41:
      case 42:
      case 43:
      case 44:
      case 50:
      case 61:
      case 62:
      case 63:
      case 64:
	return 1;
    }
    /* default: */
    return 0;
}

static void
print_grib(gp, prec)
     struct product_data *gp;
     int prec;			/* precision to use for printing float vals */
{

    char format[80];

    printf("-----------------------------------------------------\n");

    printf("   %24s : %s\n","    Header", gp->header);
    printf("   %24s : %d (%s)\n","Originating Center", gp->center,
	   centername(gp->center));
    printf("   %24s : %d (%s)\n","Process", gp->model,
	   modelname(gp->center,gp->model));
    printf("   %24s : %d\n","Grid", gp->grid);
    printf("             points in grid : %d\n", gp->npts);
    printf("   %24s : %d (%s)\n","Parameter", gp->param,
	   grib_pname(gp->param));
    printf("   %24s : %s\n", "Units", grib_units(gp->param));
    printf("   %24s : %s\n", "Level Type", levelname(gp->level_flg));
    switch (gp->level_flg) {
      case LEVEL_SURFACE:
      case LEVEL_CLOUD_BASE:
      case LEVEL_CLOUD_TOP:
      case LEVEL_ISOTHERM:
      case LEVEL_ADIABAT:
      case LEVEL_MAX_WIND:
      case LEVEL_TROP:
      case LEVEL_MEAN_SEA:
	break;
      case LEVEL_FH:
      case LEVEL_FHG:
	printf("   %24s : %f (M)\n", "Level", mblev(gp->level));
	break;
      case LEVEL_SIGMA:
	printf("   %24s : %f\n", "Level", (256*gp->level[0]+gp->level[1])/10000.0);
	break;
      case LEVEL_DBS:
      case LEVEL_Bls:
      case LEVEL_ISOBARIC:
	printf("   %24s : %f (Pa)\n", "Level", 100.*mblev(gp->level));
	break;
      case LEVEL_LISO:
      case LEVEL_LFHM:
      case LEVEL_LFHG:
      case LEVEL_LS:
      case LEVEL_LBls:
      case LEVEL_LISH:
      case LEVEL_LSH:
      case LEVEL_LISM:
	printf("   %24s : %f (Pa)\n", "Level 1", gp->level[0]*1000.0);
	printf("   %24s : %f (Pa)\n", "Level 2", gp->level[1]*1000.0);
	break;
    }
    printf("   %24s : %04d/%02d/%02d:%02d:%02d\n","Reference Time", 
				/* century 21 doesn't start until 2001 */
	   gp->year+(gp->century - (gp->year==0 ? 0 : 1))*100,
	   gp->month, gp->day, gp->hour, 
	   gp->minute);
    printf("   %24s : %s\n", "Time Unit", tunitsname(gp->tunit));
    printf("   %24s : %s\n", "Time Range Indicator", triname(gp->tr_flg));
    switch (gp->tr_flg) {
      case TRI_P1:
      case TRI_IAP:
	printf("   %24s : %d\n", "Time 1 (P1)", gp->tr[0]);
	break; 
      case TRI_P12:
      case TRI_Ave:
      case TRI_Acc:
      case TRI_Dif:
      case TRI_LP1:
      case TRI_AvgN:
      case TRI_AvgN1:
      case TRI_AccN1:
      case TRI_AvgN2:
      case TRI_AvgN3:
      case TRI_AccN3:
	printf("   %24s : %d\n", "Time 1 (P1)", gp->tr[0]);
	printf("   %24s : %d\n", "Time 2 (P2)", gp->tr[1]);
	break; 
    }
    printf("   %24s : %d\n", "Decimal Scale Factor", gp->scale10);
    printf("   %24s : %d\n", "Binary Scale Factor", gp->bd->bscale);
    printf("   %24s : %f\n", "Reference Value", gp->bd->ref);
    {				/* all done in float to reproduce FSL value */
	float dscal;
	dscal = gp->scale10;
	dscal = pow(10.0, dscal);
	sprintf(format, "   %%24s : %%.%dg\n", prec);
	printf(format, "Minimum Value", (float)(gp->bd->ref / dscal));
    }
    printf("   %24s : %d\n", "Number of Bits", gp->bd->nbits);
    printf("   %24s : %s\n", "BMS Included", gp->has_bms ? "TRUE" : "FALSE");
    printf("   %24s : %s\n", "GDS Included", gp->has_gds ? "TRUE" : "FALSE");
    printf("   %24s : %s\n", "IsInternationalGrid",
	   is_ixg(gp->grid) ? "TRUE" : "FALSE");
    printf("   %24s : %d\n","GRIB Edition", gp->edition);
    printf("   %24s : %d\n","Parameter Table Ver", gp->param_vers);
    print_gdes(gp->gd);		/* print Grid Description Section */
    if (! headers_only) {
	printf("                 grid values:\n");
        print_floats(gp->data, gp->cols, gp->npts/gp->cols, prec);
    }
}     


static void
print_grib_line(gp)
     struct product_data *gp;
{
    char *lev;			/* abbreviation for level flag */
    
    lev = levelsuffix(gp->level_flg);

    if (lev) {
	printf("%4d%4d%4d%4d ",
	       gp->center, gp->model, gp->grid, gp->param);
    	
	if (lev[0] == '\0')
	    lev = "isob";	/* show isobaric levels explicitly */
	printf("%7s %4d %4d", lev, level1(gp->level_flg, gp->level),
	       level2(gp->level_flg, gp->level));
	
	printf(" %4d%4d%4d %4d%4d%4d %6d %s\n",
	       gp->tr_flg, gp->tr[0], gp->tr[1],
	       gp->bits, gp->has_bms, gp->has_gds, gp->npts, gp->header);
    }
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
do_dump (fp, ep, timeout, prec, quasp)
    FILE *fp;			/* input */
    FILE *ep;			/* if non-null, where to append bad GRIBs */
    int timeout;
    int prec;			/* precision to use for printing floats */
    quas *quasp;		/* if non-null, quasi-reg expansion method */
{
    struct prod the_prod;
    struct product_data *gribp;	/* UPC structure */
    
    num_wmo_messages = 0;
    num_gribs_decoded = 0;

    if (brief_output)
	printf(" cnt mdl grd prm    lvlf lev1 lev2  trf tr0 tr1 bits bms gds   npts header\n");

    while(1) {
	int bytes = get_prod(fp, timeout, &the_prod);
	if (bytes == 0)
	  continue;
	else
	  num_wmo_messages++;
	
	/* decode it into a grib1 product structure */
	gribp = grib_decode(&the_prod, quasp);
	if (gribp == 0) {
	    if (ep) {
		if (fwrite(the_prod.bytes, the_prod.len, 1, ep) == 0) {
		    serror("writing bad GRIB to error file");
		}
	    }
	    continue;
	}
	num_gribs_decoded++;

	/* Dump grib info to stdout */
	if(brief_output)
	    print_grib_line(gribp);
	else
	    print_grib(gribp, prec);
	free_product_data(gribp);
	num_gribs_written++;
    }
}


int
main(ac,av)
     int ac ;
     char *av[] ;
{
    extern int optind;
    char *logfname = "-" ;	/* log file name, default is stdout */
    FILE *fp = stdin;		/* default input */
    FILE *ep = 0;		/* file handle for bad GRIBS output, when
				   -e badfname used */
    int prec = DEFAULT_PRECISION; /* precision to use for printing floats */
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
	extern int opterr;
	extern char *optarg;
	int ch;
	int logmask = (LOG_MASK(LOG_ERR) | LOG_MASK(LOG_NOTICE)) ;
	int errflg = 0;
	
	opterr = 1;
	
        headers_only = 0;
        
	while ((ch = getopt(ac, av, "vxhbl:t:e:p:q:")) != EOF) {
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
	    case 'h':
		headers_only = 1;
		break;
	    case 'b':
		brief_output = 1;
		headers_only = 1;
		break;
	    case 'e':
		ep = fopen(optarg, "w");
		if(!ep) {
		    serror("can't open %s", optarg);
		    errflg++;
		}
		break;
	    case 'p':
		prec = atoi(optarg) ;
		if(prec < 1 || prec > 99) {
		    fprintf(stderr, "%s: invalid precision %s",
			    av[0], optarg) ;
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

    do {
	if (optind < ac) {	/* arguments still left, use as filenames */
	    fp = fopen(av[optind++], "r");
	    if(!fp) {
		serror("can't open %s", av[optind-1]);
		return 1;
	    }
	}
	do_dump(fp, ep, timeo, prec, quasp);
    } while (optind < ac);
    
    return 0;
}

