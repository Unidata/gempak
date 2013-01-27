/*
 *  GRIBTOGEM
 *
 *  GRIB to GEMPAK decoder based on gribtonc written by Russ Rew
 *  of the Unidata Program Center.  Gribtonc is Copyrighted 1993, 
 *  University Corporation for Atmospheric Research.  
 *
 *  Gribtogem takes input from standard in, decodes the GRIB
 *  edition 1 message, and writes out the data in GEMPAK gridded
 *  data format.  GEMPAK is the General Meteorological Package
 *  copyrighted by NASA, and currently under development by the
 *  National Meteorological Center.  GEMPAK is available by license
 *  to U.S. colleges and universities from the Unidata Program.
 *
 *  Log
 *  R.Rew/Unidata	5/95	Gribtonc
 *  P.Bruehl/Unidata	5/95	Removed netCDF, added GEMPAK, Created
 *				Gribtogem version 2
 *  Chiz/Unidata       10/95    Fixed calls to uinfo
 */


#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <unistd.h>		/* access() modes */
#include <limits.h>		/* _POSIX_PATH_MAX */
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "ulog.h"
#include "mkdirs_open.h"
#include "geminc.h"
#include "gemprm.h"

/* all netCDF stuff?
#include "nc.h"
#include "centers.h"
#include "models.h"
#include "params.h"
#include "levels.h"
#include "timeunits.h"*/
#include "grib1.h"
#include "product_data.h"
#include "quasi.h"
/*#include "units.h"*/

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
static void do_nc(FILE * ep, int to, quas*, char *cdlpath, char *ncpath);
static product_data* grib_decode(prod*, quas*);
int main(int ac, char** av);
#endif

#ifdef GEMPAK
#include <fcntl.h>
static int iflno = -1;
int gempak_pack;
#endif

unsigned long num_wmo_messages;     /* for statistics on exit */
unsigned long num_gribs_decoded;
unsigned long num_gribs_written;
unsigned long num_degribbed;

/*
 * Timeout in seconds.  If no input is received for this interval, process
 * closes output file and exits.
 */
#define DEFAULT_TIMEOUT  600
#define DEFAULT_GRIDS	1500

#ifdef UNDERSCORE
#define open_gem_grid	open_gem_grid_
#define put_gem_grid	put_gem_grid_
#endif

int maxgrids;
int pdsextoff=1;



/*
 * Called at exit.
 * This callback routine registered by atexit().
 */
static void
cleanup()
{
int iret;

    uinfo("Exiting") ;
    if(iflno > 0) gd_clos (&iflno, &iret);
    uinfo("%d WMO msgs, %d GRIBs decoded, %d written ",
	  num_wmo_messages, num_gribs_decoded, num_gribs_written);
#ifndef GEMPAK
    nccleanup();		/* close open netCDF files, if any */
#endif
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
		  "Usage: %s [options] output_file\t\nOptions:\n", av0);
    (void)fprintf(stderr,
		  "\t-h           Print the help file, then exit the program\n") ;
    (void)fprintf(stderr,
		  "\t-v           Verbose, report decoding steps\n") ;
    (void)fprintf(stderr,
		  "\t-x           Debug mode\n") ;
    (void)fprintf(stderr,
		  "\t-d logfile   Send log info to file (default uses syslogd)\n") ;
    (void)fprintf(stderr,
		  "\t-t timeout   If no input, exit after \"timeout\" seconds (default %d)\n",
		  DEFAULT_TIMEOUT) ;
    (void)fprintf(stderr,
                  "\t-g GEMTBL directory, if environmental variable not found\n");
    (void)fprintf(stderr,
                  "\t-m Maximum number of grids in file (default %d)\n",DEFAULT_GRIDS);
    (void)fprintf(stderr,
                  "\t-z Strip off ensemble pds extension from variable name\n");
    (void)fprintf(stderr,
		  "\t-e errfile   Append bad GRIB products to this file\n") ;
    (void)fprintf(stderr,
		  "\t-q meth      Specs for expanding quasi-regular grids,\n\t\t\te.g. -q \"lin,dlat=2.5,dlon=5.0\"\n") ;
#ifdef GEMPAK
    (void)fprintf(stderr,
                  "\t[ PACK ]     write in packed format\n");
    (void)fprintf(stderr,
                  "\toutput_file  GEMPAK output file\n\n");
     (void)fprintf(stderr,
        "\tA template may be used to specify the output file name.  The file\n");
     (void)fprintf(stderr,
        "\tname template uses the date, time, and grid number of the grib product\n");
     (void)fprintf(stderr,
        "\tto replace the following characters.\n\n");
     (void)fprintf(stderr,"\t\tYY              Year without the century\n");
     (void)fprintf(stderr,"\t\tMM              Month number\n");
     (void)fprintf(stderr,"\t\tDD              Day\n");
     (void)fprintf(stderr,"\t\tHH              Hour\n");
     (void)fprintf(stderr,"\t\tNN              Minute\n");
     (void)fprintf(stderr,"\t\t@@@             Grid Number\n");
     (void)fprintf(stderr,"\t\t###             Model Number\n");
  

#else
    (void)fprintf(stderr,
		  "\t[ CDL_file ] CDL template, when netCDF output file does not exist\n") ;
    (void)fprintf(stderr,
		  "\tnetCDF_file  netCDF output file\n") ;
#endif
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

    if (!pdp) return 0;
    if (pdp->gd->quasi && quasp) {
	int ret = expand_quasi(quasp, pdp) ; /* Changes *pdp */
	if (!ret) {
	    uerror("%s: can't expand quasi-regular grid\0", pdp->header);
	    return 0;
	}
    }
    return pdp;
}


static void do_nc (ep, timeout, quasp, cdlname, ncname)
    FILE *ep;			/* if non-null, where to append bad GRIBs */
    int timeout;		/* exit if no data in this many seconds */
    quas *quasp;		/* if non-null, specification for how
				   quasi-regular "grids" are to be expanded */
    char *cdlname;		/* Pathname of CDL template file to be used to
				   create netCDF file, if it doesn't exist */
    char *ncname;		/* Pathname of netCDF output file */
{
    struct prod the_prod;
    struct product_data *gribp;	/* UPC structure */
    FILE *fp = stdin;		/* input */

    char filnam[132],oldfil[132],dattim[20],*cpos;
    int iret;
    int center,gedit,code;

    
#ifndef GEMPAK
    ncfile *ncp;
    int ncid;
    ncid = cdl_netcdf(cdlname, ncname);	/* get netCDF file handle */
    setncid(ncid);		/* store ncid so can be closed if interrupt */

    if (init_udunits() != 0) {
	uerror("can't initialize udunits library, exiting");
	exit(-1);
    }
    ncp = new_ncfile(ncname);
    if (!ncp) {
       uerror("can't create output netCDF file %s, exiting", ncname);
       exit(-1);
    }
#endif

    num_wmo_messages = 0; 
    num_gribs_decoded = 0;
    oldfil[0] = '\0';
    center = -1; gedit = -1;

    while(1) {			/* usual exit is timeout in get_prod() */
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
#ifdef GEMPAK
        {int cnt, mdl, gid, prm, ltyp, lvl1, lvl2, gx, gy, gribed, yr, ccyr;
         int mo, da, hr, mn, tun, tr1, tr2, tflg, bits, pckflg, edition;
	 int idtarr[5];
         char ensext[33],*dirp,stripped[PATH_MAX+1];
         int ensextlen,ensparm,has_gds;
         float lat1, lon1, lat2, lon2;
         float *dp,*fg;
         char wmotbl[80],ctbl[80],vtbl[80],ntbl[80];
         int code;
         int dirlen;
         mode_t dirmode=S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH;
      
/*   Break out each product element from the product structure  */

        prm  = gribp->param ;
        ensext[0] = '\0'; ensextlen = 0;
        if((gribp->pdslen > 40)&&(pdsextoff != 0))
           {
           gempds(gribp->pdsloc,ensext,&ensparm);
           ensextlen = strlen(ensext);
           if(ensparm != gribp->param) prm = ensparm;
           }
           
	lat1 = gribp->gd->grid.ll.la1;
	lat2 = gribp->gd->grid.ll.la2;
	lon1 = gribp->gd->grid.ll.lo1;
	lon2 = gribp->gd->grid.ll.lo2;

         cnt  = gribp->center ; 
         gid  = gribp->grid ; 
         ltyp = gribp->level_flg ; 
         lvl1 = gribp->level[0] ; 
         lvl2 = gribp->level[1] ; 
         /* year 2000 is year=100, century=20 , but don't take chances if year == 0*/
         yr   = gribp->year ; 
         if(yr > 0) 
	    ccyr = (gribp->century - 1)*100 + yr ; 
         else
            ccyr = gribp->century * 100 + yr ;
         mo   = gribp->month ; 
         da   = gribp->day ; 
         hr   = gribp->hour ; 
         mn   = gribp->minute ; 
         tun  = gribp->tunit ; 
         tr1  = gribp->tr[0] ; 
         tr2  = gribp->tr[1] ;
         tflg = gribp->tr_flg ;
         gx   = gribp->cols ;
         gy   = gribp->npts / gx;
         dp   = gribp->data;
         bits = gribp->bits;
         gribed = gribp->edition;
         edition = gribp->param_vers;
         mdl  = gribp->model;
         has_gds = gribp->has_gds;


         if(has_gds == 1) gemgds(gribp->pdsloc + gribp->pdslen);

         fg = NULL;
         switch(gribp->gd->type)
            {
            case 201:
               /*grid ids 94, 96, 99, 192, 196*/
                 fg = (float *)stagger_fill(dp, &gx, &gy, prm, gid,gribp->gd->scan_mode);
                 dp = fg;
                 break;
            case 203:
               /* grid ID 192, 190*/
               fg = (float *)stagger_fill_e(dp, &gx, &gy, prm, gid,gribp->gd->scan_mode,
                  gribp->gd->grid.ll.ni,gribp->gd->grid.ll.nj,(unsigned char *)(gribp->pdsloc + gribp->pdslen));
               dp = fg;
               break;

            }
 
         idtarr[0] = ccyr;
         idtarr[1] = mo;
         idtarr[2] = da;
         idtarr[3] = hr;
         idtarr[4] = mn;
         memset(dattim,0,sizeof(dattim));
         ti_itoc(idtarr,dattim,&iret,sizeof(dattim)-1);
         iret = sizeof(dattim) - 2;
         while((iret >=0)&&(dattim[iret] < ' '))
            {
            dattim[iret] = '\0';
            iret--;
            }
         
         filnam[0] = '\0'; 
         strcpy(filnam,ncname);
         cfl_mnam (dattim,ncname,filnam,&iret);
         if((cpos = (char *)strstr(filnam,"@@@")) != NULL)
            {
            cpos[0] = 48 + (gid / 100);
            cpos[1] = 48 + ((gid / 10)%10);
            cpos[2] = 48 + (gid % 10);
            }
         if((cpos = (char *)strstr(filnam,"###")) != NULL)
            {
            cpos[0] = 48 + (mdl / 100);
            cpos[1] = 48 + ((mdl / 10)%10);
            cpos[2] = 48 + (mdl % 10);
            }
         if(strcmp(filnam,oldfil) != 0)
            {
            if(iflno != -1)
               {
               gd_clos (&iflno, &iret);
               uinfo("closing %s %d\0",oldfil,iret);
               iflno = -1;
               }
            strcpy(oldfil,filnam);
            }
         
         if (iflno == -1) {
            /* make sure directories to file exist */
            if((dirp = strrchr(filnam, '/'))!= NULL) /* a directory path is used */
               {
               dirlen = dirp - filnam;
               if(dirlen >= PATH_MAX)
                  {
                  serror("name too long %s",filnam);
                  exit(1);
                  }
               memcpy(stripped, filnam, dirlen);
               stripped[dirlen] = 0;
               mkdirs(stripped, (dirmode | 0111));
               }
               

/*  open a new file, if needed */

           if (diraccess(filnam,  (R_OK | W_OK), !0) == -1) {
               serror("can't access directories leading to %s", filnam);
               exit(1);
               }

           if ( (iflno = open_gem_grid (filnam, &cnt, &mdl, &gid, 
		&gribed, &has_gds, &gx, &gy, &lat1, &lon1, 
                &lat2, &lon2, &maxgrids, strlen(filnam)) ) == -1) {
                    serror("can't open file %s", filnam);
                    exit(1);
                    }
           else
               uinfo("Opened %s %d %d",filnam,mdl,gid);

            }

            if((cnt != center)||(gribed != gedit)||(edition != code))
               {
               center = cnt;
               gedit = gribed;
               na_init (&iret);
               code = edition;
               wmotbl[0] = ntbl[0] = vtbl[0] = ctbl[0] = '\0';
               strncat(wmotbl,"?",1);
               strncat(ntbl,"?",1);
               strncat(vtbl,"?",1);
               strncat(ctbl,"?",1);
               na_rtbl(&gedit,&center,&code,wmotbl,ntbl,vtbl,ctbl,&iret);
               if(iret == 0)
                  uinfo("read grib tables %d %d %d %d\0",iret,center,gedit,code);
               else
                  {
                  uerror("can't open grib tables, set GEMTBL %d\0",iret);
                  exit(2);
                  }
               }

/*... and send data off to the storing routine  */

            if (put_gem_grid (&iflno, &cnt, &gid, &edition, &has_gds, &prm,
                   &ltyp, &lvl1, &lvl2, &ccyr, &mo, &da, &hr, &mn, 
                   &tun, &tr1, &tr2, &tflg, &gx, &gy, dp, &bits, 
                   &gribed, &gempak_pack, &lat1, &lon1, &lat2, 
                   &lon2, ensext, &ensextlen) != -1) {
                if(strlen(gribp->header) <= 5)
                   uinfo("%-4s OK -- %s Grid ID %4d %3d %3d %3d %3d\0",
                   gribp->header,dattim,gid,prm,ltyp,lvl1,lvl2);
                else
                   if(strncmp(gribp->header,"header not found",16)!=0)
                      uinfo("%s OK\0",gribp->header);
                   else
                      uinfo("%4d OK -- %s Grid ID %4d %3d %3d %3d %3d\0",
                      num_gribs_written,dattim,gid,prm,ltyp,lvl1,lvl2);
                num_gribs_written++;
                }
            else {
                uerror("%s skipped %s Grid ID %4d %3d %3d %3d %3d\0",
                   gribp->header,dattim,gid,prm,ltyp,lvl1,lvl2);
                uinfo("filnam %s old %s\0",filnam,oldfil);
                }

        if(fg != NULL) free(fg);
        }
#else
	/* Write grib info to netCDF file */

        if (nc_write(gribp, ncp) == 0) {   /* OK */
            num_gribs_written++;
        }
#endif
	free_product_data(gribp);
    }
}


/*
 * Reads GRIB data from standard input.
 * GRIB data may be contained in WMO envelope or not.
 * Decodes GRIB data to in-memory structure.
 * Writes decoded data that conforms to GEMPAK gridded data file or
 * one specified netCDF output file.
 * May be used as an in-line LDM decoder, invoked by pqact(1).
 */
int
main(ac,av)
     int ac ;
     char *av[] ;
{
    char *logfname = 0 ;	/* log file name, default uses syslogd */
    char *ofile = 0 ;		/* output GEMPAK or netCDF file name */
    char *cdlfile = 0 ;		/* CDL template file name or Null string */
    FILE *ep = 0;		/* file handle for bad GRIBS output, when
				   -e badfname used */
    int timeo = DEFAULT_TIMEOUT ; /* timeout */
    quas *quasp = 0;		/* default, don't expand quasi-regular grids */
    int ret;
    char GEMTBL[133];

    maxgrids = DEFAULT_GRIDS;


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
	
	while ((ch = getopt(ac, av, "hvxzd:t:e:q:g:m:")) != EOF) {
	    switch (ch) {
	    case 'h':
		usage(av[0]);
                break;
	    case 'v':
		logmask |= LOG_MASK(LOG_INFO) ;
		break;
	    case 'x':
		logmask |= LOG_MASK(LOG_DEBUG) ;
		break;
	    case 'd':
		logfname = optarg ;
		break;
	    case 'g':
                sprintf(GEMTBL,"GEMTBL=%s\0",optarg);
                if(putenv(GEMTBL) != 0)
                   serror("Can't set %s environmental variable",GEMTBL);
                break;
	    case 'm':
		maxgrids = atoi(optarg) ;
		if((maxgrids < 1)||(maxgrids > MMHDRS))
                   {
                   fprintf(stderr, "%s: invalid number of grids %s, max = %d\n",
                      av[0], optarg, MMHDRS);
                   errflg++;
                   }
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
            case 'z':
                pdsextoff = 0;
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
#ifdef GEMPAK
	    gempak_pack = strcmp("PACK", av[optind])==0;
#else
	    cdlfile = av[optind] ;
#endif
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

#ifdef GEMPAK
    if (fcntl(1, F_GETFD, 0) == -1 && errno == EBADF) {
        open("/dev/null", O_WRONLY);
    }
    if (fcntl(2, F_GETFD, 0) == -1 && errno == EBADF) {
        open("/dev/null", O_WRONLY);
    }
   /*
    *  Call IN_BDTA to initialize GEMPAK block data
    *  careful re: C/FORTRAN interface
    */
    in_bdta (&ret);


#endif


    do_nc(ep, timeo, quasp, cdlfile, ofile);
    
    return 0;
}

#ifdef GEMPAK
/*
 *  Advise - called to record message
 *  Careful re: C/FORTRAN interface
 */
#ifdef UNDERSCORE
   void advise_ ( mess, len )
#else
   void advise ( mess, len )
#endif
    char *mess;
    int len;
{
    char *b, *r;
    int i;

    r = (char *) malloc(len*sizeof(char));
    for(i=0 ; i < len; ++i)
        if ((r[i] = mess[i]) != ' ') b = r+i;
    *(++b) = '\0';
    unotice(r);
    free(r);
}
#endif

