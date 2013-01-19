#include "geminc.h"
#include "gemprm.h"
#include "imgdef.h"
#include "AODT/v72/odtapi.h" 

/* Default image filename                                       */
#define DEFIFILE        "NONE"
/* Default cpf filename                                         */
#define DEFCFILE        "NONE"
/* Default output filename                                      */
#define DEFOFILE        "SCREEN"
/* Default history filename                                     */
#define DEFYFILE        "AODTDUMP.AODTv72"
/* Dummy date                                                   */
#define DATEDUMMY       "0000XXX00"
/* List of valid options processed by getopt                    */
#define OPTSTRING       "a:c:d:f:i:ehlm:n:o:s:v:y:"

/* Cloud and Eye scene types					*/
static char scenetype[][16] = {"EYE", "PINHOLE_EYE", "LARGE_EYE",
                               "UNIFORM_CDO", "EMBEDDED_CENTER",
                               "IRREGULAR_CDO", "CURVED_BAND", "SHEAR"};

/************************************************************************
 * aodtv72drive.c                                                       *
 *                                                                      *
 * CONTENTS:                                                            *
 * aodtv72_drive                                                        *
 * aodtv72_exit                                                         *
 ***********************************************************************/

/*
 *  Prototypes for aodtv72_drive and aodtv72_exit
 */

void aodtv72_runautomode ( int fcsttype, char *fcstfile, char *imagefile,
                      float *cenlat, float *cenlon, int *posm );
int aodtv72_drive( int argc, char **argv );
void aodtv72_exit (char *errormsg);

int aodtv72_drive( int argc, char **argv )
/************************************************************************
 * aodtv72_drive							*
 *									*
 * This program performs the Advanced Objective Dvorak Technique, V6.4	*
 *  From: Chris Velden, Tim Olander                                     *
 *  University of Wisconsin - Madison                                   *
 *  Cooperative Institute for Meteorological Satellite Studies          *
 *                                                                      *
 * OPTIONS, passed in as an input string:                               *
 *                                                                      *
 * -a   n,file  Run AODT intensity estimation in automated mode.        *
 *                                                                      *
 *      note: Cannot be used with the "-c" option.                      *
 *      n = 0 - ATCF Tropical Cyclone Forecast Record files.            *
 *          1 - TPC WTNT4?  or WTPZ3? discussion files                  *
 *          2 - JTWC WTPN3? TROPICAL CYCLONE WARNING file.              *
 *          3 - Generic format file.                                    *
 *      file = Name of the input file.                                  *
 *									*
 * -c   fn      NMAP2 cursor point file filename (must be *.cpf)        *
 *              containing storm center location                        *
 *              (NOTE: seperator in cpf file is a semicolon, NOT comma) *
 *      -or-    -OR-                                                    *
 *    clat,clon Storm center location in the format "clat,clon"         *
 *              (NOTE: clon negative west, no whitespace allowed,       *
 *              seperator is a comma, NOT a semicolon)                  *
 *      Note:  Cannot be used with the "-a" option.                     *
 *                                                                      *
 * -d  date1/time1/date2/time2                                          *
 *              (McIDAS keyword option "DATE")                          *
 *              Range of dates/times desired (same fmt as history file) *
 *              Enter "date" as YrMonDay (e.g. 97OCT05)                 *
 *              Enter "time" as HHMMSS   (e.g. 131500)                  *
 *              Defaults :                                              *
 *                LIST (-l option):                                     *
 *                  date1/time1 : first record                          *
 *                  date2/time2 : last record                           *
 *                DELETE (-e option):                                   *
 *                  date1/time1 : no defaults, must specify             *
 *                  date2/time2 : date1/time1                           *
 *                COMMENT (-n option):                                   *
 *                  date1/time1 : no defaults, must specify             *
 *                  date2/time2 : date1/time1                           *
 *                                                                      *
 * -e    --     Allows for manual deletion of a date or a range of dates*
 *              (use in conjuction with -d option).                     *
 *                                                              	*
 * -f   f1,f2   Flags to allow intensity analysis over land, f1,        *
 *              ( 0 = don't allow, 1 = allow)                           *
 *              and to set max log spiral search flag, f2               *
 *              ( 0 = search OFF, 1 = search ON )                       *
 *              (default = 0, 0)                                        *
 *                                                                      *
 * -h           Display this help message                               *
 *                                                                      *
 * -i   nf,nv   Allows for manual settings of  initial classification   *
 *              flag, nf,                                               *
 *              ( 0 = value not set, 1 = value set to nv )              *
 *              and initial classification value, nf                    *
 *              (default = 0, RMISSD)                                   *
 *                                                                      *
 * -l   --      (McIDAS keyword option "LIST")                          *
 *              List contents of history file (-y fn option) to text    *
 *              window.  (default=NO)                                   *
 *                                                                      *
 * -m   "AUTO"  (McIDAS keyword option "DOMAIN")                        *
 *      -or-    Atlantic or West Pacific T# and CI intensity estimate & *
 *      "ATL"   related pressure/wind value.  For East Pac, user must   *
 *      -or-    decide which domain is correct to use for pressure.     *
 *      "PAC"   (default "AUTO")                                        *
 *                                                                      *
 * -n  "comment" Comment to be added to a line in the history file.     *
 *               (for version 6.4 and later)                            *
 *                                                                      *
 * -o   fn      (McIDAS keyword option "OUTPUT")                        *
 *              output to the text file "fn"				*
 *              (omitting this option results in printing to screen).   *
 *                                                                      *
 * -s   scene   (replaces McIDAS keyword option "OVER")                 *
 *              Allow user to manually override AODT scene              *
 *              identification and/or automated center positioning.     *
 *                                                                      *
 * -v  version  Version of the AODT library                             *
 *              (default = first entry in tble aodtvers.tbl)            *
 *                                                                      *
 * -y   fn      (McIDAS keyword option "HISTORY")                       *
 *              File to which the history record will be written.       *
 *                                                                      *
 *                                                                      *
 * image_filename        IR image filename                          	*
 *                                                                      *
 * REMARKS								*
 *									*
 * - Use of default history file will only provide user			*
 *   with CI value (which is actually the RAW T#).			*
 *   NO TIME AVERAGING OR OTHER ADJUSTMENTS WILL BE PERFORMED,		*
 *   and any output will only list this CI (raw T#) value.		*
 * - Use of default history file is only possible when AODT 		*
 *   analysis is performed 						*
 *   (ie, LIST and/or DELETE options disabled).				*
 * - If either LIST=YES or DELETE=YES					*
 *   AODT analysis will NOT be performed on current image.		*	
 * - If a record is modified/added within an existing history		*
 *   file, all subsequent records within the history file		*
 *   will be recalculated using the new/modified AODT record.		*
 * - If DATE is provided in conjunction with LIST *AND*			*
 *   DELETE keywords, DATE will apply ONLY to the DELETE		*
 *   keyword (and a LIST will be drawn/listed for			*
 *   the entire history file (defaults)).				*
 *                                                                      *
 * aodtv72_drive   ( argc, argv )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  argc   int      number of parameters of command line                *
 *  argv   char**   parameter array of command line                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **									*
 * Log:									*
 * M. Li/SAIC		12/06	Modified from aodtv64drive.c		*
 * M. Li/SAIC           01/07   Added audomated mode                    *
 * M. Li/SAIC		01/07	Get correct lat/lon from .cpf file	*
 * S. Gilbert/NCEP      01/07   Removed recalculation of lat/lons       *
 ***********************************************************************/
{
    int         one=1, mode=1, istat, idate[3], jyear, jday;
    int     	imark=1, imkhw=0, imkwid=3, process;
    int		pagflg, curdate, curtime, cursat, landflag, searchflag, inclassflag;
    int         g_oland, g_osearch, g_classf;
    int         g_posf, g_domain, g_imdate, g_imtime, g_sat;
    int         origeye = 0, origcloud = 0;
    int 	s_neweye, s_newcloud, eyeSize;
    int         g_neweye, g_newcloud, g_oldeye, g_oldcloud, g_eyesize; 
    int         indx, n_records, h_modified, h_records, h_deleted, n_comm;
    int         iaodt, ier, ier2, iret, len, lauto, nauto, posm;
    int         radius, irad, np, ii, jj, numx, numy, length;
    int     	lodt, idomain, ldump, loverride, ldelete, llist, lcomment, ltmp;
    int     	time1, time2, itype;
    size_t	ifl, ipr;
    long	ifilesize;
    float   	szmark=3.0F;
    float	g_class;
    float       g_lat, g_lon;
    float   	cenlat, cenlon, inclassvalu, templat[2], templon[2];
    float       *ftmps, *flats, *flons, cenlon2;
    float       **temps, **lats, **lons;
    char    	prog[12], g_csat[20];
    char    	device[256], cq[20];
    char        imagefile[128], imagefname[128], historyfile[FILE_FULLSZ];
    char      	dumpfile[128], miscflags[3], initflags[3];
    char    	cpf[128], cpfname[128], clrbar[80], cauto[128], fauto[128];
    char    	latlon[32], result[16], *cptr;
    char	bulletin[5000]="\0", listing[5000]="\0";
    char        retmsg[10000]="\0", infomsg[5000]="\0";
    char    	cdomain[8], cdate[128], overridescene[16];
    char    	date1[20], date2[20];
    char	comment[128], srcID[512], strmID[512];
    char        hist_dir[FILE_FULLSZ], temp_dir[FILE_FULLSZ], compfile[FILE_FULLSZ];
    Boolean     can_read, can_write;

    FILE	*fdump;
    struct odtdata *historyrec;

   /*
    * These variables are used by getopt. Unset the error reporting.
    */
    int             ch, errflg;

/*---------------------------------------------------------------------*/
    iaodt  = 0;
    radius = 225;
    pagflg = G_FALSE;

    lodt        = G_TRUE;
    ldelete     = G_FALSE;
    ldump       = G_FALSE;
    llist       = G_FALSE;
    loverride   = G_FALSE;
    lcomment	= G_FALSE;
    cenlat      = RMISSD;
    cenlon      = RMISSD;
    inclassvalu = RMISSD;
    time1       = -1;
    idomain     = 0;
    landflag    = 0;
    searchflag  = 0;
    inclassflag = 0;
    time2       = time1;
    strcpy ( historyfile, DEFYFILE );
    strcpy ( imagefile,   DEFIFILE );
    strcpy (       cpf,   DEFCFILE );
    strcpy (     date1,   DATEDUMMY );
    strcpy (     date2,   date1 );
    opterr = 1;
    errflg = 0;
    optopt = 0;
    optind = 1;
    optarg = NULL;
    ch = 0;
    opterr = 1;
    errflg = 0;
    lauto  = G_FALSE;

    /*
     * Retrieve the storage directory for history files.
     */
    ctb_rdprf ( "prefs.tbl", "config", "AODT_HIST_DIR", hist_dir, &ier );
    cst_rmbl ( hist_dir, hist_dir, &length, &ier );
    if ( hist_dir[length-1] != '/' ) strcat ( hist_dir, "/" );
    strcpy ( compfile, hist_dir );
    strcat ( compfile, DEFYFILE );

    /*
     * Save the program name.
     */
    strcpy ( prog, "AODT" );

    /*
     * Get the options and set the appropriate flags.
     */
    while ( ( ch = getopt ( argc, argv, OPTSTRING ) ) != EOF ) {
        switch ( ch ) {

	    case 'a':                           /* Automated mode       */

                lauto = G_TRUE;
                strcpy ( cauto, optarg );
                break;
    
            case 'c':                           /* cpf file options     */

                strcpy ( cpf, optarg );
                break;

            case 'd':                           /* date option          */
                                                /* use w/ -l option     */
                                                /* or  -e option        */

                if ( ldelete == G_TRUE )  {
                    strcpy ( date1, DATEDUMMY );
                    time1 = -1;
                    strcpy ( date2, date1 );
                    time2 = time1;
                }
                else  {
                    strcpy ( date1, DATEDUMMY );
                    time1 = -1;
                    strcpy ( date2, DATEDUMMY );
                    time2 = -1;
                }
                strcpy ( cdate, optarg );
                cptr = cst_split ( cdate, '/', sizeof(date1), date1, &ier );
                if ( cptr != (char *)NULL )  {
                 cptr = cst_split ( cptr, '/', sizeof(cq), cq, &ier );
                 cst_numb ( cq, &time1, &ier );
                }
                if ( cptr != (char *)NULL )
                 cptr = cst_split ( cptr, '/', sizeof(date2), date2, &ier );
                if ( cptr != (char *)NULL )  {
                 cptr = cst_split ( cptr, '/', sizeof(cq), cq, &ier );
                 cst_numb ( cq, &time2, &ier );
                }
                break;

            case 'e':                           /* delete option        */
               
                lodt = G_FALSE;
                ldelete = G_TRUE;
                break;

            case 'f':                           /* misc options flags   */

                strcpy ( miscflags, optarg );
                cptr = cst_split ( miscflags, ',', sizeof(result), result, &ier );
                landflag = atoi(result);
                searchflag = atoi(cptr);
                break;

            case 'h':                           /* help                 */

                ip_help ( prog, &pagflg, &ier, strlen(prog) );
                exit (0);
                break;

            case 'i':                           /* initial flag, value  */

                strcpy ( initflags, optarg );
                cptr = cst_split ( initflags, ',', sizeof(result), result, &ier );
                inclassflag = atoi(result);
                inclassvalu = atof(cptr);
                break;

            case 'l':                           /* list option          */

                lodt  = G_FALSE;
                llist = G_TRUE;
                break;

            case 'm':                           /* domain spec          */
                                                /* "auto", "atl", "pac" */
                strcpy ( cdomain, optarg );
                cst_lcuc ( cdomain, cdomain, &ier );
                if ( strcmp(cdomain,"AUTO") == 0 )  idomain = 0;
                if ( strcmp(cdomain,"ATL")  == 0 )  idomain = 1;
                if ( strcmp(cdomain,"PAC")  == 0 )  idomain = 2;
                break;

	    case 'n':				/* comment		*/
		
		lcomment = G_TRUE;
		lodt     = G_FALSE;  
		strcpy ( comment, optarg );		
		break;

            case 'o':                           /* output option        */
                                                /* use w/ -l option     */
                lodt  = G_FALSE;
                llist = G_TRUE;
                strcpy ( dumpfile, optarg );
                if ( strcmp(dumpfile,DEFOFILE) != 0 )  ldump = G_TRUE;
                break;

            case 's':                           /* override scene       */

                loverride = G_TRUE;
                strcpy ( overridescene, optarg );
                cst_lcuc ( overridescene, overridescene, &ier );
                break;

            case 'v':                           /* version option       */

                break;

            case 'y':                           /* history file spec    */

                strcpy ( historyfile, optarg );
                break;

            case '?':                           /* unknown option       */

                errflg++;
                break;
        }
    }

    strcpy ( temp_dir, hist_dir ); 
    strcat ( temp_dir, historyfile );
    strcpy ( historyfile, temp_dir );


    /*
     * Adjust the number of arguments left on the command line.
     */
    argc -= optind;
    argv += optind;

    /*
     * If there are invalid parameters, display the usage message and exit.
     */
    if  ( errflg > 0 )  {
        ip_help ( prog, &pagflg, &ier, strlen(prog) );
        exit (1);
    }
    if ( lodt == G_TRUE )  {

    /*
     *  Check if imagefile was specified.
     */
    if ( strcmp(imagefile,DEFIFILE) == 0 )  {
        if ( argc != 1 )  {
            ip_help ( prog, &pagflg, &ier, strlen(prog) );
            exit (0);
        }
        else  {
            strcpy ( imagefile, argv[0] );
            cfl_inqr ( imagefile, NULL, &ifilesize, imagefname, &ier );
            if ( ier != 0 )  {
                printf("Invalid image filename %s\n", imagefile );
            }
        }
    }
    /*
     *  Either cfp (-c) option or auto (-a) option must be specified (or both).
     */
    if ( (strcmp(cpf,DEFCFILE) == 0 && lauto == G_FALSE) ||
         (strcmp(cpf,DEFCFILE) != 0 && lauto == G_TRUE) )  {
        printf( " -c (manual lat,lon information) or -a (automated) option"
                " must be specified. Both can not be specified together\n" );
        exit (0);
    }

    /*
     *  Option -c on command line is either
     *  1) NMAP2 cpf file containing center lat and lon, or
     *  2) center paired lat and lon in format "lat,lon", lon west neg, or
     */
    if ( strcmp(cpf,DEFCFILE) != 0 ) {
        cfl_inqr ( cpf, NULL, &ifilesize, cpfname, &ier );
        if ( ier == 0 )  {
            if ( strcmp(&(cpfname[strlen(cpfname)-4]),".cpf") != 0 )  {
                printf("Invalid cpf filename %s\n", cpf );
                exit (0);
            }
            else  {
                /*
                 *  Valid filename - extract cenlat and cenlon from cpf file.
                 */
		ctb_rdcpf ( cpfname, &np, templat, templon, &ier );
                cenlat = templat[0];
                cenlon = templon[0];
            }
        }
        else  {
            /*
             *  Not a valid filename; check if valid paired lat and lon.
             */
            process = G_TRUE;
            cptr = cst_split ( cpf, ',', sizeof(result), result, &ier );
            if ( ier != 0 || cptr == (char *)NULL )  process = G_FALSE;
            if ( process == G_TRUE )  {
                cst_crnm ( result, &cenlat, &ier );
                if ( ERMISS(cenlat) )  process = G_FALSE;
            }
            if ( process == G_TRUE )  {
                cst_crnm ( cptr, &cenlon, &ier );
                if ( ERMISS(cenlon) )  process = G_FALSE;
            }
            if ( process == G_FALSE )  {
                printf("Invalid or missing value for option -c %s\n", cpf );
                exit (0);
            }
        }
    }

    if ( lauto == G_TRUE ) {
        process = G_TRUE;
            cptr = cst_split ( cauto, ',', sizeof(result), result, &ier );
            if ( ier != 0 || cptr == (char *)NULL )  process = G_FALSE;
            if ( process == G_TRUE )  {
                cst_numb ( result, &nauto, &ier );
                if ( nauto < 0 || nauto > 3 )  process = G_FALSE;
            }
            if ( process == G_TRUE )  {
                strcpy ( fauto, cptr );
                if ( strlen(fauto) <= 0 )  process = G_FALSE;
            }
            if ( process == G_FALSE )  {
                printf("Invalid or missing value for option -a %s\n", cauto );
                exit (0);
            }
    }

    ginitp ( &mode, &istat, &iret );
    if ( iret != 0 )  {
        printf("GINITP failed...iret=%d\n", iret );
        exit (0);
    }

    im_simg ( "sat", imagefile, &ier, strlen("sat"), strlen(imagefile) );
    gsicmn ( &ier );
    sprintf( device, "xw|AODT|%s", imagefile );
    gg_sdev ( device, &iret, strlen(device) );
    if ( iret != 0 )  {
        printf("GSDEVA failed...iret=%d\n", iret );
        exit (0);
    }

    im_drop ( &ier );
    strcpy (clrbar, "1/V/LL/0;.05/.90");
    im_cbar ( clrbar, &ier, strlen(clrbar) );

    strcpy ( latlon, "32////10;10" );
    gg_ltln ( latlon, &ier, strlen(latlon) );
    gsmrkr ( &imark, &imkhw, &szmark, &imkwid, &ier );
    gmark ( sys_M, &one, &cenlat, &cenlon, &ier, strlen(sys_M) );
    geplot ( &ier );


    /*
     *  AODT calculations expect longitude to be west positive.
     */
    cenlon *= -1.0F;
    idate[0] = imdate/100/100;
    idate[1] = (imdate/100) % 100;
    idate[2] = imdate % 100;
    ti_itoj ( idate, &jyear, &jday, &ier );
    curdate = (jyear%100)*1000 + jday;
    curtime = imtime;

    }
    else  {
       if ( strcmp(date1,DATEDUMMY)==0 )  {
           printf("DATE required with -e or -l or -n options.\n");
           exit ( 0 );
       }
       if ( strcmp(historyfile,compfile)==0 )  {
           printf("NO DEFAULT HISTORY FILE allowed with -e or -l or -n options.\n");
           exit ( 0 );
       }
       if ( llist==1 && strcmp(date2,DATEDUMMY)==0 )  {
           printf("Both start and end date required with -l option.\n");
           exit ( 0 );
       }
    }

    if ( lodt == G_TRUE ) 
      printf("Starting AODTV64 image processing with the following options:\n"
        "\tIMAGE FILE = \"%s\", DATE/TIME = %d/%d\n"
        "\tCENTER latitude/longitude (pos west) = (%6.2f,%7.2f)\n"
        "\tDOMAIN = %d\n"
        "\tANALYSIS OVER LAND FLAG = %d\n"
        "\tMAX LOG SPIRAL FLAG     = %d\n"
        "\tINITIAL CLASSIFICATION FLAG  = %d\n"
        "\tINITIAL CLASSIFICATION VALUE = %f\n",
        imagefile, curdate, curtime, cenlat, cenlon,
        idomain, landflag, searchflag, inclassflag, inclassvalu);
    else
      printf("Calling AODT for historyfile list or delete only:\n");

    printf("\tHISTORY FILE = %s\n", historyfile );


    if ( lodt == G_TRUE )  {
      loverride == G_TRUE
        ? printf("\tOVERRIDE SCENE = TRUE (new scene=%s)\n", overridescene)
        : printf("\tOVERRIDE SCENE = FALSE\n");
    }
    else  {
      llist == G_TRUE
        ? printf("\tLIST = TRUE, OUTPUT FILE = %s\n",
                ldump==G_FALSE ? "SCREEN" : dumpfile )
        : printf("\tLIST = FALSE\n");

      ldelete == G_TRUE
        ? printf("\tDELETE = TRUE\n" )
        : printf("\tDELETE = FALSE\n" );
    }

    if ( ldelete == G_TRUE || llist == G_TRUE )
        printf("\tDATE = %s/%d %s/%d\n", date1, time1, date2, time2 );

 /*
  *  Set miscoptions flags in AODT
  */
     eyeSize = -99;
     iaodt=aodtv72_setmiscoptions ( landflag, searchflag, eyeSize );
 /*
  *  Get miscoptions flags from AODT
  */
     iaodt=aodtv72_getmiscoptions ( &g_oland, &g_osearch, &g_eyesize );
 /*
  *  Set initial classification flag and value in AODT
  */
     iaodt=aodtv72_setstartstr ( inclassflag, inclassvalu );
 /*
  *  Get initial classification flag and value from AODT
  */
     iaodt=aodtv72_getstartstr ( &g_classf, &g_class );
 /*
  *  Initialize AODT current structure element
  */
     iaodt = aodtv72_initialize ( );
 /*
  * Set history filename in AODT
  */
     if (strlen(historyfile) > (size_t)0 ) {
         iaodt=aodtv72_sethistoryfile ( historyfile );
         if ( iaodt != 0 ) {
            aodtv72_qmessage (iaodt, 0, historyfile, retmsg);
            if ( iaodt < 0 ) {
               aodtv72_exit (retmsg);
            }
         }
        /*
         *   Get history filename from the AODT
         */
         iaodt=aodtv72_gethistoryfile ( historyfile );
         if ( iaodt != 0 ) {
            aodtv72_qmessage (iaodt, 0, NULL, retmsg);
            if ( iaodt < 0 ) {
               aodtv72_exit (retmsg);
            }
         }
     }
    /*---------------------------------------
     *   Start AODT intensity analysis
     **/
     if ( lodt ) {
        /*
         *  Set image date/time info in AODT
         */
	 cursat = -1;
         iaodt = aodtv72_setIRimageinfo ( curdate, curtime, cursat );
        /*
         *  Get image date/time info from AODT
         */
         iaodt = aodtv72_getIRimageinfo ( &g_imdate, &g_imtime, &g_sat, g_csat );

	/*
	 * Get storm center lat/lon
	 */
	 posm = 0;
         if ( lauto == G_TRUE ) {
             aodtv72_runautomode( nauto, fauto, imagefile, &cenlat, &cenlon, &posm  );
         }

        /*
         *  Set center location in AODT
         */
         iaodt=aodtv72_setlocation ( cenlat, cenlon, posm );
         if ( iaodt != 0 ) {
            aodtv72_qmessage (iaodt, 0, NULL, retmsg);
            if ( iaodt < 0 ) {
               aodtv72_exit (retmsg);
            }
         }
        /*
         *  Get center location in AODT
         */
         iaodt=aodtv72_getlocation ( &g_lat, &g_lon, &g_posf );
         if ( iaodt != 0 ) {
            aodtv72_qmessage (iaodt, 0, NULL, retmsg);
            if ( iaodt < 0 ) {
               aodtv72_exit (retmsg);
            }
         }
        /*
         *  Set domain FLAG in AODT
         */
         iaodt = aodtv72_setdomain ( idomain );
         if ( iaodt != 0 ) {
            aodtv72_qmessage (iaodt, 0, NULL, retmsg);
            if ( iaodt < 0 ) {
               aodtv72_exit (retmsg);
            }
         }
        /*
         *  Get domain FLAG from AODT
         */
         iaodt=aodtv72_getdomain ( &g_domain );
         if ( iaodt != 0 ) {
            aodtv72_qmessage (iaodt, g_domain, NULL, retmsg);
            if ( iaodt < 0 ) {
               aodtv72_exit (retmsg);
            }
         }
        /*
         *  Retrieve temperatures from image.
         */
         cenlon2 = -1.0 * cenlon;
         irad = radius/4 + 5;
         numx = numy = irad*2 + 1;
         ftmps = (float *)malloc( (size_t)((numx)*(numy))* sizeof(float) );
         flats = (float *)malloc( (size_t)((numx)*(numy)) * sizeof(float) );
         flons = (float *)malloc( (size_t)((numx)*(numy)) * sizeof(float) );
         im_gtmp ( imagefile, "dset", sys_M, &cenlat, &cenlon2, &irad,
                   &numx, &numy, ftmps, flats, flons, &ier2,
                   strlen(imagefile), strlen("dset"), strlen(sys_M) );

         if ( ier2 != 0 )  {
            er_wmsg  ( "IM", &ier2, " ", &ier, strlen("IM"), strlen(" ") );
            exit (0);
         }

         ifl = sizeof(float);
         ipr = sizeof(float*);
	 temps = (float **)calloc((size_t)numy, ipr);
    	 lats  = (float **)calloc((size_t)numy, ipr);
    	 lons  = (float **)calloc((size_t)numy, ipr);
    	 for ( jj = 0; jj < numy; jj++ ) {
            temps[jj] = (float *)calloc((size_t)numx, ifl);
            lats[jj]  = (float *)calloc((size_t)numx, ifl);
            lons[jj]  = (float *)calloc((size_t)numx, ifl);
         }

         for ( jj = 0; jj < (numy); jj++ )  {
             for ( ii = 0; ii < (numx); ii++ )  {
                 indx = jj*(numy)+ii;
                 temps[jj][ii] = ftmps[indx];
                 lats [jj][ii] = flats[indx];
                 lons [jj][ii] = flons[indx] *= -1.0F;
             }
         }
        /*
         *   Free up all malloc'd memory.
         */
         free ( flons );
         free ( flats );
         free ( ftmps );
        /*
         *  Load the IR imge information in AODT
         */
         iaodt = aodtv72_loadIRimage ( temps, lats, lons, numx, numy );

	/*
	 * Free memory.
	 */
	 for ( jj = 0; jj < numy; jj++ ) {
	     free ( temps[jj] );
	     free ( lats[jj]  );
	     free ( lons[jj]  );
	 }
	 free( lons  );
         free( lats  );
         free( temps );

        /*
         *  Set eye and cloud temperature values in AODT,
         *  return position for IR image data read
         */
         iaodt=aodtv72_seteyecloudtemp ( );
         if ( iaodt != 0 ) {
            aodtv72_qmessage (iaodt, 0, NULL, retmsg);
            if ( iaodt < 0 ) {
               aodtv72_exit (retmsg);
            }
         }
        /*
         *   Determine scene type
         */
         iaodt=aodtv72_scenetype ( );
         if ( iaodt != 0 ) {
            aodtv72_qmessage (iaodt, 0, NULL, retmsg);
            if ( iaodt < 0 ) {
               aodtv72_exit (retmsg);
            }
         }
        /*
         *   Get scene type from AODT
         */
         iaodt=aodtv72_getscenetypes ( &g_neweye, &g_newcloud,
                                       &g_oldeye, &g_oldcloud );
        /*
         *   Override scene type
         */
         if ( loverride ) {
            for ( ii = 0; ii < (int)(sizeof(scenetype)/sizeof(scenetype[0])); ii++ )  {
                if ( strcmp(overridescene,scenetype[ii]) == 0 )  {
                   if ( ii <= 3 ) {
                      s_neweye   = ii;
                      s_newcloud = 0;
                      break;
                   }
                   if ( ii > 3 ) {
                      s_neweye   = 3;
                      s_newcloud = ii-3;
                      break;
                   }
                }
            }
           /*
            *   Set user-defined scene type in AODT
            */
	    iaodt=aodtv72_setscenetypes (s_neweye, s_newcloud, origeye, origcloud );
            if ( iaodt != 0 ) {
               aodtv72_qmessage (iaodt, 0, NULL, retmsg);
               if ( iaodt < 0 ) {
                  aodtv72_exit (retmsg);
               }
            }
           /*
            *   Get user-defined scene type from AODT
            */
            iaodt=aodtv72_getscenetypes ( &g_neweye, &g_newcloud,
                                          &g_oldeye, &g_oldcloud );
         }

        /*
         *   Determine intensity
         */
         iaodt=aodtv72_intensity ( );
         if ( iaodt != 0 ) {
            aodtv72_qmessage (iaodt, 0, NULL, retmsg);
            if ( iaodt < 0 ) {
               aodtv72_exit (retmsg);
            }
         }
        /*
         *   Print AODT intensity estimate in bulletin format
         */
         iaodt=aodtv72_bulletinoutput ( bulletin );
         aodtv72_qmessage (500, 0, bulletin, retmsg);
        /*
         *   Write output to history file
         */
         if (strlen(historyfile) > (size_t)0 ) {

            /*
             * Check write permission.
             */
             cfl_perms ( historyfile, &can_read, &can_write, &ier2 );
             if ( !can_write ) {
                 printf ("No permission to write, or invalid directory!\n");
             }
             else {

                /*
                 *   Write current intensity analysis into history file
                 */
                 iaodt=aodtv72_historyrecordinsert ( &h_modified, &h_records );

                 if ( iaodt != 0 ) {
                    aodtv72_qmessage (iaodt, h_records-h_modified + 1, historyfile, retmsg);
                    if ( h_modified != 0 ) {
                       aodtv72_qmessage (65, h_modified, historyfile, retmsg);
                    }
                 } 
	        /* 
	         *   Write updated history records to file
                 */
                 iaodt=aodtv72_historywritefile ( &n_records);

                 if ( iaodt != 0 ) {
                    aodtv72_qmessage (iaodt, n_records, historyfile, retmsg);
                    if ( iaodt < 0 ) {
                       aodtv72_exit (retmsg);
                    }
                 }
	     }
         }
    /**
     *   End AODT intensity analysis
     *--------------------------------------*/
     }
     else {
    /*---------------------------------------
     *   Start AODT history records handling
     **/
	 ltmp = (ldelete || lcomment) ? 1 : 0;
         iaodt=aodtv72_setdatetime ( time1, time2, date1, date2, ltmp );
        /*
         * Start history records listing portion
         */
         if ( llist) {
            /*
             *  Open dump file
             */
             if ( ldump ) {
                fdump = fopen(dumpfile, "w+") ;

                if ( fdump != 0 ) {
                   aodtv72_qmessage (-71, 0, dumpfile, retmsg);
                   if ( iaodt < 0 ) {
                      aodtv72_exit (retmsg);
                   }
                }
             }
            /*
             *   Get history file header
             */
	     itype = -1;
             srcID[0] = CHNULL;
             iaodt = aodtv72_historylistfmt ( 0, itype, srcID, strmID, listing);
             printf (" listing: \n %s \n", listing);
            /*
             *   Write file header to output ascii file
             */
             if ( ldump ) fprintf (fdump, "%s", listing);
            /*
             *   Get pointer to first record in history file data structure
             */
             iaodt = aodtv72_historygetnextrec ( 0, &historyrec);
             if ( iaodt != 0 ) {
                aodtv72_qmessage (iaodt, 0, NULL, retmsg);
                if ( iaodt < 0 ) {
                   aodtv72_exit (retmsg);
                }
             }
            /*
             *   Loop through records in history file
             */
             while ( historyrec != NULL ) {
                /*
                 *   Get current record
                 */
                 iaodt = aodtv72_historylistfmt (historyrec, itype, srcID, strmID, listing);
                 printf (" listing: %s \n", listing);
                /*
                 *   Write listing to output ascii file
                 */
                 if ( ldump ) fprintf (fdump, "%s", listing);
                /*
                 *   Get current record in bulletin format
                 */
                 iaodt = aodtv72_historybullfmt (historyrec, listing);
                 printf (" bulletin: %s \n", listing);
                /*
                 *   Write listing in bulletin format to output ascii file
                 if ( ldump ) fprintf (fdump, "%s", listing);
                 */
                 /*
                  *   Get pointer to the next record
                  */
                  iaodt = aodtv72_historygetnextrec ( 1, &historyrec);
                  if ( iaodt != 0 ) {
                     aodtv72_qmessage (iaodt, 0, NULL, retmsg);
                     if ( iaodt < 0 ) {
                        aodtv72_exit (retmsg);
                     }
                  }
             }
             aodtv72_qmessage (101, 0, NULL, retmsg);
            /*
             *   Close output ascii file
             */
             if ( ldump ) {
                fclose (fdump);
                aodtv72_qmessage (13, 0, dumpfile, retmsg);
             }
         }
        /*
         * Start history records deleting portion
         */
         if ( ldelete ) {
            /*
             *   Delete records requested within AODT
             */
             iaodt = aodtv72_historydeleterec ( &h_deleted, &h_modified );
             if ( iaodt != 0 ) {
                aodtv72_qmessage (iaodt, h_deleted, historyfile, retmsg);
                if ( iaodt < 0 ) {
                   aodtv72_exit (retmsg);
                }
                if ( h_modified > 0 ) {
                   aodtv72_qmessage (65, h_modified, historyfile, retmsg);
                }
             }
             aodtv72_qmessage (103, 0, NULL, retmsg);
            /*
             *   Write updated history structure to history file
             */
             iaodt = aodtv72_historywritefile ( &n_records );
             if ( iaodt != 0 ) {
                aodtv72_qmessage (iaodt, n_records, historyfile, retmsg);
                if ( iaodt < 0 ) {
                   aodtv72_exit (retmsg);
                }
              }
         }

        /*
         * Add a comment to a line in the history file.
         */
         if ( lcomment ) {
            /*
             *  Add a comment to history file if requested. 
             */
	     cst_rxbl ( comment, comment, &len, &ier );
             iaodt = aodtv72_historyaddcomment ( comment, &n_comm );

	     if ( iaodt != 0 ) {
	         aodtv72_qmessage (iaodt, n_comm, historyfile, retmsg);
                 if ( iaodt < 0 ) {
                   aodtv72_exit (retmsg);
                 }
             }

            /*
             *   Write updated history structure to history file
             */
             iaodt = aodtv72_historywritefile ( &n_records );
             if ( iaodt != 0 ) {
                aodtv72_qmessage (iaodt, n_records, historyfile, retmsg);
                if ( iaodt < 0 ) {
                   aodtv72_exit (retmsg);
                }
              }
         }

         free (historyrec);
    /*---------------------------------------
     *   End AODT history records handling
     **/
     }

    /*
     *   Print out all diagnostic messages to screen
     */
     iaodt=aodtv72_qdiagnostics ( infomsg );
     printf ("%s \n", infomsg);
    /*
     * free any allocated memory
     */
     aodtv72_freememory();

    return(0);
}

void aodtv72_exit (char *errormsg)
{
     char       infomsg[5000] = "\0";

     aodtv72_qdiagnostics ( infomsg );
     printf ("%s \n", infomsg);
     printf ("AODT Error: %s \n\n", errormsg);
    /*
     * free any allocated memory
     */
     aodtv72_freememory();

     return;
}
