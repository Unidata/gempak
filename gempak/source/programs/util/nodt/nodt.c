#include "geminc.h"
#include "gemprm.h"
#include "imgdef.h"
#include "inc/odt.h"


/* Default image filename                                       */
#define DEFIFILE	"NONE"
/* Default cpf filename                                         */
#define DEFCFILE	"NONE"
/* Default history filename                                     */
#define DEFYFILE	"ODTDUMP.ODT"
/* Default fixfile filename                                     */
#define DEFXFILE	"AUTOFIX"
/* Default output filename                                      */
#define DEFOFILE	"SCREEN"
/* Default dump filename                                        */
#define DEFLFILE	"ODTDUMP.ASC"
/* Dummy date							*/
#define DATEDUMMY	"0000XXX00"

/* Flag value to perform automatic center finder		*/
#define AUTO		"AUTO"

/* List of valid options processed by getopt			*/
#define	OPTSTRING	"a:c:d:ehlm:o:r:s:w:y:"



/************************************************************************
 * nodt.c                                                            	*
 *                                                                      *
 * CONTENTS:                                                            *
 * nodt                                                              	*
 ***********************************************************************/

int main ( int argc, char **argv )
/************************************************************************
 * NODT                                                              	*
 *                                                                      *
 * This program performs the Objective Dvorak Technique.		*
 *  From: Chris Velden, Tim Olander					*
 *  University of Wisconsin - Madison					*
 *  Cooperative Institute for Meteorological Satellite Studies		*
 *            								*
 *                                                                      *
 * Usage: nodt [options] image_filename					*
 *									*
 * OPTIONS:								*
 * -a   typ/fn  (McIDAS keyword option "AUTO")       			*
 *              Allow for totally automated operation of the ODT.       *
 *              May be used in conjunction with -v option.       	*
 *              "typ" may be:       					*
 *              typ=1:TPC WTNT4? or WTPZ3? DISCUSSION file.       	*
 *              typ=2:JTWC WTPN3? TROPICAL CYCLONE WARNING file.       	*
 *              and "fn" is the file (default "AUTOFIX").       	*
 *              (NOTE: no whitespace allowed in "typ/fn" string)       	*
 *                                                                      *
 * -c   fn      NMAP2 cursor point file filename (must be *.cpf)       	*
 *              containing storm center location       			*
 *		(NOTE: seperator in cpf file is a semicolon, NOT comma)	*
 *      -or-    -OR-       						*
 *    clat,clon Storm center location in the format "clat,clon"       	*
 *              (NOTE: clon negative west, no whitespace allowed,	*
 *              seperator is a comma, NOT a semicolon)       		*
 *                                                                      *
 * -d  date1;time1;date2;time2       					*
 *              (McIDAS keyword option "DATE")       			*
 *              Range of dates/times desired (same fmt as history file)	*
 *              Enter "date" as YrMonDay (e.g. 97OCT05)		        *
 *              Enter "time" as HHMMSS   (e.g. 131500) 			*
 *              Defaults :       					*
 *                LIST (-l option):       				*
 *                  date1/time1 : first record       			*
 *                  date2/time2 : last record       			*
 *                DELETE (-e option):       				*
 *                  date1/time1 : no defaults, must specify       	*
 *                  date2/time2 : date1/time1       			*
 *                                                                      *
 * -e   "yes"   Allows for manual deletion of a date or a range of dates*
 *      -or-    (use in conjuction with -d option).    			*
 *      "no"       							*
 *                                                                      *
 * -h           Display this help message       			*
 *                                                                      *
 * -l   "yes"   (McIDAS keyword option "LIST")       			*
 *      -or-    List contents of history file (-y fn option) to text    *
 *      "no"    window.  (default=NO)       				*
 *                                                                      *
 * -m   "ATL"   (McIDAS keyword option "DOMAIN")       			*
 *      -or-    Atlantic or West Pacific T# and CI intensity estimate &	*
 *      "PAC"   related pressure/wind value.  For East Pac, user must   *
 *              decide which domain is correct to use for pressure.     *
 *              (default "ATL")       					*
 *                                                                      *
 * -o   fn      (McIDAS keyword option "OUTPUT")       			*
 *              output to the text file "fn" (default fn="ODTDUMP.ASC").*
 *              (default is print to screen).       			*
 *                                                                      *
 * -r   "off"   (McIDAS keyword option "RULE48")       			*
 *      -or-    Turn off application of special "first 48 hour rule"    *
 *      "on"    which subtracts 0.5 from the Raw T#       		*
 *              (default=ON).       					*
 *                                                                      *
 * -s   scene   (replaces McIDAS keyword option "OVER")       		*
 *              Allow user to manually override ODT scene identification*
 *              and/or automated center positioning.  May be utilized   *
 *              with -a option, if desired.  (no default).       	*
 *                                                                      *
 * -w   "yes"   (McIDAS keyword option "WIND")       			*
 *      -or-    List intensity units in terms of wind speed (knots)     *
 *      "no"    instead of MSLP.  Used with LIST or with ODA analysis   *
 *              output (default=NO).       				*
 *                                                                      *
 * -y   fn      (McIDAS keyword option "HISTORY")       		*
 *              File to which the history record will be written.       *
 *                                                                      *
 *                                                                      *
 * image_filename        McIDAS image filename				*
 *                                                                      *
 *                                                                      *
 * main(argc, argv)                                                     *
 *                                                                      *
 * Input parameters:                                                    *
 *  argc   int      number of parameters of command line                *
 *  argv   char**   parameter array of command line                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      3/03	Create                                  *
 * D.W.Plummer/NCEP      4/03	Upgrade options and documentation	*
 * M. Li/SAIC		11/03	Added im_cbar				*
 ***********************************************************************/

{
char	*prog = NULL, ctmp[32];
int	np, ier, iret;
int	curdate, curtime;
int     pagflg, one=1, mode=1, istat, idate[3], jyear, jday;
long	ifilesize;
float	cenlat, cenlon;
char    errgrp[8], device[32], cq[20];
char	imagefile[128], imagefname[128], historyfile[128], fixfil[128];
char	outfil[128];
char	cpf[128], cpfname[128], clrbar[80];

char	latlon[32], result[16], *cptr;
int	imark=1, imkhw=0, imkwid=3, process;
float	szmark=3.0F;

int	itype;
int	lodt, lauto, lwind, ldomain, ldump, lrule48, loverride, ldelete;
int	llist, lspotanal;
char	cauto[128], ctype[8], cdomain[8], cdate[128], overridescene[16];
char	date1[20], date2[20];
int	time1, time2;

/*
 * These variables are used by getopt. Unset the error reporting.
 */
int             ch, errflg;

/*---------------------------------------------------------------------*/

    pagflg = G_FALSE;
    strcpy ( errgrp, "NODT" );

    lodt       = G_TRUE;
    lauto      = G_FALSE;
    ldelete    = G_FALSE;
    ldomain    = G_FALSE;
    ldump      = G_FALSE;
    llist      = G_FALSE;
    loverride  = G_FALSE;
    lwind      = G_FALSE;
    lrule48    = G_TRUE;
    lspotanal  = G_TRUE;
    strcpy (   imagefile, DEFIFILE );
    strcpy (         cpf, DEFCFILE );
    strcpy ( historyfile, DEFYFILE );
    strcpy (      fixfil, DEFXFILE );
    strcpy (      outfil, DEFLFILE );
    cenlat = RMISSD;
    cenlon = RMISSD;
    strcpy ( date1, DATEDUMMY );
    time1 = -1;
    strcpy ( date2, date1 );
    time2 = time1;

    /*
     * Save the program name.
     */
    prog = (char *) malloc ( strlen(argv[0]) );
    strcpy ( prog, argv[0] );

    /*
     * Get the options and set the appropriate flags.
     */
    opterr = 1;
    errflg = 0;
    while ( ( ch = getopt ( argc, argv, OPTSTRING ) ) != EOF ) {
        switch ( ch ) {
            case 'a':				/* AUTO mode		*/

		lauto = G_TRUE;
		strcpy ( cauto, optarg );
		cptr = cst_split ( cauto, '/', sizeof(ctype), ctype, &ier );
		cst_numb ( ctype, &itype, &ier );
		if ( itype == 1 || itype == 2 )  {
		    if ( cptr != (char *)NULL )  strcpy ( fixfil, cptr );
		}
		else  {
		    printf("Invalid value (%d) for '-a' option.\n", itype );
		    errflg = 1;
		}
                break;

            case 'c':				/* cpf file options	*/

		strcpy ( cpf, optarg );
                break;

            case 'd':				/* date option		*/
						/* use w/ -l option	*/
						/* or  -e option	*/

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

            case 'e':				/* delete option	*/
		
		lodt = G_FALSE;
		ldelete = G_TRUE;
                break;

            case 'h':				/* help			*/

                ip_help ( prog, &pagflg, &ier, strlen(prog) );
                exit (0);
                break;

            case 'l':				/* list option		*/
		
		lodt  = G_FALSE;
		llist = G_TRUE;
                break;

            case 'm':				/* domain spec		*/
						/* "atl" or "pac"	*/
		strcpy ( cdomain, optarg );
		cst_lcuc ( cdomain, cdomain, &ier );
		if ( strcmp(cdomain,"ATL") == 0 )  ldomain = 0;
		if ( strcmp(cdomain,"PAC") == 0 )  ldomain = 1;
                break;

            case 'o':				/* output option	*/
						/* use w/ -l option	*/
		lodt  = G_FALSE;
		llist = G_TRUE;
		strcpy ( outfil, optarg );
		if ( strcmp(outfil,DEFOFILE) != 0 )  ldump = G_TRUE;
                break;

            case 'r':				/* rule 48 option	*/
						/* "yes" (def) or "no"	*/
		strcpy ( ctmp, optarg );
		cst_lcuc ( ctmp, ctmp, &ier );
		if ( strncmp(ctmp,"N",1) == 0 )  lrule48 = G_FALSE;
                break;

            case 's':				/* override scene 	*/
		
		loverride = G_TRUE;
		strcpy ( overridescene, optarg );
		cst_lcuc ( overridescene, overridescene, &ier );
                break;

            case 'w':				/* wind option		*/
						/* "yes" or "no" (def)	*/
		strcpy ( ctmp, optarg );
		cst_lcuc ( ctmp, ctmp, &ier );
		if ( strncmp(ctmp,"Y",1) == 0 )  lwind = G_TRUE;
                break;

            case 'y':				/* history file spec	*/

		strcpy ( historyfile, optarg );
		if ( strstr(historyfile,DEFYFILE) != (char *)NULL )  
		    lspotanal = G_FALSE;
                break;

            case '?':				/* unknown option	*/

                errflg++;
                break;
        }
    }

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
            ip_help ( errgrp, &pagflg, &ier, strlen(errgrp) );
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
    if ( strcmp(cpf,DEFCFILE) == 0 && lauto == G_FALSE )  {
        printf( "Either -c (manual lat,lon information) or"
		" -a (automatic lat,lon determination) option"
		" must be specified (or both).\n" );
        exit (0);
    }
    
    /*
     *  Option -c on command line is either 
     *  1) NMAP2 cpf file containing center lat and lon, or
     *  2) center paired lat and lon in format "lat,lon", lon west neg, or
     */
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
	    ctb_rdcpf ( cpfname, &np, &cenlat, &cenlon, &ier );
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


    ginitp ( &mode, &istat, &iret );
    if ( iret != 0 )  {
	printf("GINITP failed...iret=%d\n", iret );
	exit (0);
    }

    im_simg ( "sat", imagefile, &ier, strlen("sat"), strlen(imagefile) );
    gsicmn ( &ier );

    sprintf( device, "xw|ODT|%d;%d", imnpix, imnlin );
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
     *  ODT calculations expect longitude to be west positive.
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
	    printf("DATE required with -e or -l options.\n");
	    exit ( 0 );
	}
	if ( llist==1 && strcmp(date2,DATEDUMMY)==0 )  {
	    printf("Both start and end date required with -l option.\n");
	    exit ( 0 );
	}

    }

    if ( lodt == G_TRUE )
      printf("Calling ODT image processing with the following options:\n"
	"\tIMAGE FILE = \"%s\", DATE/TIME = %d/%d\n"
	"\tCENTER latitude/longitude (pos west) = (%6.2f,%7.2f)\n"
	"\tDOMAIN = %s\n"
        "\tRULE48 = %s\n"
        "\tWIND = %s\n",
	imagefile, curdate, curtime, cenlat, cenlon,
	ldomain == 0       ? "ATL"   : "PAC",
        lrule48 == G_TRUE  ? "TRUE"  : "FALSE",
        lwind   == G_FALSE ? "FALSE" : "TRUE" );
    else
      printf("Calling ODT for historyfile list or delete only:\n");

    printf("\tHISTORY FILE = %s\n", historyfile );


    if ( lodt == G_TRUE )  {
      loverride == G_TRUE 
	? printf("\tOVERRIDE SCENE = TRUE (new scene=%s)\n", overridescene)
	: printf("\tOVERRIDE SCENE = FALSE\n");

      lauto == G_TRUE 
	? printf("\tAUTO = TRUE (file=%s)\n", fixfil )
	: printf("\tAUTO = FALSE\n");
    }
    else  {
      llist == G_TRUE
	? printf("\tLIST = TRUE, OUTPUT FILE = %s\n", 
		ldump==G_FALSE ? "SCREEN" : outfil )
	: printf("\tLIST = FALSE\n");

      ldelete == G_TRUE
	? printf("\tDELETE = TRUE\n" )
	: printf("\tDELETE = FALSE\n" );
    }

    if ( ldelete == G_TRUE || llist == G_TRUE )  
	printf("\tDATE = %s/%d %s/%d\n", date1, time1, date2, time2 );

    ier = odtdrive( curdate, curtime, imagefile, cenlat, cenlon, 
		    historyfile, lodt, lauto, ldelete, ldomain, ldump, llist,
		    lrule48, loverride, overridescene, lwind, lspotanal,
		    fixfil, outfil, date1, time1, date2, time2);

    free ( prog );

    return (0);

}
