#include "geminc.h"
#include "gemprm.h"

#include "zlib.h"

/* Local prototypes */
static void usage ( char * );
void skiphdr ( int, unsigned char *, long * );

/* Local parameters */
#define	NEXTBL		"nexrad.tbl"
#define	TBLDIR		"stns"

#define	MAXSTN		200

/* Zlib stuff */
#define MAX_BLOCK	5000

#define CHECK_ERR(err, msg) { \
    if (err != Z_OK) { \
        printf("%s error: %d\n", msg, err); \
    } \
}

/*=====================================================================*/

int main ( int ac, char **av )

/************************************************************************
 * ndinfo 								*
 *									*
 * Takes a NIDS file and returns requested information about the image	*
 * from the header based on what the user requests.			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/97	After GNINFO and ARINFO			*
 * S. Jacobs/NCEP	12/00	Updated for version 2.0			*
 * S. Jacobs/NCEP	 7/01	Changed to use SCAN date and time (2.2)	*
 * S. Jacobs/NCEP	10/12	Adjust limit on the product id value	*
 ***********************************************************************/

{
	union nhdr_u {
	    unsigned char	b[160];
	    unsigned short	s[80];
	    int			i[40];
	} nhdr;

	unsigned char	*flbyte;

	char		*infile, newfil[160];
	int		ch, i, j, k, m, n, iret, nr, fndflg, cmprss,
			ival1, ival2;
	long		istart, flen;
	FILE		*ifl;

	char		tblnam[73], defdir[12],
			stid[MAXSTN][9], stnnam[MAXSTN][33],
			stat[MAXSTN][3], coun[MAXSTN][3],
			tbchrs[MAXSTN][21];
	int		found, maxstn, nstn, istn, istnm[MAXSTN],
			ispri[MAXSTN], irg;
	float		slat[MAXSTN], slon[MAXSTN], selv[MAXSTN];

	int		nyear, jyear, jday, ivtim,
			idtarr[5], jdtarr[5], kdtarr[5],
			ihour[3], iminut[3], isec[3];

	int		ilat, ilon;
	float		clat, clon;

	int		isrc, iprod, nlev, len1, len2;
	float		res;
	char		prdnam[9], units[9], desc[21];

	int		order[10], errflg, onum, opterr;
	/*
	extern char	*optarg;
	extern int	optind, optopt;
	*/

	char		month[][4] = {  "JAN", "FEB", "MAR", "APR",
					"MAY", "JUN", "JUL", "AUG",
					"SEP", "OCT", "NOV", "DEC" };

	char		regn[][3]  = {  "NE", "SE", "NC", "SC",
					"CE", "NW", "SW" };

	int		one = 1, swpflg, nswp;
	char		*cp = (char *)&one;

        /*
	 * ZLIB decompression variables.
	 */
	z_stream	d_stream;
	uLong		lentot, lenout;
	uLong		uncomprLen = MAX_BLOCK;
	Byte		uncompr[MAX_BLOCK];

	int		ccblen, lenhdr, lenprd, nbytes, err;
	unsigned char	*boff, b1, b2;

/*---------------------------------------------------------------------*/
	
	opterr = 1;
	errflg = 0;
	onum   = 0;

/*
 *	Check if swapping is needed.
 */
 	swpflg = ( *cp != 0 );

/*
 *	Get the user options.
 */
	while ( ( ch = getopt ( ac, av, "hazdtslpnf" ) ) != EOF ) {
	    switch (ch) {
		case 'h':
			usage ( av[0] );
			exit (0);
			break;
		case 'a':
			fndflg = G_FALSE;
			for ( m = 0; m < onum; m++)
			    if  ( order[m] == 'a' )  fndflg = G_TRUE;
			if  ( ! fndflg )  order[onum++] = 'a';
			break ;
		case 'z':
			fndflg = G_FALSE;
			for ( m = 0; m < onum; m++)
			    if  ( order[m] == 'z' )  fndflg = G_TRUE;
			if  ( ! fndflg )  order[onum++] = 'z';
			break ;
		case 'd':
			fndflg = G_FALSE;
			for ( m = 0; m < onum; m++)
			    if  ( order[m] == 'd' )  fndflg = G_TRUE;
			if  ( ! fndflg )  order[onum++] = 'd';
			break ;
		case 't':
			fndflg = G_FALSE;
			for ( m = 0; m < onum; m++)
			    if  ( order[m] == 't' )  fndflg = G_TRUE;
			if  ( ! fndflg )  order[onum++] = 't';
			break ;
		case 's':
			fndflg = G_FALSE;
			for ( m = 0; m < onum; m++)
			    if  ( order[m] == 's' )  fndflg = G_TRUE;
			if  ( ! fndflg )  order[onum++] = 's';
			break ;
		case 'l':
			fndflg = G_FALSE;
			for ( m = 0; m < onum; m++)
			    if  ( order[m] == 'l' )  fndflg = G_TRUE;
			if  ( ! fndflg )  order[onum++] = 'l';
			break ;
		case 'p':
			fndflg = G_FALSE;
			for ( m = 0; m < onum; m++)
			    if  ( order[m] == 'p' )  fndflg = G_TRUE;
			if  ( ! fndflg )  order[onum++] = 'p';
			break ;
		case 'n':
			fndflg = G_FALSE;
			for ( m = 0; m < onum; m++)
			    if  ( order[m] == 'n' )  fndflg = G_TRUE;
			if  ( ! fndflg )  order[onum++] = 'n';
			break ;
		case 'f':
			fndflg = G_FALSE;
			for ( m = 0; m < onum; m++)
			    if  ( order[m] == 'f' )  fndflg = G_TRUE;
			if  ( ! fndflg )  order[onum++] = 'f';
			break ;
		case '?':
			errflg++ ;
			break ;
	    }
	}

	if  ( errflg ) {
	    usage ( av[0] ) ;
	    exit ( -1 );
	}

/*
 *	Get the file name.
 */
	if  ( ac - optind == 1 ) {
	    infile = av[optind] ;
	}
	else {
	    fprintf ( stderr, "No input file specified.\n" );
	    usage ( av[0] ) ;
	    exit ( -1 );
	}

/*
 *	Get the file size, then open the input file
 */
	cfl_inqr ( infile, NULL, &flen, newfil, &iret );

	ifl = cfl_ropn ( infile, NULL, &iret );
	if ( iret != 0 ) {
	    fprintf ( stderr, "Could not open input file.\n" ) ;
	    exit ( -1 ) ;
	}

/*
 *	Read the first few bytes to see if there is a FOS or WMO header.
 */
	cfl_read ( ifl, 160, nhdr.b, &nr, &iret );

	if  ( ( nhdr.b[0] ==  1 ) &&
	      ( nhdr.b[1] == 13 ) &&
	      ( nhdr.b[2] == 13 ) &&
	      ( nhdr.b[3] == 10 ) ) {

/*
 *	    Get past the header, then check for compressed data.
 */
	    skiphdr ( 4, nhdr.b, &istart );
	    if  ( istart == IMISSD )  exit(1);

	    n = 0;
	    j = 2;
	    i = (int) istart;
	    mv_btoi ( nhdr.b, &i, &j, &n, &ival1, &iret );
	    i += 30;
	    mv_btoi ( nhdr.b, &i, &j, &n, &ival2, &iret );

 	    if ( ( ival1 == ival2 ) &&
	    	 ( ival1 >=    16 ) &&
		 ( ival1 <=   999 ) ) {

		cmprss = G_FALSE;
		cfl_seek ( ifl, istart, SEEK_SET, &iret );

	    }
	    else {
	    	cmprss = G_TRUE;
	    }

	}
	else if  ( ( nhdr.b[0] == 'S' ) &&
		   ( nhdr.b[1] == 'D' ) &&
		   ( nhdr.b[2] == 'U' ) &&
		   ( nhdr.b[3] == 'S' ) ) {
/*
 *	    Get past the header, then check for compressed data.
 */
	    skiphdr ( 2, nhdr.b, &istart );
	    if  ( istart == IMISSD )  exit(1);

	    n = 0;
	    j = 2;
	    i = (int) istart;
	    mv_btoi ( nhdr.b, &i, &j, &n, &ival1, &iret );
	    i += 30;
	    mv_btoi ( nhdr.b, &i, &j, &n, &ival2, &iret );

 	    if ( ( ival1 == ival2 ) &&
	    	 ( ival1 >=    16 ) &&
		 ( ival1 <=   999 ) ) {

		cmprss = G_FALSE;
		cfl_seek ( ifl, istart, SEEK_SET, &iret );

	    }
	    else {
	    	cmprss = G_TRUE;
	    }
 	    
	}
	else {

/*
 *	    No header. Rewind the file.
 */
	    cmprss = G_FALSE;
	    cfl_seek ( ifl, 0, SEEK_SET, &iret );
	}
/*
 *	Read the compressed NIDS product, then close the file,
 *	inflate the data and swap the data.
 */
	if  ( cmprss )  {
	    cfl_seek ( ifl, 0, SEEK_SET, &iret );
	    flbyte = (unsigned char *) malloc ( flen *
						sizeof(unsigned char) );
	    cfl_read ( ifl, (int) flen, flbyte, &nbytes, &iret );
	    cfl_clos ( ifl, &iret );

	    lentot = 0;
	    lenout = 0;
	    boff = flbyte + istart;

	    lenprd = nbytes - istart - 4;
	    lenhdr = 156 + 128;

	    while  ( ( lentot < (unsigned long)lenprd ) &&
	    	     ( lentot < (unsigned long)lenhdr ) )  {
		d_stream.zalloc = (alloc_func) 0;
		d_stream.zfree  = (free_func) 0;
		d_stream.opaque = (voidpf) 0;

		err = inflateInit ( &d_stream );
		CHECK_ERR ( err, "inflateInit" );

		d_stream.next_in   = (Byte *)(boff + lentot);
		d_stream.avail_in  = nbytes - istart - lentot; 
		d_stream.next_out  = uncompr + lenout; 
		d_stream.avail_out = (uInt)uncomprLen - lenout; 

		for  (;;)  { 
		    err = inflate ( &d_stream, Z_NO_FLUSH );

		    if  ( err == Z_STREAM_END )  break;  

		    CHECK_ERR ( err, "large inflate" );
		    free ( flbyte ); 
		    exit(1);
		}

		lentot += d_stream.total_in;
		lenout += d_stream.total_out;

	    }

	    err = inflateEnd ( &d_stream );
	    CHECK_ERR ( err, "inflateEnd" );

	    /*
	     * Strip off CCB, WMO and PIL inside compressed
	     * product.
	     */
	    b1 = uncompr[0];
	    b2 = uncompr[1];

	    ccblen = 2 * ( ( ( b1 & 63 ) << 8 ) + b2 );

	    k = ccblen;
	    for ( m = 0 ; m < 2 ; m++ )  {
		while ( ( (unsigned long)k < lenout ) &&
			( uncompr[k] != '\n' ) )  {
		    k++;
		}
		k++;
	    }

	    for ( m = 0; m < 160; m++ )  {
	    	nhdr.b[m] = uncompr[k+m];
	    }

	    if  ( swpflg )  {
		nswp = 40;
		iret = mv_swp2 ( &nswp, nhdr.i, nhdr.i );
	    }

	}
	else {
/*
 *	    Read the standard NIDS header, then close the file and
 *	    swap the data.
 */
	    cfl_read ( ifl, 160, nhdr.b, &nr, &iret );
	    cfl_clos ( ifl, &iret );

	    if  ( swpflg )  {
		nswp = 40;
		iret = mv_swp2 ( &nswp, nhdr.i, nhdr.i );
	    }

	}


	found = G_FALSE;
	if  ( nhdr.s[6] < 10000 )  {
/*
 *	    If the source ID number is less than 10000, read the
 *	    station information, and find the correct station.
 */
	    strcpy ( tblnam, NEXTBL );
	    strcpy ( defdir, TBLDIR );
	    maxstn = MAXSTN;
	    ctb_astn ( tblnam, defdir, &maxstn, &nstn, stid, stnnam,
		       istnm, stat, coun, slat, slon, selv, ispri,
		       tbchrs, &iret );
	    k = 0;
	    while ( ! found  && k < nstn ) {
		if  ( istnm[k] == nhdr.s[6] )  {
		    found = G_TRUE;
		    istn  = k;
		}
		else {
		    k++;
		}
	    }
	}
	else if  ( nhdr.s[6] == 10000 )  {
/*
 *	    If the source ID number equals 10000, set the national
 *	    product information.
 */
	    found = G_TRUE;
	    istn  = 0;
	    strcpy ( stat[istn], "US" );
	    strcpy ( stid[istn], "NATL" );
	}
	else if  ( nhdr.s[6] >= 10001 && nhdr.s[6] <= 10007 )  {
/*
 *	    If the source ID number is between 10001 and 10007, set
 *	    the regional product information.
 */
	    found = G_TRUE;
	    istn  = 0;
	    irg   = ( nhdr.s[6] % 10 ) - 1;
	    strcpy ( stat[istn], regn[irg] );
	    strcpy ( stid[istn], "REGL" );
	}

/*
 *	Compute the three different dates and times.
 *	They may or may not have different values.
 */
	nyear = (int) ( ( nhdr.s[1] - 1 + .5 ) / 365.25 );
	jyear = 1970 + nyear;
	jday  = nhdr.s[1] - ( (int) ( nyear * 365.25 + .25 ) );
	ti_jtoi ( &jyear, &jday, idtarr, &iret );
	ivtim  = nhdr.s[2];
	ivtim  = ( ivtim << 16 ) + nhdr.s[3];
	ihour[0]  = ivtim / 3600;
	iminut[0] = ( ivtim / 60 ) % 60;
	isec[0]   = ivtim % 60;

	nyear = (int) ( ( nhdr.s[20] - 1 + .5 ) / 365.25 );
	jyear = 1970 + nyear;
	jday  = nhdr.s[20] - ( (int) ( nyear * 365.25 + .25 ) );
	ti_jtoi ( &jyear, &jday, jdtarr, &iret );
	ivtim  = nhdr.s[21];
	ivtim  = ( ivtim << 16 ) + nhdr.s[22];
	ihour[1]  = ivtim / 3600;
	iminut[1] = ( ivtim / 60 ) % 60;
	isec[1]   = ivtim % 60;

	nyear = (int) ( ( nhdr.s[23] - 1 + .5 ) / 365.25 );
	jyear = 1970 + nyear;
	jday  = nhdr.s[23] - ( (int) ( nyear * 365.25 + .25 ) );
	ti_jtoi ( &jyear, &jday, kdtarr, &iret );
	ivtim  = nhdr.s[24];
	ivtim  = ( ivtim << 16 ) + nhdr.s[25];
	ihour[2]  = ivtim / 3600;
	iminut[2] = ( ivtim / 60 ) % 60;
	isec[2]   = ivtim % 60;

/*
 *	Get the center latitude and longitude.
 */
	ilat = nhdr.s[10];
	ilat = ( ilat << 16 ) + nhdr.s[11];
	clat = ilat / 1000.0;
	ilon = nhdr.s[12];
	ilon = ( ilon << 16 ) + nhdr.s[13];
	clon = ilon / 1000.0;

/*
 *	Get the product information.
 */
	isrc  = nhdr.s[6];
	iprod = nhdr.s[0];
	tb_nids ( &isrc, &iprod, prdnam, &nlev, units, &res, desc, &iret );
	st_null ( prdnam, prdnam, &len1, &iret,
		  sizeof(prdnam), sizeof(prdnam) );
	st_null ( units, units, &len2, &iret,
		  sizeof(units), sizeof(units) );
	st_null ( desc, desc, &len2, &iret,
		  sizeof(desc), sizeof(desc) );

/*
 *	Print the output
 */	
	for ( i = 0 ; i < onum ; i++ ) {
	    switch (order[i]) {
		
		case 'a':
			printf ( "\n" );
			printf ( "Filename   : %s\n", infile );
			printf ( "Date (1)   : %02d  %s  %d\n",
				  idtarr[2], month[idtarr[1]-1],
				  idtarr[0] );
			printf ( "Time (1)   : %02d:%02d:%02d\n",
				  ihour[0], iminut[0], isec[0] );
			printf ( "Date (2)   : %02d  %s  %d\n",
				  jdtarr[2], month[jdtarr[1]-1],
				  jdtarr[0] );
			printf ( "Time (2)   : %02d:%02d:%02d\n",
				  ihour[1], iminut[1], isec[1] );
			printf ( "Date (3)   : %02d  %s  %d\n",
				  kdtarr[2], month[kdtarr[1]-1],
				  kdtarr[0] );
			printf ( "Time (3)   : %02d:%02d:%02d\n",
				  ihour[2], iminut[2], isec[2] );
			if  ( found ) {
			    if  ( ( strcmp(stid[istn],"REGL") == 0 ) ||
			     	  ( strcmp(stid[istn],"NATL") == 0 ) )  {
				printf ( "Station    : %s - %s\n",
					  stid[istn], stat[istn] );
			    }
			    else {
				printf ( "Station    : %s - %s\n",
					  stid[istn], stnnam[istn] );
			    }
			}
			else {
			    printf ( "Source ID  : %d is unknown\n",
				      nhdr.s[6] );
			}
			printf ( "Latitude   : %8.2f\n", clat );
			printf ( "Longitude  : %8.2f\n", clon );
			printf ( "Product ID : %d\n", iprod );
			printf ( "Prod name  : %s\n", prdnam );
			printf ( "Num levels : %d\n", nlev );
			printf ( "Prod units : %s\n", units );
			printf ( "Prod res   : %.2f\n", res );
			printf ( "Prod desc  : %s\n", desc );
			printf ( "Elevation #: %d\n", nhdr.s[28] );
			printf ("\n") ;
			break;
		case 'd':
			printf ( "%04d%02d%02d ",
				 idtarr[0], idtarr[1], idtarr[2] );
			printf ( "%04d%02d%02d ",
				 jdtarr[0], jdtarr[1], jdtarr[2] );
			printf ( "%04d%02d%02d\n",
				 kdtarr[0], kdtarr[1], kdtarr[2] );
			break;
		case 't':
			printf ( "%02d%02d ", ihour[0], iminut[0] );
			printf ( "%02d%02d ", ihour[1], iminut[1] );
			printf ( "%02d%02d\n", ihour[2], iminut[2] );
			break;
		case 'l':
			printf ( "%8.2f %8.2f\n", clat, clon );
			break;
		case 's':
			if  ( found ) {
			    printf ( "%s-%s\n", stat[istn], stid[istn] );
			}
			else {
			    printf ( "%d\n", nhdr.s[6] );
			}
			break;
		case 'p':
			if  ( nhdr.s[28] != 0 ) {
			    printf ( "%s%d_%.2f\n",
				     prdnam, nhdr.s[28], res );
			}
			else {
			    printf ( "%s_%.2f\n", prdnam, res );
			}
			break;
		case 'n':
			if  ( nhdr.s[28] != 0 )  {
			    printf ( "%s%d_%.2f_%04d%02d%02d_%02d%02d\n",
				     prdnam, nhdr.s[28], res,
				     jdtarr[0], jdtarr[1], jdtarr[2],
				     ihour[1], iminut[1] );
			}
			else {
			    printf ( "%s_%.2f_%04d%02d%02d_%02d%02d\n",
				     prdnam, res,
				     jdtarr[0], jdtarr[1], jdtarr[2],
				     ihour[1], iminut[1] );
			}
			break;
		case 'f':
			if  ( found )  {
			    if  ( nhdr.s[28] != 0 )  {
				printf ( "%s-%s/%s%d_%.2f/%s%d_%.2f_%04d%02d%02d_%02d%02d\n",
					 stat[istn], stid[istn], prdnam, nhdr.s[28], res,
					 prdnam, nhdr.s[28], res, jdtarr[0], jdtarr[1],
					 jdtarr[2], ihour[1], iminut[1] );
			    }
			    else {
				printf ( "%s-%s/%s_%.2f/%s_%.2f_%04d%02d%02d_%02d%02d\n",
					 stat[istn], stid[istn], prdnam, res,
					 prdnam, res, jdtarr[0], jdtarr[1],
					 jdtarr[2], ihour[1], iminut[1] );
			    }
			}
			else {
			    if  ( nhdr.s[28] != 0 )  {
				printf ( "%d/%s%d_%.2f/%s%d_%.2f_%04d%02d%02d_%02d%02d\n",
					 nhdr.s[6], prdnam, nhdr.s[28], res,
					 prdnam, nhdr.s[28], res, jdtarr[0], jdtarr[1],
					 jdtarr[2], ihour[1], iminut[1] );
			    }
			    else {
				printf ( "%d/%s_%.2f/%s_%.2f_%04d%02d%02d_%02d%02d\n",
					 nhdr.s[6], prdnam, res,
					 prdnam, res, jdtarr[0], jdtarr[1],
					 jdtarr[2], ihour[1], iminut[1] );
			    }
			}
			break;
		case 'z':
			for ( j = 0; j < 79; j++ )  {
			    printf ( "nhdr.s[%2d] = %8d\n",
			    	     j+1, nhdr.s[j] );
			}
			break;
	    }
 	}

	return (0);
}

/*=====================================================================*/

static void usage ( char *prgnam )

/************************************************************************
 * usage								*
 *									*
 * Displays usage help text.						*
 *									*
 * usage ( prgnam )							*
 *									*
 * Input parmeters:							*
 *	*prgnam		char		Program name			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/00	Update for version 2.0			*
 * S. Jacobs/NCEP	10/12	Use the main GEMPAK version number	*
 ***********************************************************************/

{
    char    vers[128];
    int     iret;

/*---------------------------------------------------------------------*/

    ss_vers ( vers, &iret, sizeof(vers) );

    fprintf ( stderr, "\n" );
    fprintf ( stderr, "%s: %s - Information on NIDS file.\n",
    			prgnam, vers );
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "Usage: %s [-option(s)] nids_file\n", prgnam );
    fprintf ( stderr, "Options:  -h   display this help screen\n" );
    fprintf ( stderr, "          -a   dump all decoded information\n" );
    fprintf ( stderr, "          -z   dump the values of the header\n" );
    fprintf ( stderr, "          -d   dates as YYYYMMDD\n" );
    fprintf ( stderr, "          -t   times as HHNN\n" );
    fprintf ( stderr, "          -l   location\n" );
    fprintf ( stderr, "\n" );
    fprintf ( stderr, "          -s   N-AWIPS site directory name\n" );
    fprintf ( stderr, "          -p   N-AWIPS prod directory name\n" );
    fprintf ( stderr, "          -n   N-AWIPS file name\n" );
    fprintf ( stderr, "          -f   N-AWIPS full path\n" );
    fprintf ( stderr, "\n" );

}

/*=====================================================================*/

void skiphdr ( int nline, unsigned char *byte, long *istart )

/************************************************************************
 * skiphdr								*
 *									*
 * This function
 *									*
 * skiphdr ( byte, istart )						*
 *									*
 * Input parameters:							*
 *	nline		int		Number of lines of header	*
 *	*byte		unsigned char	Array of bytes			*
 *									*
 * Output parameters:							*
 *	*istart		long		Location in array after header	*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/00	Created					*
 ***********************************************************************/

{

	int	i, j;

/*---------------------------------------------------------------------*/

	i = 0;
	j = nline; 

	while  ( ( i < 100 ) && ( j > 0 ) )  {

	    if  ( ( j == 2 ) &&
		  ( ( i == 0 ) || ( byte[i-1] == '\n' ) ) )  {

		if  ( ( byte[i]   != 'S' ) ||
		      ( byte[i+1] != 'D' ) ||
		      ( byte[i+2] != 'U' ) ||
		      ( byte[i+3] != 'S' ) )  {
		    *istart = IMISSD;
		    return;
		}

	    }

	    if  ( ( byte[i] == '\r' ) && ( byte[i+1] == '\n' ) )  {
		j -= 1;
		i += 2;
	    }
	    else {
		i += 1;
	    }

	}

	*istart = i;

}
