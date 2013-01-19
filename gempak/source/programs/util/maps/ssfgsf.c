#include "geminc.h"
#include "gemprm.h"

#define DAFILE	"dafil.int"
#define SQFILE	"seqfil.int"

#ifdef UNDERSCORE
#define map_ssfgsf map_ssfgsf_
#endif
void map_ssfgsf ( const char*, const char*, const char*, int*,
                  size_t, size_t, size_t );

/************************************************************************
 * ssfgsf.c                                                             *
 *                                                                      *
 * This module contains the main program of ssfgsf.  The program ssfgsf *
 * converts map file in Sequential Standard Format (SSF) to a GEMPAK	*
 * Standard Format (GSF).						*
 *                                                                      *
 * CONTENTS:                                                            *
 *      main()           main program of ssfgsf.                        *
 ***********************************************************************/

/*=====================================================================*/

int main ( int argc, char *argv[] )
/************************************************************************
 * main                                                                 *
 *                                                                      *
 * Main program of ssfgsf.                                              *
 *                                                                      *
 * int main ( argc, argv )                                              *
 *                                                                      *
 * Input parameters:                                                    *
 *      argc            int		Number of arguments             *
 *      **argv          char            arguments array                 *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          2/06           Initial coding                  *
 ***********************************************************************/
{
    char infile[MXFLSZ], outfil[MXFLSZ], window[MXFLSZ], prognm[7];
    int opt, hasgsf, pagflg, iret;
/*---------------------------------------------------------------------*/
    strcpy ( prognm, "ssfgsf" );
    hasgsf = G_FALSE;
    pagflg = G_FALSE;

    /*
     * Set default subset window.
     */
    strcpy ( window, "-9999.;-9999.;-9999.;-9999." );

    /*
     * Parse command line arguments.
     */
    while ( ( opt = getopt ( argc, argv, "o:w:h" ) ) != -1 ) {
        switch ( opt ) {
	    case 'o':
	        strcpy ( outfil, optarg );
		hasgsf = G_TRUE;
	    break;

	    case 'w':
	        strcpy ( window, optarg );
	    break;

	    case 'h':
	    default:
		ip_help ( prognm, &pagflg, &iret, strlen(prognm) );
		exit (0);
	    break;
	}
    }

    /*
     * Check for any input error.
     */
    if ( argc - optind != 1 || hasgsf == G_FALSE ) {
	ip_help ( prognm, &pagflg, &iret, strlen(prognm) );
	exit (-1);
    }

    /*
     * Get input SSF file name.
     */
    strcpy ( infile, argv[optind] );

    /*
     * Call Fortran subroutine to do the job.
     */
    map_ssfgsf ( infile, outfil, window, &iret,
                 strlen(infile), strlen(outfil), strlen(window) );

    /*
     * Remove intermediate files.
     */
    remove ( DAFILE );
    remove ( SQFILE );

    return 0;
}
