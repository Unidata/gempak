#include "geminc.h"
#include "gemprm.h"

#ifdef UNDERSCORE
#define map_gsfssf map_gsfssf_
#endif
void map_gsfssf ( const char*, const char*, int*,
                  size_t, size_t );

/************************************************************************
 * gsfssf.c                                                             *
 *                                                                      *
 * This module contains the main program of gsfssf.  The program gsfssf *
 * converts a GEMPAK Standard Format (GSF) to a map file in Sequential	*
 * Standard Format (SSF).						*
 *                                                                      *
 * CONTENTS:                                                            *
 *      main()           main program of gsfssf.                        *
 ***********************************************************************/

/*=====================================================================*/

int main ( int argc, char *argv[] )
/************************************************************************
 * main                                                                 *
 *                                                                      *
 * Main program of gsfssf.                                              *
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
    strcpy ( prognm, "gsfssf" );
    hasgsf = G_FALSE;
    pagflg = G_FALSE;

    /*
     * Parse command line arguments.
     */
    while ( ( opt = getopt ( argc, argv, "o:h" ) ) != -1 ) {
        switch ( opt ) {
	    case 'o':
	        strcpy ( outfil, optarg );
		hasgsf = G_TRUE;
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
    map_gsfssf ( infile, outfil, &iret,
                 strlen(infile), strlen(outfil) );

    return 0;
}
