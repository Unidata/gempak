#include "geminc.h"
#include "gemprm.h"

int main ( int argc, char **argv )
{
/************************************************************************
 * chgCWAcentroid                                                       *
 *                                                                      *
 * This program will change the CWA centroid information from the true	*
 * geographical centroid to the location of the attending WFO.		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	12/02						*
 ***********************************************************************/
int     ii, nn, found, nwfo, ier;
char	buffer[80], wt[200][80];
char	bnd[16], wfo[16], cenlat[16], cenlon[16], nparts[16];
char	dum[80], newlat[16], newlon[16];
FILE    *gembnd, *wfotbl;
/*---------------------------------------------------------------------*/

/*
 *      Open the GEMPAK CWA table and read in.
 */
    wfotbl = (FILE *)cfl_tbop ( "wfo.tbl", "stns", &ier );
    if ( ier != 0 )  {
	printf("Error opening WFO table\n" );
	exit (0);
    }
/*
 *      Open the GEMPAK bounds file.
 */
    gembnd = (FILE *)cfl_ropn ( argv[1], NULL, &ier );
    if ( ier != 0 )  {
	printf("Error opening file %s\n", argv[1] );
	exit (0);
    }

    cfl_tbnr ( wfotbl, &nwfo, &ier );
    for ( ii = 0; ii < nwfo; ii++ )  {
	cfl_trln ( wfotbl, sizeof(wt[ii]), wt[ii], &ier );
    }

    while ( ier == 0 )  {
	cfl_rdln ( gembnd, sizeof(buffer), buffer, &ier );
	if ( ier == 0 )  {
	    if ( buffer[0] == 'B' )  {
		sscanf( buffer, "%s %s %s %s %s %s",
		    bnd, wfo, cenlat, cenlon, nparts, wfo );
		nn = 0;
		found = G_FALSE;
		while ( found == G_FALSE && nn < nwfo )  {
		    if ( strncmp ( wfo, wt[nn], 3 ) == 0 )  {
			found = G_TRUE;
		    }
		    else  {
			nn++;
		    }
		}
		if ( found == G_TRUE )  {
		    sscanf( wt[nn], "%s %s %s %s %s %s %s",
                        dum, dum, dum, dum, dum, newlat, newlon );
		    cst_rpst ( buffer, cenlat, newlat, buffer, &ier );
		    cst_rpst ( buffer, cenlon, newlon, buffer, &ier );
		}
	    }

	    printf("%s\n", buffer );

	}
    }

    return 0;
}
