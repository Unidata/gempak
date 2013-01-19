#include "geminc.h"
#include "gemprm.h"

void cgr_linelen ( float *lat, float *lon, int np, float *length, 
		   int *iret )
/************************************************************************
 * cgr_linelen								*
 *									*
 * This routine calculates the running length of an input line.	The 	*
 * input points must be in the map coordinates.	The return length is	*
 * nautical miles.							*
 *									*
 * cgr_linelen ( lat, lon, np, length, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*lat	float	array of lats	 				*
 *	*lon	float	array of lons					*
 *	np	int	number of points in the array			*
 *									*
 * Output parameters:                                                   *
 *	*length	float	running length of the input line		*
 *	*iret	int	return code. 0: normal				*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          4/06	Created					*
 ***********************************************************************/
{
    int		ii, one, ier;
    float	dist;
/*---------------------------------------------------------------------*/

    one	    = 1;
    *iret   = 0;
    *length = 0.0;

    for ( ii = 0; ii < np - 1; ii++ ) {

        clo_dist ( &lat[ ii ], &lon[ ii ], &one, &lat[ ii + 1 ],
                   &lon[ ii + 1 ], &dist, &ier );

        if ( ier == 0 ) *length += dist;
	
    }

    /*
     *  Convert from meters to nautical miles.
     */
    *length *= M2NM;

}
