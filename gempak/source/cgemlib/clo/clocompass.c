#include "geminc.h"
#include "gemprm.h"

void clo_compass ( float *dir, char *cmpdir, int *icmp, int *iret )
/************************************************************************
 * clo_compass                                                    	*
 *                                                                      *
 * This function takes a direction (degrees from N) and computes the	*
 * 16-point compass direction (N, NNE, NE, ENE, etc.)			*
 *                                                                      *
 * clo_compass ( dir, cmpdir, icmp, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*dir	float	Direction (degrees from N)			*
 *									*
 * Output parameters:                                                   *
 *	*cmpdir	char	16-point compass direction			*
 *	*icmp	int	Length of compass direction			*
 *	*iret	int	Return value					*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 4/98	Create					*
 * T. Piper/GSC		10/98	Prolog update				*
 * M. Li/GSC		10/99	Added a check for invalid input;	*
 *				Modified to be called by FORTRAN	*
 ***********************************************************************/
{
int	idir;
char    dirs[][4]={"N","NNE","NE","ENE","E","ESE","SE","SSE",
                   "S","SSW","SW","WSW","W","WNW","NW","NNW","N"};
/*---------------------------------------------------------------------*/

	*iret = 0;

/*
 * 	Check the direction input for out of range
 */

        if ( *dir < 0.0F || *dir > 360.0F ) {
 	    *iret = -1;
 
            *cmpdir = '\0';
            *icmp = 0;

            return;
	}


	idir = G_NINT( *dir / 22.5F );

        strcpy( cmpdir, dirs[idir] );
	*icmp = (int)strlen( cmpdir );

	return;

}
