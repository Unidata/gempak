#include "geminc.h"
#include "gemprm.h"

void clo_cmpdir ( char *cmpdir, float *dir, int *iret )
/************************************************************************
 * clo_cmpdir                                                    	*
 *                                                                      *
 * This function takes a 16-point compass direction (N, NNE, NE, ENE,	*  
 * etc.) and computes the direction (degrees from N).			*
 *                                                                      *
 * clo_cmpdir (cmpdir, dir, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	*cmpdir	char	16-point compass direction (N, NNE, NE ...)	*
 *									*
 * Output parameters:                                                   *
 *	*dir	float 	Direction (degrees from N)			*	
 *	*iret	int	Return value					*
 *			= 0  -- OK					*
 *			= -1 -- invalid input				*	
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		10/99	Initial coding				*
 * F. J. Yen/NCEP	 8/01	Removed parameter icmp			*
 ***********************************************************************/
{
int	ii;
char    dirs[][4]={"N","NNE","NE","ENE","E","ESE","SE","SSE",
                   "S","SSW","SW","WSW","W","WNW","NW","NNW","N"};
/*---------------------------------------------------------------------*/

	*iret = -1;
	*dir = RMISSD;

	for ( ii = 0; ii < 16; ii++) {
       	    if ( strcmp(cmpdir, dirs[ii]) == 0 ) {              	        
                *dir = 22.5F * (float)ii; 
                *iret = 0;   
            }  
        }    

	return;

}
