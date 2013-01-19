#include "cvgcmn.h"
#include "pgprm.h"


int cvg_getFlghtLvl ( const char *flightLvl )                            
/************************************************************************
 * cvg_getFlgtLvl							*
 *									*
 * This function reads a flight level string and returns the equivalent *
 * integer value.  A value of "SFC" will return 0.  A missing value   	*
 * (i.e. a blank string for flightLvl) will return a -1.		*
 *									*
 * cvg_getFlgtLvl( flightLvl )          				*
 *									*
 * Input parameters:							*
 *	*flightLvl	const char	flight level string		*
 *									*
 * Output parameters:							*
 *			None.						*
 * Return:								*
 *			int		flight level integer value	*
 **									*
 * E. Safford/SAIC	08/04	create					*
 ***********************************************************************/
{
    int		level = 0;
/*---------------------------------------------------------------------*/

    if( strlen( flightLvl ) == (size_t)0 ) {
       level = -1;
    }
    else if( strcmp( flightLvl, "SFC") == 0 ) {
       level = 0;
    }
    else {
       level = atoi( flightLvl );
    }

    return( level ); 
}

/*=====================================================================*/
