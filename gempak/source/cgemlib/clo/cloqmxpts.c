#include "geminc.h"
#include "gemprm.h"
#include "clocmn.h"

void clo_qmxpts ( char *which, int *mxpts, int *iret )
/************************************************************************
 * clo_qmxpts								*
 *									*
 * Returns the max number of points allowed for STATION or BOIUNDS.	*
 *									*
 * clo_qmxpts ( which, mxpts, iret )					*
 *									*
 * Input parameters:							*
 * 	*which		char	Type of query - STATION or BOUNDS	*
 *									*
 * Output parameters:							*
 *	*mxpts		int	max points for stations or bounds	*
 *	*iret		int	Return code				*
 *				0  - Normal				*
 *				-1 - Invalid type			*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC	 	 6/05	initial coding				*
 ***********************************************************************/
{
    int		ier;
    char	type[16];
/*---------------------------------------------------------------------*/

    *iret = 0;
    
    cst_lcuc ( which, type, &ier );
    if ( strcmp ( type, "STATION" ) == 0 ) {
        *mxpts = _mxspts;
    }
    else if ( strcmp ( type, "BOUNDS" ) == 0 ) {
        *mxpts = _mxbpts;   
    }
    else {
        *mxpts = IMISSD;
	*iret = -1;        
    } 
      
}
