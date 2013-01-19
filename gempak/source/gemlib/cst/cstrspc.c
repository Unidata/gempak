#include "geminc.h"
#include "gemprm.h"

void cst_rspc ( char *str, int *iret )
/************************************************************************
 * cst_rspc								*
 *									*
 * This subroutine replaces space characters with underscores.		*
 *									*
 * cst_rspc ( str, iret )						*
 *									*
 * Input/Output parameters:						*
 *	*str		char		Input string			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 **									*
 * Log:									*
 * R. Tian/SAIC  	 6/04						*
 ***********************************************************************/
{
    size_t ic;
/*---------------------------------------------------------------------*/
    *iret = 0;

    for ( ic = 0; ic < strlen ( str ); ic++ ) {
        if ( str[ic] == ' ' ) {
            str[ic] = '_';
        }
    }
}
