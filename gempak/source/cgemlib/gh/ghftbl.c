#include "geminc.h"
#include "gemprm.h"
#include "ghcmn.h"


void gh_ftbl ( int *iret )
/************************************************************************
 * gh_ftbl								*
 *									*
 * This subroutine the memory allocated space for the color table used  *
 * by GPTPC.								*
 *									*
 * gh_ftbl ( iret )  							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC		 6/01   					*
 ***********************************************************************/
{
    *iret = 0;

   /*
    *  Free the allocated memory.
    */

    free ( table );
}
