#include "ardcmn.h"

void aclear ( int *iret )
/************************************************************************
 * aclear								*
 *									*
 * This subroutine clears the RBK file by reintializing the output      *
 * buffer.								*
 *									*
 * aclear  ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * A. Hardy/GSC		9/98		Modified from uclear.c          *
 ***********************************************************************/
{

        int      ier;
/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Return if no file has been opened.
 */
	if  ( ! opnfil ) {
           numout = 0;
        }
        else {
/*
 *          Close file to clear it out.
 */
            aclosp ( &ier );
        }
}
