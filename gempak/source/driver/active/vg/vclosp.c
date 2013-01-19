#include "vgcmn.h"

void vclosp ( int *iret )
/************************************************************************
 * vclosp                                                               *
 *                                                                      *
 * This function closes the output VG file.                             *
 *                                                                      *
 *  void vclosp ( iret )                                                *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret            int	        Return code                     *
 *					  0 = normal			*
 *					 -2 = error closing VG file	*
 *                                                                      *
 **                                                                     *
 * Log                                                                  *
 * J. Wu/GSCP	 02/01		Created	based on pclosp.c for PS driver	*
 ***********************************************************************/
{
    int		ier;
/*--------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     *	Close the file and mark it.
     */
    if ( opnfil )  {
	cvg_clos ( flun, &ier );
	if ( ier == G_NORMAL )	    
	    opnfil = G_FALSE;
	else 
	    *iret = -2;	
    }
}
