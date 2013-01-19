#include "vgcmn.h"

void vendd ( int *ieop, int *iret )
/************************************************************************
 * vendd								*
 *									*
 * This function closes the VG output file.				*
 *									*
 * vendd  ( ieop, iret )						*
 *									*
 * Input parameters:							*
 *	*ieop		int	Pointer to end plotting flag		*
 *				    0 = retain subprocess		*
 *				    1 = stop subprocess			*
 *									*
 * Output parameters:							*
 *	*iret		int	Return code				*
 *				    0 = normal				*
 *				   -2 = error closing VG file		*
 **									*
 * Log:									*
 * J. Wu/GSC		02/01	Created	based on pendd.c for PS driver	*
 ***********************************************************************/
{
    int		ier;
/*--------------------------------------------------------------------*/

    *iret = G_NORMAL;

    /*
     *	Close file at the end of plot.
     */
    if ( *ieop == 1 ) { 
        vclosp( &ier );
	if ( ier != G_NORMAL )  *iret = -2; 
    }
}
