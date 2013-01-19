#include "pscmn.h"
#include "color.h"

void pupdclr ( int *itype )
/************************************************************************
 * pupdclr                                                              *
 *                                                                      *
 * This function updates the graphic color bank when switching from 	*
 * XWP	device to PS device 						*
 *                                                                      *
 * pupdclr (itype)	                                                *
 *                                                                      *
 * Input parameters:                                                    *
 *	*itype	int		device type (color, bw, gs)		*
 *                                                                      *
 * Output parameters:                                                   *
 *                      None                                            *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC          11/97   		                        *
 ***********************************************************************/
{
int	ier;
/*---------------------------------------------------------------------*/

	ier = G_NORMAL;

	if ( *itype == 1 ) {
	    strcpy( tblnam, "coltbl.psg" );
	    pscint ( &ier );
	    if  ( ier != G_NORMAL)  
	        return;
	}
	else {
	    pcvtclr();
	}
}
