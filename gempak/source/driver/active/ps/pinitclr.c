#include "pscmn.h"
#include "color.h"

void pinitclr ( int *itype )
/************************************************************************
 * pinitclr                                                             *
 *                                                                      *
 * This function initializes graphic color bank for the PS driver	*
 *                                                                      *
 * pinitclr (itype)	                                                *
 *                                                                      *
 * Input parameters:                                                    *
 *	*itype	int		device type (color, bw, gs)		*
 *                                                                      *
 * Output parameters:                                                   *
 *                      None                                            *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC          11/97   					*
 ***********************************************************************/
{
int	ier;
/*---------------------------------------------------------------------*/

	ier = G_NORMAL;

	/*
	 * read GEMPAK color map table.
         */
        cctabl( "coltbl.tbl", &ier );
        if  ( ier != G_NORMAL )  
		return;
	/*
	 * load a local color table
	 */
	if ( *itype == 1 ) {
	    strcpy( tblnam, "coltbl.psg" );
	    pscint ( &ier );
	    if  ( ier != G_NORMAL)  
	        return;

	}
	else {
	    tblnam[0] = '\0';
	    pscint ( &ier );
	    if  ( ier != G_NORMAL)  
	        return;

	    pcvtclr();
	}
}
