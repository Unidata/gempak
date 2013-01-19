#include "geminc.h"
#include "gemprm.h"

#include "ctbcmn.h"


void ctb_hfdump ( int *iret )
/************************************************************************
 * ctb_pfdump								*
 *									*
 * This function dumps the info from the Hershey Fonts tables to the 	*
 * terminal screen.							*
 * 									*
 * ctb_hfdump ( iret )							*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	10/07	Created					*
 ***********************************************************************/
{
    int		ifont, ichr, ipnt;

/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;

    for ( ifont = 0; ifont < _nhfont; ifont++ ) {
	printf ( "\nFont id number = %d\n",
	       	 _hfontTbl[ifont].font_code );

	for ( ichr = 0; ichr < _hfontTbl[ifont].numchr; ichr++ ) {
	    printf ( "ASCII Value = %d (%c) -- Number of points = %d\n",
		    _hfontTbl[ifont].character[ichr].ascii_val,
		    _hfontTbl[ifont].character[ichr].ascii_val,
		    _hfontTbl[ifont].character[ichr].npts );
	    printf ( "        Min X = %d -- Max X = %d\n", 
		    _hfontTbl[ifont].character[ichr].xmin,
		    _hfontTbl[ifont].character[ichr].xmax );

	    printf ( "Original encoded character string = \n%s\n",
		    _hfontTbl[ifont].character[ichr].point_code );

	    for ( ipnt = 0;
		  ipnt < _hfontTbl[ifont].character[ichr].npts;
		  ipnt++ ) {
		printf ( "(%3d,%3d)  ",
		    _hfontTbl[ifont].character[ichr].point[ipnt].x,
		    _hfontTbl[ifont].character[ichr].point[ipnt].y );
		if ( (ipnt+1) % 6 == 0 ) printf ( "\n" );
	    }
	    printf ( "\n" );
	}
    }

}

/*=====================================================================*/
