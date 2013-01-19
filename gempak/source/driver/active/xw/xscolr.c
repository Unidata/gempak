#include "xwcmn.h"
#include "color.h"

void xscolr ( int *cbank, int *jcolr, int *iret )
/************************************************************************
 * xscolr								*
 *									*
 * This subroutine sets the foreground color for the X Windows device	*
 * driver.								*
 *									*
 * xscolr  ( cbank, jcolr, iret )					*
 *									*
 * Input parameters:							*
 *	*cbank		int	        Color bank ID			*
 *	*jcolr		int		Color number			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 **									*
 * Log:									*
 * J. Whistler/SSAI	 7/91	C call for X device driver		*
 * M. desJardins/NMC	01/91	GEMPAK 5.1				*
 * M. desJardins/NMC	 1/92	Change color map method			*
 * C. Lin/EAI	         1/94	Add type parameter   			*
 * S. Jacobs/NMC	 7/94	General clean up			*
 * C. Lin/EAI	         7/94	multi-window, multi-pixmap		*
 * D. Himes/COMET        8/94	Save global foreground color in ifrcol  *
 *                              if itype is 0 for graphics cell.        *
 * C. Lin/EAI	         1/95	Add Radar image type			*
 * C. Lin/EAI	         2/95	Clean up & error handling		*
 *				Use ColorBank structure			*
 * R. Tian/SAIC		05/02	Added FaxCid				*
 ***********************************************************************/
{
	int		icolr;

	GC              gemgc; 
/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	if  ( gemwindow[current_window].name[0] == '\0' )
	    return;

	gemgc = gemwindow[current_window].gc; 

	/*
 	 * Set this as the current color for the 
         * correct set of colors.
 	 */

	if ( *cbank == GraphCid ) {

		/*
 		 * Check for background color.
 		 */

		if  ( ( *jcolr == 101 ) || ( *jcolr == 0 ) ) {
	    		icolr = ibkcol;
		}
		else if ( ( *jcolr >= 1 ) && ( *jcolr < ColorBanks.banks[GraphCid] )) { 

			/*
 		 	 * Check that color number is 
			 *  in the proper range.
 		 	 */

	         	icolr  = *jcolr;
  	    		ifrcol = *jcolr; 

		}
		else {
			/*
 		 	 * Otherwise, do nothing.
 		 	 */

	    		return;
		}

	    	XSetForeground ( gemdisplay, gemgc, ColorBanks.colrs[GraphCid][icolr] );
	}
	else if ( *cbank == SatCid ) {

		if ( (*jcolr >= 0) && ( *jcolr < ColorBanks.banks[SatCid] ) )
	    		XSetForeground ( gemdisplay, gemgc, 
					ColorBanks.colrs[SatCid][*jcolr] );

	}
	else if ( *cbank == RadCid ) {

		if ( (*jcolr >= 0) && ( *jcolr < ColorBanks.banks[RadCid] ) ) 
	    		XSetForeground ( gemdisplay, gemgc, 
					ColorBanks.colrs[RadCid][*jcolr] );

	}
	else if ( *cbank == FaxCid ) {

		if ( (*jcolr >= 0) && ( *jcolr < ColorBanks.banks[FaxCid] ) )
	    		XSetForeground ( gemdisplay, gemgc, 
					ColorBanks.colrs[FaxCid][*jcolr] );

	}

}
