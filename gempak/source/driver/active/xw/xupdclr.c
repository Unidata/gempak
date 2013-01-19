#include "xwcmn.h"
#include "color.h"


void xupdclr ( void )
/************************************************************************
 * xupdclr                                                              *
 *                                                                      *
 * This function updates the current selected color structure gemColr	*
 * for XW driver 							*
 *                                                                      *
 * void xupdclr ()                                                      *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      None                                            *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC          11/97                                           *
 * T. Piper/SAIC	01/04	changed colrs to Pixel colr_long	*
 * T. Piper/SAIC	07/04	Replaced ratio with COLR_SCAL		*
 ***********************************************************************/
{
int	ii, bank, ncolors, iret;
int     red, green, blue;
Pixel	colr_long[MAXCOLORS];
XColor  def;

/*---------------------------------------------------------------------*/

    bank = GraphCid;
    xqclrs( &bank, &ncolors, colr_long, &iret);
    if ( iret != 0 ) {
	return;
    } 
    else {
	if ( ncolors > GRAPH_COLORS ) {
            ncolors = GRAPH_COLORS;
        }
    }
    /*
     * update RGB components
     */
    for ( ii=0; ii<ncolors; ii++ ) {
	def.pixel = colr_long[ii];
	XQueryColor( gemdisplay, gemmap, &def );
	red  = def.red/COLR_SCAL;
	green = def.green/COLR_SCAL;
	blue  = def.blue/COLR_SCAL;

	cscrgb ( &ii, &red, &green, &blue, &iret );
    }
}

