#include "xwcmn.h"
#include "color.h"

void xqcolr ( int *cbank, int *jcolr, unsigned long *colpxl, int *iret )
/************************************************************************
 * xqcolr								*
 *									*
 * This subroutine sets the foreground color for the X Windows device	*
 * driver.								*
 *									*
 * xqcolr  ( cbank, jcolr, colpxl, iret )				*
 *									*
 * Input parameters:							*
 *	*cbank		int	        Color bank ID			*
 *	*jcolr		int		Color number			*
 *									*
 * Output parameters:							*
 *      colpxl           unsigned long*  color index to the X ColorMap  *
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 **									*
 * Log:									*
 * C. Lin/EAI	         9/97						*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *      Exit if color bank number invalid
 */
        if ( (*cbank >= ColorBanks.nbank ) ||
                ( *cbank < 0 ) ) {
            *iret = G_NICBANK;
            return;
        }

/*
 *      Check that color bank has already been allocated
 */
        if ( (*cbank == GraphCid && !GColorIsInitialized ) ||
             (*cbank >  GraphCid && !allocflag[*cbank]) ) {
            *iret = G_NCBALOC;
            return;
        }

        *colpxl = ColorBanks.colrs[*cbank][*jcolr];

}
