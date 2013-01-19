#include "xwcmn.h"

void xinitclr ( void )
/************************************************************************
 * xinitclr                                                             *
 *                                                                      *
 * This function reads the GEMPAK color map table.			*
 *                                                                      *
 * xinitclr ()                                                          *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      None                                            *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC          11/97   extract from xinita.c                   *
 * T. Piper/SAIC	07/04	moved xscint back to xinita.c; this 	*
 *				solves an initialization problem in the	*
 *				16/24 bit case and solves a double	*
 *				initialization problem in GUI programs	*
 *				when NOT sharing colors.		*
 ***********************************************************************/
{

int	iret;

/*---------------------------------------------------------------------*/

	iret = G_NORMAL;

        /*
         * Read GEMPAK color map table
         */
	 
        cctabl ( NULL, &iret );

}
