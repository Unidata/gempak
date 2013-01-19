#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"

void cas_wrmhdr ( FILE *ifpout, char *dattim, char *fhour, char *fxytbl,
                 char *center, int *iret )
/************************************************************************
 * cas_wrmhdr								*
 *                                                                      *
 * This function writes the message header to the ASCII file, based on	*
 * the input parameter values. 						*
 *                                                                      *
 * cas_wrmhdr ( ifpout, dattim, fhour, fxytbl, center, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout		FILE		Output file name pointer	*
 *	*dattim		char		GEMPAK data/time     		*
 *	*fhour 		char		Forecast hour        		*
 *	*fxytbl		char		Chart type/alias/FXY file name	*
 *	*center		char		Originating center id		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		05/04		Modified from cas_wrhdr		*	
 * M. Li/SAIC		09/04		Removed HI_BASE, HI_TOP, 	*
 *				        MID_TOP, and MID_BASE		* 
 ***********************************************************************/
{
    int		idtarr[5], jdtarr[5], ifmin, ihr;
    char	newl[5];
    int         ier;
/*---------------------------------------------------------------------*/
    *iret   = 0;

   /*
    * Get the valid date/time and the valid date/time arrays.
    */

    ti_ctoi ( dattim, idtarr, &ier, strlen (dattim) );

    cst_numb ( fhour, &ihr, &ier );
    ifmin = ihr * 60;
    ti_addm ( idtarr, &ifmin, jdtarr, &ier);

   /*
    * Write out the header information.
    */

    fprintf (ifpout,"%s\n", center );

    fprintf (ifpout,"%d   %02d   %02d   %02d   %02d\n", idtarr[0], idtarr[1],
                       idtarr[2], idtarr[3], idtarr[4]);

    fprintf (ifpout,"%d   %02d   %02d   %02d   %02d\n", jdtarr[0], jdtarr[1],
                       jdtarr[2], jdtarr[3], jdtarr[4]);

   /*
    * Check for level.
    */

    cst_lcuc ( fxytbl, newl, &ier );
    if ( strcmp ( newl, "SWH") == 0 ) {
        fprintf (ifpout,"%11.1f     %11.1f\n", HI_BASE, HI_TOP);
    }
    else if ( strcmp ( newl, "SWM") == 0 ) {
        fprintf (ifpout,"%11.1f     %11.1f\n", MID_BASE, MID_TOP);
    }
    else {
        fprintf (ifpout,"%11.1f     %11.1f\n", SIGRLMS, SIGRLMS );
    }

}
