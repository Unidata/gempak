#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

extern  SpcInfo_t    newinfo;
extern  SpcInfo_t    spcinfo;

void vfspc ( char fname[], char strin[], int *iret )
/************************************************************************
 * vfspc								*
 *                                                                      *
 * This function reads watch format text product created by NMAP and    *
 * creates the SAW, SEV, SEL, WCL and cancelled SAW and SEL SPC weather *
 * watch text products.        						*
 *                                                                      *
 * vfspc ( fname, strin, iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *      fname[]		char		Input File Name			*
 *	strin[]		char		Continuing Watches String	*
 *									*
 * Output parameters:                                                   *
 *      *iret            int            Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC          7/99	Created					*
 * M. Li/GSC		10/99	Added a '\r' to the end of each line	*
 * A. Hardy/GSC         11/99	Added cancel option			*
 * A. Hardy/GSC		 2/00   Extracted from SPCTXT;added strin       *
 * A. Hardy/GSC		 3/00   Added VFWWCL; cleaned up prolog         *
 * A. Hardy/GSC		 3/00   Added VFWOUI, VFWAWN and VFWPWN		*
 * A. Hardy/GSC		 5/00   Added VFWWCP				*
 * A. Hardy/GSC		12/00   Removed '&' from iret			*
 * R. Tian/SAIC		06/02	Removed call to vfwwcl			*
 * T. Piper/SAIC	07/05	Removed calls to vfwawn and vfwpwn	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    *iret = 0;
   /*
    * Read the text file.
    */

    vfgttxt ( fname, iret );

   /*
    * Create the text files.
    */
     if ( *iret == 0 ) {
         vfwsel ( strin, iret );
         vfwsaw ( iret );
         vfwsev ( iret );
         vfwoui ( iret );
         vfwwcp ( iret );

   /*
    * Create the cancel text files.
    */
         vfcnsaw ( iret );
         vfcnsel ( iret );
      }
}
