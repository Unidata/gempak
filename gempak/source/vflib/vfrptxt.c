#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

extern  SpcInfo_t      newinfo;
extern	SpcInfo_t      spcinfo;

void vfrptxt ( char fname[], int *iret )
/************************************************************************
 * vfrptxt                                                              *
 *                                                                      *
 * This function gets and reads the ww----.txt file information which   *
 * is used to create SEL, SAW, SEV and the cancel text products for     *
 * SAW and SEL.                                                         *
 *                                                                      *
 * vfrptxt ( fname, iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *      fname[]		char		Input File Name                 *
 *									*
 * Output parameters:                                                   *
 *      *iret           int		Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC          2/00	Created					*
 * A. Hardy/GSC          3/00	Added check on the return code 		*
 * A. Hardy/GSC          5/00	Removed print statement on bad filename *
 * A. Hardy/GSC         12/00	Removed '&' from iret                   *
 ***********************************************************************/
{
    int		 ierr;
/*---------------------------------------------------------------------*/
    *iret = 0;

   /*
    * Store the text filename.
    */

    strcpy ( newinfo.file_info.filnam, fname );

   /*
    * Open text file to be read.
    */
    newinfo.file_info.ifp = cfl_ropn(
	                  newinfo.file_info.filnam, NULL, &ierr);

    *iret = ierr;
    if ( *iret == 0 ) {
       /*
        * Read in verification file.
        */

        vfrdrp ( iret );

       /*
        * Close text file.
        */

         cfl_clos ( newinfo.file_info.ifp, iret );
         newinfo.file_info.ifp = NULL;
    }
    /*
     * If the WW filename is incorrect, send an error.
     */

    else {
         *iret = -1;
    }
}
