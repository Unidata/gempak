#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

extern	SpcInfo_t      newinfo;
extern	SpcInfo_t      spcinfo;

void vfgttxt ( char fname[], int *iret )
/************************************************************************
 * vfgttxt                                                              *
 *                                                                      *
 * This function gets and reads the ww----.txt file information which   *
 * is used to create SEL, SAW, SEV and the cancel text products for     *
 * SAW and SEL.                                                         *
 *                                                                      *
 * vfgttxt ( fname, iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *      fname[]		char		Input File Name                 *
 *									*
 * Output parameters:                                                   *
 *      *iret            int            Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC          2/00	Created					*
 * A. Hardy/GSC          5/00	Removed 'file not found' statement      *
 * A. Hardy/GSC         12/00	Removed '&' from iret			*
 ***********************************************************************/
{
    int          ierr;
/*---------------------------------------------------------------------*/
    *iret = 0;

   /*
    * Store the text filename.
    */

    strcpy ( spcinfo.file_info.filnam, fname );

   /*
    * Open text file to be read.
    */
    spcinfo.file_info.ifp = cfl_ropn(
	                  spcinfo.file_info.filnam, NULL, &ierr);
			  
    *iret = ierr;
    if ( *iret == 0 ) {
       /*
        * Read in verification file.
        */

        vfread ( iret );

       /*
        * Close text file.
        */

         cfl_clos ( spcinfo.file_info.ifp, iret );
         spcinfo.file_info.ifp = NULL;
    }
    /*
     * If the WW filename is incorrect, send an error.
     */
    else {
	 *iret = -1;
    }
}
