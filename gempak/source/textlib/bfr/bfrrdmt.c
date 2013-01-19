#include <mel_bufr.h>           /* BUFR library include file */
#include "geminc.h"
#include "gemprm.h"
#include "bfrcmn.h"

#define   LENBUF    256

void bfr_rdmt ( bfrtbl_t ptrtbl[], int *nitem, int *iret ) 
/************************************************************************
 * bfr_rdmt                                                            	*
 *                                                                      *
 * This subroutine opens, reads and closes the master FXY table 	*
 * for a BUFR message type.                                             *
 *                                                                      *
 * bfr_rdfxy ( ptrtbl, nitem, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      ptrtbl[] 	bfrtbl_t   Bufr table structure.		*
 *	*nitem		int	   Total items in the table		*
 *      *iret           int        Return code                      	*
 *				      0 = normal                        *
 *				     -1 = error opening FXY table       *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC	    10/03	Created                                	* 
 * M. Li/SAIC	    11/03	Changed table name.			*
 * M. Li/SAIC	    05/04	Read the fourth column in the table	*
 ***********************************************************************/
{
    int   	ii, lenb, ier;
    FILE  	*fp;
    char 	mttbl[20], buff[LENBUF];
/*---------------------------------------------------------------------*/
     *iret  = 0;
     strcpy ( mttbl, "masterfxy.tbl");

    /* 
     * Open the master FXY table.
     */

     fp = cfl_tbop (mttbl, "bufr", &ier);
     if ( fp == NULL || ier != 0 ) {
	 *iret = -1;
	 return;
     }

    /*
     * Read the table.
     */
     ii = 0;
     while ( !feof(fp) ) {

         /*
          * read a record
          */
          cfl_trln(fp, LENBUF, buff, &ier);

          if ( ier == 0 ) {
	      cst_rxbl ( buff, buff, &lenb, &ier );
	      ptrtbl[ii].alias   = (char *) malloc ( lenb * sizeof(char));
	      ptrtbl[ii].fxyfils = (char *) malloc ( lenb * sizeof(char));
	      ptrtbl[ii].bufrout = (char *) malloc ( lenb * sizeof(char));
	      ptrtbl[ii].vgftype = (char *) malloc ( lenb * sizeof(char));
	      sscanf(buff, "%s %s %s %s", ptrtbl[ii].alias, ptrtbl[ii].fxyfils, 
		     ptrtbl[ii].bufrout, ptrtbl[ii].vgftype);

	      ii++;
	  }
     }
     *nitem = ii;

     cfl_clos( fp, &ier );
     return;
}
