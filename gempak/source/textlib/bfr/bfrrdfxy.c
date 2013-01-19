#include <mel_bufr.h>           /* BUFR library include file */
#include "geminc.h"
#include "gemprm.h"
#include "bfrcmn.h"

#define   LENBUF    256

void bfr_rdfxy ( char *fxytbl, int maxfxy, char* datatype, int *nfxy, 
                 int *fxy_i, int *fxy_vals, int *iret ) 
/************************************************************************
 * bfr_rdfxy                                                            *
 *                                                                      *
 * This subroutine opens, reads and closes the FXY table for a BUFR     *
 * message type.                                                        *
 *                                                                      *
 * bfr_rdfxy ( fxytbl, maxfxy, datatype, nfxy, fxy_i, fxy_vals, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*fxytbl		char	   FXY table name                       *
 *	maxfxy		int	   Maximum number of FXYs allowed       *
 *                                                                      *
 * Output parameters:                                                   *
 *      *datatype	char	   Header value in FXY table		*
 *      *nfxy		int	   Number of FXYs (descriptors)         *
 *	*fxy_i		int	   Array of decimal FXYs                *
 *      *fxy_vals	int	   Array of fxy_i value			* 
 *      *iret           int        Return code                      	*
 *				      0 = normal                        *
 *				     -7 = error opening FXY table       *
 *                                  -10 = error reading FXY table       *
 *				    -12 = too many descriptors in table *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D. Kidwell/NCEP   9/03	Extracted from SIGBENC sige routines    *
 * M. Li/SAIC	    10/03	Changed call sequence			*
 ***********************************************************************/
{
    int   	ij, nn, num, intarr[2], ipos, ier;
    FILE  	*fd;

    int		leverr;
    char 	errgrp[8], cc[12];
    char        buff[LENBUF], temp[LENBUF], *ptr;
/*---------------------------------------------------------------------*/
     *iret  = 0;
     *nfxy = 0;
     leverr = 0;
     strcpy ( errgrp, "BFR" );

    /* 
     * Open and determine number of records in the FXY file.
     */

     fd = cfl_tbop (fxytbl, "bufr", &ier);
     cfl_tbnr ( fd, &nn, &ier );

    /* 
     * Check for errors.
     */

     if ( ier != 0 ) {
         *iret = -7;
	 strcpy ( cc, fxytbl );
     }
     else if ( nn > maxfxy ) {
         *iret = -12;
	 cst_inch ( maxfxy, cc, &ier );
     }
     else {
	*nfxy = nn - 1;

        /* 
         * Read FXY table. 
         */

         for ( ij = 0; ij < nn; ij++ ) {
	     fxy_vals[ij] = IMISSD;
             cfl_trln ( fd, LENBUF, buff, &ier );

	     /*
	      * Read the header.
	      */
	     if ( ij == 0 ) {
		strcpy ( datatype, buff );
	     }

	     /*
	      * Read fxy_i and fxy_vals.
	      */
	     else {
		ptr = strchr(buff, '!');
		if (ptr) {
		   cst_srch ( 0, strlen(buff), "!", buff, &ipos, &ier );
		   cst_ncpy (temp, buff, ipos, &ier);
	        }
		else {
		   strcpy (temp, buff);
		}

		ptr = strchr(temp, '=');
		if (ptr) {
		   cst_ilst (temp, '=', IMISSD, 2, intarr, &num, &ier);
		   fxy_i[ij-1] = intarr[0];
		   fxy_vals[ij-1] = intarr[1];
		}
		else {
	           cst_numb ( temp, &fxy_i[ij-1], &ier );
		}
             	if ( ier != 0 ) {
                   *iret = -10;
	           strcpy ( cc, fxytbl );
                }
	     }
         }
     }

     if ( *iret != 0 ) {
         er_lmsg ( &leverr, errgrp, iret, cc, &ier,
                   strlen(errgrp), strlen(cc) );
     }

    /* 
     * Close FXY file.
     */

     cfl_clos( fd, &ier );
    
     return;
}
