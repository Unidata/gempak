#include <mel_bufr.h>        	/* BUFR library include file */
#include "geminc.h"
#include "gemprm.h"
#include "bfrcmn.h"

#define   NITEMS    10  	/* no. of elems. in structure + 
				   fxys (008007 & 008011) */
#define   TOPINFO   18          /* 1st 15 FXYs + 3 trop counts */

void bfr_trp ( char *ofname, int rnum, trop_t *ptrr, int hnum, 
               trophi_t *ptrh, int lnum, troplo_t *ptrl, int itime[], 
               int jtime[], int *nfxy, int *fxy_i, int *fxy_vals, 
	       int idcent, int info_bufr[], char *section2, int *iret ) 
/************************************************************************
 * bfr_trp                                                              *
 *                                                                      *
 * This subroutine encodes the High Level Significant Weather ASCII     *
 * tropopause information into BUFR format. 				*
 *                                                                      *
 * bfr_trp ( ofname, rnum, ptrr, hnum, ptrh, lnum, ptrl, itime,         *
 *           jtime, nfxy, fxy_i, fxy_vals, idcent, info_bufr, section2, *
 *	     iret )							*
 *                                                                      *
 * Input parameters:                                                    *
 *      *ofname         char       Output file name                     *
 *      rnum            int	   Number of regular trop. levels       *
 *      *ptrr		trop_t     Pointer to reg. TROP link list	*
 *      hnum            int	   Number of regular trop. levels       *
 *      *ptrh		trophi_t   Pointer to high TROP link list	*
 *      lnum            int	   Number of regular trop. levels       *
 *      *ptrl		troplo_t   Pointer to low TROP link list	*
 *      itime[]		int	   Issued date/time 			*
 *      jtime[]		int	   Forecasted date/time 		*
 *      *nfxy           int        Number of FXYs                       *
 *      *fxy_i          int        Array of decimal FXYs                *
 *      *fxy_vals       int        Array of fxy_i value                 *
 *      idcent          int        Originating center id                *
 *      info_bufr[]     int        BUFR initialization values           *
 *      *section2       char       Section2 SWM area string             *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int        Return code                      	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D. Kidwell/NCEP   9/03       From sigetrp                            *
 * M. Li/SAIC       10/03       Changed call sequence                   *
 * M. Li/SAIC       11/03       Modified the comparion of fxy_vals      *
 * M. Li/SAIC	    01/04	Free values				*
 * M. Li/SAIC	    04/04	Removed trp_fxy				*
 * M. Li/SAIC       09/04       Added idcent                            *
 * M. Li/SAIC       10/04       Added info_bufr and section2            *
 ***********************************************************************/
{
    float 	*values;        /* array of data values */
    float       tmpval1, tmpval2;
    int  	num_vals;       /* number of values in array */
    Data_MixVal_t *val_mx;


    int   	ij, jj, ier;
    FILE  	*fptr;

    int		nerr, leverr, num_trops, icnt,
                trnum, tlnum, thnum, mreg, mlow, mhi; 
    float       first_order;
    char 	errgrp[8];
    Boolean	readflg, done, first; 
/*---------------------------------------------------------------------*/
     *iret   = 0;
     leverr  = 0;
     readflg = False; 
     strcpy ( errgrp, "BFR" );

     fptr = cas_open ( ofname, readflg, &ier );
     if ( ier != 0 ) {
         nerr = -6;
         er_lmsg ( &leverr, errgrp, &nerr, ofname, &ier,
                      strlen(errgrp), strlen(ofname) );
	 *iret = nerr;
	 return;
     }

    /* 
     * Create a single array containing the TROP information.
     * Number of data values to be read including replication 
     * factors.
     *
     * Determine the array size for the data set. 
     */

     icnt = 0;
     if (rnum > 0) icnt++;
     if (hnum > 0) icnt++;
     if (lnum > 0) icnt++;

     if ( icnt == 0 ) {
         num_vals  =  TOPINFO;
     }
     else {
         num_trops =  rnum + hnum + lnum;
         num_vals  =  ( num_trops * NITEMS ) + TOPINFO;
     }

    /*
     * Memory allocation for the values.
     */

     values = (float *)malloc( sizeof(float) * (uint_t)num_vals);
     val_mx = (Data_MixVal_t *) malloc( sizeof(Data_MixVal_t));
    
    /* 
     * Read data structure into data array. 
     */

     jj = 0;
     if ( idcent != IMISSD ) {
         values[jj] = (float)idcent;
     }
     else if ( fxy_vals[jj] != IMISSD ) {
         values[jj] = (float)fxy_vals[jj];
     }
     else {
         values[jj] = 7.0F;
     }
     jj++;
     if ( fxy_vals[jj] != IMISSD ) {
         values[jj] = (float)fxy_vals[jj];
     }
     else {
         values[jj] = 16.0F;
     }
     jj++;
     values[jj] = (float)itime[0]; jj++;
     values[jj] = (float)itime[1]; jj++;
     values[jj] = (float)itime[2]; jj++;
     values[jj] = (float)itime[3]; jj++;
     values[jj] = (float)itime[4]; jj++;
     if ( fxy_vals[jj] != IMISSD ) {
         values[jj] = (float)fxy_vals[jj];
     }
     else {
         values[jj] = 4.0F;
     }
     jj++;
     values[jj] = (float)jtime[0]; jj++;;
     values[jj] = (float)jtime[1]; jj++;
     values[jj] = (float)jtime[2]; jj++;
     values[jj] = (float)jtime[3]; jj++;
     values[jj] = (float)jtime[4]; jj++;
     if ( fxy_vals[jj] != IMISSD ) {
         values[jj] = (float)fxy_vals[jj];
     }
     else {
         values[jj] = 7620.0F;
     }
     jj++;
     if ( fxy_vals[jj] != IMISSD ) {
         values[jj] = (float)fxy_vals[jj];
     }
     else {
         values[jj] = 19200.0F;
     }
     jj++;

    /*
     * Determine if there are > 250 tropopause boxes for each box type. 
     * If so, write out multiple records with 250 elements in each group.
     */
     mreg = 0;
     if ( rnum > 250 ) {
         mreg = rnum/250; 
	 icnt = icnt + mreg;
     }
     mlow = 0;
     if ( lnum > 250 ) {
         mlow = lnum/250; 
	 icnt = icnt + mlow;
     }
     mhi = 0;
     if ( hnum > 250 ) {
         mhi = hnum/250; 
	 icnt = icnt + mhi;
     }

     values[jj] = ( float ) icnt; jj++; 

     if ( icnt != 0 ) {
	 done = False;
         if ( rnum > 0 ) {
	     trnum = rnum;
	     first = True;
	     while ( !done ) {
		if ( trnum > 250 ) {
		    trnum = trnum - 250;
		    rnum  = 250;
		}
		else {
		    rnum  = trnum;
		    done = True;
		}
     
                first_order = (float) BUFR_MISSING_VALUE;

                if ( first && jj < *nfxy && fxy_vals[jj+1] != IMISSD ) {
                    tmpval1 = (float)fxy_vals[jj+1];
                }
                else {
                    tmpval1 = 16.0F;
                }
                values[jj] = tmpval1; jj++;

                if ( first && jj < *nfxy && fxy_vals[jj+1] != IMISSD ) {
                    tmpval2 = (float)fxy_vals[jj+1];
                }
                else {
                    tmpval2 = 0.0F;
                }
                values[jj] = tmpval2; jj++;
                first = False;

                values[jj] = first_order; jj++;
                values[jj] = (float)rnum; jj++;

                for ( ij = 0; ij < rnum; ij++) {

                   /*
	            * Fill in latitude, longitude and height values.
	            */

                    values[jj] = (float) ptrr->lat; jj++;
                    values[jj] = (float) ptrr->lon; jj++;

	            if ( ptrr->level < 0.0F ) {
                        values[jj] = (float) BUFR_MISSING_VALUE; jj++;
	            }
	            else {
	                values[jj] = (float) ptrr->level; jj++;
	            }
                    ptrr = ptrr -> next;
    	        }
                values[jj] = (float) BUFR_MISSING_VALUE; jj++;
                values[jj] = (float) BUFR_MISSING_VALUE; jj++;
                values[jj] = (float) BUFR_MISSING_VALUE; jj++;
	     } /* while */
         }
	 done = False;
         if ( lnum > 0 ) {
	     tlnum = lnum;
	     first = True;
	     while ( !done ) {
		 if ( tlnum > 250 ) {
		    tlnum = tlnum - 250;
		    lnum  = 250;
		 }
		 else {
		    lnum  = tlnum;
		    done = True;
		 }

                 first_order = 3.0F;

             	 if ( first && jj < *nfxy && fxy_vals[jj] != IMISSD ) {
                    tmpval1 = (float)fxy_vals[jj];
                 }
                 else {
                    tmpval1 = 16.0F;
                 }
                 values[jj] = tmpval1; jj++;

                 if ( first && jj < *nfxy && fxy_vals[jj] != IMISSD ) {
                    tmpval2 = (float)fxy_vals[jj];
                 }
                 else {
                    tmpval2 = 0.0F;
                 }
                 values[jj] = tmpval2; jj++;
		 first = False;

                 values[jj] = first_order; jj++;
                 values[jj] = (float)lnum; jj++;
    
                 for ( ij = 0; ij < lnum; ij++) {
                    /*
    	             * Fill in latitude, longitude and height values.
	             */

                     values[jj] = (float) ptrl->lat; jj++;
                     values[jj] = (float) ptrl->lon; jj++;

	             if ( ptrl->level < 0.0F ) {
                         values[jj] = (float) BUFR_MISSING_VALUE; jj++;
	             }
	             else {
	                 values[jj] = (float) ptrl->level; jj++;
	             }
                     ptrl = ptrl -> next;
    	         }

                 values[jj] = (float) BUFR_MISSING_VALUE; jj++;
                 values[jj] = (float) BUFR_MISSING_VALUE; jj++;
                 values[jj] = (float) BUFR_MISSING_VALUE; jj++;
	     } 
         }
	 done = False;
         if ( hnum > 0 ) {
	     thnum = hnum;
	     first = True;
	     while ( !done ) {
		 if ( thnum > 250 ) {
		    thnum = thnum - 250;
		    hnum  = 250;
		 }
		 else {
		    hnum  = thnum;
		    done = True;
		 }

    	         first_order = 2.0F;

                 if ( first && jj < *nfxy && fxy_vals[jj] != IMISSD ) {
                    tmpval1 = (float)fxy_vals[jj];
                 }
                 else {
                    tmpval1 = 16.0F;
                 }
                 values[jj] = tmpval1; jj++;

                 if ( first && jj < *nfxy && fxy_vals[jj] != IMISSD ) {
                    tmpval2 = (float)fxy_vals[jj];
                 }
                 else {
                    tmpval2 = 0.0F;
                 }
                 values[jj] = tmpval2; jj++;
                 first = False;

                 values[jj] = first_order; jj++;
                 values[jj] = (float)hnum; jj++;

                 for ( ij = 0; ij < hnum; ij++) {

                    /*
	             * Fill in latitude, longitude and height values.
	             */

                     values[jj] = (float) ptrh->lat; jj++;
                     values[jj] = (float) ptrh->lon; jj++;

	             if ( ptrh->level < 0.0F ) {
                         values[jj] = (float) BUFR_MISSING_VALUE; jj++;
	             }
	             else {
	                 values[jj] = (float) ptrh->level; jj++;
	             }
                     ptrh = ptrh -> next;
    	         }

                 values[jj] = (float) BUFR_MISSING_VALUE; jj++;
                 values[jj] = (float) BUFR_MISSING_VALUE; jj++;
                 values[jj] = (float) BUFR_MISSING_VALUE; jj++;
	     }
         }
     }
    
    /*
     * Encode information into BUFR.
     */

     bfr_make ( ofname, *nfxy, fxy_i, values, num_vals, jtime, val_mx,
                idcent, info_bufr, section2, &ier );
     *iret = ier;

     free (values);
     free (val_mx);

    /*
     * Close output file.
     */

     cas_clos ( fptr, &ier );

     return;
}
