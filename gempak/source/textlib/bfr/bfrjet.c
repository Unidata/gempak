#include <mel_bufr.h>        	/* BUFR library include file */
#include "geminc.h"
#include "gemprm.h"
#include "bfrcmn.h"

#define   NITEMS    5  		/* no. of elems. in structure + 
				   fxys (008007 & 008011) */
#define   TOPINFO   16          /* 1st 15 FXYs + 1 for number of groups */

void bfr_jet ( char *ofname, int numjet, jets_t *ptrj, int itime[], 
               int jtime[], int *nfxy, int *fxy_i, int *fxy_vals, 
	       int idcent, int info_bufr[], char *section2, int *iret ) 
/************************************************************************
 * bfr_jet                                                              *
 *                                                                      *
 * This subroutine encodes the High Level Significant Weather ASCII     *
 * jets information into BUFR format.   				*
 *                                                                      *
 * bfr_jet ( ofname, numjet, ptrj, itime, jtime, nfxy, fxy_i, fxy_vals, *
 *           idcent, info_bufr, section2, iret )                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      *ofname         char       Output file name                     *
 *      numjet          int	   Number of jets 			*
 *      *ptrj		jets_t     Pointer to JETS link list		*
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
 * D. Kidwell/NCEP   9/03       From sigejet                            *
 * M. Li/SAIC       10/03       Changed call sequence                   *
 * M. Li/SAIC       11/03       Modified the comparion of fxy_vals      *
 * M. Li/SAIC	    04/04	Added flight level delta values		*
 * M. Li/SAIC       09/04       Added idcent                            *
 * M. Li/SAIC       10/04       Added info_bufr and section2            *
 ***********************************************************************/
{
    float 	*values;        /* array of data values */
    float	tmpval1, tmpval2, tmpval3, tmpval4;
    int  	num_vals;       /* number of values in array */
    Data_MixVal_t *val_mx;


    int   	ii, ij, jj, ier;
    FILE  	*fptr;
    jets_t      *ptr, *ptr2;

    int		nerr, leverr, tpts; 
    char 	errgrp[8];
    Boolean	readflg;
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
     * Create a single array containing the JETS information.
     * Number of data values to be read including replication 
     * factors.
     *
     * Determine the array size for the data set.  Add up all
     * lat./lon. pairs in the linked list. 
     */

     tpts = 0;
     if ( numjet > 0 ) {
         ptr2 = ptrj;
         while ( ptr2  != NULL ) {
	        tpts = tpts + ptr2 -> npt;
                ptr2 = ptr2 -> next;
         }
     }

     if ( numjet == 0 ) {
         num_vals = TOPINFO;
     }
     else {
         num_vals = ( tpts * 9 ) + ( numjet * NITEMS ) + TOPINFO;
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

     values[jj] = (float) numjet; jj++;

     if ( numjet != 0 ) {
         ptr = ptrj;
         for ( ii = 0; ii < numjet; ii++){
             if ( ii == 0 && fxy_vals[jj+1] != IMISSD ) {
                tmpval1 = (float)fxy_vals[jj+1];
             }
             else {
                tmpval1 = 10.0F;
             }
             values[jj] = tmpval1; jj++;

             if ( ii == 0 && fxy_vals[jj+1] != IMISSD ) {
                tmpval2 = (float)fxy_vals[jj+1];
             }
             else {
                tmpval2 = 1.0F;
             }
             values[jj] = tmpval2; jj++;

             values[jj] = (float) ptr->npt; jj++;
             for ( ij = 0; ij < ptr->npt; ij++) {
                /*
	         * Fill in lat/lon.
	         */

                 values[jj] = (float) ptr->lat[ij]; jj++;
                 values[jj] = (float) ptr->lon[ij]; jj++;

                /*
	         * Fill in height.
	         */

	         if ( ptr->level[ij] < 0.0F ) {
                     values[jj] = (float) BUFR_MISSING_VALUE; jj++;
	         }
	         else {
	             values[jj] = (float) ptr->level[ij]; jj++;
	         }

                /*
	         * Fill in wind.
	         */

	         if ( ptr->speed[ij] < 0.0F ) {
                     values[jj] = (float) BUFR_MISSING_VALUE; jj++;
	         }
	         else {
	             values[jj] = (float) ptr->speed[ij]; jj++;
	         }

		/*
		 * Flight level significance for level above jet.
		 */

		 if ( (ii == 0) && (jj < *nfxy) && (fxy_vals[jj+1] != IMISSD) ) {
                     tmpval3 = (float) fxy_vals[jj+1];
                 }
                 else {
                     tmpval3 = 60.0F;
                 }
                 values[jj] = tmpval3; jj++;


		/*
		 * Flight level above jet.
	         */
		 if ( ptr->levabv[ij] < 0) {
                     values[jj] = BUFR_MISSING_VALUE;
                 }
                 else {
                     values[jj] = ptr->levabv[ij]; 
                 }
                 jj++;

		/*
                 * Flight level significance for level below jet.
                 */

                 if ( (ii == 0) && (jj < *nfxy) && (fxy_vals[jj+1] != IMISSD) ) {
                     tmpval4 = (float) fxy_vals[jj+1];
                 }
                 else {
                     tmpval4 = 61.0F;
                 }
                 values[jj] = tmpval4; jj++;

		/*
                 * Flight level below jet.
                 */
		 if ( ptr->levblw[ij] < 0.0F ) {
                     values[jj] = BUFR_MISSING_VALUE; 
                 }
                 else {
                     values[jj] = ptr->levblw[ij]; 
                 }
                 jj++;

		/*
		 * Cancel the previous flight level significance descriptor.
		 */
		 values[jj] = BUFR_MISSING_VALUE;
		 jj++;

	     }

             values[jj] = (float) BUFR_MISSING_VALUE; jj++;
             values[jj] = (float) BUFR_MISSING_VALUE; jj++;

             ptr = ptr -> next;
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
