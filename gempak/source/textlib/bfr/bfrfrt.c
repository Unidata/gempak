#include <mel_bufr.h>        	/* BUFR library include file */
#include "geminc.h"
#include "gemprm.h"
#include "bfrcmn.h"

#define   NITEMS    5  		/* no. of elems. in structure + 
				   fxys (008007 & 008011) */
#define   TOPINFO   16          /* 1st 15 FXYs + 1 for number of groups */

void bfr_frt ( char *ofname, int numfrt, front_t *ptrf, int itime[], 
               int jtime[], int *nfxy, int *fxy_i, int *fxy_vals, 
	       int idcent, int info_bufr[], char *section2, int *iret ) 
/************************************************************************
 * bfr_frt                                                              *
 *                                                                      *
 * This subroutine encodes the High Level Significant Weather ASCII   	*
 * front information into BUFR format.   				*
 *                                                                      *
 * bfr_frt ( ofname, numfrt, ptrf, itime, jtime, nfxy, fxy_i, fxy_vals, * 
 *           idcent, info_bufr, section2, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ofname         char       Output file name                     *
 *      numfrt          int	   Number of front lines		*
 *      *ptrf		front_t    Pointer to FRONT link list		*
 *      itime[]		int	   Issued date/time 			*
 *      jtime[]		int	   Forecasted date/time 		*
 *      *nfxy           int        Number of FXYs                       *
 *      *fxy_i          int        Array of decimal FXYs                *
 *      *fxy_vals       int        Array of fxy_i value                 *
 *      idcent          int        Originating center id                *
 *      info_bufr[]     int        BUFR initialization values           *
 *      *section2       char       Section2 SWM area string   		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int        Return code                      	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D. Kidwell/NCEP   9/03       From sigefrt                            *
 * M. Li/SAIC       10/03       Changed call sequence                   *
 * M. Li/SAIC	    01/04	Added a check for numfrt		*
 * M. Li/SAIC	    04/04	Removed frt_fxy				*
 * M. Li/SAIC       09/04       Added idcent                            *
 * M. Li/SAIC       10/04       Added info_bufr and section2            *
 ***********************************************************************/
{
    float 	*values;        /* array of data values */
    float	tmpval;
    int  	num_vals;       /* number of values in array */
    Data_MixVal_t *val_mx;


    int   	ii, ij, jj, ier;
    FILE  	*fptr;
    front_t     *ptr, *ptr2;

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
     * Create a single array containing the FRONT information.
     * Number of data values to be read including replication 
     * factors.
     *
     * Determine the array size for the data set.  Add up all
     * lat./lon. pairs in the linked list. 
     */

     tpts = 0;
     if ( numfrt > 0 ) {
        ptr2 = ptrf;
        while ( ptr2  != NULL ) {
            tpts = tpts + ptr2 -> npt;
            ptr2 = ptr2 -> next;
        }
     }

     if ( numfrt == 0 ) {
         num_vals = TOPINFO;
     }
     else {
        num_vals = ( tpts * 4 ) + ( numfrt * NITEMS ) + TOPINFO;
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

     values[jj] = (float)numfrt; jj++;

     if ( numfrt != 0 ) {
         ptr = ptrf;
         for ( ii = 0; ii < numfrt; ii++){

            /*
	     * Fill in front type information.
	     */

	     if ( ptr->ftype < 0 ) {
                 values[jj] = (float) BUFR_MISSING_VALUE; jj++;
	     }
	     else {
	         values[jj] = (float) ptr->ftype; jj++;
	     }

	     if ( ii == 0 && fxy_vals[jj] != IMISSD ) {
                tmpval = (float)fxy_vals[jj];
             }
             else {
                tmpval = 1.0F;
             }
             values[jj] = tmpval; jj++;

             values[jj] = (float) ptr->npt; jj++;
             for ( ij = 0; ij < ptr->npt; ij++) {

                /*
	         * Fill in lat/lon.
	         */

                 values[jj] = ptr->lat[ij]; jj++;
                 values[jj] = ptr->lon[ij]; jj++;

                /*
	         * Fill in front speed and direction.
	         */

	         if ( ptr->fntdir[ij]< 0.0F ) {
                     values[jj] = (float) BUFR_MISSING_VALUE; jj++;
	         }
	         else {
                     values[jj] = ptr->fntdir[ij]; jj++;
	         }
	         if ( ptr->fntspd[ij]< 0.0F ) {
                     values[jj] = (float) BUFR_MISSING_VALUE; jj++;
	         }
	         else {
                     values[jj] = ptr->fntspd[ij]; jj++;
	         }
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
