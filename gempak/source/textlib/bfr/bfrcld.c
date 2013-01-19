#include <mel_bufr.h>           /* BUFR library include file */
#include "geminc.h"
#include "gemprm.h"
#include "bfrcmn.h"

#define   NITEMS    9  		/* no. of elems. in structure + 
				   fxys (008007 & 008011) */
#define   TOPINFO   16          /* 1st 15 FXYs + 1 for number of groups */

void bfr_cld ( char *ofname, int numcld, cloud_t *ptrc, int itime[], 
           int jtime[], int *nfxy, int *fxy_i, int *fxy_vals, int idcent, 
	   int info_bufr[], char *section2, int *iret ) 
/************************************************************************
 * bfr_cld                                                              *
 *                                                                      *
 * This subroutine encodes the High Level Significant Weather ASCII     *
 * cloud information into BUFR format.   				*
 *                                                                      *
 * bfr_cld ( ofname, numcld, ptrc, itime, jtime, nfxy, fxy_i, fxy_vals,	*
 *	      idcent, info_bufr, section2, iret )	        	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ofname		char 	   Output file name			*	
 *      numcld          int	   Number of cloud lines		*
 *      *ptrc		cloud_t    Pointer to CLOUD link list		*
 *      itime[]		int	   Issued date/time 			*
 *      jtime[]		int	   Forecasted date/time 		*
 *	*nfxy		int	   Number of FXYs			*
 *	*fxy_i          int        Array of decimal FXYs                *
 *      *fxy_vals       int        Array of fxy_i value                 *
 *	idcent		int	   Originating center id		*
 * 	info_bufr[]	int	   BUFR initialization values		*
 *	*section2	char	   Section2 SWM area string		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int        Return code                      	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D. Kidwell/NCEP   9/03	From sigecld                            *
 * M. Li/SAIC	    10/03	Changed call sequence			*
 * M. Li/SAIC	    11/03	Modified the comparion of fxy_vals	*
 * M. Li/SAIC	    01/04	Added a check for numcld		*
 * M. Li/SAIC	    04/04	Removed cld_fxy				*
 * M. Li/SAIC	    09/04	Added idcent				*
 * M. Li/SAIC	    10/04	Added info_bufr and section2		*
 ***********************************************************************/
{
    float 	*values;        /* array of data values */
    float	tmpval1, tmpval2;
    int  	num_vals;       /* number of values in array */
    Data_MixVal_t *val_mx; 


    int   	ii, ij, jj, ier;
    FILE  	*fptr;
    cloud_t     *ptr, *ptr2;

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
     * Create a single array containing the CLOUD information.
     * Number of data values to be read including replication 
     * factors.
     *
     * Determine the array size for the data set.  Add up all
     * lat./lon. pairs in the linked list. 
     */

     tpts = 0;
     if ( numcld > 0 ) {
     	ptr2 = ptrc;
     	while ( ptr2  != NULL ) {
	    tpts = tpts + ptr2 -> npt;
            ptr2 = ptr2 -> next;
     	}
     }

     if ( numcld == 0 ) {
         num_vals =  TOPINFO;
     }
     else {
        num_vals = ( tpts * 2 ) + ( numcld * NITEMS ) + TOPINFO;
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

     values[jj] = (float)numcld; jj++;
     if ( numcld != 0 ) {
         ptr = ptrc;

         for ( ii = 0; ii < numcld; ii++){
	     if ( ii == 0 && fxy_vals[jj+1] != IMISSD ) {
            	tmpval1 = (float)fxy_vals[jj+1];
     	     }
     	     else {
            	tmpval1 = 12.0F; 
     	     }
	     values[jj] = tmpval1; jj++;

             if ( ii == 0 && fxy_vals[jj+1] != IMISSD ) {
                tmpval2 = (float)fxy_vals[jj+1];
	     }
             else {
                tmpval2 = 2.0F;
             }
             values[jj] = tmpval2; jj++;

            /*
	     * Fill in base and top cloud values.
	     */

	     if ( ptr->level1 < 0.0F ) {
                 values[jj] = (float) BUFR_MISSING_VALUE; jj++;
	     }
	     else {
	         values[jj] = ptr->level1; jj++;
	     }
	     if ( ptr->level2 < 0.0F ) {
                 values[jj] = (float) BUFR_MISSING_VALUE; jj++;
	     }
	     else {
	         values[jj] = ptr->level2; jj++;
	     }

            /*
	     * Fill in lat/lon.
	     */

             values[jj] = (float) ptr->npt; jj++;
             for ( ij = 0; ij < ptr->npt; ij++) {
                 values[jj] = ptr->lat[ij]; jj++;
                 values[jj] = ptr->lon[ij]; jj++;
	     }

            /*
	     * Fill in cloud distribution and type.
	     */

	     if ( ptr->clddist < 0 ) {
                 values[jj] = (float) BUFR_MISSING_VALUE; jj++;
	     }
	     else {
                 values[jj] = (float) ptr->clddist; jj++;
	     }
	     if ( ptr->cldtyp < 0 ) {
                 values[jj] = (float) BUFR_MISSING_VALUE; jj++;
	     }
	     else {
                 values[jj] = (float) ptr->cldtyp; jj++;
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
