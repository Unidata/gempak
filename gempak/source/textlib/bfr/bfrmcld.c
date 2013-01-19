#include <mel_bufr.h>           /* BUFR library include file */
#include "geminc.h"
#include "gemprm.h"
#include "bfrcmn.h"

#define   NITEMS    7  		/* no. of elems. in structure fxys 
				   (008007, 3 of 031001, and 3 of 031000) */
#define   TOPINFO   16          /* 1st 15 FXYs + 1 for 008011 */

void bfr_mcld ( char *ofname, int nummcld, mcloud_t *ptrm, int itime[], 
           int jtime[], int *nfxy, int *fxy_i, int *fxy_vals, int idcent, 
	   int info_bufr[], char *section2, int *iret ) 
/************************************************************************
 * bfr_mcld                                                             *
 *                                                                      *
 * This subroutine encodes the Mid Level Significant Weather ASCII     	*
 * cloud information into BUFR format.   				*
 *                                                                      *
 * bfr_mcld (ofname, nummcld, ptrm, itime, jtime, nfxy, fxy_i, fxy_vals,*
 *	      idcent, info_bufr, section2, iret )	        	*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ofname		char 	   Output file name			*	
 *      nummcld         int	   Number of cloud lines		*
 *      *ptrm		mcloud_t   Pointer to MCLOUD link list		*
 *      itime[]		int	   Issued date/time 			*
 *      jtime[]		int	   Forecasted date/time 		*
 *	*nfxy		int	   Number of FXYs			*
 *	*fxy_i          int        Array of decimal FXYs                *
 *      *fxy_vals       int        Array of fxy_i value                 *
 *	idcent		int	   Originating center id		*
 *      info_bufr[]     int        BUFR initialization values           *
 *      *section2       char       Section2 SWM area string             *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int        Return code                      	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC	    09/04	Created                			*
 * M. Li/SAIC	    10/04	Recalculated num_vals			*
 * M. Li/SAIC	    10/04	Added info_bufr and section2		*
 ***********************************************************************/
{
    float 	*values;        /* array of data values */
    float	tmpval2;
    int  	num_vals;       /* number of values in array */
    int		*fxy_j;
    Data_MixVal_t *val_mx; 


    int   	ii, ij, jj, ier;
    FILE  	*fptr;
    mcloud_t    *ptr, *ptr2;

    int		nerr, leverr, tpts, num_fxys;
    int		tclds, ttyps, tturb, ticing, tfcb;
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
     * Create a single array containing the MCLOUD information.
     * Number of data values to be read including replication 
     * factors.
     *
     * Determine the array size for the data set.  Add up all
     * lat./lon. pairs in the linked list. 
     */

     tclds  = 0;
     ttyps  = 0;
     tturb  = 0;
     ticing = 0;
     tfcb   = 0;
     tpts = 0;
     if ( nummcld > 0 ) {
     	ptr2 = ptrm;
     	while ( ptr2  != NULL ) {
	    tclds  += ptr2 -> ncld;
	    ttyps  += ptr2 -> ntyp;
	    tturb  += ptr2 -> turb;
	    ticing += ptr2 -> icing;
	    tfcb   += ptr2 -> fcb;
	    tpts   += ptr2 -> npt;
            ptr2 = ptr2 -> next;
     	}
     }

     if ( nummcld == 0 ) {
         num_vals =  TOPINFO;
     }
     else {
        num_vals = ( tpts * 2 ) + tclds + ttyps + (3 * tturb) + (3 * ticing) 
                   + (4 * tfcb ) + ( nummcld * NITEMS ) + TOPINFO;
     }

    /*
     * Memory allocation for the values.
     */

     values = (float *)malloc( sizeof(float) * (uint_t)num_vals);
     val_mx = (Data_MixVal_t *) malloc( sizeof(Data_MixVal_t));

    /*
     * Memory allocation for the expanded list of FXYS - fxy_j.
     */

     num_fxys = TOPINFO + 27 * nummcld;
     fxy_j = (int *)malloc ( sizeof(int) * (uint_t)num_fxys );
    
    /* 
     * Read data structure into data array. 
     */

     jj = 0;
     for ( ii = 0; ii < TOPINFO; ii++ ) {
	 fxy_j[ii] = fxy_i[ii];
     }

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
         values[jj] = 3050.0F; 
     }
     jj++;
     if ( fxy_vals[jj] != IMISSD ) {
         values[jj] = (float)fxy_vals[jj];
     }
     else {
         values[jj] = 13720.0F; 
     }
     jj++;
     if ( fxy_vals[jj] != IMISSD ) {
         values[jj] = (float)fxy_vals[jj];
     }
     else {
         values[jj] = 12.0F;
     }
     jj++;


     if ( nummcld != 0 ) {
         ptr = ptrm;

         for ( ii = 0; ii < nummcld; ii++) {

	    /*
	     * Fill in the expanded FXY array.
	     */

	     for ( ij = TOPINFO; ij < *nfxy; ij++ ) {
                 fxy_j[ij + ii*(*nfxy-TOPINFO)] = fxy_i[ij];
  	     }	

	    /*
	     * Fill in Dimensional significance (area).
	     */

             if ( ii == 0 && fxy_vals[jj] != IMISSD ) {
                tmpval2 = (float)fxy_vals[jj];
	     }
             else if ( ii == 0 ) {
                tmpval2 = 2.0F;
             }
             values[jj] = tmpval2; jj++;

	    /*
	     * Fill in lat/lon.
	     */

             values[jj] = (float) ptr->npt; jj++;
             for ( ij = 0; ij < ptr->npt; ij++) {
                 values[jj] = ptr->lat[ij]; jj++;
                 values[jj] = ptr->lon[ij]; jj++;
             }

	    /*
	     * Fill in non-Cb cloud distribution.
	     */

	     values[jj] = (float) ptr -> ncld; jj++;
	     if (  ptr -> ncld > 0 ) {
	         for ( ij = 0; ij < ptr->ncld; ij++) {		
		     if ( ptr -> ncdis[ij] < 0 ) {
			 values[jj] = (float) BUFR_MISSING_VALUE;
		     }
		     else {
		         values[jj] = (float) ptr -> ncdis[ij]; 
		     }
		     jj++;
		 }
	     }

            /*
             * Fill in non-Cb cloud type.
             */

             values[jj] = (float) ptr -> ntyp; jj++;
             if (  ptr -> ntyp > 0 ) {
                 for ( ij = 0; ij < ptr->ntyp; ij++) {
		     if ( ptr -> nctyp[ij] < 0 ) {
			 values[jj] = (float) BUFR_MISSING_VALUE;
                     }
                     else {
                         values[jj] = (float) ptr -> nctyp[ij];
		     }
		     jj++;
                 }
             }

	    /*
	     * Fill in turbulence.
	     */
	    
             values[jj] = (float) ptr -> turb; jj++;
	     if ( ptr -> turb == 1 ) {
		 if ( ptr -> tbase < 0 ) {
		     values[jj] = (float) BUFR_MISSING_VALUE;
		 }
		 else {
		     values[jj] =  ptr -> tbase;
		 }
		 jj++;

		 if ( ptr -> ttop < 0 ) {
                     values[jj] = (float) BUFR_MISSING_VALUE;
                 }
                 else {
                     values[jj] =  ptr -> ttop;
                 }
		 jj++;

		 if ( ptr -> tdeg < 0 ) {
		     values[jj] = (float) BUFR_MISSING_VALUE;
		 }
		 else {
		     values[jj] = (float) ptr -> tdeg; 
		 }
		 jj++;
	     }

            /*
             * Fill in icing.
             */

             values[jj] = (float) ptr -> icing; jj++;
             if ( ptr -> icing == 1 ) {
                 if ( ptr -> icbase < 0 ) {
                     values[jj] = (float) BUFR_MISSING_VALUE;
                 }
                 else {
                     values[jj] =  ptr -> icbase;
                 }
		 jj++;

                 if ( ptr -> ictop < 0 ) {
                     values[jj] = (float) BUFR_MISSING_VALUE;
                 }
                 else {
                     values[jj] =  ptr -> ictop;
                 }
		 jj++;

		 if ( ptr -> dic < 0 ) {
		     values[jj] = (float) BUFR_MISSING_VALUE;
		 }
		 else { 
                     values[jj] = (float) ptr -> dic; 
   		 }
		 jj++;
             }

            /*
             * Fill in Cb.
             */

             values[jj] = (float) ptr -> fcb; jj++;
             if ( ptr -> fcb == 1 ) {
                 if ( ptr -> cbbase < 0 ) {
                     values[jj] = (float) BUFR_MISSING_VALUE;
                 }
                 else {
                     values[jj] =  ptr -> cbbase;
                 }
		 jj++;

                 if ( ptr -> cbtop < 0 ) {
                     values[jj] = (float) BUFR_MISSING_VALUE;
                 }
                 else {
                     values[jj] =  ptr -> cbtop;
                 }
		 jj++;

		 if ( ptr -> cbdis < 0 ) {
		     values[jj] = (float) BUFR_MISSING_VALUE;
		 }
		 else {
                     values[jj] = (float) ptr -> cbdis;
		 }
		 jj++;

		 if ( ptr -> cbtyp < 0 ) {
		     values[jj] = 9.0F;
	  	 }
		 else {
		     values[jj] = (float) ptr -> cbtyp;
		 }
		 jj++;
             }

             ptr = ptr -> next;
         }
     }

    /*
     * Encode information into BUFR.
     */

     bfr_make ( ofname, num_fxys, fxy_j, values, num_vals, jtime, val_mx,
                idcent, info_bufr, section2, &ier );
     *iret = ier;
    
     free (values);
     free (val_mx);
     free (fxy_j);

    /* 
     * Close output file. 
     */

     cas_clos ( fptr, &ier );
     
     return;
}
