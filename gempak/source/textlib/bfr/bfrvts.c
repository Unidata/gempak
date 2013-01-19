#include <mel_bufr.h>        	/* BUFR library include file */
#include "geminc.h"
#include "gemprm.h"
#include "bfrcmn.h"

#define   SNUM      3           /* 3 types of elements to encode */

#define   NITEMS    8  		/* no. of storm FXY elements */
#define   VITEMS    17  	/* no. of volcano FXY elements */
#define   RITEMS    13  	/* no. of radiation FXY elements */
#define   TOPINFO   15          /* 1st 15 FXYs - header information */
#define	  LENSTM    20		/* length of storm name	*/
#define	  LENVOL    29	 	/* length of volcano or radiation name */
#define	  NOFFV     27		/* offset of fxy_i value for volcano   */         
#define   NOFFR     46		/* offset of fxy_i value for radiation */

void bfr_vts ( char *ofname, int numstm, storm_t *ptrs, int numvlr, 
               volrad_t *ptrv, int itime[], int jtime[], int *nfxy,
               int *fxy_i, int *fxy_vals, int idcent, int info_bufr[],
	       char *section2, int *iret ) 
/************************************************************************
 * bfr_vts                                                              *
 *                                                                      *
 * This subroutine encodes the High Level Significant Weather ASCII     *
 * tropical storm, volcano and radiation events into BUFR format.	*
 *                                                                      *
 * bfr_vts ( ofname, numstm, ptrs, numvlr, ptrv, itime, jtime, nfxy, 	*
 *	     fxy_i, fxy_vals, idcent, info_bufr, section2, iret )	*
 *                                                                      *
 * Input parameters:                                                    *
 *      *ofname         char       Output file name                     *
 *      numstm          int	   Number of tropical storms		*
 *      *ptrs		storm_t    Pointer to storm link list		*
 *      numvlr          int	   Number of volcano/radiation activity *
 *      *ptrv		volrad_t   Pointer to vol./rad. link list	*
 *      itime[]		int	   Issued date/time 			*
 *      jtime[]		int	   Forecasted date/time 		*
 *      *nfxy           int        Number of FXYs                       *
 *      *fxy_i          int        Array of decimal FXYs                *
 *      *fxy_vals       int        Array of fxy_i value                 *
 *	idcent		int	   Originating center id                *
 *      info_bufr[]     int        BUFR initialization values           *
 *      *section2       char       Section2 SWM area string             *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int        Return code                      	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D. Kidwell/NCEP   9/03       From sigevts                            *
 * M. Li/SAIC       10/03       Changed call sequence                   *
 * M. Li/SAIC       11/03       Modified the comparion of fxy_vals      *
 * M. Li/SAIC	    12/03	Handlng for no storm, volcano & radia.	*	
 * M. Li/SAIC	    02/04	tstr8 -> tstr19				*
 * M. Li/SAIC	    02/04	Modified the check for fxy_vals		*
 * M. Li/SAIC	    04/04	Removed vts_fxy				*
 * M. Li/SAIC	    05/04	Added a replica. of 1 for volcano points*
 * M. Li/SAIC       09/04       Added idcent                            *
 * M. Li/SAIC       10/04       Added info_bufr and section2            *
 * M. Li/SAIC	    12/04	7620.0 -> 3050.0			*
 ***********************************************************************/
{
    int  	num_vals;       /* number of values in array */
    float       tmpval1, tmpval2, tmpval3, tmpval4, tmpval5, tmpval6;
    float	tmpval7, tmpval8, tmpval9;
    Data_MixVal_t *values;        /* Mixed array of floats & strings */
    float	*val_fl;


    int   	ij, jj, ier, iend, nerr, jj0, nn;
    int		leverr, numvol, numrad;
    char 	errgrp[8], ch[2];
    char        tstr19[LENSTM], tstr28[LENVOL];
    char        tstr28v[LENVOL], tmpnam[LENVOL], dash;
    FILE  	*fptr;
    volrad_t    *ptr2;

    Boolean	readflg, first;
/*---------------------------------------------------------------------*/
     *iret    = 0;
     leverr   = 0;
     iend     = 0;
     numvol   = 0;
     numrad   = 0;
     readflg  = False; 
     strcpy ( ch, "_" );
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
     * Determine the number of volcano eruptions and radiation
     * incidents.
     */

     dash = '_';
     if ( numvlr != 0 ) {
         ptr2  = ptrv;
         while ( ptr2 != NULL ) {
	     strcpy ( tmpnam,  ptr2->name);
             cst_lstr ( tmpnam, &iend, &ier );
	     if ( (int)tmpnam[iend-1] != (int) dash ) {
	         numvol++;
	     }
	     else {
	         numrad++;
	     }
             ptr2 = ptr2 -> next;
         }
     }
    
    /* 
     * Create a single array containing the information.
     * Number of data values to be read including replication 
     * factors.
     *
     * Determine the array size for the data set. 
     */

     num_vals = TOPINFO + SNUM;
     if ( numstm > 0 ) {
         num_vals  =  num_vals + ( numstm * NITEMS );
     }

     if ( numvol > 0 ) {
         num_vals  =  num_vals + ( numvol * VITEMS );
     }

     if ( numrad > 0 ) {
         num_vals  =  num_vals + ( numrad * RITEMS );
     }

    /*
     * Memory allocation for the values.
     */

     values = (Data_MixVal_t *) malloc( sizeof(Data_MixVal_t) * num_vals);
     val_fl = (float *)malloc( sizeof(float));
    
    /* 
     * Read data structure into data array. 
     *
     * Begin with header information.
     */

     jj = 0;
     if ( idcent != IMISSD ) {
         values[jj].Val.ffloat = (float)idcent;
     }
     else if ( fxy_vals[jj] != IMISSD ) {
         values[jj].Val.ffloat = (float)fxy_vals[jj];
     }
     else {
         values[jj].Val.ffloat = 7.0F;
     }
     values[jj].Val_Type = DT_FLOAT; jj++;
     if ( fxy_vals[jj] != IMISSD ) {
         values[jj].Val.ffloat = (float)fxy_vals[jj];
     }
     else {
         values[jj].Val.ffloat = 16.0F;
     }
     values[jj].Val_Type = DT_FLOAT; jj++;
     values[jj].Val.ffloat = (float)itime[0]; values[jj].Val_Type = DT_FLOAT; jj++;
     values[jj].Val.ffloat = (float)itime[1]; values[jj].Val_Type = DT_FLOAT; jj++;
     values[jj].Val.ffloat = (float)itime[2]; values[jj].Val_Type = DT_FLOAT; jj++;
     values[jj].Val.ffloat = (float)itime[3]; values[jj].Val_Type = DT_FLOAT; jj++;
     values[jj].Val.ffloat = (float)itime[4]; values[jj].Val_Type = DT_FLOAT; jj++;
     if ( fxy_vals[jj] != IMISSD ) {
         values[jj].Val.ffloat = (float)fxy_vals[jj];
     }
     else {
         values[jj].Val.ffloat = 4.0F;
     }
     values[jj].Val_Type = DT_FLOAT; jj++;
     values[jj].Val.ffloat = (float)jtime[0]; values[jj].Val_Type = DT_FLOAT; jj++;
     values[jj].Val.ffloat = (float)jtime[1]; values[jj].Val_Type = DT_FLOAT; jj++;
     values[jj].Val.ffloat = (float)jtime[2]; values[jj].Val_Type = DT_FLOAT; jj++;
     values[jj].Val.ffloat = (float)jtime[3]; values[jj].Val_Type = DT_FLOAT; jj++;
     values[jj].Val.ffloat = (float)jtime[4]; values[jj].Val_Type = DT_FLOAT; jj++;
     if ( fxy_vals[jj] != IMISSD ) {
         values[jj].Val.ffloat = (float)fxy_vals[jj];
     }
     else {
         values[jj].Val.ffloat = 3050.0F;
     }
     values[jj].Val_Type = DT_FLOAT; jj++;
     if ( fxy_vals[jj] != IMISSD ) {
         values[jj].Val.ffloat = (float)fxy_vals[jj];
     }
     else {
         values[jj].Val.ffloat = 19200.0F;
     }
     values[jj].Val_Type = DT_FLOAT; jj++;

	/*
	 * Fill in storm information, if any.
	 */

	 if ( numstm <= 0 ) {
             values[jj].Val.ffloat = 0.0F; values[jj].Val_Type = DT_FLOAT; jj++;
         }
         else {

	    /* 
	     * Fill in number of storms.
	     */

             values[jj].Val.ffloat = (float) numstm; values[jj].Val_Type = DT_FLOAT; jj++;

	     for (ij = 0; ij < numstm; ij++ ) {

	        /* 
	         * Fill in met. attribute and dimensional significance.
	         */
		 if ( ij == 0 && fxy_vals[jj+1] != IMISSD ) {
                    tmpval1 = (float)fxy_vals[jj+1];
                 }
                 else {
                    tmpval1 = 1.0F;
                 }
                 values[jj].Val.ffloat = tmpval1;
                 values[jj].Val_Type = DT_FLOAT; jj++;
                 if ( ij == 0 && fxy_vals[jj+1] != IMISSD ) {
                    tmpval2 = (float)fxy_vals[jj+1];
                 }
                 else {
                    tmpval2 = 0.0F;
                 }
                 values[jj].Val.ffloat = tmpval2;
                 values[jj].Val_Type = DT_FLOAT; jj++;

                /*
	         * Fill in the storm's name, latitude, longitude and storm type.
	         */

                 values[jj].Val.string = ( char *)malloc(sizeof (char) * LENSTM );
                 strcpy ( tstr19, ptrs->name);
                 strcpy (values[jj].Val.string, tstr19); values[jj].Val_Type = DT_STRING; jj++;


                 values[jj].Val.ffloat = (float) ptrs->lat; values[jj].Val_Type = DT_FLOAT; jj++;
                 values[jj].Val.ffloat = (float) ptrs->lon; values[jj].Val_Type = DT_FLOAT; jj++;

	         if ( ptrs->stmtyp < 0 ) {
                     values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE; values[jj].Val_Type = DT_FLOAT; jj++;
	         }
	         else {
	             values[jj].Val.ffloat = (float) ptrs->stmtyp; values[jj].Val_Type = DT_FLOAT; jj++;
	         }

                /*
	         * Fill in dimensional and met. attribute significance.
	         */

                 values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE; values[jj].Val_Type = DT_FLOAT; jj++;
                 values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE; values[jj].Val_Type = DT_FLOAT; jj++;

                 ptrs = ptrs -> next;
	     }
	 }

	/*
	 * Fill in volcano information, if any.
	 */
	 if ( numvol <= 0 ) {
             values[jj].Val.ffloat = 0.0F; values[jj].Val_Type = DT_FLOAT; jj++;
 	 }
	 else {

	    /*
	     * Fill in number of volcanoes.
	     */

	     ptr2 = ptrv;
             values[jj].Val.ffloat =  (float)numvol; values[jj].Val_Type = DT_FLOAT; jj++;

	     first = True;
	     while (ptr2 != NULL) {
	         strcpy ( tmpnam,  ptr2->name);
                 cst_lstr ( tmpnam, &iend, &ier );
	         if ( (int)tmpnam[iend-1] != (int) dash ) {

	            /*
	             * Fill in meteorological feature.
	             */
		     jj0 = jj;
		     if ( first && fxy_vals[NOFFV] != IMISSD ) {
                     	tmpval3 = (float)fxy_vals[NOFFV]; 
		     }
		     else {
			tmpval3 = 17.0F; 
		     }
		     values[jj].Val.ffloat = tmpval3;
   		     values[jj].Val_Type = DT_FLOAT; jj++;

                    /*
	             * Fill in the volcano's name.
	             */

                     values[jj].Val.string = ( char *)malloc(sizeof (char) * LENVOL);
		     cst_rnan ( ptr2->name, tstr28v, &ier );
                     strcpy (values[jj].Val.string , tstr28v); 
		                                values[jj].Val_Type = DT_STRING; jj++;
                    /*
	             * Fill in dimensional significance and lat/lon values.
	             */
		     nn = NOFFV + (jj - jj0);
		     if ( first && fxy_vals[nn] != IMISSD ) {
                        tmpval4 = (float)fxy_vals[nn]; 
                     }
                     else {
                        tmpval4 = 0.0F;
                     }
		     values[jj].Val.ffloat = tmpval4;
                     values[jj].Val_Type = DT_FLOAT; jj++;

		     values[jj].Val.ffloat = 1.0F;
		     values[jj].Val_Type = DT_FLOAT; jj++;
                     values[jj].Val.ffloat = ptr2->lat; values[jj].Val_Type = DT_FLOAT; jj++;
                     values[jj].Val.ffloat = ptr2->lon; values[jj].Val_Type = DT_FLOAT; jj++;

                    /*
	             * Fill in time significance and eruption time.
	             */
		     nn = NOFFV + (jj - jj0);
	  	     if ( first && fxy_vals[nn] != IMISSD ) {
                         tmpval5 = (float)fxy_vals[nn]; 
                     }
                     else {
                         tmpval5  = 17.0F;
                     }
		     values[jj].Val.ffloat = tmpval5;
                     values[jj].Val_Type = DT_FLOAT; jj++;
	             if ( ( ptr2->year < 0 ) || ( ptr2->month < 0 ) ||
		          ( ptr2->day  < 0 ) || ( ptr2->hour  < 0 ) ||
		          ( ptr2->minute < 0 ) ) {
                         values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE;values[jj].Val_Type = DT_FLOAT; jj++;
                         values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE;values[jj].Val_Type = DT_FLOAT; jj++;
                         values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE;values[jj].Val_Type = DT_FLOAT; jj++;
                         values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE;values[jj].Val_Type = DT_FLOAT; jj++;
                         values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE;values[jj].Val_Type = DT_FLOAT; jj++;
	             }
	             else {
	                 values[jj].Val.ffloat = (float) ptr2->year;  values[jj].Val_Type = DT_FLOAT; jj++;
	                 values[jj].Val.ffloat = (float) ptr2->month; values[jj].Val_Type = DT_FLOAT; jj++;
	                 values[jj].Val.ffloat = (float) ptr2->day;   values[jj].Val_Type = DT_FLOAT; jj++;
	                 values[jj].Val.ffloat = (float) ptr2->hour;  values[jj].Val_Type = DT_FLOAT; jj++;
	                 values[jj].Val.ffloat = (float) ptr2->minute;values[jj].Val_Type = DT_FLOAT; jj++;
	             }

                    /*
	             * Fill in the special cloud value, time and dimensional sigificance and
		     * meteorological feature.
	             */
		     nn = NOFFV + (jj - jj0);
		     if ( first && fxy_vals[nn] != IMISSD ) {
                        tmpval6 = (float)fxy_vals[nn]; 
                     }
                     else {
                        tmpval6 = 5.0F;
                     }
		     values[jj].Val.ffloat = tmpval6;
                     values[jj].Val_Type = DT_FLOAT; jj++;

                     values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE;values[jj].Val_Type = DT_FLOAT; jj++;
                     values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE;values[jj].Val_Type = DT_FLOAT; jj++;
                     values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE;values[jj].Val_Type = DT_FLOAT; jj++;
	         }
	         ptr2 = ptr2->next;
		 first = False;
	     }
	 }

        /*
	 * Fill in radiation information.
	 */
	 if ( numrad <= 0 ) {
	     values[jj].Val.ffloat = 0.0F; values[jj].Val_Type = DT_FLOAT; jj++;
 	 }
	 else {
             values[jj].Val.ffloat =  (float)numrad; values[jj].Val_Type = DT_FLOAT; jj++;
	     first = True;
	     while (ptrv != NULL) {

	        /*
	         * Found a radiation incident. 
	         */

	         strcpy ( tmpnam,  ptrv->name);
                 cst_lstr ( tmpnam, &iend, &ier );
	         if ( (int)tmpnam[iend-1] == (int) dash ) {
	            /*
	             * Fill in activity or facility involved in incident and 
		     * dimensional significance.
	             */
		     jj0 = jj;
		     if ( first && fxy_vals[NOFFR] != IMISSD ) {
                        tmpval7 = (float)fxy_vals[NOFFR];
                     }
                     else {
                        tmpval7 = 1.0F;
                     }
	   	     values[jj].Val.ffloat = tmpval7;
                     values[jj].Val_Type = DT_FLOAT; jj++;
		     if ( first && fxy_vals[NOFFR+1] != IMISSD ) {
                        tmpval8 = (float)fxy_vals[NOFFR+1];
                     }
                     else {
                        tmpval8 = 0.0F;
                     }
		     values[jj].Val.ffloat = tmpval8;
                     values[jj].Val_Type = DT_FLOAT; jj++;

                    /*
	             * Fill in the name of the incident area.
	             */

                     values[jj].Val.string = ( char *)malloc(sizeof (char) * LENVOL );
                     strcpy ( tstr28, ptrv->name);
		     strcpy ( values[jj].Val.string , tstr28); values[jj].Val_Type = DT_STRING; jj++;

                    /*
	             * Fill in latitude, longitude and time significance values.
	             */

                     values[jj].Val.ffloat =  ptrv->lat; values[jj].Val_Type = DT_FLOAT; jj++;
                     values[jj].Val.ffloat =  ptrv->lon; values[jj].Val_Type = DT_FLOAT; jj++;
		     nn = NOFFR + (jj - jj0);
		     if ( first && fxy_vals[nn] != IMISSD ) {
                        tmpval9 = (float)fxy_vals[nn];
                     }
                     else {
                        tmpval9 = 17.0F;
                     }
		     values[jj].Val.ffloat = tmpval9;
                     values[jj].Val_Type = DT_FLOAT; jj++;

                    /*
	             * Fill in incident time.
	             */

	             if ( ( ptrv->year < 0 ) || ( ptrv->month < 0 ) ||
		          ( ptrv->day  < 0 ) || ( ptrv->hour  < 0 ) ||
		          ( ptrv->minute < 0 ) ) {
                         values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE;values[jj].Val_Type = DT_FLOAT; jj++;
                         values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE;values[jj].Val_Type = DT_FLOAT; jj++;
                         values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE;values[jj].Val_Type = DT_FLOAT; jj++;
                         values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE;values[jj].Val_Type = DT_FLOAT; jj++;
                         values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE;values[jj].Val_Type = DT_FLOAT; jj++;
	             }
	             else {
	                 values[jj].Val.ffloat = (float) ptrv->year;  values[jj].Val_Type = DT_FLOAT; jj++;
	                 values[jj].Val.ffloat = (float) ptrv->month; values[jj].Val_Type = DT_FLOAT; jj++;
	                 values[jj].Val.ffloat = (float) ptrv->day;   values[jj].Val_Type = DT_FLOAT; jj++;
	                 values[jj].Val.ffloat = (float) ptrv->hour;  values[jj].Val_Type = DT_FLOAT; jj++;
	                 values[jj].Val.ffloat = (float) ptrv->minute;values[jj].Val_Type = DT_FLOAT; jj++;
	             }

                    /*
	             * Fill in time and dimentional significance.
	             */

                     values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE;values[jj].Val_Type = DT_FLOAT; jj++;
                     values[jj].Val.ffloat = (float) BUFR_MISSING_VALUE;values[jj].Val_Type = DT_FLOAT; jj++;
	         }
	         ptrv = ptrv->next;
		 first = False;
	     }
	 }
     
    /*
     * Set a flag to indicate mixed data type for BUFR.
     */

     val_fl[0] = (float) BUFR_MISSING_VALUE;

    /* 
     * Encode information into BUFR.
     */

     bfr_make ( ofname, *nfxy, fxy_i, val_fl, num_vals, jtime, values,
                idcent, info_bufr, section2, &ier );
     *iret = ier;

     free (values);
     free (val_fl);

    /*
     * Close output file.
     */

     cas_clos ( fptr, &ier );

     return;
}
