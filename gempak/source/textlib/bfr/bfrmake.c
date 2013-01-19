#include <mel_bufr.h>        	/* BUFR library include file */
#include "geminc.h"
#include "gemprm.h"
#include "bfrcmn.h"

void bfr_make ( char *ofname, int nfxy, int *fxy_i, float *values,
               int num_vals, int jtime[], Data_MixVal_t *values_mx,
               int idcent, int info_bufr[], char *section2, int *iret ) 
/************************************************************************
 * bfr_make                                                             *
 *                                                                      *
 * This subroutine uses input values and associated descriptors to      *
 * create and output a BUFR message.  It can handle data of type float  *
 * or mixed type data.                                                  *
 *                                                                      *
 * bfr_make ( ofname, nfxy, fxy_i, values, num_vals, jtime, values_mx,  *
 *           idcent, info_bufr, section2, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *	*ofname		char	   Output BUFR file name.               *
 *      nfxy		int	   Number of FXYs (descriptors)         *
 *	*fxy_i		int	   Array of decimal FXYs                *
 *	*values		float	   Array of data values (of type float) *
 *	num_vals	int	   Number of data values                *
 *      jtime[]		int	   Forecasted date/time 		*
 *	*values_mx   Data_MixVal_t Array of data values (of mixed type) *
 *	idcent		int	   Originating center id		*
 *	info_bufr[]	int	   BUFR initialization values		*
 *	*section2	char	   Section2 SWM area string		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int        Return code                      	*
 *				     1 = BUFR message not output        *
 *				     0 = normal return                  *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D. Kidwell/NCEP   9/03	Extracted from SIGBENC sige routines    *
 * M. Li/SAIC	    10/04	Added idcent, info_bufr and section2	*
 * S. Jacobs/NCEP   10/04	Updated for new verion of MELBUFR	*
 * M. Li/SAIC	    03/05	Added a check for idcent		*
 ***********************************************************************/
{
    BUFR_Info_t bufr_info;  	/* Largely Section 1 information. */
    FXY_t 	*fxy;     	/* array of packed FXYs */


    int   	ii, nc, lpend, lens, ier;

    int		numerr, leverr; 
    char 	errgrp[8], cc[50];
/*---------------------------------------------------------------------*/
     *iret   = 0;
     leverr  = 0;
     strcpy ( errgrp, "BFR" );
     strcpy ( cc, " " );

    /* 
     * Initialize the BUFR information structure. 
     * This function sets all of the default values for using
     * the MELBUFR library.
     */

     if( BUFR_Info_Init( &bufr_info ) ) {
	 numerr = -4;
	 er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
	           strlen(errgrp), strlen(cc) );
         *iret = 1;
         return;
     }
    
    /* 
     * Allocate memory for packed FXYs.
     */

     fxy = (FXY_t *) malloc(sizeof(FXY_t ) * (uint_t)nfxy);
    
    /*
     * Initialize the section one data.
     */

     if ( idcent == IMISSD ) {
	 bufr_info.OriginatingCenter       =  7;
     }
     else {
         bufr_info.OriginatingCenter	   =  idcent;
     }
     bufr_info.MinorLocalVersion           =  0;
     bufr_info.BUFR_MasterTable            =  info_bufr[0]; 
     bufr_info.BUFR_Edition                =  info_bufr[1];  
     bufr_info.UpdateSequenceNumber        =  info_bufr[2]; 
     bufr_info.DataCategory                =  info_bufr[3];
     bufr_info.DataSubCategory             =  info_bufr[4];
     bufr_info.VersionNumberOfMasterTables =  info_bufr[5];
     bufr_info.VersionNumberOfLocalTables  =  info_bufr[6];
     bufr_info.ObservedData                =  info_bufr[7];;

     bufr_info.Year                        =  jtime[0]%100;
     bufr_info.Month                       =  jtime[1];
     bufr_info.Day                         =  jtime[2];
     bufr_info.Hour                        =  jtime[3];
     bufr_info.Minute                      =  jtime[4];

    /*
     * Set trace and debug levels.
     BUFR_Trace( 10 );
     BUFR_Debug( 10 );
     */

    /* 
     * Initialize BUFR message.  Read standard tables. 
     */

     if( BUFR_Init(&bufr_info, ofname, ENCODING) ) {
         *iret = 1;
	 BUFR_perror( "main" );
	 numerr = -8;
	 er_lmsg ( &leverr, errgrp, &numerr, ofname, &ier,
		   strlen(errgrp), strlen(ofname) );
     }

    /*
     * Write out section2.
     */

     cst_lstr ( section2, &nc, &ier );
     if ( nc > 0 ) {
	 lens = strlen (section2);
	 if ( (lens % 60) != 0 ) { 
	     lpend = ( ( lens / 60 ) + 1 ) * 60;
	     cst_padString ( section2, ' ', 1, lpend, section2 );
	 }

     	 if ( BUFR_Put_OptionalData ( section2, strlen(section2) ) ){
	     numerr = -13;
	     er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                   strlen(errgrp), strlen(cc) );
	 } 
     }

    /* 
     * Pack FXYs for data. 
     *
     * Pack replication FXY using delayed replication, therefore the 
     * replication count is in the data array.  The FXY for delayed 
     * replication of, for instance, 4 FXYs is 1-04-000.  The 000 
     * indicates delayed replication.  The delayed replication FXY 
     * must be followed by a count descriptor (0-31-001 or 0-31-002).  
     *
     */

     for ( ii = 0; ii < nfxy; ii++) {
         fxy[ii] = FXY_Pack_Dec( fxy_i[ii]);
     }

    /* 
     * Enter data array into bufr message. 
     */

     if ( !G_DIFF(values[0], (float)BUFR_MISSING_VALUE) ) {

        /*
         * Data is not of mixed type.
         */

         if ( BUFR_Put_Array( values, num_vals, DT_FLOAT, fxy, nfxy) ) {
	     numerr = -5;
             *iret = 1;
         }
     }
     else {     

        /*
         * Data is of mixed type - e.g., storm, volcano and radiation 
	 * event data for high level sig weather.
         */

         if ( BUFR_Put_MixArray( values_mx, num_vals, fxy, nfxy) ) {
             numerr = -11;
             *iret = 1;
         }
     }

    /* 
     * Create BUFR message. 
     */

     if ( *iret != 1 ) {
         if ( BUFR_Encode( &bufr_info ) ) {
	     BUFR_perror( "main" );
	     numerr = -9;
	     er_lmsg ( &leverr, errgrp, &numerr, ofname, &ier,
		       strlen(errgrp), strlen(ofname) );
             *iret = 1;
         }
     }
     else {
         er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
		   strlen(errgrp), strlen(cc) );
     }

     free (fxy);
     BUFR_Destroy(1);

     return;
}
