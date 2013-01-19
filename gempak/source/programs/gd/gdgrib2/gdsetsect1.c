#include "gdgrib2.h"

void gdsetsect1 ( GDG2_input *input, GDG2_gemgrid *gemgrid, 
                  int *lsec1, int *iret )
/************************************************************************
 * gdsetsect1                                                           *
 *                                                                      *
 * This routine sets up an array containing information to encode       *
 * GRIB2 Section 1 - Identification Section.  The required info is      *
 * obtained from user parameter G2IDS and the grid's GEMPAK date/time.  *
 *                                                                      *
 *  Usage:                                                              *
 *      gdsetsect1( input, gemgrid, lsec1, iret );                      *
 *                                                                      *
 *  Input Arguments:                                                    *
 *      *input            GDG2_input       input structure for GDGRIB2  *
 *      *gemgrid          GDG2_gemgrid     output grid and Gempak info  *
 *                                                                      *
 *  Output Arguments:                                                   *
 *      *lsec1             int              Array of section 1 info     *
 *      *iret              int              Error return code.          *
 *                                           0 = Normal                 *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP           8/2005   Orig                              *
 ***********************************************************************/
{

    const int nids=7;
    int    ret, num, j;
    int    idate[5];
    int    g2ids1[nids], lsec1_def[7]={7,255,2,1,1,0,1};

    *iret = 0;

    /*
     *  Read user input for parameter G2IDS
     */

    cst_ilst( input->g2ids, ';', IMISSD, nids, g2ids1, &num, &ret);

    /*
     *  set default values, if necessary
     */
    for ( j=0; j < nids; j++ ) {
         if ( g2ids1[j] == IMISSD ) g2ids1[j] = lsec1_def[j];
    }

    /*
     *    Split Gempak date/time
     */
    ti_ctoi( gemgrid->ctime[0], idate, &ret, 20 );

    /*
     *    Assign values to section 1 array
     */
    lsec1[0] = g2ids1[0];         /* Id of originating centre            */
    lsec1[1] = g2ids1[1];         /* Id of originating sub-centre        */
    lsec1[2] = g2ids1[2];         /* GRIB Master Tables Version Number   */
    lsec1[3] = g2ids1[3];         /* GRIB Local Tables Version Number    */
    lsec1[4] = g2ids1[4];         /* Significance of Reference Time      */
    lsec1[5] = idate[0];          /* Ref time - 4 digit year             */
    lsec1[6] = idate[1];          /* Ref time - month of year            */
    lsec1[7] = idate[2];          /* Ref time - day of month             */
    lsec1[8] = idate[3];          /* Ref time - hour of day              */
    lsec1[9] = idate[4];          /* Ref time - minute                   */
    lsec1[10] = 0;                /* Ref time - second                   */
    lsec1[11] = g2ids1[5];        /* Production status of data           */
    lsec1[12] = g2ids1[6];        /* Type of processed data              */

}
