#include "gdgrib2.h"

#define MAXTMPL 200

void gdsetsect5 ( GDG2_input *input, int *idrtnum, int *idrtmpl, int *iret )
/************************************************************************
 * gdsetsect5                                                           *
 *                                                                      *
 * This routine sets up an array containing information to encode       *
 * GRIB2 Sections 5 and 7.  The required info is obtained from user     *
 * parameter G2DRT.                                                     *
 *                                                                      *
 *  Usage:                                                              *
 *      gdsetsect5( input, idrtnum, idrtmpl, iret );                    *
 *                                                                      *
 *  Input Arguments:                                                    *
 *      *input            GDG2_input       input structure for GDGRIB2  *
 *                                                                      *
 *  Output Arguments:                                                   *
 *      *idrtnum           int             Data Representation Template *
 *                                         number.                      *
 *      *idrtmpl           int             Data Representation Template *
 *                                         values.                      *
 *      *iret              int              Error return code.          *
 *                                           0 = Normal                 *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP           8/2005   Orig                              *
 ***********************************************************************/
{

    const int ng2drt=6;
    int    ret, num, j;
    int    g2drt[ng2drt], g2drt_defs[6]={0,0,0,1,0,255};

    *iret = 0;

    /*
     *    parse user input for parameter G2DRT
     */
    cst_ilst( input->g2drt, '|', IMISSD, ng2drt, g2drt, &num, &ret);

    /*
     *  set default values, if necessary
     */
    for ( j=0; j < ng2drt; j++ ) {
         if ( g2drt[j] == IMISSD ) g2drt[j] = g2drt_defs[j];
    }

    /*
     *    Assign values to Data Representaion Template array
     */
    *idrtnum = g2drt[0];
    for (j=0;j<MAXTMPL;j++) idrtmpl[j]=0;   /*   initialize to zero      */
    idrtmpl[1]=g2drt[2];                    /*   binary scale factor     */
    idrtmpl[2]=g2drt[1];                    /*   decimal scale factor    */

    /* 
     *   Set the Order of Differencing. if using DRT 5.3
     */
    if ( *idrtnum == 3 ) idrtmpl[16]=g2drt[3];

    /* 
     *   Set the Missing Value Management. if using DRT 5.2 or 5.3
     */
    if ( *idrtnum == 2 || *idrtnum == 3) 
         idrtmpl[6]=g2drt[4];            

    /* 
     *   Set the lossless/lossy flag/ratio. if using DRT 5.40
     */
    if ( *idrtnum == 40 ) {
         if ( g2drt[5] == 255 ) {       /*  lossless compression   */
              idrtmpl[5]=0;
              idrtmpl[6]=g2drt[5];
         }
         else {                         /*  lossy compression   */
              idrtmpl[5]=1;
              idrtmpl[6]=g2drt[5];
         }
    }

}
