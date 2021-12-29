#include "gdgrib2.h"

#define TMPLEN 80

void gdsetnav ( GDG2_input *input, float *rnvblk, int *iret )
/************************************************************************
 * gdsetnav                                                             *
 *                                                                      *
 * This routine creates a gempak grid navigation block based on user    *
 * input variables CPYFIL, KXKY, PROJ, and GRDAREA.                     *
 * If CPYFIL is set, the navigation block will be set according to its  *
 * value.  Otherwise, the nav block will be constructed with info from  *
 * KXKY, PROJ, and GRDAREA.  Then the new navigation is set in the DG   *
 * library for future grid requests.                                    *
 * If none of the parameters are set, we default to the current input   *
 * grid file's navigation.                                              *
 *                                                                      *
 *  Usage:                                                              *
 *      gdsetnav( input, rnvblk, iret );                                *
 *                                                                      *
 *  Input Arguments:                                                    *
 *      *input            GDG2_input       input structure for GDGRIB2  *
 *                                                                      *
 *  Output Arguments:                                                   *
 *      *rnvblk            float           grid navigation block        *
 *      *iret              int              Error return code.          *
 *                                           0 = Normal                 *
 *                                         -30 = could not find grid    *
 *                                               number in grdnav.tbl   *
 *                                         -31 = could not open CPYFIL  *
 *                                               grid file              *
 *                                         -33 = could not process      *
 *                                               PROJ, KXKY, GRDAREA    *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP           6/2005   Orig                              *
 * B. Hebbard/NCEP           2/2021   On gd_open of CPYFIL, pass actual *
 *                                    (vs. alloc) strlen (NAWIPS-160);  *
 *                                    remove gd_clos() of unused igdfln *
 ***********************************************************************/
{

    int    ret;
    int    nxgd, nygd, nowrit, maxgrd;
    int    mxanl, mxnav, iacss;
    char   cname[TMPLEN], cproj[TMPLEN];
    float  anlblk[LLNANL], garea[4];

    *iret=0;

    if ( strlen(input->cpyfil) != (size_t)0 ) {
        /*
         *  CPYFIL set
         */
        if ( input->cpyfil[0] == '#' ) {
           /*
            * get navigation from grdnav.tbl
            */ 
           na_gtbl( input->cpyfil, cname, cproj, &nxgd, &nygd, garea,
                    rnvblk, anlblk, &ret );
           if ( ret != 0 ) {
               *iret = -30;
               return;
           }
        }
        else {
           /*
            * get navigation from grid file specified by CPYFIL.
            */ 
           nowrit = 0;
           mxanl=0;
           mxnav=13;
           gd_open( input->cpyfil, &nowrit, &mxanl, &mxnav, &iacss,
                    anlblk, rnvblk, &maxgrd, &ret, strlen(input->cpyfil) );
           if ( ret != 0 ) {
               *iret = -31;
               return;
           }
        }
        /*
         *  Set output grid navigation in DG library
         */
        dg_onav( rnvblk, &ret );
    }
    else {
        /*
         *  CPYFIL not set
         */
        if ( strlen( input->proj ) == (size_t)0 &&
             strlen( input->kxky ) == (size_t)0 &&
             strlen( input->grdarea ) == (size_t)0 ) {
           /*
            *  Keep navigation from input grid file
            */ 
           nowrit = 0;
           mxanl=0;
           mxnav=13;
           gd_open( input->gdfile, &nowrit, &mxanl, &mxnav, &iacss,
                    anlblk, rnvblk, &maxgrd, &ret, strlen(input->gdfile) );
        }
        else {
           /*
            *  Construct navigation block from KXKY, PROJ, GRDAREA
            */
           na_gnav( input->proj, input->kxky, input->grdarea, cproj, 
                    &nxgd, &nygd, garea, rnvblk, &ret );
           if ( ret != 0 ) {
               *iret = -33;
               return;
           }
           /*
            *  Set output grid navigation in DG library
            */
           dg_onav( rnvblk, &ret );
        }
    }
 
}
