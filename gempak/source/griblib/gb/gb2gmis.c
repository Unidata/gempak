#include "gb2def.h"

void gb2_gmis ( gribfield *gfld, float *rmiss, int *nmiss )
/************************************************************************
 * gb2_gmis								*
 *									*
 * This routine checks the Data Representation Template to see if       *
 * missing value management is used, and returns the missing value(s)   *
 * in the data field.                                                   *
 *									*
 * gb2_gmis ( gfld, rmiss, nmiss )                                      *
 *									*
 * Input parameters:                                                    *
 *  *gfld     struct gribfield              decoded GRIB2 structure     *
 *									*
 * Output parameters:                                                   *
 *  *rmiss     float    Missing values used.                            *
 *  *nmiss      int     Number of missing values included in field      *
 **									*
 * Log:									*
 * S. Gilbert/NCEP       11/04                                          *
 ***********************************************************************/
{
    int	itype;

/*---------------------------------------------------------------------*/

    /*
     *  Missing value management currnetly only used in 
     *  DRT's 5.2 and 5.3.
     */
    if ( gfld->idrtnum != 2  &&  gfld->idrtnum != 3 ) {
       *nmiss=0;
       return;
    }
   
    itype = gfld->idrtmpl[4];
    if ( gfld->idrtmpl[6] == 1 ) {
         *nmiss=1;
         if (itype == 0)
            g2_rdieee(gfld->idrtmpl+7,rmiss+0,1);
         else
            rmiss[0]=(float)gfld->idrtmpl[7];
    }
    else if ( gfld->idrtmpl[6] == 2 ) {
         *nmiss=2;
         if (itype == 0) {
            g2_rdieee(gfld->idrtmpl+7,rmiss+0,1);
            g2_rdieee(gfld->idrtmpl+8,rmiss+1,1);
         }
         else {
            rmiss[0]=(float)gfld->idrtmpl[7];
            rmiss[1]=(float)gfld->idrtmpl[8];
         }
    }
    else {
       *nmiss=0;
    }

}
