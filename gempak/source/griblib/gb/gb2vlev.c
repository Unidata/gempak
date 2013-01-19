#include "gb2def.h"

void gb2_vlev ( gribfield *gfld, float *rlevel, int *iret )
/************************************************************************
 * gb2_vlev								*
 *									*
 * This routine determines the vertical levels for a grid.		*
 *									*
 * gb2_vlev ( gfld, rlevel, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *      *gfld   struct gribfield        Decoded GRIB2 structure         *
 *									*
 * Output parameters:							*
 *	*rlevel 	float   	upper and lower Level(s)	*
 *	*iret		int		Return value			*
 *                                         -27 = Not recognized PDT     *
 **									*
 * Log:									*
 * Chiz/Unidata		 3/00	Created from gb_gpds			*
 * M. Li/GSC		 5/00	Added iret and cleaned up		*
 * S. Gilbert/NCEP       11/04  Modified from gb_vlev for use with GRIB2*
 * S. Gilbert/NCEP       5/05   Added checks for "missing" indicators   *
 *                              for level values and corresponding scale*
 *                              factors.                                *
 ***********************************************************************/
{
    double    scale,sfact;
/*---------------------------------------------------------------------*/
    *iret = 0;
   
    rlevel[0] = -1.0;
    rlevel[1] = -1.0;
    if ( gfld->ipdtnum < 20 ) {

        /*
        ** Process first Level
        */

        if ( gfld->ipdtmpl[9] != 255 ) {
           if ( gfld->ipdtmpl[10] == -127 ) {     /* missing scale factor */
              sfact = 1.0;
           }
           else {
              scale=(double)(-1.0*gfld->ipdtmpl[10]);
              sfact=pow((double)10.0,scale);
           }
           if ( gfld->ipdtmpl[11] == (-1 * 0x7fffffff) )     /* missing */
              rlevel[0]=0.0;
           else
              rlevel[0]=(float)(gfld->ipdtmpl[11]) * sfact;
           
        }

        /*
        ** Process second Level
        */

        if ( gfld->ipdtmpl[12] != 255 ) {
           if ( gfld->ipdtmpl[13] == -127 ) {     /* missing scale factor */
              sfact = 1.0;
           }
           else {
              scale=(double)(-1.0*gfld->ipdtmpl[13]);
              sfact=pow((double)10.0,scale);
           }
           if ( gfld->ipdtmpl[14] == (-1 * 0x7fffffff) )     /* missing */
              rlevel[0]=-1.0;
           else
              rlevel[1]=(float)(gfld->ipdtmpl[14]) * sfact;
        }

    }
    else {
       /* Template not accounted for...  */
       *iret=-27;
    }

}
