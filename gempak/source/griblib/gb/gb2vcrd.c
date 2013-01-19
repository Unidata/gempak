#include "gb2def.h"

void gb2_vcrd ( char *wmolvltbl, char *lcllvltbl, Gribmsg *cmsg, 
                int *levels, int *vcord, int *iret )
/************************************************************************
 * gb2_vcrd								*
 *									*
 * This routine computes the vertical coordinate level/layer values     *
 * from the GRIB2 PDS.	Also, the GEMPAK vertical coordinate type is    *
 * obtained from the appropriate GRIB2 vertical coordinate table.       *
 *									*
 * If either wmolvltbl or lcllvltbl are NULL, the default tables are    *
 * read.                                                                *
 *									*
 * gb2_vcrd ( wmolvltbl, lcllvltbl, cmsg, levels, vcord, iret )		*
 *									*
 * Input parameters:							*
 *      *wmolvltbl      char            WMO vertical coordinate table   *
 *      *lcllvltbl      char            Local vertical coordinate table *
 *	*cmsg  	    struct Gribmsg      GRIB2  message structure        *
 *									*
 * Output parameters:							*
 *      levels[2]       int             lower/upper level/layer values  *
 *	*vcord		int		GEMPAK vertical goordinate type *
 *	*iret		int		return code			*
 *                                        -27 = Unrecognized PDT number *
 **									*
 * Log:									*
 * S. Gilbert/NCEP      11/04                                           *
 * S. Gilbert/NCEP      10/05           Use new routines to read tables *
 * S. Gilbert/NCEP          03/06    Chngs to remove compiler warnings  *
 ***********************************************************************/
{
    int     ret, ier, lvl1, lvl2, iver, lclver;
    G2level g2lev;
    G2lvls  *g2lvltbl;
    char    vparm[12], ctemp[20];
    float   rlevel[2];
    double  sfact;

/*---------------------------------------------------------------------*/

    *iret = 0;

    /* 
     *  Get Level into from vertical coordinate table(s).
     */
    iver=cmsg->gfld->idsect[2];
    lclver=cmsg->gfld->idsect[3];
    lvl1=cmsg->gfld->ipdtmpl[9];
    lvl2=cmsg->gfld->ipdtmpl[12];

    if ( ( lvl1 < 192 || lvl1 == 255 ) && 
         ( lvl2 < 192 || lvl2 == 255 ) ) {
       /* 
        *  Get WMO vertical coordinate table.
        */
        gb2_gtwmolvltbl( wmolvltbl, iver, &g2lvltbl, &ier);
        if ( ier == 0 ) {
           /* 
            *  Get Level into from WMO vertical coordinate table.
            */
            gb2_sklvl( lvl1, lvl2, g2lvltbl, &g2lev, &ier);
        }
    }
    else {
       /* 
        *  Get Local vertical coordinate table.
        */
        gb2_gtlcllvltbl( lcllvltbl, cmsg->origcntr, lclver, &g2lvltbl, &ier);
        if ( ier == 0 ) {
           /* 
            *  Get Level into from Local vertical coordinate table.
            */
            gb2_sklvl( lvl1, lvl2, g2lvltbl, &g2lev, &ier);
        }
    }
    if ( ier != 0 ) {
        *iret=ier;
        if ( ier == -30 ) {
            sprintf(ctemp,"%d|%d", lvl1, lvl2);
            er_wmsg("GB",&ier,ctemp,&ret,2,strlen(ctemp));
        }
        return;
    }

    /* 
     *  Calculate GEMPAK vertical coordinate type
     */
    lv_cord ( g2lev.abbrev, vparm, vcord, &ier, 4, 12);
    if ( ier != 0 ) {
        *iret=-7;
        return;
    }

    /* 
     *  Extract vertical coordinate levels from GRIB2 PDS
     */
    gb2_vlev ( cmsg->gfld, rlevel, &ier );
    if ( ier != 0 ) {
        *iret=ier;
        return;
    }

    /* 
     *  Scale level values by scale factor found in vertical 
     *  coordinate table.
     */
    sfact=pow((double)10.0,g2lev.scale);
    levels[0] = G_NINT( rlevel[0] * sfact );
    if  ( G_DIFF ( -1.0, rlevel[1] ) )  {
        levels[1] = -1;
    }
    else {
        levels[1] = G_NINT( rlevel[1] * sfact );
    }

    if ( lvl1 == 1 ) {          /*  Surface  */
        levels[0] = 0;
        levels[1] = -1;
    }


}
