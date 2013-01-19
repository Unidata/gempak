#include "gdgrib2.h"

#define MAXTMPL 200

void gdsetsect4 ( GDG2_input *input, GDG2_gemgrid *gemgrid, 
                  int *lsec1, int *discipline, 
                  int *ipdtnum, int *ipdtmpl, int *iret )
/************************************************************************
 * gdsetsect4                                                           *
 *                                                                      *
 * This routine sets up an array containing Product Definition Template *
 * values to encode GRIB2 Section 4 - Product Definition Section.       *
 * The required info is obtained from user parameter G2PDT and the      *
 * GEMPAK parameter/level/date/time information.                        *
 *                                                                      *
 * User supplied values in G2PDT override values automatically          *
 * generated from the GEMPAK grid information.                          *       
 *                                                                      *
 *  Usage:                                                              *
 *      gdsetsect4( input, gemgrid, lsec1, discipline, ipdtnum,         *
 *                  ipdtmpl, iret );                                    *
 *                                                                      *
 *  Input Arguments:                                                    *
 *      *input            GDG2_input       input structure for GDGRIB2  *
 *      *gemgrid          GDG2_gemgrid     output grid and Gempak info  *
 *      *lsec1             int             Section 1 values             *
 *                                                                      *
 *  Output Arguments:                                                   *
 *      *discipline        int             GRIB2 discipline number      *
 *      *ipdtnum           int             Product Definition Template  *
 *                                         number.                      *
 *      *ipdtmpl           int             Product Definition Template  *
 *                                         values.                      *
 *      *iret              int              Error return code.          *
 *                                           0 = Normal                 *
 *                                           -29 = bad G2PDT parameter  *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP           6/2005   Orig                              *
 ***********************************************************************/
{

    const int misstmpl = 65535;
    int    ret, num, j;
    int    kpdtnum, kpdtmpl[MAXTMPL];
    char   tmppdt[2][LLMXLN], *tlist[2], *cdef="\0";
    unsigned int allones;

/*---------------------------------------------------------------------*/

    *iret = 0;
    allones = 0xFFFFFFFF;
    *ipdtnum = misstmpl;

    /*
     *   Generate PDT values from gempak grid info.
     */
    for (j=0;j<MAXTMPL;j++) ipdtmpl[j]=allones;
    gdmakepdt( input, gemgrid, lsec1, discipline, ipdtnum, ipdtmpl, &ret );

    if ( strlen( input->g2pdt ) != (size_t)0 ) {
        /*
         *   Parse parameter G2PDT for template and values
         */
        tlist[0] = tmppdt[0];
        tlist[1] = tmppdt[1];
        cst_clst( input->g2pdt, '|', cdef, 2, LLMXLN, tlist, &num, &ret);
        if ( num != 2 ) {
            *iret = -29;
            return;
        }
        cst_ilst( tlist[0], ';', misstmpl, 1, &kpdtnum, &num, &ret);
        cst_ilst( tlist[1], ';', allones, MAXTMPL, kpdtmpl, &num, &ret);

        /*
         *   If user specified PDT number doesn't match table PDT number,
         *   use all template values supplied in G2PDT
         *   Else, overwite auto-generated template values with 
         *   the non missing value supplied in G2PDT.
         */
        if ( *ipdtnum != kpdtnum && kpdtnum != misstmpl ) {
            *ipdtnum = kpdtnum;
            for (j=0;j<MAXTMPL;j++) ipdtmpl[j] = kpdtmpl[j];
        }
        else {
            for (j=0;j<MAXTMPL;j++) {
                if ( (unsigned int)kpdtmpl[j] != allones ) ipdtmpl[j] = kpdtmpl[j];
            }
        }
    }
    
}
