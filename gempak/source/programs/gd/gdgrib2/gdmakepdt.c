#include "gdgrib2.h"
#include "ctbcmn.h"
#include "gb2def.h"


void gdmakepdt ( GDG2_input *input, GDG2_gemgrid *gemgrid, int *sec1, 
                 int *discipline, int *pdtnum, int *pdtmpl, int *iret )
/************************************************************************
 * gdmakepdt                                                            *
 *                                                                      *
 * This routine generates a GRIB2 Product Definition Template from the  *
 * Gempak parameter name, level info, date/time info, section 1 info,   *
 * and appropriate conversion tables.                                   *
 *                                                                      *
 *  Usage:                                                              *
 *      gdmakepdt( input, gemgrid, origcntr, wmovers, localvers,        *
 *                 discipline, pdtnum, pdtmpl, iret)                    *
 *                                                                      *
 *  Input Arguments:                                                    *
 *      *input            GDG2_input       input structure for GDGRIB2  *
 *      *gemgrid          GDG2_gemgrid     grid and Gempak info         *
 *      *sec1              int             GRIB2 Section 1 values       *
 *                                                                      *
 *  Output Arguments:                                                   *
 *      *discipline        int             GRIB2 discipline number      *
 *      *pdtnum            int             GRIB2 Product Definition     *
 *                                               Template number        *
 *      *pdtmpl            int             GRIB2 Product Definition     *
 *                                               Template values        *
 *      *iret              int              Error return code.          *
 *                                           0 = Normal                 *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP	 8/05	Orig					*
 * S. Jacobs/NCEP	 2/11	Replaced strncpy with cst_ncpy		*
 ***********************************************************************/
{
    int    ret, ier, itmp, ier2;
    int    consec, quant, nocc, pos;
    int    wmover, lclver;
    char   parmtr[9], parmtmp[9], quantstr[9];
    char   tmpcntr[8], wmocntr[8];
    size_t jj;

    G2Vinfo   tblentry;
    G2level   lvlentry;
/*---------------------------------------------------------------------*/
    *iret = 0;
    *discipline = 255;
    *pdtnum = 65535;

    /*
     *  Get Originating Center from wmocenter.tbl
     */
    gb2_gtcntr( sec1[0], input->tbllist[4], tmpcntr, &ier);
    if ( ier != 0 ) {
        er_wmsg("GB", &ier, " ", &itmp, 2, 1);
    }
    cst_uclc(tmpcntr, wmocntr, &ier);

    /*
     *  Check for any numeric info in the parameter and replace
     *  it with the string '--' for searching the parameter tables
     */
    quant = IMISSD;
    consec = 0;
    quantstr[0] = '\0';
    cst_ncpy ( parmtr, gemgrid->param, 8, &ier2 );
    for ( jj=0; jj<strlen(gemgrid->param); jj++ ) {
        if ( isdigit(gemgrid->param[jj]) ) {
             strncat( quantstr, gemgrid->param+jj, 1);
             consec++;
        }
        else {
            if ( consec >= 2 ) break;
            if ( consec == 1 ) {
                consec = 0;
                quantstr[0] = '\0';
            }
        }
    }
    
    if ( strlen( quantstr ) >= (size_t)2 ) {
        cst_rpst( gemgrid->param, quantstr, "--", parmtr, &ret);
        cst_nocc( parmtr, '-', 2, 0, &nocc, &ret );
        if ( nocc == (int)strlen(parmtr)-1 && nocc > 4 ) {
            cst_rmst( parmtr, "--", &pos, parmtmp, &ret );
            strcpy ( parmtr, parmtmp );
        }
        sscanf( quantstr, "%d", &quant );
    }

    /*
     *  Get GRIB2 discipline, category, and number for parameter
     */
    wmover = sec1[2];
    lclver = sec1[3];
    gb2_param2g2( parmtr, input->tbllist[0], input->tbllist[1], 
                  wmover, lclver, wmocntr, &tblentry, &ier );
    if ( ier != 0 ) {
        er_wmsg("GB", &ier, " ", &itmp, 2, 1);
    }
    *discipline = tblentry.discpln;
    *pdtnum = tblentry.pdtnmbr;
    pdtmpl[0] = tblentry.categry;
    pdtmpl[1] = tblentry.paramtr;


    /*
     *  Get GRIB2 level identifiers.
     */
    gb2_level2g2( gemgrid->vcord, input->tbllist[2], input->tbllist[3], 
                  wmover, lclver, wmocntr, &lvlentry, &ier );
    if ( ier != 0 ) {
        er_wmsg("GB", &ier, " ", &itmp, 2, 1);
    }
    pdtmpl[9] = lvlentry.id1;
    pdtmpl[10] = lvlentry.scale;
    pdtmpl[11] = gemgrid->level[0];
    pdtmpl[12] = lvlentry.id2;
    if ( gemgrid->level[1] != -1 ) {
        pdtmpl[12] = pdtmpl[9];
        pdtmpl[13] = lvlentry.scale;
        pdtmpl[14] = gemgrid->level[1];
    }
    else if ( pdtmpl[12] == 255 ) {
        pdtmpl[13] = 0;
        pdtmpl[14] = 0;
    }
    else {
        pdtmpl[13] = lvlentry.scale;
        pdtmpl[14] = gemgrid->level[1];
    }

    /*
     *  Set GRIB2 forecast time 
     */
    quant *= 60;    /* convert to minutes  */
    gb2_setftime( gemgrid->ctime[0], quant, *pdtnum, pdtmpl, &ier );

}
