#include "gdgrib2.h"

void gdg2in( GDG2_input *input, int *iret )
/************************************************************************
 * gdg2in                                                               *
 *                                                                      *
 * This routine reads the user input variables for gdgrib2.             *
 *                                                                      *
 *  Usage:                                                              *
 *       gdg2in( input, iret );                                         *
 *                                                                      *
 *                                                                      *
 *  Output Arguments:                                                   *
 *    *input        GDG2_input          Structure to hold user input    *
 *    *iret         int                 Error return                    *
 *                                      0  = Successfull                *
 *                                      -2 = Error getting one or more  *
 *                                           variables                  *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP          05/2005                                     *
 ***********************************************************************/
{
    int ret1, ret2, ret3, ret4, ret5, ret6, ret7, ret8, ret9, ret10;
    int ret11, ret12, ret13, ret14, ret15, ret16;
    int sumret, len, num, i;
    static int ivarlen=8;
    const int numtbls=5;

    char gdfile[]  = "GDFILE  ";
    char g2file[]  = "GBFILE  ";
    char gfunc[]   = "GFUNC   ";
    char gdattim[] = "GDATTIM ";
    char glevel[]  = "GLEVEL  ";
    char gvcord[]  = "GVCORD  ";
    char proj[]    = "PROJ    ";
    char grdarea[] = "GRDAREA ";
    char kxky[]    = "KXKY    ";
    char cpyfil[]  = "CPYFIL  ";
    char g2tbls[]  = "G2TBLS  ";
    char g2is[]    = "G2IS    ";
    char g2ids[]   = "G2IDS   ";
    char g2pdt[]   = "G2PDT   ";
    char g2drt[]   = "G2DRT   ";
    char wmohdr[]  = "WMOHDR  ";

    /*
     *  Get GDFILE
     */
    memset(input->gdfile,0,LLMXLN);
    ip_str( gdfile, input->gdfile, &ret1, ivarlen, LLMXLN);
    st_null(input->gdfile, input->gdfile, &len, iret, LLMXLN, LLMXLN);

    /*
     *  Get GBFILE
     */
    memset(input->g2file,0,LLMXLN);
    ip_str( g2file, input->g2file, &ret2,ivarlen,LLMXLN);
    st_null(input->g2file, input->g2file, &len, iret, LLMXLN, LLMXLN);

    /*
     *  Get GFUNC
     */
    memset(input->gfunc,0,LLMXLN);
    ip_str( gfunc, input->gfunc, &ret3,ivarlen,LLMXLN);
    st_null(input->gfunc, input->gfunc, &len, iret, LLMXLN, LLMXLN);

    /*
     *  Get GDATTIM
     */
    memset(input->gdattim,0,LLMXLN);
    ip_str( gdattim, input->gdattim, &ret4,ivarlen,LLMXLN);
    st_null(input->gdattim, input->gdattim, &len, iret, LLMXLN, LLMXLN);

    /*
     *  Get GLEVEL
     */
    memset(input->glevel,0,LLMXLN);
    ip_str( glevel, input->glevel, &ret5,ivarlen,LLMXLN);
    st_null(input->glevel, input->glevel, &len, iret, LLMXLN, LLMXLN);

    /*
     *  Get GVCORD
     */
    memset(input->gvcord,0,LLMXLN);
    ip_str( gvcord, input->gvcord, &ret6,ivarlen,LLMXLN);
    st_null(input->gvcord, input->gvcord, &len, iret, LLMXLN, LLMXLN);

    /*
     *  Get PROJ
     */
    memset(input->proj,0,LLMXLN);
    ip_str( proj, input->proj, &ret8,ivarlen,LLMXLN);
    st_null(input->proj, input->proj, &len, iret, LLMXLN, LLMXLN);

    /*
     *  Get GRDAREA
     */
    memset(input->grdarea,0,LLMXLN);
    ip_str( grdarea, input->grdarea, &ret9,ivarlen,LLMXLN);
    st_null(input->grdarea, input->grdarea, &len, iret, LLMXLN, LLMXLN);

    /*
     *  Get KXKY
     */
    memset(input->kxky,0,LLMXLN);
    ip_str( kxky, input->kxky, &ret10,ivarlen,LLMXLN);
    st_null(input->kxky, input->kxky, &len, iret, LLMXLN, LLMXLN);

    /*
     *  Get CPYFIL
     */
    memset(input->cpyfil,0,LLMXLN);
    ip_str( cpyfil, input->cpyfil, &ret11,ivarlen,LLMXLN);
    st_null(input->cpyfil, input->cpyfil, &len, iret, LLMXLN, LLMXLN );

    /*
     *  Get G2TBLS
     */
    memset(input->g2tbls,0,LLMXLN);
    ip_str( g2tbls, input->g2tbls, &ret12, ivarlen, LLMXLN);
    st_null(input->g2tbls, input->g2tbls, &len, iret, LLMXLN, LLMXLN);
    memset( input->tables, 0, numtbls*LLMXLN);
    for (i=0; i<numtbls; i++) {
       input->tbllist[i]=input->tables[i];
    }
    if (len != 0 ) {
        cst_clst( input->g2tbls, ';', "\0", numtbls, LLMXLN, input->tbllist,
                  &num, iret );
    }

    /*
     *  Get G2IS
     */
    memset(input->g2is,0,LLMXLN);
    ip_str( g2is, input->g2is, &ret13,ivarlen,LLMXLN);
    st_null(input->g2is, input->g2is, &len, iret, LLMXLN, LLMXLN );

    /*
     *  Get G2IDS
     */
    memset(input->g2ids,0,LLMXLN);
    ip_str( g2ids, input->g2ids, &ret7,ivarlen,LLMXLN);
    st_null(input->g2ids, input->g2ids, &len, iret, LLMXLN, LLMXLN );

    /*
     *  Get G2PDT
     */
    memset(input->g2pdt,0,LLMXLN);
    ip_str( g2pdt, input->g2pdt, &ret14,ivarlen,LLMXLN);
    st_null(input->g2pdt, input->g2pdt, &len, iret, LLMXLN, LLMXLN );

    /*
     *  Get G2DRT
     */
    memset(input->g2drt,0,LLMXLN);
    ip_str( g2drt, input->g2drt, &ret15,ivarlen,LLMXLN);
    st_null(input->g2drt, input->g2drt, &len, iret, LLMXLN, LLMXLN );

    /*
     *  Get WMOHDR
     */
    memset(input->wmohdr,0,LLMXLN);
    ip_str( wmohdr, input->wmohdr, &ret16,ivarlen,LLMXLN);
    st_null(input->wmohdr, input->wmohdr, &len, iret, LLMXLN, LLMXLN );


    sumret= ret1+ret2+ret3+ret4+ret5+ret6+ret7+ret8+ret9+ret10+ret11+ret12;
    sumret= sumret + ret13 + ret14 + ret15 + ret16;

    if ( sumret == 0 ) *iret = 0;
    else *iret = -2;

}
