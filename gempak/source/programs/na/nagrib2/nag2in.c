#include "nagrib2.h"

void nag2in( G2_input *input, int *iret )
/************************************************************************
 * nag2in                                                               *
 *                                                                      *
 * This routine reads the user input variables for nagrib2.             *
 *                                                                      *
 *  Usage:                                                              *
 *       int = nag2in( input, iret );                                   *
 *                                                                      *
 *                                                                      *
 *  Output Arguments:                                                   *
 *    *input        G2_input            Structure to hold user input    *
 *    *iret         int                 Error return                    *
 *                                      0  = Successfull                *
 *                                      -2 = Error getting one or more  *
 *                                           variables                  *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP          10/2004                                     *
 * m.gamazaychikov          10/2005	Added ovrwrt			*
 * S. Gilbert/NCEP          11/2007     Added pdsext                    *
 ***********************************************************************/
{
    int ret1,ret2,ret3,ret4,ret5,ret6,ret7,ret8,ret9,ret10,ret11,ret12,ret13;
    int sumret, len, num, i;
    char tmpstr[10];
    static int ivarlen=8;
    const int numtbls=5;

    char *cptr[2];
    /*char vbar[]="|";*/
    char tmpout[LLMXLN], tmpout2[LLMXLN];

    char gbfile[] ="GBFILE  ";
    char gdoutf[] ="GDOUTF  ";
    char proj[]   ="PROJ    ";
    char grdarea[]="GRDAREA ";
    char kxky[]   ="KXKY    ";
    char maxgrd[] ="MAXGRD  ";
    char cpyfil[] ="CPYFIL  ";
    char garea[]  ="GAREA   ";
    char output[] ="OUTPUT  ";
    char g2tbls[] ="G2TBLS  ";
    char g2diag[] ="G2DIAG  ";
    char ovrwrt[] ="OVERWR  ";
    char pdsext[] ="PDSEXT  ";

    /*
     *  Get GBFILE
     */
    memset(input->g2file,0,LLMXLN);
    ip_str( gbfile, input->g2file, &ret1, ivarlen, LLMXLN);
    st_null(input->g2file, input->g2file, &len, iret, LLMXLN, LLMXLN);

    /*
     *  Get GDOUTF
     */
    memset(input->gdoutf,0,LLMXLN);
    ip_str( gdoutf, input->gdoutf, &ret2,ivarlen,LLMXLN);
    st_null(input->gdoutf, input->gdoutf, &len, iret, LLMXLN, LLMXLN);
    cst_lcuc( input->gdoutf, tmpout, &ret2 );
    if ( strcmp( tmpout, "LIST") == 0 ) {
        input->lstflag = 1;
    }
    else {
        input->lstflag = 0;
    }

    /*
     *  Get PROJ
     */
    memset(input->proj,0,LLMXLN);
    ip_str( proj, input->proj, &ret3,ivarlen,LLMXLN);
    st_null(input->proj, input->proj, &len, iret, LLMXLN, LLMXLN);

    /*
     *  Get GRDAREA
     */
    memset(input->grdarea,0,LLMXLN);
    ip_str( grdarea, input->grdarea, &ret4,ivarlen,LLMXLN);
    st_null(input->grdarea, input->grdarea, &len, iret, LLMXLN, LLMXLN);

    /*
     *  Get KXKY
     */
    memset(input->kxky,0,LLMXLN);
    ip_str( kxky, input->kxky, &ret5,ivarlen,LLMXLN);
    st_null(input->kxky, input->kxky, &len, iret, LLMXLN, LLMXLN);

    /*
     *  Get MAXGRD
     */
    memset(input->maxgrd,0,LLMXLN);
    ip_str( maxgrd, input->maxgrd, &ret6,ivarlen,LLMXLN);
    st_null(input->maxgrd, input->maxgrd, &len, iret, LLMXLN, LLMXLN);
    st_numb(input->maxgrd, &(input->maxgrid), iret, len);
    if ( input->maxgrid >= MMHDRS ) {
        input->maxgrid = MMHDRS-1;
        /*st_inch(&input->maxgrid, tmpstr, iret, 10);*/
        cst_inch (input->maxgrid, tmpstr, iret );
        *iret=6;
        er_wmsg("NAGRIB2",iret,tmpstr,&num,7,10);
    }

    /*
     *  Get CPYFIL
     */
    memset(input->cpyfil,0,LLMXLN);
    ip_str( cpyfil, input->cpyfil, &ret7,ivarlen,LLMXLN);
    st_null(input->cpyfil, input->cpyfil, &len, iret, LLMXLN, LLMXLN );


    /*
     *  Split cpyfil using "|".  If optional subarea exists,
     *  set garea to it.  Otherwise get garea from dynamic tutor.
     */
    cptr[0]=tmpout;
    cptr[1]=tmpout2;
    cst_clst(input->cpyfil, '|', "\0", 2, LLMXLN, cptr, &num, iret);
    strncpy(input->cpyfil, cptr[0], (size_t)LLMXLN);
    if ( num == 2 ) {
        strncpy(input->garea, cptr[1], (size_t)LLMXLN);
    }
    else {
        memset(input->garea,0,LLMXLN);
        ip_str( garea, input->garea, &ret8,ivarlen,LLMXLN);
        st_null(input->garea, input->garea, &len, iret, LLMXLN, LLMXLN);
    }


    /*
     *  Get OUTPUT
     */
    memset(input->output,0,LLMXLN);
    ip_str( output, input->output, &ret9,ivarlen,LLMXLN);
    st_null(input->output, input->output, &len, iret, LLMXLN, LLMXLN);

    /*
     *  Get G2TBLS
     */
    memset(input->g2tbls,0,LLMXLN);
    ip_str( g2tbls, input->g2tbls, &ret10, ivarlen, LLMXLN);
    st_null(input->g2tbls, input->g2tbls, &len, iret, LLMXLN, LLMXLN);
    memset( input->tables, 0, numtbls*LLMXLN);
    for (i=0; i<numtbls; i++) {
       input->tbllist[i]=input->tables[i];
    }
    if (len != 0 ) {
        cst_clst( input->g2tbls, ';', "\0", numtbls, LLMXLN, input->tbllist,
                  &num, iret );
        /*st_clst( input->g2tbls, ";", "\0", &numtbls, input->tables, &num, iret,
                  LLMXLN       , 1  , 1   ,          LLMXLN);
        for (i=0; i<5; i++) {
            st_null(input->tables[i],input->tables[i],&len,iret,LLMXLN,LLMXLN);
            input->tbllist[i]=input->tables[i];
        }*/
    }

    /*
     *  Get G2DIAG
     */
    memset(input->g2diag,0,LLMXLN);
    ip_str( g2diag, input->g2diag, &ret11,ivarlen,LLMXLN);
    st_null(input->g2diag, input->g2diag, &len, iret, LLMXLN, LLMXLN);
    nadiag ( input->g2diag, input->g2dglst, input->maxgrid );

    /*
     *  Get OVERWR
     */
    ip_log( ovrwrt, &(input->overwr), &ret12,ivarlen);

    /*
     *  Get PDSEXT
     */
    ip_log( pdsext, &(input->pdsext), &ret13,ivarlen);

    sumret=ret1+ret2+ret3+ret4+ret5+ret6+ret7+ret8+ret9+ret10+ret11+ret12+ret13;

    if ( sumret == 0 ) *iret = 0;
    else *iret = -2;

}
