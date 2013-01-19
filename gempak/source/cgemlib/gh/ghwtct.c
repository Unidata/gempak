#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "ghcmn.h"

void gh_wtct ( char *filnam, char *zone, char *sev, char *vtime,
       	       char *stnum, char *stname,  char *wbas,  char *advnm, 
               char *fpd, 
               int *ilcl,  int *iltp, 
               int *icount, float lat[],   float lon[],  
               char *advtm, char *tau, char *mxwnd, char *wgust, 
               char *presr, char *tcdv,char *dvlbl, char *tcdir,
               char *tcspd, char *dtlbl, char *stsrc,
               int *iret)
/************************************************************************
 * gh_wtct                                                              *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC 06/07   Created					*
 * m.gamazaychikov/SAIC 10/08	Added stsrc to CS and to writing into   *
 *                              TCT element structure                   *
 * A. Krautkramer/NHC	 5/09	Add time zone to function input		*
 ***********************************************************************/
{
    int         	loc, gptyp, ii, ier;
    int			maxnum, maxch, istnum, npts;
    char		ofname[128], repst[30];
    char		**starr0;
    char		**starr1;
    char		**starr2;
    char		**starr3;
    char		**starr4;
    char		**starr5;
    char		**starr6;
    char		**starr7;
    char		**starr8;
    char		**starr9;
    char		**starr10;
    long		ifilesize;

    VG_DBStruct		el;
    FILE            	*ofptr;

/*---------------------------------------------------------------------*/
    *iret = 0;

    maxnum = 11;
    maxch = 30;
    cfl_inqr (  filnam, NULL, &ifilesize, ofname, &ier );
 
    if ( ier != 0 ) {
       cvg_crvgf ( filnam, &ier );
       ofptr = (FILE *) cfl_ropn (filnam, "",&ier);
    }
    else {
       ofptr = (FILE *) cfl_ropn (filnam, "", &ier);
    }

    /* 
     * Replace blank spaces with underscores
     */
    cst_rspc ( tcdv,  &ier);
    cst_rspc ( stsrc, &ier);
    /*
     *  Create header
     */
    el.hdr.vg_class = (char) CLASS_MET;
    el.hdr.vg_type = (char) TCTRK_ELM;
    el.hdr.filled  = 0;
    el.hdr.closed  = 0;
    el.hdr.delete  = 0;
    el.hdr.version = 0;
    el.hdr.maj_col = 2;
    el.hdr.min_col = 2;
    gptyp = 7;
    el.hdr.grptyp = (char) gptyp;
    el.hdr.grpnum = 10;
    /*
     *  Create TcInfo
     */
    sprintf ( el.elem.tct.info.issueStatus, "%s", "O" );
    sprintf ( el.elem.tct.info.stormNum, "%s", stnum);
    sprintf ( el.elem.tct.info.basin, "%s", wbas);
    sprintf ( el.elem.tct.info.advisoryNum, "%s" ,advnm);
    sprintf ( el.elem.tct.info.stormName, "%s", stname );
    sprintf ( el.elem.tct.info.stormType, "%s", sev );
    sprintf ( el.elem.tct.info.validTime, "%s", vtime );
    sprintf ( el.elem.tct.info.timezone, "%s", zone ); 
    sprintf ( el.elem.tct.info.fcstpd, "%s", fpd );
    /*
     *  Create TcTrack Data
     */
    el.elem.tct.lincol = *ilcl;
    el.elem.tct.lintyp = *iltp;
    el.elem.tct.numTrackPts   = *icount;
    npts = *icount;
    /*
     * Allocate memory for array size.
     */
    if ( !(el.elem.tct.trackPnt = malloc ( el.elem.tct.numTrackPts
                                  *sizeof( TcTrack) ))) {
           return;
    }
    starr0 = (char **)malloc(maxnum * sizeof(char *));
    starr1 = (char **)malloc(maxnum * sizeof(char *));
    starr2 = (char **)malloc(maxnum * sizeof(char *));
    starr3 = (char **)malloc(maxnum * sizeof(char *));
    starr4 = (char **)malloc(maxnum * sizeof(char *));
    starr5 = (char **)malloc(maxnum * sizeof(char *));
    starr6 = (char **)malloc(maxnum * sizeof(char *));
    starr7 = (char **)malloc(maxnum * sizeof(char *));
    starr8 = (char **)malloc(maxnum * sizeof(char *));
    starr9 = (char **)malloc(maxnum * sizeof(char *));
    starr10= (char **)malloc(maxnum * sizeof(char *));
    for ( ii = 0; ii < maxnum; ii++ ) {
         starr0[ii] = (char *)malloc((maxch+1) * sizeof(char *));
         starr1[ii] = (char *)malloc((maxch+1) * sizeof(char *));
         starr2[ii] = (char *)malloc((maxch+1) * sizeof(char *));
         starr3[ii] = (char *)malloc((maxch+1) * sizeof(char *));
         starr4[ii] = (char *)malloc((maxch+1) * sizeof(char *));
         starr5[ii] = (char *)malloc((maxch+1) * sizeof(char *));
         starr6[ii] = (char *)malloc((maxch+1) * sizeof(char *));
         starr7[ii] = (char *)malloc((maxch+1) * sizeof(char *));
         starr8[ii] = (char *)malloc((maxch+1) * sizeof(char *));
         starr9[ii] = (char *)malloc((maxch+1) * sizeof(char *));
         starr10[ii]= (char *)malloc((maxch+1) * sizeof(char *));
    }
    cst_clst (advtm, '|', "", maxnum, maxch, starr0, &istnum, &ier);
    cst_clst (tau,   '|', "", maxnum, maxch, starr1, &istnum, &ier);
    cst_clst (mxwnd, '|', "", maxnum, maxch, starr2, &istnum, &ier);
    cst_clst (wgust, '|', "", maxnum, maxch, starr3, &istnum, &ier);
    cst_clst (presr, '|', "", maxnum, maxch, starr4, &istnum, &ier);
    cst_clst (tcdv,  '|', "", maxnum, maxch, starr5, &istnum, &ier);
    cst_clst (dvlbl, '|', "", maxnum, maxch, starr6, &istnum, &ier);
    cst_clst (tcdir, '|', "", maxnum, maxch, starr7, &istnum, &ier);
    cst_clst (tcspd, '|', "", maxnum, maxch, starr8, &istnum, &ier);
    cst_clst (dtlbl, '|', "", maxnum, maxch, starr9, &istnum, &ier);
    cst_clst (stsrc, '|', "", maxnum, maxch, starr10,&istnum, &ier);

    for ( ii = 0; ii < *icount; ii++ )  {
        el.elem.tct.trackPnt[ii].lat = lat [ii];
        el.elem.tct.trackPnt[ii].lon = lon [ii];
        sprintf ( el.elem.tct.trackPnt[ii].advDate, starr0[ii] );
        sprintf ( el.elem.tct.trackPnt[ii].tau,     starr1[ii] );
        sprintf ( el.elem.tct.trackPnt[ii].mxWnd,   starr2[ii] );
        sprintf ( el.elem.tct.trackPnt[ii].wGust,   starr3[ii] );
        sprintf ( el.elem.tct.trackPnt[ii].mslp,    starr4[ii] );
        cst_rpst ( starr5[ii], "_"," ", repst, &ier );
        sprintf ( el.elem.tct.trackPnt[ii].tcDv,    repst );
        sprintf ( el.elem.tct.trackPnt[ii].tcDvLbl, starr6[ii] );
        sprintf ( el.elem.tct.trackPnt[ii].tcDir,   starr7[ii] );
        sprintf ( el.elem.tct.trackPnt[ii].tcSpd,   starr8[ii] );
        sprintf ( el.elem.tct.trackPnt[ii].dtLbl,   starr9[ii] );
        cst_rpst ( starr10[ii], "_"," ", repst, &ier );
        sprintf ( el.elem.tct.trackPnt[ii].stSrc,  repst);
    }


    el.hdr.recsz = (int) ( sizeof(VG_HdrStruct) + sizeof ( TcInfo)
                        + sizeof (int)*3 + npts*sizeof (TcTrack )) ;

    cvg_writefD( &el, -1, el.hdr.recsz, filnam, &loc, &ier );

    cfl_clos ( ofptr, &ier );

   /*
    * Free memory.
    */
    for( ii = 0; ii < maxnum; ii++ ) {
        free( starr0[ii] );
        free( starr1[ii] );
        free( starr2[ii] );
        free( starr3[ii] );
        free( starr4[ii] );
        free( starr5[ii] );
        free( starr6[ii] );
        free( starr7[ii] );
        free( starr8[ii] );
        free( starr9[ii] );
        free( starr10[ii] );
    }
                                                                                                 
    if ( starr0 ) free( (char **) starr0 );
    if ( starr1 ) free( (char **) starr1 );
    if ( starr2 ) free( (char **) starr2 );
    if ( starr3 ) free( (char **) starr3 );
    if ( starr4 ) free( (char **) starr4 );
    if ( starr5 ) free( (char **) starr5 );
    if ( starr6 ) free( (char **) starr6 );
    if ( starr7 ) free( (char **) starr7 );
    if ( starr8 ) free( (char **) starr8 );
    if ( starr9 ) free( (char **) starr9 );
    if ( starr10 ) free( (char **) starr10 );
}
