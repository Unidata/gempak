#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "cds.h"
                                                                                    
void cds_tce ( VG_DBStruct *el, int *iret )
/************************************************************************
 * cds_tce                                                            	*
 *                                                                      *
 * This function displays tc erro cone to the output device.            *
 *                                                                      *
 * cds_tce (el, indx, iret)  	                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      *el             VG_DBStruct     Pointer to VG record structure  *
 *      indx            int             Index into user attribute table *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                                                      *
 **                                                                     *
 * Log:									*
 * m.gamazaychikov/SAIC	06/07   	Created				*
 ***********************************************************************/
{
    int         np, ier, xoff, yoff;
    int         width, lthw, lwhw, mkhw;
    int         ismtypx, icolrx, iltypx, ilthwx, iwidthx, iwhwx, imarkx;
    int         imkhwx, imkwidx, itxfnx, itxhwx, itxwidx, ibrdrx;
    int         irrotnx, ijustx, iftypx;
    float       *plat, *plon, size, rotn, szfill;
    float       densx, sztextx, szmarkx, szfillx;
/*---------------------------------------------------------------------*/
                                                                                    
    *iret = 0;
                                                                                    
    /*
     *  Save plot and text attributes.
     */
    gqsmth ( &ismtypx, &densx, &ier );
    gqfill ( &szfillx, &iftypx, &ier );
    gqcolr ( &icolrx, &ier );
    gqline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gqmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &ier );
    gqtext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
             &ijustx, &ier );
                                                                                    
    /*
     * setup basic information
     */
    lthw  = 0;
    lwhw  = 0;
    mkhw  = 0;
    width = 2;
    size  = 1.25F;
    width = 3;
    np    = el->elem.tce.cone.npts;
    rotn  = 0.0F;
    xoff  = 2;
    yoff  = 2;
    szfill = 1.5F;
                                                                                    
    /*
     * Draw the cone
     */
    plat = el->elem.tce.cone.latlon;
    plon = &(el->elem.tce.cone.latlon[np]);
    gscolr (&(el->elem.tce.cone.lincol), &ier);
    gsline (&(el->elem.tce.cone.lintyp), &lthw, &width, &lwhw, &ier);
    gline  (sys_M, &np, plat, plon, &ier, strlen(sys_M));
    gscolr (&(el->elem.tce.cone.filcol), &ier);
    gsfill ( &szfill, &(el->elem.tce.cone.filtyp), &ier );
    gfill(sys_M, &np, plat, plon, &ier, strlen(sys_M));

    /*
     *  Restore the saved plot and text attribute values
     */
    gstext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
             &ijustx, &ier );
    gsmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &ier );
    gsline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gscolr ( &icolrx, &ier );
    gssmth ( &ismtypx, &densx, &ier );
    gsfill ( &szfillx, &iftypx, &ier );
                                                                                    
}

/*=====================================================================*/

void cds_tct ( VG_DBStruct *el, int *iret )
/************************************************************************
 * cds_tct                                                            	*
 *                                                                      *
 * This function displays tc track to the output device.                *
 *                                                                      *
 * cds_tct (el, indx, iret)  	                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      *el             VG_DBStruct     Pointer to VG record structure  *
 *      indx            int             Index into user attribute table *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                                                      *
 **                                                                     *
 * Log:									*
 * m.gamazaychikov/SAIC	06/07   	Created				*
 * m.gamazaychikov/SAIC	10/08   	Cleaned up, made changes for	*
 *					plotting extratropical cyclones *
 ***********************************************************************/
{
    int         np, ier, xoff, yoff, ii,one=1;
    int         color, width, lthw, lwhw, mkhw;
    int         ismtypx, icolrx, iltypx, ilthwx, iwidthx, iwhwx, imarkx;
    int         imkhwx, imkwidx, itxfnx, itxhwx, itxwidx, ibrdrx;
    int         irrotnx, ijustx, iftypx;
    int         itxfn, itxhw, itxwid, ibrdr, irrotn, ijust;
    int         imtype, tcolor;
    float       *plat, *plon, size, rotn, sztext, size2;
    float       densx, sztextx, szmarkx, szfillx;
/*---------------------------------------------------------------------*/
                                                                                    
    *iret = 0;
                                                                                    
    /*
     *  Save plot and text attributes.
     */
    gqsmth ( &ismtypx, &densx, &ier );
    gqfill ( &szfillx, &iftypx, &ier );
    gqcolr ( &icolrx, &ier );
    gqline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gqmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &ier );
    gqtext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
             &ijustx, &ier );
                                                                                    
    /*
     * setup basic information
     */
    np    = el->elem.tct.numTrackPts;
    lthw  = 0;
    width = 2;
    lwhw  = 0;
                                                                                    
    /*
     * Plot line
     */
    G_MALLOC ( plat, float, np, "cds_tct: plat" );
    G_MALLOC ( plon, float, np, "cds_tct: plon" );
    for ( ii = 0; ii < np; ii++) {
      plat[ii] = el->elem.tct.trackPnt[ ii ].lat;
      plon[ii] = el->elem.tct.trackPnt[ ii ].lon;
    }
    gscolr (&(el->elem.tct.lincol), &ier);
    gsline (&(el->elem.tct.lintyp), &lthw, &width, &lwhw, &ier);
    gline  (sys_M, &np, plat, plon, &ier, strlen(sys_M));

    /*
     * Plot marker for the current position 
     *
     * color = 1  -white
     * color = 32 -black
     *
     */
    imtype = 17;
    mkhw  = 0;
    width = 2;

    size = 1.8F;
    color = 1; 
    gscolr (&color, &ier);
    gsmrkr (&imtype, &mkhw, &size, &width, &ier);
    gmark  (sys_M, &one, &(plat[0]), &(plon[0]), &ier, strlen(sys_M));
    size = size - 0.2F;
    color = 32;
    gscolr (&color, &ier);
    gsmrkr (&imtype, &mkhw, &size, &width, &ier);
    gmark  (sys_M, &one, &(plat[0]), &(plon[0]), &ier, strlen(sys_M));
    size  = 0.75F;
    color = 1;
    gscolr (&color, &ier);
    gsmrkr (&imtype, &mkhw, &size, &width, &ier);
    gmark  (sys_M, &one, &(plat[0]), &(plon[0]), &ier, strlen(sys_M));

    itxfn = 22;
    itxhw = 2;
    sztext =0.6F;
    itxwid = 1;
    ibrdr = 111;
    irrotn = 1;
    ijust   = 2;
    rotn  = 0.0F;
    xoff  = 0;
    yoff  = 0;

    /*
     * Plot marker and letter of intensity for forecast positions:
     */
    for ( ii = 1; ii < np; ii++) {
        if ( !strcmp( el->elem.tct.trackPnt[ii].stSrc, "ExtratropicalCyclone") ) {
        /*
         *    For extratropical storms
         *    marker first
         */
              color = 1; 
              size = 1.5F;
              gscolr (&color, &ier);
              gsmrkr (&imtype, &mkhw, &size, &width, &ier);
              gmark  (sys_M, &one, &(plat[ii]), &(plon[ii]), &ier, strlen(sys_M));
              color = 32; 
              size2 = size-0.2;
              tcolor = 1; 
        /*
         *    For extratropical storms
         *    marker first
         */
         } else {
        /*
         *    For tropical storms
         */
              color = 1; 
              tcolor = 32; 
              size2 = 1.5F;
         }
         gscolr (&color, &ier);
         gsmrkr (&imtype, &mkhw, &size2, &width, &ier);
         gmark  (sys_M, &one, &(plat[ii]), &(plon[ii]), &ier, strlen(sys_M));
         gscolr (&tcolor, &ier);
         gstext( &itxfn, &itxhw, &sztext, &itxwid, &ibrdr, &irrotn,
                 &ijust, &ier );
         gtext (sys_M, &(plat[ii]), &(plon[ii]), 
                el->elem.tct.trackPnt[ii].tcDvLbl, &rotn, &xoff, &yoff,
                 &ier, strlen(sys_M), strlen(el->elem.tct.trackPnt[ii].tcDvLbl));
    }

    /*
     * Plot Date Label at each track point
     */
    itxfn = 22;
    itxhw = 2;
    itxwid = 1;
    ibrdr = 111;
    irrotn = 1;
    ijust   = 2;
    color = 1;
    xoff  = -2;
    yoff  = -5;
    gscolr (&color, &ier);
    gstext( &itxfn, &itxhw, &sztext, &itxwid, &ibrdr, &irrotn,
            &ijust, &ier );
    for ( ii = 0; ii < np; ii++) {
        gtext (sys_M, &(plat[ii]), &(plon[ii]),
               el->elem.tct.trackPnt[ii].dtLbl, &rotn, &xoff, &yoff,
               &ier, strlen(sys_M), strlen(el->elem.tct.trackPnt[ii].dtLbl));
    }

    /*
     *  Restore the saved plot and text attribute values
     */
    gstext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
             &ijustx, &ier );
    gsmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &ier );
    gsline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gscolr ( &icolrx, &ier );
    gssmth ( &ismtypx, &densx, &ier );
    gsfill ( &szfillx, &iftypx, &ier );
                                                                                    
    G_FREE ( plat, float );
    G_FREE ( plon, float );
}

/*=====================================================================*/

void cds_tcb ( VG_DBStruct *el, int *iret )
/************************************************************************
 * cds_tcb                                                            	*
 *                                                                      *
 * This function displays tc erro cone to the output device.            *
 *                                                                      *
 * cds_tcb (el, indx, iret)  	                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *      *el             VG_DBStruct     Pointer to VG record structure  *
 *      indx            int             Index into user attribute table *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                                                      *
 **                                                                     *
 * Log:									*
 * m.gamazaychikov/SAIC	06/07   	Created				*
 ***********************************************************************/
{
    int         np, ier, lintyp,ii;
    int         lthw, lwhw, width;
    int         ismtypx, icolrx, iltypx, ilthwx, iwidthx, iwhwx, imarkx;
    int         imkhwx, imkwidx, itxfnx, itxhwx, itxwidx, ibrdrx;
    int         irrotnx, ijustx, iftypx;
    float       *plat, *plon;
    float       densx, sztextx, szmarkx, szfillx;
/*---------------------------------------------------------------------*/
                                                                                    
    *iret = 0;
                                                                                    
    /*
     *  Save plot and text attributes.
     */
    gqsmth ( &ismtypx, &densx, &ier );
    gqfill ( &szfillx, &iftypx, &ier );
    gqcolr ( &icolrx, &ier );
    gqline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gqmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &ier );
    gqtext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
             &ijustx, &ier );
                                                                                    
    /*
     * setup basic information
     */
    lthw  = 0;
    lwhw  = 0;
    np    = el->elem.tcb.numBkPts;
    lintyp = 1;
    width = el->elem.tcb.linwid;

    /*
     * draw the watch warning line segment 
     */
    G_MALLOC ( plat, float, np, "cds_tcb: plat" );
    G_MALLOC ( plon, float, np, "cds_tcb: plon" );
    for ( ii = 0; ii < np; ii++) {
      plat[ii] = el->elem.tcb.bkPntLn[ii].lat;
      plon[ii] = el->elem.tcb.bkPntLn[ii].lon;
    }
    gscolr (&(el->elem.tcb.lincol), &ier);
    gsline (&lintyp, &lthw, &width, &lwhw, &ier);
    gline  (sys_M, &np, plat, plon, &ier, strlen(sys_M));

    /*
     *  Restore the saved plot and text attribute values
     */
    gstext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
             &ijustx, &ier );
    gsmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &ier );
    gsline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gscolr ( &icolrx, &ier );
    gssmth ( &ismtypx, &densx, &ier );
    gsfill ( &szfillx, &iftypx, &ier );
                                                                                    
    G_FREE ( plat, float );
    G_FREE ( plon, float );
}
