#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

void cds_line ( VG_DBStruct *el, int indx, int *iret )
/************************************************************************
 * cds_line								*
 *									*
 * This function displays lines to the output device.			*
 *									*
 * cds_line ( el, indx, iret )						*
 *									*
 * Input parameters:							*
 * 	*el		VG_DBStruct	Pointer to VG record structure	*
 *	indx		int		Index into user attribute table	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	 3/97	Copied from cvg_frdsp			*
 * D. Keiser/GSC	 5/97	Renamed from cvg_lndsp			*
 * D.W.Plummer/NCEP	 7/97	Added cdsColor				*
 * E. Wehner/EAi	 7/97	Handle closed polygons			*
 * S. Jacobs/NCEP	 7/97	Handle filled polygons			*
 * D.W.Plummer/NCEP	 9/97	Changes for new vgstruct header file	*
 * D.W.Plummer/NCEP	10/97	Bug fix for numpts			*
 * S. Jacobs/NCEP	 2/98	Always draw the bounding line		*
 * C. Lin/EAI	         3/98	Add scaling factor to line width	*
 * S. Jacobs/NCEP	 3/98	Added call to GSFILL for fill pattern	*
 * S. Law/GSC		04/98	Added call to GSSMTH			*
 * S. Jacobs/NCEP	 4/98	Changed to smoothing type 2		*
 * S. Jacobs/NCEP	 4/98	Reset fill type to solid		*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include  *
 * F. J. Yen/NCEP	 5/98	Consolidated w/ cds_spln; Ignored stroke*
 * G. Krueger/EAI	 9/99	Preserve plot and text attributes	*	
 * F. J. Yen/NCEP	10/99	Handled user attribute table.		*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 * J. Wu/SAIC		10/01	use "splstr" value for kink lines	*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 * T. Piper/SAIC        12/05   redone with new Setting_t structure     *
 ***********************************************************************/
{
    int		ii, np, istroke, iwdth, iftyp, jftyp, istyp, ier; 
    int		ilintyp, ilthw, ilwhw, iclosed, ispltyp, ispldir, ifilled;
    int		ismtypx, icolrx, iltypx, ilthwx, iwidthx, iwhwx;
    int		isltypx, islstrx, isldirx, islwidx, iftypx;
    float	splsiz, szfil;
    float	densx, slsizx, szfilx;
    LineType	*plin;
    SpLineType  *pspl;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Save plot and text attributes.
     */
    gqsmth ( &ismtypx, &densx, &ier );
    gqcolr ( &icolrx, &ier );
    gqline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gqspln ( &isltypx, &islstrx, &isldirx, &slsizx, &islwidx, &ier );
    gqfill ( &szfilx, &iftypx, &ier );
   
    /*
     * Set the color for the feature.
     */
    if ( cdsColor == 0 )
	if ( cdsUattr[indx].maj_col == 0 )
	    gscolr ( &(el->hdr.maj_col), &ier);
	else
	    gscolr ( &cdsUattr[indx].maj_col, &ier);
    else
	gscolr ( &cdsColor, &ier);

    /*
     *  Set the smoothing level.
     */
    ii = ( cdsUattr[indx].smooth == -1 ) ?
	(int) el->hdr.smooth : cdsUattr[indx].smooth;
    istyp = (ii == 0) ? 0 : 2;
    gssmth (&istyp, &cdsSmthDens[ii], &ier);

    if (el->hdr.vg_type == LINE_ELM)
    {
	/*
	 * Set line attributes.
	 */
	plin = &(el->elem.lin);
	ilintyp = ( cdsUattr[indx].info.lin->lintyp == 0 ) ?
		plin->info.lintyp : cdsUattr[indx].info.lin->lintyp; 
	ilthw = ( cdsUattr[indx].info.lin->lthw == 0 ) ?
		plin->info.lthw : cdsUattr[indx].info.lin->lthw;
	iwdth = ( cdsUattr[indx].info.lin->width == 0 ) ?
		(int)((float)plin->info.width * cdsLineWdth) :
		(int)((float)cdsUattr[indx].info.lin->width * cdsLineWdth);
	ilwhw = ( cdsUattr[indx].info.lin->lwhw == 0 ) ?
		plin->info.lwhw : cdsUattr[indx].info.lin->lwhw;
	gsline( &ilintyp, &ilthw, &iwdth, &ilwhw, &ier);

	np = plin->info.numpts;

	if ( cdsUattr[indx].closed == 0 ) {
	    iclosed = (int)el->hdr.closed ;
	}
	else {
	    iclosed = cdsUattr[indx].closed;
	}
	if (iclosed != 0)
	{
	    for ( ii = np*2-1; ii >= np; ii-- ) 
		plin->latlon[ii+1] = plin->latlon[ii];
	    plin->latlon[np] = plin->latlon[0];
	    plin->latlon[2*np+1] = plin->latlon[np+1];
	    np = plin->info.numpts + 1;
	    plin->info.numpts++;
	}
    }
    else	/* Here for SPLN_ELM */
    {
        /*
         * Set line attributes.
         */
        pspl = &(el->elem.spl);
	ispltyp = ( cdsUattr[indx].info.spl->spltyp == 0 ) ?
			pspl->info.spltyp : cdsUattr[indx].info.spl->spltyp;
	
	if ( ispltyp == 24 || ispltyp == 25 ) { /* kink lines */
	    istroke = pspl->info.splstr;
	}
	else {
	    istroke = 0; 
	}
	ispldir = ( cdsUattr[indx].info.spl->spldir == 0 ) ?
			pspl->info.spldir : cdsUattr[indx].info.spl->spldir;
        splsiz  = ( G_DIFF(cdsUattr[indx].info.spl->splsiz, 0.0F) ) ?
		        pspl->info.splsiz * cdsSplSiz :
			cdsUattr[indx].info.spl->splsiz * cdsSplSiz;
        iwdth = ( cdsUattr[indx].info.spl->splwid == 0 ) ?
			(int)((float)pspl->info.splwid * cdsSplWdth) :
		(int)((float)cdsUattr[indx].info.spl->splwid * cdsSplWdth);
        gsspln(&ispltyp, &istroke, &ispldir, &splsiz, &iwdth, &ier);

        np = pspl->info.numpts;

	if ( cdsUattr[indx].closed == 0 ) {
	    iclosed = (int)el->hdr.closed ;
	}
	else {
	    iclosed = cdsUattr[indx].closed;
	}
	if (iclosed != 0)
        {
            for ( ii = np*2-1; ii >= np; ii-- )
                pspl->latlon[ii+1] = pspl->latlon[ii];
            pspl->latlon[np] = pspl->latlon[0];
            pspl->latlon[2*np+1] = pspl->latlon[np+1];
            np = pspl->info.numpts + 1;
            pspl->info.numpts++;
       }
    }

    /*
     * Display the line with application layer command.
     * If fill is set, fill the polygon.
     */

    ifilled = ( cdsUattr[indx].filled == 0 ) ?
		(int) el->hdr.filled : cdsUattr[indx].filled;
    if ( ifilled >= 1 && cdsFill == 1 )
    {
	if  ( ifilled == 1 )
	    iftyp = 2;
	else
	    iftyp = ifilled - 1;

	szfil = 1.0F;
	gsfill ( &szfil, &iftyp, &ier );

        if (el->hdr.vg_type == LINE_ELM) {
	    gfill(sys_M, &np, plin->latlon, &(plin->latlon[np]),
		&ier, strlen(sys_M));
	}
	else {
	    gfill(sys_M, &np, pspl->latlon, &(pspl->latlon[np]),
                &ier, strlen(sys_M));
	}

	jftyp = 1;
	gsfill ( &szfil, &jftyp, &ier );
    }

    /*
     * Always draw the line around the polygon.
     */
    if (el->hdr.vg_type == LINE_ELM) {
        gline(sys_M, &np, plin->latlon, &(plin->latlon[np]),
                &ier, strlen(sys_M));
    }
    else {
        gspln(sys_M, &np, pspl->latlon, &(pspl->latlon[np]),
                &ier, strlen(sys_M));
    }

    /*
     * Reset smoothing level to 1
     */
    if (istyp > 0) {
	istyp = 0;
	gssmth (&istyp, &cdsSmthDens[0], &ier);
    }

    /*
     *  Restore the saved plot and text attribute values
     */
    gsfill ( &szfilx, &iftypx, &ier );
    gsspln ( &isltypx, &islstrx, &isldirx, &slsizx, &islwidx, &ier );
    gsline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gscolr ( &icolrx, &ier );
    gssmth ( &ismtypx, &densx, &ier );

}
