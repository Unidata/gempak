#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

void cds_frnt ( VG_DBStruct *el, int indx, int *iret )
/************************************************************************
 * cds_frnt								*
 *									*
 * This function displays fronts to the output device.			*
 *									*
 * cds_frnt ( el, indx, iret )						*
 *									*
 * Input parameters:							*
 * 	*el		VG_DBStruct	Pointer to VG record structure	*
 *	indx		int		Index into user attribute table *
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	10/96	Created					*
 * D. Keiser/GSC	 1/97	Clean up				*
 * D. Keiser/GSC	 5/97	Renamed from cvg_frdsp			*
 * D.W.Plummer/NCEP	 7/97	Added cdsColor				*
 * D.W.Plummer/NCEP	 9/97	Changes for new vgstruct header file	*
 * W. Li/EAI		11/97	Removed argument in GSFRNT call		*
 * C. Lin/EAI		03/97	Added scaling factors			*
 * S. Law/GSC		04/98	Added call to GSSMTH			*
 * S. Jacobs/NCEP	 4/98	Changed gscolr to gsclr2		*
 * S. Jacobs/NCEP	 4/98	Changed to smoothing type 2		*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * F. J. Yen		 5/98	Set pip stroke to 0 (for no change)	*
 * S. Jacobs/NCEP	 6/98	Changed pip size to type float		*
 * G. Krueger/EAI	 9/99	Preserve plot and text attributes	*
 * F. J. Yen/NCEP	10/99	Handled user attribute table		*
 * S. Jacobs/NCEP	 3/00	Changed >10 to >9 for strength scaling	*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 * T. Piper/SAIC        12/05   redone with new Setting_t structure     *
 ***********************************************************************/
{
    int		ii, ipstroke, istrngth, vgstrngth, icode, ityp, istyp;
    int		imaj_col, imin_col, ifpipsz, ifpipdr;
    int		idigt3, ier; 
    int		ismtypx, icolr1x, icolr2x, icolrx, ifcodx, ipipstx,
		ipipdrx;
    float	psiz;
    float	densx, pipszx;
    FrontType	*pfrt;

/*---------------------------------------------------------------------*/
    *iret = 0;

    /*
     *  Save plot and text attributes.
     */
    gqsmth ( &ismtypx, &densx, &ier );
    gqclr2 ( &icolr1x, &icolr2x, &ier );
    gqcolr ( &icolrx, &ier );
    gqfrnt ( &ifcodx, &pipszx, &ipipstx, &ipipdrx, &ier );

    pfrt = &(el->elem.frt);

    /*
     * Set the color for the feature.
     */
    imaj_col = ( cdsUattr[indx].maj_col == 0 ) ?
		el->hdr.maj_col : cdsUattr[indx].maj_col;
    imin_col = ( cdsUattr[indx].min_col == 0 ) ?
		el->hdr.min_col : cdsUattr[indx].min_col;
    if ( cdsColor == 0 )  
	gsclr2( &imaj_col, &imin_col, &ier);
    else
	gscolr( &cdsColor, &ier);
    /*
     *  Set the smoothing level.
     */
    ii = ( cdsUattr[indx].smooth == -1 ) ?
	 (int) el->hdr.smooth : cdsUattr[indx].smooth;
    istyp = (ii == 0) ? 0 : 2;
    gssmth (&istyp, &cdsSmthDens[ii], &ier);

    /*
     * Applying pip size scaling factor if it is 
     * not a dry line
     */
    ityp     = (pfrt->info.fcode)/100; 
    vgstrngth = (pfrt->info.fcode - ityp*100)/10;
    if ( cdsUattr[indx].info.frt->fwidth <= 0 ) {
	istrngth = vgstrngth;
    }
    else {
	istrngth = cdsUattr[indx].info.frt->fwidth;
    }
    idigt3   = (pfrt->info.fcode - ityp*100 - vgstrngth*10);

    ifpipsz = ( cdsUattr[indx].info.frt->fpipsz == 0 ) ?
		pfrt->info.fpipsz : cdsUattr[indx].info.frt->fpipsz;
    if (ityp != 8 ) { 
        psiz = ( (float)ifpipsz / 100.0F ) * cdsPipSiz;
    }
    else {
        psiz = (float)ifpipsz / 100.0F;
    }

    istrngth = (int)((float)istrngth * cdsFrntStrngth);

    if ( istrngth > 9 )
	istrngth = 9;

    icode = ityp*100 + istrngth*10 + idigt3;
    ipstroke = 0;

    ifpipdr = ( cdsUattr[indx].info.frt->fpipdr == 0 ) ?
		pfrt->info.fpipdr : cdsUattr[indx].info.frt->fpipdr;

    gsfrnt(&icode, &psiz, &ipstroke, &ifpipdr, &ier);

    /*
     * Display the front with application layer command.
     */
    gfrnt(sys_M, &(pfrt->info.numpts), 
		pfrt->latlon, &(pfrt->latlon[pfrt->info.numpts]),
		&ier, strlen(sys_M));

    /*
     * Reset smooth level to 0
     */
    if (istyp > 0) {
	istyp = 0;
	gssmth (&istyp, &cdsSmthDens[0], &ier);
    }

    /*
     *  Restore the saved plot and text attribute values
     */
    gsfrnt ( &ifcodx, &pipszx, &ipipstx, &ipipdrx, &ier );
    gscolr ( &icolrx, &ier );
    gsclr2 ( &icolr1x, &icolr2x, &ier );
    gssmth ( &ismtypx, &densx, &ier );
}
