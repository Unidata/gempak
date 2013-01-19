#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

void cds_wind ( VG_DBStruct *el, int indx, int *iret )
/************************************************************************
 * cds_wind								*
 *									*
 * This function displays wind symbols to the output device.		*
 *									*
 * cds_wind ( el, indx, iret )						*
 *									*
 * Input parameters:							*
 * 	*el		VG_DBStruct	Pointer to VG record structure	*
 * 	indx		int		Index into user attribute table	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	 3/97	Copied from cvg_sydsp			*
 * D. Keiser/GSC	 5/97	Renamed from cvg_wddsp			*
 * D.W.Plummer/NCEP	 9/97	Changes for new vgstruct header file	*
 * C. Lin/EAI	         3/98	Added scaling factor			*
 * I. Durham/GSC	 4/98	Added DARR_ELM and HASH_ELM		*
 * W. Li/EAI		 4/98	Add darr and hash			*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * S. Jacobs/NCEP	 2/99	Added check for form-fitting bg blank	*
 * S. Jacobs/NCEP	 3/99	Fixed bug in check for form-fitting bg	*
 * G. Krueger/EAI	 9/99	Preserve plot and text attributes	*
 * F. J. Yen/NCEP	10/99	Handled user attribute table		*
 * A. Hardy/GSC		10/00   Calling sequence changes gqhash, gshash *
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * A. Hardy/GSC         07/01   changed hdsize to 0.0; removed int	*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 * T. Piper/SAIC        12/05   redone with new Setting_t structure     *
 ***********************************************************************/
{
int	  np, width, iw, jw, ier, iwidth;
int	  icolrx, ibrbwidx, ibrbtypx, iarwwidx, iarwtypx, iarrwidx;
int 	  iarrtypx, ihshtypx, ihshwidx;
float     size, hdsize;
float	  szbrbx, szarwx, arwhdszx, szarrx, arrhdszx, szhshx;
WindType  *pwnd;
/*---------------------------------------------------------------------*/

    *iret = 0;
    pwnd = &(el->elem.wnd);

    /*
     *  Save plot attributes.
     */
    gqcolr ( &icolrx, &ier );
    gqbarb ( &szbrbx, &ibrbwidx, &ibrbtypx, &ier );
    gqarrw ( &szarwx, &arwhdszx, &iarwwidx, &iarwtypx, &ier );
    gqdarr ( &szarrx, &arrhdszx, &iarrwidx, &iarrtypx, &ier );
    gqhash ( &szhshx, &ihshwidx, &ihshtypx, &ier );

    /*
     * Set the color for the feature.
     */
    if ( cdsColor == 0 )
	if ( cdsUattr[indx].maj_col == 0 )
	    gscolr( &(el->hdr.maj_col), &ier);
	else
	    gscolr ( &cdsUattr[indx].maj_col, &ier);
    else
	gscolr( &cdsColor, &ier);

    /*
     * Set wind symbol attributes.
     *
     * Check for a value that indicates using the form-fitting
     * background blank out.
     */
    size  = ( G_DIFF(cdsUattr[indx].info.wnd->size, 0.0F) ) ?
		pwnd->info.size * cdsWindSiz :
		cdsUattr[indx].info.wnd->size * cdsWindSiz;

    iwidth = ( cdsUattr[indx].info.wnd->width == 0 ) ?
		pwnd->info.width :
		cdsUattr[indx].info.wnd->width;
    if  ( iwidth > 99 )  {
	jw = iwidth % 100;
	iw = iwidth / 100;
	width = ( iw * 100 ) + (int)((float)jw * cdsWindWdth);
    }
    else {
	if  ( ( pwnd->info.wndtyp % 10 ) == 4 )  {
	    width = 800 + (int)((float)iwidth * cdsWindWdth);
	}
	else {
	    width = (int)((float)iwidth * cdsWindWdth);
	}
    }

    switch ( el->hdr.vg_type ) {

	case BARB_ELM:
	    gsbarb ( &size, &width,
		     &(pwnd->info.wndtyp), &ier);

	    break;

	case ARROW_ELM:
	    hdsize = ( G_DIFF(cdsUattr[indx].info.wnd->hdsiz, 0.0F) ) ?
			pwnd->info.hdsiz * cdsWindHdsiz :
			cdsUattr[indx].info.wnd->hdsiz * cdsWindHdsiz;
	    gsarrw ( &size, &hdsize, &width,
		     &(pwnd->info.wndtyp), &ier);
	    break;
	case DARR_ELM:
	    hdsize = ( G_DIFF(cdsUattr[indx].info.wnd->hdsiz, 0.0F) ) ?
			pwnd->info.hdsiz * cdsWindHdsiz :
			cdsUattr[indx].info.wnd->hdsiz * cdsWindHdsiz;
	    gsdarr( &size, &hdsize, &width, 
		     &(pwnd->info.wndtyp), &ier);

	    break;
	case HASH_ELM:
	    gshash ( &size, &width, &(pwnd->info.wndtyp), &ier );

	    break;

	default:
	    break;
    }

    /* 
     * Display the symbol with application layer command.
     */

    np = pwnd->info.numwnd;
    switch ( el->hdr.vg_type ) {

	case BARB_ELM:
	    gbarb ( sys_M, &np,
		    pwnd->data.latlon, &(pwnd->data.latlon[np]),
		    pwnd->data.spddir, &(pwnd->data.spddir[np]),
		    &ier, strlen(sys_M));
	    break;

	case ARROW_ELM:
	    garrw ( sys_M, &np,
		    pwnd->data.latlon, &(pwnd->data.latlon[np]),
		    pwnd->data.spddir, &(pwnd->data.spddir[np]),
		    &ier, strlen(sys_M));

	    break;

	case DARR_ELM:
	    gdarr ( sys_M, &np,
		    pwnd->data.latlon, &(pwnd->data.latlon[np]),
		    &(pwnd->data.spddir[np]), &ier, strlen(sys_M));

	    break;
	case HASH_ELM:
	    ghash ( sys_M, &np,
		    pwnd->data.latlon, &(pwnd->data.latlon[np]),
		    &(pwnd->data.spddir[np]), &ier, strlen(sys_M));

	    break;

	default:
	    break;

    }

    /*
     *  Restore the saved plot attribute values
     */
    gshash ( &szhshx, &ihshwidx, &ihshtypx, &ier );
    gsdarr ( &szarrx, &arwhdszx, &iarrwidx, &iarrtypx, &ier );
    gsarrw ( &szarwx, &arrhdszx, &iarwwidx, &iarwtypx, &ier );
    gsbarb ( &szbrbx, &ibrbwidx, &ibrbtypx, &ier );
    gscolr ( &icolrx, &ier );
}
