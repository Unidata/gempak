#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

void cds_symb ( VG_DBStruct *el, int indx, int *iret )
/************************************************************************
 * cds_symb								*
 *									*
 * This function displays symbols to the output device.			*
 *									*
 * cds_symb ( el, indx, iret )						*
 *									*
 * Input parameters:							*
 * 	*el		VG_DBStruct	Pointer to VG record structure	*
 *	indx		int		Index into user setting table	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/97	Copied from cvg_frdsp			*
 * D. Keiser/GSC	 5/97	Renamed from cvg_sydsp			*
 * D.W.Plummer/NCEP	 9/97	Changes for new vgstruct header file	*
 * C. Lin/EAI	         3/98	Add scaling factors			*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * S. Jacobs/NCEP	 6/98	Changed marker code to use array element*
 * C. Lin/EAI	         6/98	add set txtsiz=1.0 for offset 		*
 * A. Hardy/GSC         10/98   Added CMBSY_ELM                         *
 * S. Jacobs/NCEP	 3/99	Added check for clear under symbols	*
 * G. Krueger/EAI	 9/99	Preserve plot and text attributes	*
 * F. J. Yen/NCEP	10/99	Handled user attribute table		*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * A. Hardy/GSC          7/01   changed size check to 0.0; removed int  *
 * E. Safford/SAIC	04/02	initialize all inputs to gstext()	*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 * T. Piper/SAIC        12/05   redone with new Setting_t structure     *
 ***********************************************************************/
{
int	np, mkcod, width, ier, iw, jw, iwidth;
int     itxfn = 0, itxhw = 0, itxwid = 0, ibrdr = 0, irrotn = 0, ijust = 0;
int     icolrx, itxfnx, itxhwx, itxwidx, ibrdrx, irrotnx, ijustx;
int     iwxwidx, ictwidx, iicwidx, iptwidx, ipwwidx, isktpx, iskwidx;
int     ispwidx, itbwidx, imarkx, imkhwx, imkwidx, icmwidx;
float   size, sztext;
float   szwxx, szctx, szicx, szptx, szpwx, szskx, szspx, sztbx, szmarkx;
float   szcmx, sztextx;
SymType	*psym;

/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Save plot and text attributes.
     */
    gqcolr ( &icolrx, &ier );
    gqtext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
	     &ijustx, &ier );
    gqwthr ( &szwxx, &iwxwidx, &ier );
    gqctyp ( &szctx, &ictwidx, &ier );
    gqicng ( &szicx, &iicwidx, &ier );
    gqptnd ( &szptx, &iptwidx, &ier );
    gqpwth ( &szpwx, &ipwwidx, &ier );
    gqsky  ( &szskx, &isktpx, &iskwidx, &ier );
    gqspcl ( &szspx, &ispwidx, &ier );
    gqturb ( &sztbx, &itbwidx, &ier );
    gqmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &ier );
    gqcmbo ( &szcmx, &icmwidx, &ier );

    psym = &(el->elem.sym);

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
     * Set symbol attributes.
     *
     * Check for a width value that indicates using the form-fitting
     * background blank out.
     */
    
    size  = ( G_DIFF(cdsUattr[indx].info.sym->info.size, 0.0F) ) ?
		psym->info.size * cdsSymSiz :
		cdsUattr[indx].info.sym->info.size * cdsSymSiz;

    iwidth = ( cdsUattr[indx].info.sym->info.width == 0 ) ?
		psym->info.width : cdsUattr[indx].info.sym->info.width;
    if  ( iwidth > 99 )  {
	jw = iwidth % 100;
	iw = iwidth / 100;
	width = ( iw * 100 ) + (int) ( (float)jw * cdsSymWdth );
    }
    else {
	width = (int) ( (float)iwidth * cdsSymWdth);
    }

    switch ( el->hdr.vg_type ) {

	case WXSYM_ELM:
	    gswthr ( &size, &width, &ier);
	    break;

	case CTSYM_ELM:
	    gsctyp ( &size, &width, &ier);
	    break;
	    
	case ICSYM_ELM:
	    gsicng ( &size, &width, &ier);
	    break;

	case PTSYM_ELM:
	    gsptnd ( &size, &width, &ier);
	    break;

	case PWSYM_ELM:
	    gspwth ( &size, &width, &ier);
	    break;

	case SKSYM_ELM:
	    gssky  ( &size, &(psym->info.ityp),
		     &width, &ier);
	    break;

	case SPSYM_ELM:
	    gsspcl ( &size, &width, &ier);
	    break;

	case TBSYM_ELM:
	    gsturb ( &size, &width, &ier);
	    break;

	case MARK_ELM:
	    mkcod = (int) psym->data.code[0];
	    gsmrkr ( &mkcod,
		     &(psym->info.ityp),
		     &size, &width, &ier);
	    break;

	case CMBSY_ELM:
	    gscmbo ( &size, &width, &ier);
	    break;

	default:
	    break;
    }

    /*
     * Display the symbol with application layer command.
     */
    if ( psym->data.offset_xy[0] != 0 || psym->data.offset_xy[1] != 0 ) {   
	sztext = 1.0F;
	gstext(&itxfn, &itxhw, &sztext, &itxwid, &ibrdr, 
					&irrotn, &ijust, &ier);
    }   
    
    np = psym->info.numsym;
    switch ( el->hdr.vg_type ) {

	case WXSYM_ELM:
	    gwthr ( sys_M, &np,
		    psym->data.code,
		    psym->data.latlon, &(psym->data.latlon[np]),
		    psym->data.offset_xy, &(psym->data.offset_xy[np]),
		    &ier, strlen(sys_M));
	    break;

	case CTSYM_ELM:
	    gctyp ( sys_M, &np,
		    psym->data.code,
		    psym->data.latlon, &(psym->data.latlon[np]),
		    psym->data.offset_xy, &(psym->data.offset_xy[np]),
		    &ier, strlen(sys_M));
	    break;

	case ICSYM_ELM:
	    gicng ( sys_M, &np,
		    psym->data.code,
		    psym->data.latlon, &(psym->data.latlon[np]),
		    psym->data.offset_xy, &(psym->data.offset_xy[np]),
		    &ier, strlen(sys_M));
	    break;

	case PTSYM_ELM:
	    gptnd ( sys_M, &np,
		    psym->data.code,
		    psym->data.latlon, &(psym->data.latlon[np]),
		    psym->data.offset_xy, &(psym->data.offset_xy[np]),
		    &ier, strlen(sys_M));
	    break;

	case PWSYM_ELM:
	    gpwth ( sys_M, &np,
		    psym->data.code,
		    psym->data.latlon, &(psym->data.latlon[np]),
		    psym->data.offset_xy, &(psym->data.offset_xy[np]),
		    &ier, strlen(sys_M));
	    break;

	case SKSYM_ELM:
	    gsky  ( sys_M, &np,
		    psym->data.code,
		    psym->data.latlon, &(psym->data.latlon[np]),
		    psym->data.offset_xy, &(psym->data.offset_xy[np]),
		    &ier, strlen(sys_M));
	    break;

	case SPSYM_ELM:
	    gspcl ( sys_M, &np,
		    psym->data.code,
		    psym->data.latlon, &(psym->data.latlon[np]),
		    psym->data.offset_xy, &(psym->data.offset_xy[np]),
		    &ier, strlen(sys_M));
	    break;

	case TBSYM_ELM:
	    gturb ( sys_M, &np,
		    psym->data.code,
		    psym->data.latlon, &(psym->data.latlon[np]),
		    psym->data.offset_xy, &(psym->data.offset_xy[np]),
		    &ier, strlen(sys_M));
	    break;

	case MARK_ELM:
	    gmark ( sys_M, &np,
		    psym->data.latlon, &(psym->data.latlon[np]),
		    &ier, strlen(sys_M));
	    break;

	case CMBSY_ELM:
	    gcmbo ( sys_M, &np,
		    psym->data.code,
		    psym->data.latlon, &(psym->data.latlon[np]),
		    psym->data.offset_xy, &(psym->data.offset_xy[np]),
		    &ier, strlen(sys_M));
	    break;

	default:
	    break;

    }

    /*
     *  Restore the saved plot and text attribute values
     */
    gscmbo ( &szcmx, &icmwidx, &ier );
    gsmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &ier );
    gsturb ( &sztbx, &itbwidx, &ier );
    gsspcl ( &szspx, &ispwidx, &ier );
    gssky  ( &szskx, &isktpx, &iskwidx, &ier );
    gspwth ( &szpwx, &ipwwidx, &ier );
    gsptnd ( &szptx, &iptwidx, &ier );
    gsicng ( &szicx, &iicwidx, &ier );
    gsctyp ( &szctx, &ictwidx, &ier );
    gswthr ( &szwxx, &iwxwidx, &ier );
    gstext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
             &ijustx, &ier );
    gscolr ( &icolrx, &ier );

}
