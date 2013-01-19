#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

void cds_circ ( VG_DBStruct *el, int indx, int *iret )
/************************************************************************
 * cds_circ								*
 *									*
 * This function displays circles to the output device.			*
 *									*
 * cds_circ ( el, indx, iret )						*
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
 * A. Hardy/GSC		12/98	Copied from cds_line			*
 * G. Krueger/EAI	 5/99	Modified circles for latlon array	*
 * G. Krueger/EAI	 9/99	Preserve plot and text attributes	* 
 * F. J. Yen/NCEP	10/99	Handled user attribute table 		*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 * T. Piper/SAIC	12/05   redone with new Setting_t structure     *
 ***********************************************************************/
{
    int		ii, iwdth, ilintyp, ilthw, ilwhw, istyp, np, ier; 
    int		ismtypx, icolrx, iltypx, ilthwx, iwidthx, iwhwx;
    float	densx;
    CircType	*pcirc;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Save plot and text attributes.
     */
    gqsmth ( &ismtypx, &densx, &ier );
    gqcolr ( &icolrx, &ier );
    gqline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );

    pcirc = &(el->elem.cir);
   
    /*
     * Set the color for the feature.
     */
    if ( cdsColor == 0 )
	if ( cdsUattr[indx].maj_col == 0 )
	    gscolr( &(el->hdr.maj_col), &ier);
	else
	    gscolr (&cdsUattr[indx].maj_col, &ier);
    else
	    gscolr( &cdsColor, &ier);

    /*
     * Set the smoothing level.
     */
    ii = (int) el->hdr.smooth;
    istyp = (ii == 0) ? 0 : 2;
    gssmth (&istyp, &cdsSmthDens[ii], &ier);

    /*
     * Set circle attributes.
     */

    pcirc = &(el->elem.cir);
    ilintyp = ( cdsUattr[indx].info.cir->lintyp == 0 ) ?
		pcirc->info.lintyp : cdsUattr[indx].info.cir->lintyp;
    ilthw = ( cdsUattr[indx].info.cir->lthw == 0 ) ?
		pcirc->info.lthw : cdsUattr[indx].info.cir->lthw;
    iwdth = ( cdsUattr[indx].info.cir->width == 0 ) ?
		(int)((float)pcirc->info.width * cdsLineWdth) :
		(int)((float)cdsUattr[indx].info.cir->width * cdsLineWdth);
    ilwhw = ( cdsUattr[indx].info.cir->lwhw ==0 ) ?
		pcirc->info.lwhw : cdsUattr[indx].info.cir->lwhw;
    gsline( &ilintyp, &ilthw, &iwdth, &ilwhw, &ier);

    /*
     * Display the circle with application layer command.
     */

     np = 40;

     gcircl (sys_M, &(pcirc->data.latlon[0]), &(pcirc->data.latlon[2]),
	     &(pcirc->data.latlon[1]), &(pcirc->data.latlon[3]), 
	     &np, &ier, strlen(sys_M) );

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
    gsline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gscolr ( &icolrx, &ier );
    gssmth ( &ismtypx, &densx, &ier );
}
