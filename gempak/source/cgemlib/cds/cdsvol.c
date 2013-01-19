#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

void cds_vol ( VG_DBStruct *el, int indx, int *iret )
/************************************************************************
 * cds_vol								*
 *									*
 * This function displays the volcano element to the output device.	*
 *									*
 * cds_vol ( el, indx, iret )						*
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
 * D.W.Plummer/NCEP	 6/03	Copied from cdssym.c			*
 * T. Piper/SAIC        12/05   redone with new Setting_t structure     *
 ***********************************************************************/
{
int	np, width, ier, iw, jw, iwidth;
int     itxfn = 0, itxhw = 0, itxwid = 0, ibrdr = 0, irrotn = 0, ijust = 0;
int     icolrx, itxfnx, itxhwx, itxwidx, ibrdrx, irrotnx, ijustx;
int     iwxwidx;
float   size, sztext;
float   szwxx;
float   sztextx;
VolType	*pvol;

/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Save plot and text attributes.
     */
    gqcolr ( &icolrx, &ier );
    gqtext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
	     &ijustx, &ier );
    gqwthr ( &szwxx, &iwxwidx, &ier );

    pvol = &(el->elem.vol);

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
     * Set volcano symbol attributes.
     *
     * Check for a width value that indicates using the form-fitting
     * background blank out.
     */
    
    size  = ( G_DIFF(cdsUattr[indx].info.vol->size, 0.0F) ) ?
		pvol->info.size * cdsSymSiz :
		cdsUattr[indx].info.vol->size * cdsSymSiz;

    iwidth = ( cdsUattr[indx].info.vol->width == 0 ) ?
		pvol->info.width : cdsUattr[indx].info.vol->width;
    if  ( iwidth > 99 )  {
	jw = iwidth % 100;
	iw = iwidth / 100;
	width = ( iw * 100 ) + (int) ( (float)jw * cdsSymWdth );
    }
    else {
	width = (int) ( (float)iwidth * cdsSymWdth);
    }

    gswthr ( &size, &width, &ier);

    /*
     * Display the volcano symbol with application layer command.
     */
    if ( pvol->offset_xy[0] != 0 || pvol->offset_xy[1] != 0 ) {   
	sztext = 1.0F;
	gstext(&itxfn, &itxhw, &sztext, &itxwid, &ibrdr, 
					&irrotn, &ijust, &ier);
    }   
    
    np = 1;
    gwthr ( sys_M, &np,
            &(pvol->info.code),
            &(pvol->latlon[0]), &(pvol->latlon[1]),
            &(pvol->offset_xy[0]), &(pvol->offset_xy[1]),
            &ier, strlen(sys_M));


    /*
     *  Restore the saved plot and text attribute values
     */
    gswthr ( &szwxx, &iwxwidx, &ier );
    gstext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
             &ijustx, &ier );
    gscolr ( &icolrx, &ier );

}
