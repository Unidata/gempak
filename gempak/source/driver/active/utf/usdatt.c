#include "utfcmn.h"

void usdatt ( int *iunit, char *fname, int *lenf, int *itype, 
		int *ibase, float *xsz, float *ysz, int *ileft, 
		int *ibot, int *iright, int *itop, int *numclr, int *iret )
/************************************************************************
 * usdatt								*
 * 									*
 * This subroutine redefines the device attributes.			*
 * 									*
 * usdatt ( iunit, fname, lenf, itype, ibase, xsz, ysz, ileft, ibot,	*
 *	    iright, itop, numclr, iret )				*
 *									*
 * Input parameters:							*
 *	*iunit		int		Output type (Used for XW only)	*
 *	*fname		char		Name of file as output		*
 *	*lenf		int		Number of char in fname		*
 *	*itype		int		Output file type		*
 *					    2 = NAFOS format		*
 *					  <>2 = AFOS format		*
 *	*ibase		int		Base time for product		*
 *	*xsz		float		X size in pixels		*
 *	*ysz		float		Y size in pixels		*
 *									*
 * Output parameters:							*
 *	*ileft		int		Left device coordinate		*
 *	*ibot		int		Bottom device coordinate	*
 *	*iright		int		Right device coordinate		*
 *	*itop		int		Top device coordinate		*
 *	*numclr		int		Max number of colors for device	*
 * 	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Safford/GSC	11/96	Initial coding				*
 * S. Jacobs/NCEP	 8/97	Rewrote					*
 * S. Jacobs/NCEP	 9/97	Subtract century from kyy		*
 * S. Jacobs/NCEP	 9/97	Removed year-to-date minutes		*
 * S. Jacobs/NCEP	10/97	Replaced year-to-date minutes		*
 * S. Jacobs/NCEP	10/97	Added base time to calling sequence	*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * A. Hardy/NCEP	 6/03	Added tmzn to CSS_DATE			*
 * B. Yin/SAIC          03/04   changed css_date calling sequences      *
 ***********************************************************************/
{

	char	tmpfil[161], tmzn[4];
	int	ier, jtyp, pflag, jmin, itarr[5], jtarr[5];

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Set the temporary file name.
 */
	if  ( *lenf > 3 )  {
	    strcpy ( tmpfil, fname );
	    pflag = G_FALSE;
	}
	else {
	    cst_lcuc ( fname, pil, &ier );
	    sprintf ( tmpfil, "NMCGPH%s", pil );
	    pflag = G_TRUE;
	}

/*
 *	Compare the temporary and save file names. If they are the
 *	same, return. Otherwise, close the current file and reset
 *	the device attributes.
 */
	if  ( strcmp ( filnam, tmpfil ) != 0 )  {

	    uclosp ( &ier );

	    if  ( ! pflag )  {
		strcpy ( pil, " " );
		strcpy ( descr, " " );
		kxsize = 2048;
		kysize = 1536;
		kmap   = 1;
		ktime  = 0;
		kgscl  = 2000;
	    }
	    else {
		ctb_afos ( pil, descr, &kxsize, &kysize, &kmap,
			   &ktime, &kgscl, &ier );
		if  ( ier != G_NORMAL )  {
		    *iret = G_NOPROD;
		    return;
		}
	    }
	    strcpy ( filnam, tmpfil );

/*
 *	    Get date/time stamp information.
 */
	    jtyp = 1;
	    css_date ( &jtyp, &kyy, &kmm, &kdd, &khh, &knn, &kss, &kjd,
		       tmzn, &ier );

/*
 *	    Compute the number of minutes from the current time.
 */
	    minutes = kjd * 1440 + khh * 60;

/*
 *	    Add the number of hours for the forecast to the base time.
 *	    If the user has not supplied a base time, use the current
 *	    time.
 */
	    itarr[0] = kyy;
	    itarr[1] = kmm;
	    itarr[2] = kdd;
	    if  ( *ibase != IMISSD )  {
		itarr[3] = *ibase;
	    }
	    else {
		itarr[3] = khh;
	    }
	    itarr[4] = knn;

	    jmin = ktime * 60;
	    ti_addm ( itarr, &jmin, jtarr, &ier );

	    kyy = jtarr[0] - 1900;
	    kmm = jtarr[1];
	    kdd = jtarr[2];
	    khh = jtarr[3];
	    knn = jtarr[4];

/*
 *	    Set the device sizes.
 */
	    *xsz    = (float) kxsize;
	    *ysz    = (float) kysize;

	    *ileft  = 0;
	    *ibot   = 0;
	    *iright = kxsize - 1;
	    *itop   = kysize - 1; 

/*
 *	    Set the global output file type.
 */
	    kctype = *itype;
	    kunit  = 1;

/*
 *	    Set the new file to closed.
 */
	    opnfil = G_FALSE;

/*
 *	    Initialize the output buffer.
 */
	    uclear ( &ier );

	}

}
