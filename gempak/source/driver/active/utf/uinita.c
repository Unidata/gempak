#define UGLOBAL

#include "utfcmn.h"

void uinita ( int *iunit, char *fname, int *lenf, int *itype, 
		int *ibase, float *xsz, float *ysz, int *ileft, 
		int *ibot, int *iright, int *itop, int *numclr, int *iret )
/************************************************************************
 * uinita								*
 *									*
 * This subroutine is called to initialize a new device driver.		*
 *									*
 * For the UTF driver, ITYPE is used to control the type of output	*
 * file. A value of 2 generates a file suitable for display by NAFOS.	*
 * Any other value generates a file suitable for processing by the	*
 * data flow system at NCEP and OSO.					*
 *									*
 * uinita ( iunit, fname, lenf, itype, ibase, xsz, ysz, ileft, ibot,	*
 *	    iright, itop, numclr, iret )				*
 *									*
 * Input parameters:							*
 *	*iunit		int		Output type (Used for XW only)	*
 *	*fname		char		Output file name		*
 *	*lenf		int		File name length		*
 *	*itype		int		Output file type		*
 *					    2 = NAFOS format		*
 *					  <>2 = AFOS format		*
 *	*ibase		int		Base time for product		*
 *									*
 * Output parameters:							*
 *	*xsz		float		Xsize				*
 *	*ysz		float		Ysize				*
 *	*ileft		int		Left device coordinate		*
 *	*ibot		int		Bottom device coordinate	*
 *	*iright		int		Right device coordinate		*
 *	*itop		int		Top device coordinate		*
 *	*numclr		int		Max number of colors for device	*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Safford/GSC	11/96	Adapted for use with UTF driver         *
 * S. Jacobs/NCEP	 7/97	Modified def text sizes txszx and txszy	*
 * S. Jacobs/NCEP	 8/97	Rewrote					*
 * S. Jacobs/NCEP	 9/97	Subtract the century from kyy		*
 * S. Jacobs/NCEP	 9/97	Removed year-to-date minutes		*
 * S. Jacobs/NCEP	10/97	Replaced year-to-date minutes		*
 * S. Jacobs/NCEP	10/97	Added base time to calling sequence	*
 * S. Jacobs/NCEP	 2/98	Changed calc of base time to use mins	*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * S. Jacobs/NCEP	 7/98	Changed values of txszx and txszy	*
 * S. Jacobs/NCEP	 3/99	Added init of med range group info	*
 * A. Hardy/NCEP	 6/03	Added tmzn to CSS_DATE			*
 * B. Yin/SAIC          03/04   changed css_date calling sequences      *
 ***********************************************************************/
{

	int	ier, jtyp, jmin, itarr[5], jtarr[5];
	char	tmzn[4];

/*---------------------------------------------------------------------*/

        *iret = G_NORMAL;   

/*
 *	Get the AFOS product information.
 */
	if  ( *lenf > 3 )  {
	    strcpy ( pil, " " );
	    strcpy ( descr, " " );
	    kxsize = 2048;
	    kysize = 1536;
	    kmap   = 1;
	    ktime  = 0;
	    kgscl  = 2000;
	    strcpy ( filnam, fname );
	}
	else {
	    cst_lcuc ( fname, pil, &ier );
	    ctb_afos ( pil, descr, &kxsize, &kysize, &kmap,
		       &ktime, &kgscl, &ier );
	    if  ( ier != G_NORMAL )  {
		*iret = G_NOPROD;
		return;
	    }
	    sprintf ( filnam, "NMCGPH%s", pil );
	}

/*
 *      Get date/time stamp information.
 */ 
	jtyp = 1;
  	css_date ( &jtyp, &kyy, &kmm, &kdd, &khh, &knn, &kss, &kjd,
		   tmzn, &ier );

/*
 *	Compute the number of minutes from the current time.
 */
	minutes = kjd * 1440 + khh * 60;

/*
 *	Add the number of hours for the forecast to the base time. If
 *	the user has not supplied a base time, use the current time.
 */
	itarr[0] = kyy;
	itarr[1] = kmm;
	itarr[2] = kdd;
	if  ( *ibase != IMISSD )  {
	    itarr[3] = *ibase / 100;
	    itarr[4] = *ibase % 100;
	}
	else {
	    itarr[3] = khh;
	    itarr[4] = knn;
	}

	jmin = ktime * 60;
	ti_addm ( itarr, &jmin, jtarr, &ier );

	kyy = jtarr[0] - 1900;
	kmm = jtarr[1];
	kdd = jtarr[2];
	khh = jtarr[3];
	knn = jtarr[4];

/*
 *	Set the device sizes.
 */
	*xsz    = (float) kxsize;
	*ysz    = (float) kysize;

	*ileft  = 0;
	*ibot   = 0;
	*iright = kxsize - 1;
	*itop   = kysize - 1; 
	
/*
 *      Set the global output file type.
 */
  	kctype = *itype;
	kunit  = 1;
   
/*
 *	Set txszx, txszy (hardware text sizes) here if hardware
 *	text is used.
 */
	txszx  = 17.5F;
	txszy  = 22.5F;
	nfntsz = 0;
	asize  = 1.0F;

/*
 *	Since the AFOS system is monochrome, set the number of
 *	colors to 1.
 */
	*numclr = 1;
	
/*
 *	Set file to initially closed.
 */
  	opnfil = G_FALSE;

/*
 *	Initialize the output buffer.
 */
	uclear ( &ier );

/*
 *	Initialize the blank line flag. This flags pen up and pen down
 *	lines.
 */
	blank = G_FALSE;

/*
 *	Initialize the group type and group number.
 */
	kgtyp = 0;
	kgnum = 0;

/*
 *	Initialize the Medium Range product information.
 */
	nchr = 0;

	mlen[0] = 0;
	mlen[1] = 0;
	mlen[2] = 0;
	mlen[3] = 0;

	mrchr[0][0] = CHNULL;
	mrchr[1][0] = CHNULL;
	mrchr[2][0] = CHNULL;
	mrchr[3][0] = CHNULL;

}
