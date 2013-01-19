#define UGLOBAL

#include "ardcmn.h"

void ainita ( int *iunit, char *fname, int *lenf, int *itype, 
		int ibase[5], float *xsz, float *ysz, int *ileft, 
		int *ibot, int *iright, int *itop, int *numclr, int *iret )
/************************************************************************
 * ainita								*
 *									*
 * This subroutine is called to initialize a new device driver.		*
 *									*
 *									*
 * ainita ( iunit, fname, lenf, itype, ibase, xsz, ysz, ileft, ibot,	*
 *	    iright, itop, numclr, iret )				*
 *									*
 * Input parameters:							*
 *	*iunit		int		Output type (Used for XW only)	*
 *	*fname		char		Output file name		*
 *	*lenf		int		File name length		*
 *	*itype		int		Output file type		*
 *					    2 = NAFOS format		*
 *					  <>2 = AFOS format		*
 *	ibase[5]	int		Base time for product as 	*
 *					    YYYY, MM, DD, HH, NN	*	
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
 * A. Hardy/GSC		 9/98	Copied from UINITA			*
 * A. Hardy/GSC		 9/98	Added default pil name			*
 * S. Jacobs/NCEP	 8/99	Added init of med range group info	*
 * A. Hardy/GSC		 2/00   Corrected issue time calc. for jhh,jmm  * 
 * A. Hardy/GSC		 3/00   Modified how itarr is calculated        *
 * A. Hardy/GSC          9/00   Modified base time calculation		*
 * A. Hardy/SAIC         5/02   Check filename length & pil existance	* 
 * A. Hardy/NCEP	 6/03	Added tmzn to CSS_DATE			*
 * B. Yin/SAIC          03/04   changed css_date calling sequences      *
 ***********************************************************************/
{

	int	ier, jtyp, jmin, itarr[5], jtarr[5], ii;
	char    tmzn[4];

/*---------------------------------------------------------------------*/

        *iret = G_NORMAL;   

/*
 *      Check file name length.
 */

        if  ( *lenf > 3 )  {
	    *iret = G_NFILENM;
	    return;
	}
        else {
	    cst_lcuc ( fname, pil, &ier );
	    sprintf ( filnam, "NMCGPH%s", pil );
	}

/*
 *	Get the AWIPS product information.
 */
	ctb_awdef ( pil, awpid, kloc, flcls, prodid, extnd, wmo, 
	            bckgnd, &rettim, &kmap, &crdflg, &sclint, 
		    &sclfac, &areacd, &label, &cordfg, &nrefpt, 
		    &ktime, &pick, &ier );
        
	if  ( ier != G_NORMAL )  {
	    if ( ier == -1 ) {
	        *iret = G_NOTBL;
	    }
	    else {
	        *iret = G_NOPROD;
	    }
	    return;
        }
	else {
	    sprintf ( filnam, "NMCGPH%s", pil );
        }

	ctb_awmap ( pick, &kxsize, &kysize, ilat, ilon, iangles, &ier );

	if  ( ier != G_NORMAL )  {
	    *iret = G_NOPROD;
	     return;
	}

/*
 *      Get date/time stamp information. K variables are for the
 *      valid time and J variables are for the creation time.
 */ 

	jtyp = 1;
  	css_date ( &jtyp, &jyy, &jmm, &jdd, &jhh, &jnn, &jss, &jjd,
		   tmzn, &ier );
/*
 *	Add the number of hours for the forecast to the base time. If
 *	the user has not supplied a base time, use the current time.
 */

	if ( ibase[0] == IMISSD) {
	    itarr[0] = jyy;
	    itarr[1] = jmm;
	    itarr[2] = jdd;
	    itarr[3] = jhh;
	    itarr[4] = jnn;
	}
	else {
	    for ( ii = 0; ii < 5; ii++ ) {
		itarr[ii] = ibase [ii];
	    }
	    jyy = itarr[0];
	    jmm = itarr[1];
	    jdd = itarr[2];
	    jhh = itarr[3];
	    jnn = itarr[4];
	}

	jmin = ktime * 60;
	ti_addm ( itarr, &jmin, jtarr, &ier );

	kyy = jtarr[0];
	kmm = jtarr[1];
	kdd = jtarr[2];
	khh = jtarr[3];
	knn = jtarr[4];

/*
 *	Compute the number of minutes from the current time.
 */

	minutes = jjd * 1440 + jhh * 60;

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
 *	Since the AWIPS system is monochrome, set the number of
 *	colors to 1.
 */
	*numclr = 1;

/*
 *      Set text attributes.
 */
        txtchr = 128;

/*
 *	Set file to initially closed.
 */
  	opnfil = G_FALSE;

/*
 *	Initialize the output buffer.
 */
	aclear ( &ier );

/*
 *	Initialize the blank line flag. This flags pen up and pen down
 *	lines.
 */
	blank = G_FALSE;

/*
 *      Initialize the group type and group number.
 */
	kgtyp = 0;
	kgnum = 0;

/*
 *      Initialize the Medium Range product information.
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
