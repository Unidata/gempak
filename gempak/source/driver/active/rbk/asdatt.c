#include "ardcmn.h"

void asdatt ( int *iunit, char *fname, int *lenf, int *itype, 
		int ibase[5], float *xsz, float *ysz, int *ileft, 
		int *ibot, int *iright, int *itop, int *numclr, int *iret )
/************************************************************************
 * asdatt								*
 * 									*
 * This subroutine redefines the device attributes.			*
 * 									*
 * asdatt ( iunit, fname, lenf, itype, ibase, xsz, ysz, ileft, ibot,	*
 *	    iright, itop, numclr, iret )				*
 *									*
 * Input parameters:							*
 *	*iunit		int		Output type (Used for XW only)	*
 *	*fname		char		Name of file as output		*
 *	*lenf		int		Number of char in fname		*
 *	*itype		int		Output file type		*
 *					    2 = AWIPS database format   *
 *					  <>2 = OSO format		*
 *	ibase[5]	int		Base time for product		*
 *					    YYYY, MM, DD, HH, NN	*
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
 * A. Hardy/GSC		 9/98	Copied from USDATT			*
 * A. Hardy/GSC		 9/98	Added default pil name			*
 * S. Jacobs/NCEP	 8/99	Fixed hours and minutes computation	*
 * A. Hardy/GSC		 3/00   Modified how itarr is calculated        *
 * A. Hardy/GSC		 9/00   Modified base time calculation		*
 * A. Hardy/SAIC         5/02   Check filename length & pil existance	*
 * A. Hardy/NCEP	 6/03	Added tmzn to CSS_DATE			*
 * B. Yin/SAIC          03/04   changed css_date calling sequences      *
 ***********************************************************************/
{

	char	tmpfil[161], tmzn[4];
	int	ier, jtyp, jmin, itarr[5], jtarr[5], ii;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Check filename length.  If it is alright, set the 
 *      temporary file name.
 */
	if  ( *lenf > 3 )  {
	    *iret = G_NFILENM;
	    return;
	}
	else {
	    cst_lcuc ( fname, pil, &ier );
	    sprintf ( tmpfil, "NMCGPH%s", pil );
	}

/*
 *	Compare the temporary and save file names. If they are the
 *	same, return. Otherwise, close the current file and reset
 *	the device attributes.
 */
	if  ( strcmp ( filnam, tmpfil ) != 0 )  {

	    aclosp ( &ier );

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
        
	    strcpy ( filnam, tmpfil);
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
 *	    Add the number of hours for the forecast to the base time.
 *	    If the user has not supplied a base time, use the current
 *	    time.
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
 *	    Compute the number of minutes from the current time.
 */

	    minutes = jjd * 1440 + jhh * 60;

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
	    aclear ( &ier );

	}

}
