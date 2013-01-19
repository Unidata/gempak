#define PGLOBAL
#include "faxcmn.h"
#include "pattern.h"

void rinita ( char *pname, float *xsz, float *ysz, 
		int *ileft, int *ibot, int *iright, int *itop,
		int *numclr, int *iret )
/************************************************************************
 * rinita								*
 * 									*
 * This subroutine initializes the FAX device driver.			*
 * 									*
 * rinita ( pname, xsz, ysz, ileft, ibot, iright, itop, numclr, iret )	*
 *									*
 * Input parameters:							*
 *	*pname		char		Output file name		*
 *	*xsz		float		Xsize				*
 *	*ysz		float		Ysize				*
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
 * E. Wehner/EAi	 5/96	Adopted for FAX driver			*
 * E. Wehner/EAi	12/96	Eliminated unused parameters		*
 * E. Wehner/Eai	 3/97	Set map size based on rotation		*
 * S. Jacobs/NCEP	 6/97	Removed line width variables and rslwid	*
 * S. Jacobs/NCEP	 7/97	Rewrote and reorganized code		*
 * S. Jacobs/NCEP	 7/97	Cleaned up header and global variables	*
 * S. Jacobs/NCEP	 7/97	Added indent value from product table	*
 * S. Jacobs/NCEP	 7/97	Added check for 180 and 270 rotation	*
 * S. Jacobs/NCEP	 8/97	Added reserved value from prod table	*
 * G. Krueger/EAI	10/97	CST_xLST: Removed RSPTB; Add str limit	*
 * S. Jacobs/NCEP	 3/98	Added fill pattern include file		*
 * S. Jacobs/NCEP	 5/98	Changed to allow for multiple subsets	*
 * T. Piper/SAIC	02/04	Removed lenf parameter			*
 * T. Piper/SAIC	12/07	Removed txsizr; not used!		*
 ***********************************************************************/
{

	int	ier, num, ii;
	char	tmpfil[133], **aryptr, tsub[5];

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

	if  ( strchr ( pname, ';' ) != NULL )  {

/*
 *	    If the product/file name contains a semi-colon, then parse
 *	    the name for the "wheel" and "subset" values. The wheel and
 *	    subset are then used to get the product information from the
 *	    FAX product table.
 *
 *	    The parsing searches for either a semi-colon or a space to
 *	    terminate each string.
 */
	    aryptr = (char **) malloc ( 2 * sizeof(char *) );
	    for ( ii = 0; ii < 2; ii++ )  {
		aryptr[ii] = (char *) malloc ( 80 * sizeof(char) );
	    }

	    cst_clst ( pname, ';', " ", 2, 80, aryptr, &num, &ier );
	    strcpy ( wheel, aryptr[0] );
	    strcpy ( tsub,  aryptr[1] );

	    for ( ii = 0; ii < 2; ii++ )  {
		free ( aryptr[ii] );
	    }
	    free ( (char **) aryptr );

/*
 *	    Find the requested product in the FAX product
 *	    definition table.
 *
 *	    A subset of "0000", will return all subsets for a given
 *	    wheel number. It is assumed that all subsets have the
 *	    same dimensions and rotation.
 */
	    ctb_prod ( wheel, tsub, MAXSUB, &nsub, subset, descr, kbit,
		       klin, krot, kind, krsv, &ier );
	    if  ( ier != G_NORMAL )  {
		*iret = G_NOPROD;
		return;
	    }

	    faxflg = G_TRUE;

	}
	else {

/*
 *	    This is not a FAX product. However, still create a raster
 *	    image of the product.
 */

	    if  ( ERMISS ( *xsz ) || ERMISS ( *ysz ) ) {
		kbit[0] = 800;
		klin[0] = 800;
	    }
	    else {
		if  ( *ysz > *xsz )  {
		    kbit[0] = (int) *ysz;
		    klin[0] = (int) *xsz;
		    krot[0] = 90;
		}
		else {
		    kbit[0] = (int) *xsz;
		    klin[0] = (int) *ysz;
		    krot[0] = 0;
		}
	    }
	    nsub = 1;

	    faxflg = G_FALSE;

	}

/*
 *	Make sure that there are enough bytes per raster line. If the
 *	number of bits is not divisible by 8 then add enough bits to
 *	make the number divisible by 8.
 */
	if  ( kbit[0] % 8 != 0 )  {
	    kbit[0] = kbit[0] + (8 - kbit[0]%8);
	}

/*
 *	If this is a FAX product and the number of bits per line is
 *	greater than 1728, set the number to 1728.
 */
	if  ( kbit[0] > 1728 && faxflg )  {
	    kbit[0] = 1728;
	}

/*
 *	Compute the number of bytes for this raster image.
 *	If the size is larger than the maximum, return with an error.
 */
	msize = (kbit[0]/8) * klin[0];

	if  ( msize > MAXSIZ )  {
	    *iret = G_NIDSIZ;
	    return;
	}

/*
 *	Clear the entire image.
 */
	rclear ( &ier );

/*
 *	Set the device bounds.
 */
	if  ( ( krot[0] == 0 ) || ( krot[0] == 180 ) )  {
	    *ileft  = 1;
	    *ibot   = klin[0];
	    *iright = kbit[0];
	    *itop   = 1;
	}
	else if  ( ( krot[0] == 90 ) || ( krot[0] == 270 ) )  {
	    *ileft  = 1;
	    *ibot   = kbit[0];
	    *iright = klin[0];
	    *itop   = 1;
	}

/*
 *	Set txszx, txszy (hardware text sizes) here if hardware text is
 *	used.
 */
	txszx  = 14.0F;
	txszy  = 14.0F;
	irfont = 1;
	isfont = 0;
	kfntsz = 0;

/*
 *	Set file to initially closed.
 */
	opnfil = G_FALSE;

/*
 *      Save the file name.
 */
	if  ( faxflg )  {
	    sprintf ( tmpfil, "%s.ras", wheel );
	}
	else {
	    strcpy ( tmpfil, pname );
	}

/*
 *	If the new file name is not empty, set the current file name.
 */
	if  ( tmpfil[0] != CHNULL )  {
	    strcpy ( filnam, tmpfil );
	    *iret = G_NEWWIN;
	}

/*
 *	Set the number of colors to be returned to DEVCHR.
 *
 *	nncolr (numclr) = number of device colors
 *	( A maximum of MXCLNM = 32 may be initialized. )
 */
	nncolr  = 1;
	*numclr = nncolr;

/*
 *	Set up the on and off bit masks.
 *
 *	MSKON is a set of masks to turn a specific bit on.
 *	The masks set a particular bit for ORing with the specified
 *	byte.
 *
 *	MSKOFF is a set of masks to turn of specific bit off.
 *	The masks set all but a particular bit for ANDing with the
 *	specified byte.
 *
 *	MSKONR and MSKOFFR are the same as above, but they have the
 *	bits in the reverse order.
 */
	for ( ii = 0; ii < 8; ii++ )  {
	    mskon[ii]  = 1 << (7-ii);
	    mskoff[ii] = ~ mskon[ii];
	    mskonr[ii]  = 1 << ii;
	    mskoffr[ii] = ~ mskonr[ii];
	}

}
