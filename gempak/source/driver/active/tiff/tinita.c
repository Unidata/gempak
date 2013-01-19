#define TGLOBAL
#include "tiffcmn.h"
#include "pattern.h"

void tinita ( char *pname, int *itype, float *xsz, 
		float *ysz, int *ileft, int *ibot, int *iright, 
		int *itop, int *numclr, int *iret )
/************************************************************************
 * tinita								*
 * 									*
 * This subroutine initializes the TIFF device driver.			*
 * 									*
 * tinita ( pname, itype, xsz, ysz, ileft, ibot, iright, itop,		*
 *	    numclr, iret )						*
 *									*
 * Input parameters:							*
 *	*pname		char		Output file name		*
 *	*itype		int		Pixmap type			*
 *					  0 = Uncompressed		*
 *					  1 = Compressed		*
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
 * S. Jacobs/NCEP	12/98						*
 * S. Jacobs/NCEP	 9/00	Added itype to the calling sequence	*
 * R. Tian/SAIC		05/02	Modified to display FAX as image	*
 * T. Piper/SAIC	02/04	Removed lenf parameter			*
 ***********************************************************************/
{

	int	ier, i;
	char	tmpfil[133], pnuc[133], descr[41];

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Set the global pixmap type
 */
 	ktype = *itype;

	cst_lcuc ( pname, pnuc, &ier );
	if ( strncmp(pnuc, "FAX", 3) == 0 ) {
/*
 *	    There is no entry for FAX product in the TIFF product
 *	    definition table. The size are passed in.
 */ 
	    kbit = (int)(*xsz);
	    klin = (int)(*ysz);
	    krot = 0; 
	} else {
/*
 *	    Find the requested product in the TIFF product
 *	    definition table.
 */
	    ctb_tiff ( pnuc, descr, &kbit, &klin, &krot, &ier );
	}
	if  ( ier != G_NORMAL )  {
	    *iret = G_NOPROD;
	    return;
	}

/*
 *	Make sure that there are enough bytes per raster line. If the
 *	number of bits is not divisible by 8 then add enough bits to
 *	make the number divisible by 8.
 */
	if  ( kbit % 8 != 0 )  {
	    kbit = kbit + (8 - kbit%8);
	}

/*
 *	Compute the number of bytes for this raster image.
 *	If the size is larger than the maximum, return with an error.
 */
	msize = (kbit/8) * klin;

	if  ( msize > MAXSIZ )  {
	    *iret = G_NIDSIZ;
	    return;
	}

/*
 *	Clear the entire image.
 */
	tclear ( &ier );

/*
 *	Set the device bounds.
 */
	if  ( ( krot == 0 ) || ( krot == 180 ) )  {
	    *ileft  = 1;
	    *ibot   = klin;
	    *iright = kbit;
	    *itop   = 1;
	}
	else if  ( ( krot == 90 ) || ( krot == 270 ) )  {
	    *ileft  = 1;
	    *ibot   = kbit;
	    *iright = klin;
	    *itop   = 1;
	}

/*
 *	Set file to initially closed.
 */
	opnfil = G_FALSE;

/*
 *      Save the file name.
 */
	sprintf ( tmpfil, "%s.tiff", pnuc );

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
 */
	for ( i = 0; i < 8; i++ )  {
	    mskon[i]  = 1 << (7-i);
	    mskoff[i] = ~ mskon[i];
	}

}
