#include "faxcmn.h"

void rsdatt ( char *pname, float *xsz, float *ysz, 
		int *ileft, int *ibot, int *iright, int *itop, 
		int *numclr, int *iret )
/************************************************************************
 * rsdatt								*
 * 									*
 * This subroutine defines the device attributes.			*
 * 									*
 * rsdatt ( pname, xsz, ysz, ileft, ibot, iright, itop,	numclr, iret )	*
 *									*
 * Input parameters:							*
 *	*pname		char		Name of file as output		*
 *	*xsz		float		X size in inches or pixels	*
 *	*ysz		float		Y size in inches or pixels	*
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
 * E. Wehner/EAi	 3/96	Adopted from hsdatt.f			*
 * S. Jacobs/NCEP	 4/96	Added ileft,ibot,iright,itop,nncolr	*
 *				to calling sequence; added calculation	*
 *				of paper and device size		*
 * E. Wehner/EAi	1/97	Use wheel to index fax products		*
 * E. Wehner/Eai	3/97	Set x and ysize based on rotation	*
 * S. Jacobs/NCEP	 7/97	Rewrote and reorganized code		*
 * S. Jacobs/NCEP	 7/97	Cleaned up header and global variables	*
 * S. Jacobs/NCEP	 7/97	Added indent value from product table	*
 * S. Jacobs/NCEP	 7/97	Added check for 180 and 270 rotation	*
 * S. Jacobs/NCEP	 8/97	Added reserved value from prod table	*
 * G. Krueger/EAI	10/97	CST_xLST: Removed RSPTB; Add str limit	*
 * S. Jacobs/NCEP	 5/98	Changed to allow for multiple subsets	*
 * T. Piper/SAIC	02/04	Removed lenf parameter			*
 ***********************************************************************/
{

	int	ier, num, ii, fxfg;
	char	tmpfil[133], **aryptr, twhl[5], tsub[5];

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
	    strcpy ( twhl, aryptr[0] );
	    strcpy ( tsub, aryptr[1] );

	    for ( ii = 0; ii < 2; ii++ )  {
		free ( aryptr[ii] );
	    }
	    free ( (char **) aryptr );

	    fxfg = G_TRUE;
	}
	else {

/*
 *	    This is not a FAX product. However, still create a raster
 *	    image of the product.
 */
	    fxfg = G_FALSE;

	}

/*
 *      Save the file name.
 */
	if  ( fxfg )  {
	    sprintf ( tmpfil, "%s.ras", twhl );
	}
	else {
	    strcpy ( tmpfil, pname );
	}

/*
 *	If the passed in filename is different from the global filename,
 *	change the name after closing the old file.
 */
	if  ( strcmp ( filnam, tmpfil ) != 0 )  {

	    rclosp ( &ier );

	    if  ( fxfg )  {

/*
 *	    	Find the requested product in the FAX product
 *		definition table.
 */
		strcpy ( wheel,  twhl );
		ctb_prod ( wheel, tsub, MAXSUB, &nsub, subset, descr,
			   kbit, klin, krot, kind, krsv, &ier );
		if  ( ier != G_NORMAL )  {
		    *iret = G_NOPROD;
		    return;
		}

		faxflg = G_TRUE;

	    }
	    else {

/*
 *		Set the dimensions of the raster only output.
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
 *	    Make sure that there are enough bytes per raster line. If the
 *	    number of bits is not divisible by 8 then add enough bits to
 *	    make the number divisible by 8.
 */
	    if  ( kbit[0] % 8 != 0 )  {
		kbit[0] = kbit[0] + (8 - kbit[0]%8);
	    }

/*
 *	    If this is a FAX product and the number of bits per line is
 *	    greater than 1728, set the number to 1728.
 */
	    if  ( kbit[0] > 1728 && faxflg )  {
		kbit[0] = 1728;
	    }

/*
 *	    Compute the number of bytes for this raster image.
 *	    If the size is larger than the maximum, return with an error.
 */
	    msize = (kbit[0]/8) * klin[0];

	    if  ( msize > MAXSIZ )  {
		*iret = G_NIDSIZ;
		return;
	    }

/*
 *	    Clear the entire image.
 */
	    rclear ( &ier );

/*
 *	    Set the device bounds.
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
 *	    Set file to initially closed.
 */
	    opnfil = G_FALSE;

/*
 *	    If the new file name is not empty, set the current file name.
 */
	    if  ( tmpfil[0] != CHNULL )  {
                strcpy ( filnam, tmpfil );
		*iret = G_NEWWIN;
	    }


/*
 *	    Set the number of colors to be returned to DEVCHR.
 *
 *	    nncolr (numclr) = number of device colors
 *		( A maximum of MXCLNM = 32 may be initialized. )
 */
	    nncolr  = 1;
	    *numclr = nncolr;

	}

}
