#include "tiffcmn.h"

void tsdatt ( char *pname, float *xsz, float *ysz, 
		int *ileft, int *ibot, int *iright, int *itop,
		int *numclr, int *iret )
/************************************************************************
 * tsdatt								*
 * 									*
 * This subroutine defines the device attributes.			*
 * 									*
 * tsdatt ( pname, xsz, ysz, ileft, ibot, iright, itop,	numclr, iret )	*
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
 * S. Jacobs/NCEP	12/98						*
 * R. Tian/SAIC         05/02   Modified to display FAX as image        *
 * T. Piper/SAIC	02/04	Removed lenf parameter			*
 ***********************************************************************/
{

	int	ier;
	char	tmpfil[133], pnuc[133], descr[41];
	int	newfile = 0;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *      Save the file name.
 */
	cst_lcuc ( pname, pnuc, &ier );
	sprintf ( tmpfil, "%s.tiff", pnuc );

	if  ( strcmp ( filnam, tmpfil ) != 0 )  {
/*
 *	    If the passed in filename is different from the global filename,
 *	    change the name after closing the old file.
 */
	    tclosp ( &ier );
/*
 *	    Find the requested product in the FAX product
 *	    definition table.
 */
	    ctb_tiff ( pnuc, descr, &kbit, &klin, &krot, &ier );
	    if  ( ier != G_NORMAL )  {
		*iret = G_NOPROD;
		return;
	    }
	    newfile = 1;
	}
	else if ( strncmp ( pnuc, "FAX", 3 ) == 0 ) {
/*
 *          There is no entry for FAX product in the TIFF product
 *          definition table. The size is passed in.
 */
	    tclosp ( &ier );

            kbit = (int)(*xsz);
            klin = (int)(*ysz);
            krot = 0;
	    newfile = 1;
	}

	if ( newfile ) {
/*
 *	    Make sure that there are enough bytes per raster line. If the
 *	    number of bits is not divisible by 8 then add enough bits to
 *	    make the number divisible by 8.
 */
	    if  ( kbit % 8 != 0 )  {
		kbit = kbit + (8 - kbit%8);
	    }

/*
 *	    Compute the number of bytes for this raster image.
 *	    If the size is larger than the maximum, return with an error.
 */
	    msize = (kbit/8) * klin;

	    if  ( msize > MAXSIZ )  {
		*iret = G_NIDSIZ;
		return;
	    }

/*
 *	    Clear the entire image.
 */
	    tclear ( &ier );

/*
 *	    Set the device bounds.
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
