#include "pscmn.h"
#include "color.h"

void psdatt ( int *iunit, char *fname, int *lenf, int *itype, 
		float *xsz, float *ysz, int *ileft, int *ibot, 
		int *iright, int *itop, int *numclr, int *iret )
/************************************************************************
 * psdatt								*
 * 									*
 * This subroutine defines the device attributes.			*
 * 									*
 * psdatt ( iunit, fname, lenf, itype, xsz, ysz, ileft, ibot, iright,	*
 *	    itop, numclr, iret )					*
 *									*
 * Input parameters:							*
 *	*iunit		int		Output type (Used for XW only)	*
 *	*fname		char		Name of file as output		*
 *	*lenf		int		Length of fname			*
 *	*itype		int		Device type (color,bw,gs)	*
 *	*xsz		float		X size in inches or pixels	*
 *	*ysz		float		Y size in inches or pixels	*
 *									*
 * Output parameters:							*
 *	*ileft		int		Left device coordinate		*
 *	*ibot		int		Bottom device coordinate	*
 *	*iright		int		Right device coordinate		*
 *	*itop		int		Top device coordinate		*
 *	*numclr         int		Max number of colors for device	*
 * 	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAi	 3/96	Adopted from hsdatt.f			*
 * S. Jacobs/NCEP	 4/96	Added ileft,ibot,iright,itop,nncolr	*
 *				to calling sequence; added calculation	*
 *				of paper and device size		*
 * S. Jacobs/NCEP	 5/96	Removed PIX_BW, PIX_GS and PIX_COLOR;	*
 *				Added new global variables for queries	*
 * S. Jacobs/NCEP	 5/96	Removed coltbl.psb			*
 * S. Jacobs/NCEP	 9/98	Changed plot dim to leave 1/4 in margin	*
 * S. Jacobs/NCEP	 9/98	Added large paper 24x36 inches		*
 * S. Jacobs/NCEP	 8/99	Changed 24x36 to 20x32; Added 32x42	*
 ***********************************************************************/
{
	char	tmpfil[133];
	int	ier;

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 *	Set the global output and color scheme types
 */
	kctype = *itype;
	kunit  = *iunit;

/*
 *	Set the temporary filename.
 */
	strncpy ( tmpfil, fname, *lenf );
	tmpfil[*lenf] = CHNULL;

/*
 *	If the passed in filename is different from the global filename,
 *	change the name after closing the old file.
 */
	if  ( strcmp ( filnam, tmpfil ) != 0 )
	{

	    pclosp ( &ier );

/*
 *	    Only change the attributes if the file is new.
 *
 *	    Set the paper size in inches and in pixels.
 *
 *	    xsize = x paper size in inches
 *	    ysize = y paper size in inches
 *
 *	    landscape = landscape/portrait orientation flag
 *
 *	    ileft  = left device coordinate
 *	    ibot   = bottom device coordinate
 *	    iright = right device coordinate
 *	    itop   = top device coordinate
 *
 *	    Device coords are based on 72 pixels/inch and a scaling
 *	    factor of 32 multiplied by the plot area in inches.
 */
	    *ileft  = 1;
	    *ibot   = 1;

	    if  ( ( *xsz >= *ysz ) || ( *xsz < 0 ) || ( *ysz < 0 ) )
	    {
		if  ( ( *xsz >= 33.0 ) || ( *ysz >= 21.0 ) )
		{
		    xsize = 42.5;
		    ysize = 32.5;
		    strcpy ( pprnam, "/Roll2" );
		    *iright = xsize * 72 * 32;
		    *itop   = ysize * 72 * 32;
		}
		if  ( ( *xsz >= 18.0 ) || ( *ysz >= 12.0 ) )
		{
		    xsize = 32.5;
		    ysize = 20.5;
		    strcpy ( pprnam, "/Roll1" );
		    *iright = xsize * 72 * 32;
		    *itop   = ysize * 72 * 32;
		}
		else if  ( ( *xsz >= 11.5 ) || ( *ysz >= 9.0 ) )
		{
		    xsize = 17.0;
		    ysize = 11.0;
		    strcpy ( pprnam, "/Tabloid" );
		    *iright = ( xsize - 0.5 ) * 72 * 32;
		    *itop   = ( ysize - 0.5 ) * 72 * 32;
		}
		else
		{
		    xsize = 11.0;
		    ysize =  8.5;
		    strcpy ( pprnam, "/Letter" );
		    *iright = ( xsize - 0.5 ) * 72 * 32;
		    *itop   = ( ysize - 0.5 ) * 72 * 32;
		}
	    }
	    else
	    {
		if  ( ( *xsz >= 21.0 ) || ( *ysz >= 33.0 ) )
		{
		    xsize = 32.5;
		    ysize = 42.5;
		    strcpy ( pprnam, "/Roll2" );
		    *iright = xsize * 72 * 32;
		    *itop   = ysize * 72 * 32;
		}
		else if  ( ( *xsz >= 12.0 ) || ( *ysz >= 18.0 ) )
		{
		    xsize = 20.5;
		    ysize = 32.5;
		    strcpy ( pprnam, "/Roll1" );
		    *iright = xsize * 72 * 32;
		    *itop   = ysize * 72 * 32;
		}
		else if  ( ( *xsz >= 9.0 ) || ( *ysz >= 11.5 ) )
		{
		    xsize = 11.0;
		    ysize = 17.0;
		    strcpy ( pprnam, "/Tabloid" );
		    *iright = ( xsize - 0.5 ) * 72 * 32;
		    *itop   = ( ysize - 0.5 ) * 72 * 32;
		}
		else
		{
		    xsize =  8.5;
		    ysize = 11.0;
		    strcpy ( pprnam, "/Letter" );
		    *iright = ( xsize - 0.5 ) * 72 * 32;
		    *itop   = ( ysize - 0.5 ) * 72 * 32;
		}
	    }

	    if  ( xsize > ysize )
		landscape = G_TRUE;
	    else
		landscape = G_FALSE;

/*
 *	    If the new file name is not empty, set the current file name.
 */
	    if  ( tmpfil[0] != CHNULL )
	    {
		strcpy ( filnam, tmpfil );
		*iret = G_NEWWIN;
	    }

/*
 *	    Load a local color table based on the pixel type.
 */
	    if  ( *itype == 2 )
	    {
		strcpy ( tblnam, "coltbl.psc" );
		nncolr = 32;
	    }
	    else
	    {
		strcpy ( tblnam, "coltbl.psg" );
		nncolr = 20;
	    }

/*
 *	    Set the number of colors to be returned to DEVCHR.
 *
 *	    nncolor (numclr) = number of device colors
 *		( A maximum of MXCLNM = 32 may be initialized. )
 */
	    *numclr = nncolr;

	    pscint ( &ier );
	    if  ( ier != G_NORMAL)  return;
	
	}

}
