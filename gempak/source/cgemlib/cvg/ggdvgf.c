#include "geminc.h"
#include "gemprm.h"

void gg_dvgf ( char *vgfile, int *icol, int *iret )
/************************************************************************
 * gg_dvgf								*
 *									*
 * This routine displays the contents of a Vector Graphics File.	*
 *									*
 * gg_dvgf ( vgfile, icol, iret )					*
 *									*
 * Input parameters:							*
 *	*vgfile		char		Vector Graphics File name	*
 *	*icol		int		Plot color			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					 -17 = invalid VGF file		*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/97						*
 * S. Jacobs/NCEP	 3/97	Added return for invalid file		*
 * S. Jacobs/NCEP	 6/97	Added write flag of FALSE to cvg_load	*
 * D.W.Plummer/NCEP	 7/97	Added icol				*
 * E. Wehner/EAi	 8/97	No more "loadx"				*
 * D.W.Plummer/NCEP	 9/97	Bug fix -- now pass *icol to cvg_load	*
 * T. Lee/GSC		 6/99	Reset text attributes			*
 * S. Jacobs/NCEP	 9/99	Changed to call cvg_load2		*
 * J. Wu/SAIC		11/01	adjust param in cvg_load2() calling	*
 ***********************************************************************/
{

int     font, type, width, ibrdr, irrotn, ijust, ier;
float	size;

/*---------------------------------------------------------------------*/
	*iret = 0;

	/*
	 * Query the text attributes and reset.
	 */
	gqtext ( &font, &type, &size, &width,
		 &ibrdr, &irrotn, &ijust, &ier );
	cvg_load2 ( vgfile, *icol, &ier );
	gstext ( &font, &type, &size, &width,
		 &ibrdr, &irrotn, &ijust, &ier );

	if  ( ier != 0 )  *iret = -17;

}
