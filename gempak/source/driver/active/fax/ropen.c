#include "faxcmn.h"

void ropen ( int *iret )
/************************************************************************
 * ropen								*
 *									*
 * This subroutine sets the open file flag to true. This allows		*
 * plot data to be written to the raster image for later output to 	*
 * a file.								*
 *									*
 * ropen ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Wehner/EAI	 4/96	Created.				*
 * S. Jacobs/NCEP	 7/97	Renamed rsopen to ropen			*
 * S. Jacobs/NCEP	 7/97	Cleaned up header and global variables	*
 * M. Linda/GSC		 9/97	Changed a key word in the prologue	*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

 /*     
  *	If no file name is specified, return with an error.
  */
	if  ( strlen ( filnam ) == (size_t)0 )  {
	    *iret = G_NOPSFL;
	    return;
	}

/*
 *	Mark bitmap as opened.
 */
	opnfil = G_TRUE;

}
