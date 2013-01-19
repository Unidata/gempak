#include "tiffcmn.h"

void tsopen ( int *iret )
/************************************************************************
 * tsopen								*
 *									*
 * This subroutine sets the open file flag to true. This allows		*
 * plot data to be written to the raster image for later output to 	*
 * a file.								*
 *									*
 * tsopen ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/98						*
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
