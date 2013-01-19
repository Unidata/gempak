#include "geminc.h"
#include "gemprm.h"
#include "imgdef.h"
#include "proto_xw.h"

static int setflg = G_FALSE;

void csinit ( int *iret )
/************************************************************************
 * csinit								*
 *									*
 * This subroutine initializes the image data arrays.			*
 *									*
 * csinit ( iret )							*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					G_NORMAL = normal return	*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 1/97						*
 * S. Jacobs/NCEP	 4/97	Added #define IMGDEF			*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/
	
	*iret = G_NORMAL;

/*
 *	Initialize the image data arrays and size.
 */
	if  ( ! setflg )  {
	    imgData = NULL;
	    rawData = NULL;
	    last_rawsize = 0;
	    setflg = G_TRUE;
	}

}
