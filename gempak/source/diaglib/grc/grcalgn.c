#include "geminc.h"
#include "gemprm.h"

void grc_algn ( const float *grdin, const float *deltax,
                const float *deltay, float *grdout, int *kx, int *ky, 
                int *iret )
/************************************************************************
 * GR_ALGN								*
 *									*
 * This subroutine aligns a grid on grid points.  The lower left	*
 * corner specified in the input grid corners is moved to the left	*
 * and down if necessary.  The input and output grid corners are	*
 * arrays ordered as follows:  lower left lat, lower left lon, upper 	*
 * right lat, upper right lon.						*
 *									*
 * GR_ALGN  ( GRDIN, DELTAX, DELTAY, GRDOUT, KX, KY, IRET )		*
 *									*
 * Input parameters:							*
 *	GRDIN  (4)	REAL		Input grid corners		*
 *	DELTAX		REAL		X grid spacing			*
 *	DELTAY		REAL		Y grid spacing			*
 *									*
 * Output parameters:							*
 *	GRDOUT (4)	REAL		Aligned grid corners		*
 *	KX		INTEGER		Number of points in x dir	*
 *	KY		INTEGER		Number of points in y dir	*
 *	IRET		INTEGER		Return code			*
 *					  0 = normal return		*
 *					 -5 = invalid grid spacing	*
 **									*
 * Log:									*
 * M. desJardins/GSFC	 8/85						*
 * M. desJardins/GSFC	 8/88	Modified from OAGAGN			*
 * R. Tian/SAIC         07/06   Translated from Fortran                 *
 ************************************************************************/
{
/*----------------------------------------------------------------------*/

    gr_algn ( grdin, deltax, deltay, grdout, kx, ky, iret );

    return;
}
