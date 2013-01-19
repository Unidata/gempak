#include "tiffcmn.h"

void tcircpts ( int ix, int iy, int icentx, int icenty, int *iret )
/************************************************************************
 * tcircpts								*
 *									*
 * This function draws a filled circle by computing the outer edge in	*
 * each octant of the circle, then doing a polygon fill.		*
 *									*
 * tcircpts  ( ix, iy, icentx, icenty, iret )				*
 *									*
 * Input parameters:							*
 *	ix 		int		X location on the circle	*
 *	iy		int		Y location on the circle	*
 *	icentx		int		Center of circle		*
 *	icenty		int		Center of circle		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/98						*
 ***********************************************************************/
{

	int	dx, dy, mfillt, iftyp, ier, np, jx[9], jy[9];
	float	ssfill, szfil;

/*---------------------------------------------------------------------*/

	*iret = 0;

/*
 *	Save the current fill type. Set the fill type to solid.
 */
	mfillt = kfillt;
	ssfill = tszfil;
	szfil = 1.0;
	iftyp = 2;
	tsfill ( &szfil, &iftyp, &ier );

/*
 *	Construct and fill the circular polygon.
 */
	dx = ix - icentx;
	dy = iy - icenty;
	np = 9;
	jx[0] = icentx + dx;
	jy[0] = icenty + dy;
	jx[1] = icentx + dy;
	jy[1] = icenty + dx;
	jx[2] = icentx - dy;
	jy[2] = icenty + dx;
	jx[3] = icentx - dx;
	jy[3] = icenty + dy;
	jx[4] = icentx - dx;
	jy[4] = icenty - dy;
	jx[5] = icentx - dy;
	jy[5] = icenty - dx;
	jx[6] = icentx + dy;
	jy[6] = icenty - dx;
	jx[7] = icentx + dx;
	jy[7] = icenty - dy;
	jx[8] = jx[0];
	jy[8] = jy[0];
	tfill ( &np, jx, jy, &ier );

/*
 *	Reset the previous fill type.
 */
	szfil = mfillt;
	iftyp = ssfill;
	tsfill ( &szfil, &iftyp, &ier );

}
