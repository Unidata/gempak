#include "faxcmn.h"

void rcircpts ( int ix, int iy, int icentx, int icenty, int *iret )
/************************************************************************
 * rcircpts								*
 *									*
 * This function draws a filled circle by computing the outer edge in	*
 * each octant of the circle, then doing a polygon fill.		*
 *									*
 * rcircpts  ( ix, iy, icentx, icenty, iret )				*
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
 * E. Wehner/EAi	 6/96	Created			 		*
 * E. Wehner/EAi	 3/97	Removed x and y size from calls		*
 * S. Jacobs/NCEP	 7/97	Cleaned up header and global variables	*
 * S. Jacobs/NCEP	 1/98	Modified to use rfill to fill circle	*
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
	ssfill = tsfill;
	szfil = 1.0F;
	iftyp = 2;
	rsfill ( &szfil, &iftyp, &ier );

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
	rfill ( &np, jx, jy, &ier );

/*
 *	Reset the previous fill type.
 */
	szfil = (float)mfillt;
	iftyp = (int)ssfill;
	rsfill ( &szfil, &iftyp, &ier );

}
