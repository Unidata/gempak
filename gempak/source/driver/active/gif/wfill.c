#include "gifcmn.h"
#include "pattern.h"

int _cmp_xout ( float *val1, float *val2 );
void fillScan ( int jscan, int jx1, int jx2, int *iret );

/************************************************************************
 * wfill.c								*
 *									*
 * CONTENTS:								*
 ***********************************************************************/

/*=====================================================================*/

void wfill ( int *np, int ix[], int iy[], int *iret )
/************************************************************************
 * wfill								*
 *									*
 * This function draws a filled polygon using the GIF driver.		*
 *									*
 * wfill ( np, ix, iy, iret )						*
 *									*
 * Input parameters:							*
 *	*np		int		Number of points in the polygon	*
 *	ix []		int		Array of x coordinates		*
 *	iy []		int		Array of y coordinates		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/98						*
 * T. Lee/GSC		 7/00   Cleaned up; Added return value		*
 * S. Danz/AWC   	11/03	Adapted code from TIFF driver for fills *
 * J. Wu/SAIC  		06/05	allocate working arrays dynamically	*
 ***********************************************************************/
{

	int	npts, *ixarr, *iyarr, iymin, iymax, i, j, index,
		nx1, nx2, ny2;
	float	tau, xout[100];

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;
/*
 *      Return for fewer than three points.
 */
        if ( *np <= 2 ) return;

/*
 *	Get the number of points and make sure that the polygon
 *	is closed.
 */
	npts = *np;
	if ( ix[0] != ix[npts-1] || iy[0] != iy[npts-1] )  npts++;

/*
 *	Allocate space for the working arrays.
 */
	G_MALLOC ( ixarr, int, npts, "WFILL" );
	G_MALLOC ( iyarr, int, npts, "WFILL" );

	iymin =  INT_MAX;
	iymax = -INT_MAX;

/*
 *	Double the dimensions for easier computations. Find the min
 *	and max in the Y direction.
 */
	for ( i = 0; i < npts-1; i++ )  {
	    ixarr[i] = ix[i] * 2;
	    iyarr[i] = iy[i] * 2;
	    iymin = G_MIN ( iyarr[i], iymin );
	    iymax = G_MAX ( iyarr[i], iymax );
	}

	ixarr[npts-1] = ixarr[0];
	iyarr[npts-1] = iyarr[0];

/*
 *	For each scan line, compute intersections and fill.
 */
	for ( j = iymin+1; j < iymax; j = j+2 )  {
	
	    index = 0;

	    for ( i = 0; i < npts-1; i++ )  {

		if  ( iyarr[i] != iyarr[i+1] )  {

		    tau = (float) ( j - iyarr[i] ) /
			  (float) ( iyarr[i+1] - iyarr[i] );

		    if ( tau >= 0.0 && tau <= 1.0 ) {

			xout[index] = tau * ( ixarr[i+1] - ixarr[i] ) +
				      ixarr[i];
			index++;

		    }
		}
	    }

/*
 *	    Sort the values of the X coordinate.
 */
	    qsort ( xout, index, sizeof(float), (int (*)(const void *, const void *))_cmp_xout );

/*
 *	    Loop over all the scan lines, filling the pixels
 *	    in the pattern.
 */
	    for ( i = 0; i < index; i=i+2 ) {
		ny2 = j / 2;
		nx1 = xout[i  ] / 2;
		nx2 = xout[i+1] / 2;
		fillScan ( ny2, nx1, nx2, iret );
	    }

	}

/*
 *	Free the working arrays.
 */
	G_FREE ( ixarr, int );
	G_FREE ( iyarr, int );

}

/*---------------------------------------------------------------------*/

int _cmp_xout ( float *val1, float *val2 )
/************************************************************************
 * _cmp_xout								*
 *									*
 * This function is used by qsort to compare successive values of the	*
 * an array to be sorted. Qsort will take different action based on	*
 * the sign of the output of this function.				*
 *									*
 * int _cmp_xout ( val1, val2 )                                        	*
 *									*
 * Input parameters:                                                    *
 *	*val1		float		First value			*
 *	*val2		float		Second value			*
 *									*
 * Output parameters:                                                   *
 *	_cmp_xout	int		Return value			*
 *									*
 **									*
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 8/97						*
 ***********************************************************************/
{

        return ( *val1 - *val2 );

}

/*---------------------------------------------------------------------*/

void fillScan ( int jscan, int jx1, int jx2, int *iret )
/************************************************************************
 * fillScan								*
 *									*
 * This function writes all the pixels in a scan line based on the 	*
 * fill pattern selected.						*
 *									*
 * fillScan ( jscan, jx1, jx2, iret )					*
 *									*
 * Input parameters:							*
 *	jscan		int		Index of scan line to be filled	*
 *	jx1		int		First X value			*
 *	jx2		int		Second X value			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 4/96	Created					*
 * M. Linda/GSC		 7/97	Corrected so fill includes right border	*
 * D.W.Plummer/NCEP	 8/97	Rewrite					*
 * S. Jacobs/NCEP	 3/98	Moved pattern definition to include file*
 ***********************************************************************/
{

	int 	kx;

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *	Loop over all X values for this scan line.
 */
	for ( kx = jx1; kx <= jx2; kx++ ) {

/*
 *	    If the pixels are to be turned on, fill using the pattern.
 */
	    if  ( CurrentColorIndex != CurrentBGColorIndex )  {
		if  ( kfpat[kfillt-1][jscan%PDIM][kx%PDIM] == 1 )  {
		    gdImageSetPixel (Current_Im, kx, jscan, CurrentColorIndex);
		}
	    }
	    else {
/*
 *		Otherwise, turn all of the pixels off.
 */
		gdImageSetPixel (Current_Im, kx, jscan, CurrentBGColorIndex);
	    }

	}

}
