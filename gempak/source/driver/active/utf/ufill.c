#include "utfcmn.h"

int _cmpa_xout ( float *val1, float *val2 ); 

/************************************************************************
 * ufill.c								*
 *									*
 * CONTENTS:								*
 ***********************************************************************/

/*=====================================================================*/

void ufill ( int *np, int ix[], int iy[], int *iret )
/************************************************************************
 * ufill								*
 *									*
 * This function draws a filled polygon as a series of vectors in the   *
 * UTF file.  The polygon is constructed by building an edge table and  *
 * processing through the edges to create an active edge table and      *
 * drawing lines between active edges. 					*
 *									*
 * ufill ( np, ix, iy, iret )						*
 *									*
 * Input parameters:							*
 *	*np		int		Number of points in the polygon	*
 *	ix [np]		int		Array of x coordinates		*
 *	iy [np]		int		Array of y coordinates		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 **									*
 * Log:									*
 * E. Safford/GSC	11/96	Initial coding				*
 * S. Jacobs/NCEP	 8/97	Recoded based on RFILL in FAX driver	*
 * S. Jacobs/NCEP	 9/97	Compute the spacing for the fill lines	*
 * S. Jacobs/NCEP	 9/97	Changed order of the free memory calls	*
 * S. Jacobs/NCEP	 9/97	Changed spacing for fill lines (even #)	*
 * S. Jacobs/NCEP	 2/98	Removed drawing of bounding line	*
 ***********************************************************************/
{

	int	npts, *ixarr, *iyarr, iymin, iymax, i, j, index,
		num, nx[2], ny[2], jinc, jdiff;
	float	tau, xout[100];

/*---------------------------------------------------------------------*/

    	*iret =  G_NORMAL; 

/*
 *	Check for trying to draw something using the background
 *	color and return.
 */
	if  ( kcolr == 101 )  return;

/*
 *	If the file is not open, then open it first.
 */
	if  ( ! opnfil )  {
            uopen ( iret );
	    if  ( *iret != G_NORMAL )  return;
	}


/*
 *	Get the number of points and make sure that the polygon
 *	is closed.
 */
	npts = *np;
	if  ( ix[0] != ix[npts-1] || iy[0] != iy[npts-1] ) npts++;

/*
 *	Allocate space for the working arrays.
 */
	ixarr = (int *) malloc ( npts * sizeof(int) );
	iyarr = (int *) malloc ( npts * sizeof(int) );

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
 *	Compute the line spacing for the fill. The value for jinc must
 *	be even to work in the fill algorithm below.
 */
	jdiff = iymax - iymin;
	if  ( jdiff < 200 )  {
	    jinc = 24;
	}
	else  {
	    jinc = 24 * ( jdiff / 200 );
	}

/*
 *	For each line in the Y direction, compute the intersections
 *	and fill the polygon.
 */
	for ( j = iymin+1; j < iymax; j += jinc )  {

	    index = 0;

	    for ( i = 0; i < npts-1; i++ )  {

		if  ( iyarr[i] != iyarr[i+1] )  {

		    tau = (float) ( j - iyarr[i] ) /
			  (float) ( iyarr[i+1] - iyarr[i] );

		    if  ( tau >= 0.0 && tau <= 1.0 )  {

			xout[index] = tau * ( ixarr[i+1] - ixarr[i] ) +
				      ixarr[i];
			index++;

		    }
		}
	    }

/*
 *	    Sort the values of the X coordinate.
 */
	    qsort ( xout, index, sizeof(float), (int (*)(const void *, const void *))_cmpa_xout );

/*
 *	    Loop over all the points in the Y direction, drawing a 
 *	    horizontal line between two intersections at a time.
 */
	    num   = 2;
	    ny[0] = j / 2;
	    ny[1] = j / 2;
	    for ( i = 0; i < index; i+=2 )  {
		nx[0] = xout[i  ] / 2;
		nx[1] = xout[i+1] / 2;
		uline ( &num, nx, ny, iret );
	    }

	}

/*
 *	Free the working arrays.
 */
	free ( iyarr );
	free ( ixarr );

}

/*======================================================================*/

int _cmpa_xout ( float *val1, float *val2 )
/************************************************************************
 * _cmpa_xout								*
 *									*
 * This function is used by qsort to compare successive values of the	*
 * an array to be sorted. Qsort will take different action based on	*
 * the sign of the output of this function.				*
 *									*
 * int _cmpa_xout ( val1, val2 )					*
 *									*
 * Input parameters:							*
 *	*val1		float		First value			*
 *	*val2		float		Second value			*
 *                                                                      *
 * Output parameters:							*
 *	_cmpa_xout	int		Return value			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 8/97	Copied from FAX driver			*
 ***********************************************************************/
{

	return ( *val1 - *val2 );

}
