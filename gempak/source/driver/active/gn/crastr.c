#include "geminc.h"
#include "gemprm.h"

#include "imgdef.h"
#include "proto_xw.h"


void crastr ( unsigned char *radial, int num_radial, int size, 
				float start, float delta, int *iret )
/************************************************************************
 * crastr								*
 *									*
 * This routine adds a radial to the raster array. The technique 	*
 * depends on which quadrant of the circle the radial is in.  If it is 	*
 * in the top (315-45) or bottom (135-225) quadrant, the rasterization 	*
 * is done by rows from the bottom to the top, and from left to right	*
 * within the rows. Thus the rasterization start from the beginning of 	*
 * the radial in the top quadrant and from the end of the radial in the	*
 * bottom quadrant. If it is in the right of left quadrant, the 	*
 * rasterization is done by columns from left to right, and from bottom	*
 * to top witin	the rows.						*
 *									*
 * crastr ( radial, num_radial, size, start, delta, iret )		*
 *									*
 * Input parameters:							*
 *	*radial		unsigned char	Ptr to array with radial data	*
 *	num_radial	int		Number of pnts in radial array	*
 *	size		int		Size of image			*
 *	start		float		Start angle for this radial	*
 *	delta		float		Angle delta for this radial	*
 * Output parameters:							*
 *	*iret		int		Staus retuen			*
 **									*
 * Log:									*
 * H. Edmon/UW		 1994	Part of NIDS decoder			*
 * J. Cowie/COMET	 3/95	Minor mods; store data with origin in	*
 *				upper-left				*
 * S. Jacobs/NCEP	 1/97	Copied from XRASTRIZ; Replaced DegToRad	*
 *				with DTR; Cleaned up			*
 ***********************************************************************/
{

    float	az,		/* Angle of mid-point of radial */
		az1,		/* Angle of left/bottom of radial */
		az2,		/* Angle of right/top of radial */
		pgs,		/* raster points per radial point */
		tmp;
    double	sin1, cos1,	/* sine and cosine of az1 */
		sin2, cos2,	/* sine and cosine of az2 */
		inc1,		/* Tangent/cotangent of az1 */
		inc2,		/* Tangent/cotangent of az2 */
		rowinc,		/* Change in radial index per row */
		colinc,		/* Change in radial index per column */
		x0,		/* Raster x value of end of radial */
		y0,		/* Raster y value of end of radial */
		gate,		/* Radial index of start of row/column */
		gate_col,	/* Radial index of current column */
		gate_row,	/* Radial index of current row */
		left, right,	/* Raster index limits for column */
		bottom, top;	/* Raster index limits for row */
    unsigned char *dp;		/* Pointer into raster array */
    int		col, row,	/* Current column/row */
		vertical,	/* Is this a vertical radial (315-45,135-225)*/
		flip=0;		/* Were az1 and az2 flipped? */

/*---------------------------------------------------------------------*/

	*iret = G_NORMAL;

/*
 *
 *	Set up radial angles az, az1 and az2.
 */
	az1 = start;
	az  = start + 0.5 * delta;
	if  ( az  >= 360.0 )  az -= 360;
	az2 = start + delta;
	if  ( az2 >= 360.0 )  az2 -= 360;

/*
 *	Figure out the orientation of the radial.  Then, if need be,
 *	swap the two angles to correspond to the direction in which
 *	we will be rasterizing (i.e. az1 is always left of az2 in a
 *	vertical radial, and az1 is always below az2 in a horizontal
 *	radial).
 */
	vertical = ( ( az <= 45.0 ) ||
		     ( ( az >= 135.0 ) && ( az <= 225.0 ) ) ||
		     ( az >= 315.0 ) );
	if  ( (  vertical && ( az >  90.0 ) && ( az < 270.0 ) ) ||
	      ( !vertical && ( az < 180.0 ) ) )  {
	    tmp = az1;
	    az1 = az2;
	    az2 = tmp;
	    ++flip;
	}
	sin1 = sin ( az1 * DTR );
	cos1 = cos ( az1 * DTR );
	sin2 = sin ( az2 * DTR );
	cos2 = cos ( az2 * DTR );

/*
 *	This used to be the ratio of date res to pixel res.
 */
	pgs = 1;

/*
 *	For vertical radials (315-45, 135-225).
 */
	if  ( vertical )  {
	    inc1   = sin1 / cos1;
	    inc2   = sin2 / cos2;
	    rowinc = ( sin1 < sin2 ) ? pgs/cos1 : pgs/cos2;
	    colinc = pgs * sin ( az * DTR );
/*
 *	    If in right side of circle, use az2 (right) side of beam
 *	    in calculation of y0 (and vice-versa).
 */
	    if  ( az < 180. )  {
		y0 = size*0.5 + size*0.5*cos2;
	    }
	    else {
		y0 = size*0.5 + size*0.5*cos1;
	    }
/*
 *	    Start rasterization from end of radial (edge).
 *	    flip => bottom quadrant of circle
 */
	    if  ( flip )  {
		gate   = num_radial - .00001;
		left   = size * 0.5 + ( y0 - size * 0.5 ) * inc1;
		right  = size * 0.5 + ( y0 - size * 0.5 ) * inc2;
		bottom = y0;
		top    = size * 0.5;
	    }
/* 
 *	    Else, start rasterization from beginning of radial (center).
 */
	    else {
		gate   = 0;
		left   = size * 0.5;
		right  = size * 0.5;
		bottom = size * 0.5;
		top    = y0;
	    }

/*
 *	    Correct the starting index of the radial for the right
 *	    side of the circle since we will be rasterizing towards
 *	    the edge.
 */
	    if  ( az < 180. )  {
		gate -= colinc * ( right - left + 1 );
	    }

/*
 *	    Set up pointer for raster array.
 */	  
	    dp = imgData + ( size - (int)bottom - 1 ) * size;

/*
 *	    Outer loop is by row.
 */
	    for ( row = (int) bottom; row < (int) top ; row++ )  {
		gate_col = gate;

/*
 *	    	Inner loop is by column.
 */
		for ( col = (int) left; col <= (int) right; col++ )  {
		    dp[col] = radial[(int) gate_col];
		    gate_col += colinc;
		}

/*
 *		Set up for next row.
 */
		gate  += rowinc;
		left  += inc1;
		right += inc2;
		dp    -= size ;
	    }
	}

/*
 *	For horizontal radials (45-135, 225-315).
 */
	else {
	    inc1   = cos1 / sin1;
	    inc2   = cos2 / sin2;
	    rowinc = pgs * cos ( az * DTR );
	    colinc = ( cos1 < cos2 ) ? pgs/sin1 : pgs/sin2 ;

/* 
 *	    If in bottom of circle, use az1 (bottom) side of beam in
 *	    calculation of x0 (and vice-versa).
 */
	    if ( ( az > 90. ) && ( az < 270. ) )  {
		x0 = size * 0.5 + size * 0.5 * sin1;
	    }
	    else {
		x0 = size * 0.5 + size * 0.5 * sin2;
	    }

/* 
 *	    Start rasterization from end of radial (edge).
 *	    not flip => left quadrant of circle
 */
	    if  ( !flip )  {
		gate   = num_radial - .00001;
		bottom = size * 0.5 + ( x0 - size * 0.5 ) * inc1;
		top    = size * 0.5 + ( x0 - size * 0.5 ) * inc2;
		left   = x0;
		right  = size * 0.5;
	    }

/*
 *	    Else, start rasterization from beginning of radial (center).
 */
	    else {
		gate   = 0;
		bottom = size * 0.5;
		top    = size * 0.5;
		left   = size * 0.5;
		right  = x0;
	    }

/*
 *	    Correct the starting index of the radial for the top of the
 *	    circle since we will be rasterizing towards the edge.
 */
	    if  ( ( az < 90. ) || ( az > 270. ) ) {
		gate -= rowinc*(top-bottom+1);
	    }

/*
 *	    Outer loop is by column.
 */
	    for ( col = (int) left; col < (int) right; col++ )  {
		dp = imgData + ( size - (int)bottom - 1 ) * size + col; 
		gate_row = gate;

/*
 *		Inner loop is by row.
 */
		for ( row = (int) bottom; row <= (int) top ; row++ )  {
		    *dp = radial[(int) gate_row];
		    gate_row += rowinc;
		    dp -= size;
		}
/*
 *		Set up for next column.
 */
		gate   += colinc;
		bottom += inc1;
		top    += inc2;
	    }
	}
}
