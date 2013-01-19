#include "geminc.h"
#include "gemprm.h"

int cgr_cntstrks ( int on_len, int off_len, int np, int ix[], int iy[], 
								int *iret )
/************************************************************************
 * cgr_cntstrks                                                         *
 *                                                                      *
 * This function counts the number of strokes along a line segment.	*
 * This is used to count elementary graphics primatives such as pips	*
 * along a front 							*
 *                                                                      *
 * int cgr_cntstrks  ( on_len, off_len, np, ix, iy, iret )		*
 * 									*
 * Input parameters:                                                    *
 *	on_len		int	Length of one "ON" segment		*
 *	off_len		int	Length of one "OFF" segment		*
 *	np		int	Number of points in arrays		*
 *	ix[]		int	Array of X points			*
 *	iy[]		int	Array of Y points			*
 *									*
 * Output parameters:							*
 *	*iret		int	Return value				*
 *	cgr_cntstrks	int	Number of on/off cycles that will fit   * 
 *				in a line as specified			*
 *                                                                      *
 **									*
 * Log:									*
 * E. Wehner/EAi	 9/96	Created					*
 * E. Wehner/EAi	11/96	Fixed casting problem on Sun platforms	*
 * T. Piper/GSC		 3/99	Corrcted prolog				*
 ************************************************************************/
{
    int one_stroke;
    double total_sz = 0.0;
    int dx;
    int dy;
    int ii;

    *iret = 0;


    one_stroke = on_len + off_len;
    
    /* if only received 1 point, get out of routine */
    if (np < 2) return 0;

    /* for each point, add the distance to the point to sum */
    for ( ii = 0; ii < np-1; ii++ )
    { 

        dx = ix[ii+1] - ix[ii];
	dy = iy[ii+1] - iy[ii];

        total_sz += sqrt( (double)( ( dx * dx) + (dy * dy)) );

    }
    return (int)(total_sz/(double)one_stroke);

}
