#include "geminc.h"
#include "gemprm.h"
#include "uscore.h"

struct	int_info
{
    float	x;
    float	y;
    int		d;
    int		i;
    int		l;
};

struct	int_info	*_I_Info;

/*
 * Private function
 */
void ggsort ( int *nint, float *x, float *y, int *d, int *i, int *l,
              int *iret );
int  ggsort_ints ( struct int_info *info1, struct int_info *info2 );

void ggsort ( int *nint, float *x, float *y, int *d, int *i, int *l, 
	      int *iret )
/************************************************************************
 * ggsort								*
 *									*
 * This subroutine sorts the graph-to-grid intersections.		*
 *									*
 * ggsort ( nint, x, y, v, d, i, l, iret )				*
 *									*
 * Input/Output parameters:						*
 *	*nint		int		Number of intersections		*
 *	*x		float		X-coords of ints		*
 *	*y		float		Y-coords of ints		*
 *	*d		int		Intersection direction		*
 *	*i		int		Intersection index		*
 *	*l		int		Intersection line number	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 *					 +N = number of duplicate ints	*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	07/03						*
 * D.W.Plummer/NCEP	09/03	Chg call seq: rm value, add line number	*
 ***********************************************************************/
{
int	ii, nn, ndup;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * Allocate space to perform the sort.
     */
    _I_Info = (struct int_info *)malloc( *nint * sizeof(struct int_info) );
	
    /*
     * Transfer incoming values to structure.
     */
    for ( ii = 0; ii < *nint; ii++ )  {
	_I_Info[ii].x = x[ii];
	_I_Info[ii].y = y[ii];
	_I_Info[ii].d = d[ii];
	_I_Info[ii].i = i[ii];
	_I_Info[ii].l = l[ii];
    }

    /*
     * Perform the sort.
     */
    qsort ( _I_Info, *nint, sizeof(struct int_info),
	    (int(*)(const void*, const void*))ggsort_ints );
	
    /*
     * Transfer values from the structure.
     */
    for ( ii = 0; ii < *nint; ii++ )  {
	x[ii] = _I_Info[ii].x;
	y[ii] = _I_Info[ii].y;
	d[ii] = _I_Info[ii].d;
	i[ii] = _I_Info[ii].i;
	l[ii] = _I_Info[ii].l;
    }

    /*
     * Free up the temporary space.
     */
    free ( _I_Info );

    /*
     * Remove duplicates.
     */
    ndup = 0;
    nn = 0;
    for ( ii = 1; ii < *nint; ii++ )  {
	if ( ( !G_DIFF(x[nn], x[ii]) ) ||
	     ( !G_DIFF(y[nn], y[ii]) ) ||
	     ( l[nn] != l[ii] ) )  {
	    nn++;
	    if ( nn != ii )  {
	        x[nn] = x[ii];
	        y[nn] = y[ii];
	        d[nn] = d[ii];
	        i[nn] = i[ii];
	        l[nn] = l[ii];
	    }
	}
	else  {
	    ndup++;
	}
    }
    nn++;
    if ( nn != *nint )  {
	for ( ii = nn; ii <= *nint; ii++ )  {
	    x[ii] = RMISSD;
	    y[ii] = RMISSD;
	    d[ii] = IMISSD;
	    i[ii] = IMISSD;
	    l[ii] = IMISSD;
	}
    }

    /*
     * Return the number of duplicates found (usually 0).
     */
    *nint = nn;
    *iret = ndup;

    return;

}

int  ggsort_ints ( struct int_info *info1, struct int_info *info2 )
/************************************************************************
 * ggsort_ints                                                      	*
 *                                                                      *
 * This function sorts the graph-to-grid intersection structure.	*
 * Sort precedent is: 1) direction, 2) direction index, 3) X-value,	*
 * unless direction #1 then Y-value.					*
 *                                                                      *
 * int ggsort_ints ( list1, list2 )                                 	*
 *                                                                      *
 * Input parameters:                                                    *
 * *list1     struct int_info	int_info structure			*
 * *list2     struct int_info	int_info structure			*
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 * Return parameters:                                                   *
 * ggsort_ints      int         Return code (from sort precedence)	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	07/03						*
 ***********************************************************************/
{
    int	ival;
/*---------------------------------------------------------------------*/

    /*
     * Compare directions, return if not equal.
     */
    ival = info1->d - info2->d;
    if ( ival != 0 )  return ( ival );

    /*
     * Compare direction indices, return if not equal.
     */
    ival = info1->i - info2->i;
    if ( ival != 0 )  return ( ival );

    /*
     * Compare X-values (unless direction #1 then Y-values), 
     * return the difference.
     */
    if ( info1->d == 1 )  {
	ival = ( info1->y - info2->y ) < 0 ? -1 : 1;
    }
    else  {
	ival = ( info1->x - info2->x ) < 0 ? -1 : 1;
    }

    return ( ival );
}
