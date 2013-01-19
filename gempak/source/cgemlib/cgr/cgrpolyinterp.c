#include "geminc.h"
#include "gemprm.h"
#include "cgrcmn.h"

/*
 * Private functions
 */
void 	intrppoly_dump 	( MPOLYGON poly, int which );

float 	intrppoly_gpt ( float p1, float p2, float pct );


/*=====================================================================*/

/*
 * Public functions
 */
void cgr_polyinterp ( int *npin0, float *xin0, float *yin0, 
	              int *npin1, float *xin1, float *yin1, float *pct,
	int *nmp, float *xmp0, float *ymp0, float *xmp1, float *ymp1,
	int *maxnpo, int *npo, float *xo, float *yo, int *iret )
/************************************************************************
 * cgr_polyinterp							*
 *									*
 * This function takes two polygons and calculates an interpolated	*
 * polygon using an interpoaltion percentage 'pct' (e.g.,0.50 for 50%).	*
 * Cartesian coordinates are assumed.					*
 * Polygons are assumed to be ordered in a ccw fashion.			*
 * Closed polygons are assumed with unequal first and last points.	*
 *									*
 * cgr_polyinterp ( npin0, xin0, yin0, npin1, xin1, yin1, pct,		*
 * 		    nmp, xmp0, ymp0, xmp1, ymp1, maxnpo,		*
 * 	            npo, xo, yo, iret )					*
 *									*
 * Input parameters:							*
 *	*npin0		int	Number of points in polygon1		*
 *	*xin0		float	X coordinates of polygon 1		*
 *	*yin0		float	Y coordinates of polygon 1		*
 *	*npin1		int	Number of points in polygon2		*
 *	*xin1		float	X coordinates of polygon 2		*
 *	*yin1		float	Y coordinates of polygon 2		*
 *	*pct		float	Interpolation percentage           	*
 *	*nmp		int	Number of mapping points		*
 *	*xmp0		float	X coordinate mapping from		*
 *	*ymp0		float	Y coordinate mapping from		*
 *	*xmp1		float	X coordinate mapping to			*
 *	*ymp1		float	Y coordinate mapping to			*
 *	*maxnpo		int	Maximum number of points in output poly	*
 *									*
 * Output parameters:							*
 *	*npo		int	Number of points in output polygon	*
 *	*xo		float	X coordinates of output polygon		*
 *	*yo		float	Y coordinates of output polygon		*
 *	*iret		int	Return code				*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 2/04						*
 ***********************************************************************/
{
int		nmap;
float		*xmap0, *ymap0, *xmap1, *ymap1;
float		x_intrp, y_intrp;
MPOLYGON	mpoly0, mpoly1;
MPOLYPOINT	*mpp_tmp, *mpp, *mpp0, *mpp1, *mpp_next;
int		*inout;
POLYGON		poly0, poly1, interp;
POLYPOINT	*pp, *pstart, *pp_tmp;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     * First, generate POLYGON structures and check if they intersect.
     */
    poly0.first = polyp_create ( npin0, xin0, yin0 );
    poly1.first = polyp_create ( npin1, xin1, yin1 );

    polyp_dup ( &poly0, &poly1, CGR_EPS );

    /*
     * Make working copies of the input polygons in linked-list structures. 
     *
     * LET WORKING POLYGON P1 (x1[npin0],y1[npin0]) BE THE POLY WITH 
     * THE FEWER NUMBER OF POINTS.
     */
    nmap = *nmp;
    xmap0  = NEW ( float, G_MAX(nmap,10) );
    ymap0  = NEW ( float, G_MAX(nmap,10) );
    xmap1  = NEW ( float, G_MAX(nmap,10) );
    ymap1  = NEW ( float, G_MAX(nmap,10) );
    inout  = NEW (   int, ((*npin0)*(*npin1)) );
    if ( *npin0 <= *npin1 )  {
	memcpy ( xmap0, xmp0, nmap*sizeof(float) );
	memcpy ( ymap0, ymp0, nmap*sizeof(float) );
	memcpy ( xmap1, xmp1, nmap*sizeof(float) );
	memcpy ( ymap1, ymp1, nmap*sizeof(float) );
    }
    else  {
	memcpy ( xmap0, xmp1, nmap*sizeof(float) );
	memcpy ( ymap0, ymp1, nmap*sizeof(float) );
	memcpy ( xmap1, xmp0, nmap*sizeof(float) );
	memcpy ( ymap1, ymp0, nmap*sizeof(float) );
    }

    /*
     * Create linked-list mapping polygons from the input polygons
     * 'poly0' and 'poly1'.
     */
    pp = poly0.first;
    do {
	if ( pp == poly0.first )  {
	    mpp = NEW(MPOLYPOINT,1);
	    nBytesAlloc += sizeof(MPOLYPOINT);
	    mpoly0.first = mpp;
	    mpp->pt = pp->pt;
	    mpp->map = NULL_MPOLYPOINT;
	}
	else  {
	    mpp_tmp = mpp;
	    mpp = NEW(MPOLYPOINT,1);
	    nBytesAlloc += sizeof(MPOLYPOINT);
	    mpp_tmp->next = mpp;
	    mpp->pt = pp->pt;
	    mpp->prev = mpp_tmp;
	    mpp->map = NULL_MPOLYPOINT;

	}
	pp = pp->next;
    } while ( pp != poly0.first );
    mpp->next = mpoly0.first;
    mpoly0.first->prev = mpp;

    pp = poly1.first;
    do {
	if ( pp == poly1.first )  {
	    mpp = NEW(MPOLYPOINT,1);
	    nBytesAlloc += sizeof(MPOLYPOINT);
	    mpoly1.first = mpp;
	    mpp->pt = pp->pt;
	    mpp->map = NULL_MPOLYPOINT;
	}
	else  {
	    mpp_tmp = mpp;
	    mpp = NEW(MPOLYPOINT,1);
	    nBytesAlloc += sizeof(MPOLYPOINT);
	    mpp_tmp->next = mpp;
	    mpp->pt = pp->pt;
	    mpp->prev = mpp_tmp;
	    mpp->map = NULL_MPOLYPOINT;
	}
	pp = pp->next;
    } while ( pp != poly1.first );
    mpp->next = mpoly1.first;
    mpoly1.first->prev = mpp;

    /*
     * Map each mapping polygon into the other.
     */
    polyp_getmap ( &mpoly0, &mpoly1, nmap, xmap0, ymap0, xmap1, ymap1 );

    /*
     * The mapping polygons 'mpoly0' and 'mpoly1' have the same number of
     * points with the first point of 'mpoly0' mapped to the first point of
     * 'mpoly1', the second to the second, and so on.
     * Traverse these mapping polygons and compute the interpolated
     * POLYGON using the percentage ('pct') variable.
     */
    mpp0 = mpoly0.first;
    mpp1 = mpoly1.first;
    do {
	if ( mpp0 == mpoly0.first )  {
	    pp = NEW(POLYPOINT,1);
	    nBytesAlloc += sizeof(POLYPOINT);
	    pstart = pp;
	    x_intrp = intrppoly_gpt ( mpp0->pt->x, mpp1->pt->x, *pct );
	    y_intrp = intrppoly_gpt ( mpp0->pt->y, mpp1->pt->y, *pct );
	    pp->pt = polyp_newpt ( x_intrp, y_intrp );
	}
	else  {
	    pp_tmp = pp;
	    pp = NEW(POLYPOINT,1);
	    nBytesAlloc += sizeof(POLYPOINT);
	    pp_tmp->next = pp;
	    x_intrp = intrppoly_gpt ( mpp0->pt->x, mpp1->pt->x, *pct );
	    y_intrp = intrppoly_gpt ( mpp0->pt->y, mpp1->pt->y, *pct );
	    pp->pt = polyp_newpt ( x_intrp, y_intrp );
	    pp->next = NULL_POLYPOINT;
	    pp->prev = pp_tmp;
	}

	mpp0 = mpp0->next;
	mpp1 = mpp1->next;
    } while ( mpp0 != mpoly0.first && mpp1 != mpoly1.first );
    pp->next = pstart;
    pstart->prev = pp;
    interp.first = pstart;

    polyp_getpts  ( interp.first, npo, xo, yo );

    /*
     * Free up all temporary memory.
     */

    polyp_destroy ( poly0.first );
    polyp_destroy ( poly1.first );
    polyp_destroy ( interp.first );

    mpp = mpoly0.first;
    do {
	mpp_next = mpp->next;
        nBytesFree += sizeof(MPOLYPOINT);
	free ( mpp );
	mpp = mpp_next;
    } while ( mpp != mpoly0.first );

    mpp = mpoly1.first;
    do {
	mpp_next = mpp->next;
        nBytesFree += sizeof(MPOLYPOINT);
	free ( mpp );
	mpp = mpp_next;
    } while ( mpp != mpoly1.first );

    free ( inout );
    free ( ymap1 );
    free ( xmap1 );
    free ( ymap0 );
    free ( xmap0 );

}

/*=====================================================================*/

void intrppoly_dump ( MPOLYGON poly, int which )
/************************************************************************
 * intrppoly_dump                                                          *
 *                                                                      *
 * This function dumps the contents of an MPOLYGON.                     *
 *                                                                      *
 * intrppoly_dump ( MPOLYGON poly, int which )				*
 *                                                                      *
 * Input parameters:                                                    *
 * poly  	MPOLYGON 	Mapped polygon				*
 * which  	int 		Which mapping				*
 *                                                                      *
 * Output parameters:                                                   *
 * none                                                                 *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP     11/03                                           *
 ***********************************************************************/

{
int		ii;
/* float		tx, ty; */
MPOLYPOINT      *pp;
/*---------------------------------------------------------------------*/

    printf("POLYGON %d :\n", which );
    pp = poly.first;
    ii = 0;
    do {
	printf("Segment %d  (%4.1f,%4.1f) - (%4.1f,%4.1f) - (%4.1f,%4.1f) MAP TO (%4.1f,%4.1f)\n",
	    ii,
	    pp->prev->pt->x, pp->prev->pt->y,
	    pp->pt->x, pp->pt->y,
	    pp->next->pt->x, pp->next->pt->y,
	    pp->map==NULL_MPOLYPOINT ? RMISSD : pp->map->pt->x,
	    pp->map==NULL_MPOLYPOINT ? RMISSD : pp->map->pt->y );

	ii++;
	pp = pp->next;
    } while ( pp != poly.first );

}

/*=====================================================================*/

float intrppoly_gpt ( float p1, float p2, float pct )
/************************************************************************
 * intrppoly_gpt							*
 *									*
 * This function calculates the position of intermediate points located	*
 * linear pct distance from point p1 (x1,y1) to point p2 (x2,y2).	*
 * The variable pct is defined to be 0.0 for 0% and 1.0 for 100%.	*
 * Cartesian coordinates are assumed.					*
 *									*
 * intrppoly_gpt ( p1, p2, pct )					*
 *									*
 * Input parameters:							*
 *	p1		float	Coordinates of starting point		*
 *	p2		float	Coordinates of starting point 		*
 *	pct		float	Percent distance from pt1 to pt2	*
 *									*
 * Output parameters:							*
 *	none								*
 *									*
 * Return parameters:							*
 *	intrppoly_gpt	float	Coordinates of intermediate point	*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 2/04						*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    return ( p1 + ( pct * ( p2 - p1 ) ) );

}
