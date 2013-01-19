#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "hints.h"
#include "proto_xw.h"

#define MAX_DISTANCE	1E+37

static float		*_dcX, *_dcY;
static int		_dcN;
static Boolean		_selectFlag;

static VG_DBStruct	_selectEl;


/*
 *  Private Callback Functions
 */
static void pgaddpt_selectCb ( Widget, XtPointer, XEvent* );
static void pgaddpt_inpoly ( char *syspt, int *npts, float *xpt, float *ypt,
                  char *syspl, int *npls, float *xpl, float *ypl, int *iret );
static void pgaddpt_segint ( char *sys1, int *np, float *xx, float *yy,
                                char *sys2, float *xin, float *yin, int *iret );
static void pgaddpt_findClosedSeg ( char *sys1, int *np, float *xx, float *yy,
                        char *sys2, float *fx, float *fy,float *distance,
                        int *nearest_vrt, int *next_vrt,
                        float *nx, float *ny, int *iret );


/************************************************************************
 * nmap_pgaddpt.c							*
 *									*
 * This module contains the routines for the point add to a GFA snapshot*
 *									*
 * CONTENTS:								*
 * pgaddpt_start	    Initialize and original selection		*
 * pgaddpt_selectCb	    Selection callback				*
 * pgaddpt_inpoly	    Determine whether the point(s) is(are) 	*
 *			    internal or external to the polygon		*
 * pgaddpt_segint   	    Determine the new segment and multipoint    *
 *			    line is intersect or not			*
 * pgaddpt_findClosedSeg    Find the closest segment in multipoint line *
 ***********************************************************************/

/*=====================================================================*/

void pgaddpt_start ( VG_DBStruct *el )
/************************************************************************
 * pgaddpt_start							*
 *									*
 * Initial setup and original selection					*
 *									*
 * void pgaddpt_start (el)						*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	selected element		*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 *  X. Guo/CWS		01/10	Initial coding				*
 ***********************************************************************/
{
    _selectFlag = FALSE;
    mcanvw_setPressFunc((XtEventHandler)&pgaddpt_selectCb, CURS_DEFAULT);

    _selectEl = *el;

    pgactv_getDevPts (&_dcN, &_dcX, &_dcY);

    mbotw_mouseSet(LMHINT_PTSELECT, MMHINT_DONE);

}

/*=====================================================================*/
/* ARGSUSED */
static void pgaddpt_selectCb ( Widget wid, XtPointer clnt, 
							XEvent *event )
/************************************************************************
 * pgaddpt_selectCb							*
 *									*
 * This function handles the selection callbacks			*
 *									*
 * static void pgaddpt_selectCb (wid, clnt, event)			*
 *									*
 * Input parameters:							*
 *	wid		Widget	calling widget				*
 *	clnt		XtPointer					*
 *	*event		XEvent						*
 *									*
 * Output parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 *  X. Guo/CWS		01/10	Initial coding				*
 ***********************************************************************/
{
    float	distance, llx, lly, urx, ury,nx,ny;
    float	x_cntr, y_cntr, c_lat, c_lon, area;
    int		mrkclr, mrknp, location, ier, xoff, yoff;
    int         el_location, one = 1,next_vrt;
    char	value[32];
    static int	nearest,pos;
    static float   xx,yy;

/*---------------------------------------------------------------------*/

    if (event->xbutton.button == Button1) {
        if (_selectFlag) {     /* confirming selected point */
	    pgundo_newStep();

	    el_location = pgactv_getElmLoc();
	    pgutls_prepNew (el_location, &_selectEl, &llx, &lly, 
                            &urx, &ury, &ier);
	    pgundo_storeThisLoc (el_location, UNDO_DEL, &ier);

	    pgactv_addPts (&xx, &yy, one, pos, &ier);
	    if ( ier < 0 ) {
 	        _selectFlag = FALSE;
                mbotw_mouseSet(LMHINT_OBJSELECT, MMHINT_EXIT);
	    }
	    else {
	        pgactv_getDevPts (&_dcN, &_dcX, &_dcY);

	        if (_dcN > 1) {

/*
 *  Snap GFA 
 */
		    if ( _selectEl.hdr.vg_type == GFA_ELM ) {
		       _selectEl.elem.gfa.info.npts = _dcN;
		       gtrans ( sys_D, sys_M, &_dcN, _dcX, _dcY, 
			    &(_selectEl.elem.gfa.latlon[0]), 
			    &(_selectEl.elem.gfa.latlon[_dcN]), &ier,
			    strlen(sys_D), strlen(sys_M) );

		       pgsmear_snapEl ( FALSE, &_selectEl, &ier );

		       _dcN = _selectEl.elem.gfa.info.npts;

		       cvg_todev( &_selectEl, &_dcN, _dcX, _dcY, &ier );

		       if ( pggfaw_isClosed() ) {
		           cgr_centroid( _dcX, _dcY, &_dcN, &x_cntr, &y_cntr, &area, &ier );
		       } else {
			    x_cntr = _dcX[ 0 ];
			    y_cntr = _dcY[ 0 ];
		       }
		       gtrans( sys_D, sys_M, &one, &x_cntr, &y_cntr, &c_lat, &c_lon, &ier, 
			     strlen(sys_D), strlen(sys_M) );

		       sprintf ( value, "%7.2f", c_lat );
		       cvg_setFld ( &_selectEl, TAG_GFA_ARROW_LAT, value, &ier );
		       sprintf ( value, "%7.2f", c_lon );
		       cvg_setFld ( &_selectEl, TAG_GFA_ARROW_LON, value, &ier );
		    }

		    pgvgf_saveNewElm(cvg_getworkfile(), sys_D, &_selectEl, 
				_dcN, _dcX, _dcY, TRUE, &location, &ier );

		    pgutls_redraw(location, &_selectEl, &ier);

		    pgundo_storeThisLoc (location, UNDO_ADD, &ier);

		    _selectFlag = FALSE;
		    mbotw_mouseSet(LMHINT_PTSELECT, MMHINT_DONE);
	        }
	        else {
		    mcanvw_disarmDynamic ();
		    _selectFlag = FALSE;

		    pghdlb_deselectAll();

		    mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);

		    _selectFlag = FALSE;
		    mbotw_mouseSet(LMHINT_OBJSELECT, MMHINT_EXIT);
	        }
                pgundo_endStep();
	    }
	}
	else {	/* selecting point */
	    xgtoff (&xoff, &yoff, &ier);
	    xx = (float) (event->xbutton.x + xoff);
	    yy = (float) (event->xbutton.y + yoff);
	    if ( _selectEl.hdr.closed == 1 ) {
		pgactv_addClosedPt ();
		pgactv_getDevPts (&_dcN, &_dcX, &_dcY);
	    }
	    pgaddpt_inpoly ( sys_D, &one, &xx, &yy,  sys_D,&_dcN, _dcX, _dcY, &ier );

	    if ( ! ier ) {
		pgaddpt_findClosedSeg (sys_D, &_dcN, _dcX, _dcY, sys_D, &xx, &yy, 
					&distance, &nearest,&next_vrt,&nx, &ny, &ier);
	    	if ( ier == 0 ) {
		    pos = next_vrt;
		    if ( nearest > next_vrt ) {
			pos = nearest;
		    }
	    	    mrkclr = 2;
	    	    mrknp  = 1;
	    	    gscolr (&mrkclr, &ier);
	    	    gmark  (sys_D, &mrknp, &xx, &yy, &ier, strlen(sys_D) );
	    	    geplot (&ier);

	    	    _selectFlag = TRUE;
	    	    mbotw_mouseSet(LMHINT_CONFIRM, MMHINT_CANCEL);
		}
		else {
                     _selectFlag = FALSE;
                     mbotw_mouseSet(LMHINT_PTSELECT, MMHINT_DONE);
		}
	   }
	   else {
                _selectFlag = FALSE;
                mbotw_mouseSet(LMHINT_PTSELECT, MMHINT_DONE);
	   }
           if ( _selectEl.hdr.closed == 1 ) {
               pgactv_deletePt ( _dcN - 1);
           }
	}
    }
    else {
        if (_selectFlag) {     /* unselecting point */
	    pghdlb_displayAllSel ();

	    _selectFlag = FALSE;
	    mbotw_mouseSet(LMHINT_PTSELECT, MMHINT_DONE);
	}
	else {	/* unselecting element */
	    mcanvw_disarmDynamic ();
	    _selectFlag = FALSE;

	    pghdlb_deselectAll();

	    mcanvw_setPressFunc ((XtEventHandler)&pgevt_locateElmCb, CURS_DEFAULT);

	    _selectFlag = FALSE;
	    mbotw_mouseSet(LMHINT_OBJSELECT, MMHINT_EXIT);
	}
    }
}

/*=====================================================================*/

static void pgaddpt_inpoly ( char *syspt, int *npts, float *xpt, float *ypt,
                  char *syspl, int *npls, float *xpl, float *ypl, int *iret )
/************************************************************************
 * pgaddpt_inpoly                                                       *
 *                                                                      *
 * This function accepts a point(s) and polygon vertices and determines *
 * whether the point(s) is(are) internal or external to the polygon.    *
 *                                                                      *
 *                                                                      *
 * pgaddpt_inpoly ( syspt, npts, xpt, ypt, syspl, npls, xpl, ypl, iret) *
 *                                                                      *
 * Input parameters:                                                    *
 *      *syspt  char            Input points coordinate system          *
 *      *npts   int             Number of points to test                *
 *      *xpt    float[]         X array  of test points                 *
 *      *ypt    float[]         Y array  of test points                 *
 *      *syspl char             Input points coordinate system          *
 *      *npls   int             Number of vertices in the polygon       *
 *      *xpl    float[]         Polygon X vertices                      *
 *      *ypl    float[]         Polygon Y vertices                      *
 *                                                                      *
 * Output parameters:							*
 *	*iret	int	Return code                                     *
 *                      0  =  Vertex lies outside of polygon            *
 *                      1  =  Vertex lies inside of polygon             *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * X. Guo/CWS	      01/10   Created 	                                *
 *									*
 ***********************************************************************/
{
int 	*inout, i,ier;
	    
    ier = 0;
    if ( (_selectEl.hdr.vg_type != LINE_ELM ) &&
         (_selectEl.hdr.vg_type != SPLN_ELM ) &&
         ( _selectEl.hdr.vg_type != TCBKL_ELM )) {
	G_MALLOC ( inout, int, *npts, "pgaddpt_inpoly: inout" );  
        cgr_inpoly ( syspt, npts, xpt, ypt,  syspl,npls, xpl, ypl,inout, iret );
	for ( i = 0; i < *npts; i ++ ) {
	    if ( inout[i] == 1 ) {
		ier = 1 ;
		break;
	    }
	}
	G_FREE ( inout, int );
    }
    *iret = ier; 
}

/*=====================================================================*/

static void pgaddpt_findClosedSeg ( char *sys1, int *np, float *xx, float *yy, 
			char *sys2, float *fx, float *fy,float *distance, 
			int *nearest_vrt, int *next_vrt,
                        float *nx, float *ny, int *iret )

/************************************************************************
 * pgaddpt_findClosedSeg                                                *
 *                                                                      *
 * This function determines the closest and next vertices of a          *
 * multipoint line to a fixed point, the closest point (on the line     *
 * segment defined by those two vertices) to the fixed point, and the   *
 * distance between the fixed point and the closest point.              *
 * The "next vertex" is simply the vertex following the nearest vertex  *
 * in the order of the points, not the next closest vertex to the fixed *
 * point.                                                               *
 *                                                                      *
 * pgaddpt_findClosedSeg ( sys1,np, xx, yy, sys2, fx, fy, distance,     *
 *                                nearest_vrt, next_vrt, nx, ny, iret ) *
 *                                                                      *
 * Input parameters:                                                    *
 *	*sys1		char		Input points coordinate system  *
 *      *np             int             Number of points in figure      *
 *      *xx             float           X coordinates of figure         *
 *      *yy             float           Y coordinates of figure         *
 *      *sys2           char            Input points coordinate system  *
 *      *fx             float           X coordinate of fixed point     *
 *      *fy             float           Y coordinate of fixed point     *
 *                                                                      *
 * Output parameters:                                                   *
 *      *distance       float           Distance to the point           *
 *      *nearest_vrt    int             Closest vertex number           *
 *      *next_vrt       int             Other end of nearest segment    *
 *      *nx             float           Nearest x coord on figure       *
 *      *ny             float           Nearest y coord on figure       *
 *      *iret           int             Status return                   *
 *                                      0 = great, 1 = not a line       *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * X. Guo/CWS       01/10   created	                                *
 ***********************************************************************/
{
    int		ii, ier, intrsct1,intrsct2;
    float	qx, qy, curr_dist, d0, d1, m2, m1, b2, b1;
    float	xmin, xmax, ymin, ymax;
    float	x1[2], y1[2];
/*---------------------------------------------------------------------*/

    if (*np == 1) {
	*iret = 1;

	*nearest_vrt = *next_vrt = 0;
	*nx = xx[0];
	*ny = yy[0];

	*distance = (float) G_DIST (xx[0], yy[0], *fx, *fy);

	return;
    }

    *iret = 1;
    *distance = MAX_DISTANCE;
    /*
     * Isolate which line segment is closest to desired point.
     */

    for (ii = 0; ii < *np-1; ii++ ) {

  	xmin = (float) G_MIN (xx[ii], xx[ii+1]);
	xmax = (float) G_MAX (xx[ii], xx[ii+1]);
	ymin = (float) G_MIN (yy[ii], yy[ii+1]);
	ymax = (float) G_MAX (yy[ii], yy[ii+1]);

	/*
	 * Must find the closest point on vertical and horiztonal
	 * seperately since the slope formula would cause a 
	 * divide by zero error
	 */

	/*
	 *  Vertical segments
	 */
	if (G_DIFF(xmin, xmax)) {
	    qx = xmin;
	    if (*fy < ymin)
	        qy = ymin;
	    else if (*fy > ymax)
		qy = ymax;
	    else
		qy = *fy;
	}

	/*
	 *  Horizontal segments
	 */
    	else if ( G_DIFF(ymin, ymax) ) {
	    qy = ymin;
	    if (*fx < xmin)
	        qx = xmin;
	    else if (*fx > xmax)
		qx = xmax;
	    else
		qx = *fx;
	}

	/*
	 *  All the rest
	 */
	else {
	    /*
	     * find slope and intercept for initial line
	     */
	    m1 = (yy[ii+1] - yy[ii]) / (xx[ii+1] - xx[ii]);
	    b1 = yy[ii] - (m1 * xx[ii]);

	    /*
	     * find slope and intercept for perpendicular
	     */
	    m2 = - 1.0F / m1;
	    b2 = *fy - (m2 * *fx);

	    /* 
	     * find the intersection of the two lines
	     * which would be the closest point
	     *
	     * formula for a line is y = mx + b
	     * y = (m1 * x) + b1  &&  y = (m2 * x) + b2
	     * (m1 * x) + b1 = (m2 * x) + b2
	     * (m1 * x) - (m2 * x) = (b2 - b1)
	     * x * (m1 - m2) = (b2 - b1)
	     * x = (b2 - b1) / (m1 - m2)
	     */
	    qx = (b2 - b1) / (m1 - m2);
	    qy = (m2 * qx) + b2;
	}

	/*
	 * find the distance
	 */
	if (xmin <= qx && qx <= xmax) {
	    curr_dist = (float) G_DIST (*fx, *fy, qx, qy);
	}
	else {
	    d0 = (float) G_DIST (*fx, *fy, xx[ii], yy[ii]);
	    d1 = (float) G_DIST (*fx, *fy, xx[ii+1], yy[ii+1]);
	    curr_dist = (d0 <= d1) ? d0 : d1;
	}

	if (curr_dist < *distance) {
	    intrsct1 = 0;
	    intrsct2 = 0;
            /*
             *  Figure which end of segment is closest to point.
             */
            d0 = (float) G_DIST (*fx, *fy, xx[ii], yy[ii]);
            d1 = (float) G_DIST (*fx, *fy, xx[ii+1], yy[ii+1]);
	    x1[0] = *fx;
	    y1[0] = *fy;
	    if ( d0 < d1) {
            	x1[1] = xx[ii];
            	y1[1] = yy[ii];
	    }
	    else
	    {
                x1[1] = xx[ii+1];
                y1[1] = yy[ii+1];
	    }
            pgaddpt_segint (sys1, np, xx, yy, sys2, x1, y1, &ier);
	    if ( ier == 0 ) {
            	x1[0] = *fx;
            	y1[0] = *fy;
		if ( d0 < d1 ) {
            	    x1[1] = xx[ii+1];
            	    y1[1] = yy[ii+1];
		}
		else {
                    x1[1] = xx[ii];
                    y1[1] = yy[ii];
		}
		pgaddpt_segint (sys1, np, xx, yy, sys2, x1, y1, &ier);
		if ( ier == 1 ) {
		    intrsct2 = 1;
		}
	    }
	    else {
		intrsct1 = 1;
 	    }
            if ( intrsct1 == 0 && intrsct2 == 0) {
 
	    	*distance = curr_dist;

	    	*nx = qx;
	    	*ny = qy;	

	    	if ( ! intrsct1 ) {
		    *nearest_vrt = ii;
		    *next_vrt    = ii + 1;
	    	}
	    	else {
		    *nearest_vrt = ii + 1;
		    *next_vrt    = ii;
	    	}

	    	if ((*nx < xmin) || (xmax < *nx)) {
		    *nx = xx[*nearest_vrt];
		    *ny = yy[*nearest_vrt];
	    	}
		*iret = 0;
	    } 
	}
    }
}

/*=====================================================================*/

static void pgaddpt_segint ( char *sys1, int *np, float *xx, float *yy,
				char *sys2, float *xin, float *yin, int *iret )
/************************************************************************
 * pgaddpt_segint	                                                *
 *                                                                      *
 * This function determines the line segment and multipoint lines if    *
 * they intersect one another.						*
 *                                                                      *
 * pgaddpt_segint ( sys1,np, xx, yy, sys2, xin, yin, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *      *sys1           char            Input points coordinate system  *
 *      *np             int             Number of points in figure      *
 *      *xx             float           X coordinates of figure         *
 *      *yy             float           Y coordinates of figure         *
 *      *sys2           char            Input points coordinate system  *
 *      *xin            float           X coordinate of fixed point     *
 *      *yin            float           Y coordinate of fixed point     *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Status return                   *
 *                                      0 = no intersect, 1 = intersect *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * X. Guo/CWS       01/10   created                                     *
 ***********************************************************************/
{
int 	i, inout, ier;
float 	xint, yint;
float	x1[2], y1[2];
float 	dist = 0.0001;

    *iret = 0;
    for ( i = 0 ; i < *np - 1 ; i ++ ) {
	x1[0] = xx[i];
	y1[0] = yy[i];
	x1[1] = xx[i+1];
	y1[1] = yy[i+1];
	cgr_segint (sys1, x1, y1, sys2, xin, yin, sys2, &xint, &yint, &inout, &ier );
	if ( inout == 1 ) {
		if ( (G_DIFFT ( x1[0], xint,dist ) && G_DIFFT ( y1[0], yint,dist )) ||
		   (G_DIFFT ( x1[1], xint,dist ) && G_DIFFT ( y1[1], yint,dist ))  ) {
		    continue;
		}
	    	*iret = 1;
	    	break;
	}   
    }
}
