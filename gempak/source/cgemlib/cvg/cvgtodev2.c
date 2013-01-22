#include "cvgcmn.h"

static void enlargePolygon(int *np, float *dx, float *dy, int pixels);

void cvg_todev2 ( VG_DBStruct *el, int *np, float *dx, float *dy, int *iret )
/************************************************************************
 * cvg_todev2								*
 *									*
 * This function converts the map based coordinates that are used in	*
 * the VGF file to a device based coordinate equivalent to the current	*
 * display. The difference with cvg_todev is that for non single point  *
 * objects (lines, polygons, etc) the cgr_range() is used instead of    *
 * gtrans() so that wrapping around the 'back' of the device is taken   *
 * into consideration and the polygon/lines generated have points that  *
 * stay consistent around the 'break'.  HOWEVER, this does not unroll   *
 * the object, but only returns the first representation of the object. *
 *									*
 * cvg_todev2 ( el, np, dx, dy, iret )					*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	Pointer to VG record structure	*
 *									*
 * Output parameters:							*
 *	*np		int		Number of points		*
 *	*dx		float		Array of device coordinates	*
 *	*dy		float		Array of device coordinates	*
 *	*iret		int		Return code			*
 *					 -4 = unknown VG record type	*
 *									*
 **									*
 * Log:									*
 * S. Danz/AWC		03/06	Copied from cvg_todev to use cgr_range 	*
 *                              instead of gtrans to handle unrolling   *
 * L. Hinson/AWC        01/12   Add SGWX_ELM and associated logic needed*
 *                              for dealing with Special Symbols and    *
 *                              Scalloped lines.                        *
 ***********************************************************************/
{
    char	grp[4];
    int		ier, ier1, qpoly, roll, nout, loglev, counter, ibkpts;
    float	lat[ MAXPTS ], lon[ MAXPTS ], xll, yll, xur, yur;
    float       *xcv, *ycv, tdens, crvscl, theta;
    int         nout2, maxpoints, i, istart, iend, cx, cy, count, r;
/*---------------------------------------------------------------------*/
   
    strcpy(grp, "CVG");
    loglev = 0;

    *iret = 0;

    if (el->hdr.closed) {
        qpoly = G_TRUE;
    } else {
        qpoly = G_FALSE;
    }

    switch ( el->hdr.vg_type)
    {
/*
 *      For point objects, just use the cvg_todev as it is today
 *      since there is no possible line/polygon for 'unrolling'
 */
        case TEXT_ELM:
        case TEXTC_ELM:
	case SPTX_ELM:
        case MARK_ELM:
        case WXSYM_ELM:
        case CTSYM_ELM:
        case ICSYM_ELM:
        case PTSYM_ELM:
        case PWSYM_ELM:
        case SKSYM_ELM:
        case SPSYM_ELM:
        case TBSYM_ELM:
        case CMBSY_ELM:
	case BARB_ELM:
	case ARROW_ELM:
	case DARR_ELM:
	case HASH_ELM:
        case VOLC_ELM:
            cvg_todev (el, np, dx, dy, iret);
	    break;

	case FRONT_ELM:

	    *np = el->elem.frt.info.numpts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, el->elem.frt.latlon,
                        &(el->elem.frt.latlon[*np]),
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case CIRCLE_ELM:

	    *np = el->elem.cir.info.numpts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, el->elem.cir.data.latlon,
                        &(el->elem.cir.data.latlon[*np]),
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case SPLN_ELM:

	    *np = el->elem.spl.info.numpts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, el->elem.spl.latlon,
                        &(el->elem.spl.latlon[*np]),
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case LINE_ELM:

            *np = el->elem.lin.info.numpts;
/*
 *          Convert all points in element to device...
 */
            cgr_range ( sys_M, np, el->elem.lin.latlon,
                        &(el->elem.lin.latlon[*np]),
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
            break;

	case WBOX_ELM:

	    *np = el->elem.wbx.info.numpts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, el->elem.wbx.latlon,
                        &(el->elem.wbx.latlon[*np]),
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case TRKSTORM_ELM:

	    *np = el->elem.trk.info.npts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, &el->elem.trk.latlon[0],
                        &el->elem.trk.latlon[*np],
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;


	case SIGAIRM_ELM:
	case SIGCONV_ELM:
	case SIGINTL_ELM:
	case SIGNCON_ELM:
	case SIGOUTL_ELM:

	    *np = el->elem.sig.info.npts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, &el->elem.sig.latlon[0],
                        &el->elem.sig.latlon[*np],
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case SIGCCF_ELM:

	    *np = el->elem.ccf.info.npts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, &el->elem.ccf.latlon[0],
                        &el->elem.ccf.latlon[*np],
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case ASHCLD_ELM:

	    *np = el->elem.ash.info.npts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, &el->elem.ash.latlon[0], 
                        &el->elem.ash.latlon[*np],
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case LIST_ELM:

	    *np = el->elem.lst.data.nitems;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, &el->elem.lst.data.lat[0],
                        &el->elem.lst.data.lon[0],
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case JET_ELM:

	    *np = el->elem.jet.line.spl.info.numpts;
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, &el->elem.jet.line.spl.latlon[0],
                        &el->elem.jet.line.spl.latlon[*np],
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	case GFA_ELM:

	    *np = el->elem.gfa.info.npts;

            cgr_range ( sys_M, np, &el->elem.gfa.latlon[0],
                        &el->elem.gfa.latlon[*np],
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;
	
        case SGWX_ELM:
            *np = el->elem.sgwx.info.npts;                        
/*
 *	    Convert all points in element to device...
 */
            cgr_range ( sys_M, np, &el->elem.sgwx.latlon[0],
                        &el->elem.sgwx.latlon[*np],
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
            if (el->elem.sgwx.info.subtype == SGWX_SPSYM) {
              /* We have a point symbol, and we have to create a polygon
                 for it to make placement work.  A 4 point polygon was found
		 to not work properly with the ability to place on NE/SW
		 diagonals, So go with an 8-point stop sign polygon*/
              cx = dx[0];
              cy = dy[0];
	      /* Draw an 8-point octagan, clockwise, 
	         vertices at 45 degree intervals */
	      count = 0;
	      r = 10; /* Radius of 10 pixels */
	      for (theta = 0; theta < (TWOPI - .01); theta+=(45*DTR)) {	        
		dx[count] = cx + cos(theta)*r;
		dy[count] = cy + sin(theta)*r;
		count++;
	      }
	      *np = count;	       
            }
              
            if (el->elem.sgwx.info.subtype == SGWX_CONV ||
                el->elem.sgwx.info.subtype == SGWX_ICETURB) {
		/* We have a Scalloped Area.  Unfortunately, the vertices
		are inside the scallops, causing issues with placing
		the labels on the edges or in between vertices.  To
		compensate, we create a parametric polygon containing more
		points; and we enlarge the associated polygon registered 
		for placement. This forces the labels to place cleanly outside 
		(and inside) the scalloped areas.
		NOTE:  Calling cv_prmt will generate a polygon containing
		many more points than the originally drawn polygon.  I've seen
		up to 600 points generated on the larger polygons.  So arrays
		are set up to about 1000 points */
                /* If this is an open area, close it */
                if (el->hdr.closed == 0) 
                  el->hdr.closed = 1;
                  
                G_MALLOC(xcv, float, MAXPTS*2, "creating xcv points");
                G_MALLOC(ycv, float, MAXPTS*2, "creating ycv points");
                tdens = 5.0;
                crvscl = 25.0;
                maxpoints = MAXPTS*2;
                istart = iend = 0;
                cv_prmt(&nout, dx, dy, &tdens, &maxpoints, &crvscl, &istart, &iend, 
                  &nout2, xcv, ycv, iret);
                for (i = 0; i < nout2; i++) {
                  dx[i] = xcv[i];
                  dy[i] = ycv[i];
                }
                *np = nout2;
                /* Now enlarge polygons by 4 pixels, the size of the scalloped lines */
		/* Reference the splpat.tbl which dictates the length (diameter) of
		   the scallop is 8 pixels.  Therefore, the radius of a scallop is 4
		   pixels */
                *np = *np - 1;
                enlargePolygon(np, dx, dy, 4);                
                G_FREE(xcv, float);
                G_FREE(ycv, float);
            }
                 
	    break;           

	case TCA_ELM:

	    *np = 0;
	    for ( counter = 0; counter < el->elem.tca.info.wwNum; counter++ ) {
		for ( ibkpts = 0; ibkpts < el->elem.tca.info.tcaww[ counter ].numBreakPts; ibkpts++ ){
		    lat[ *np ] = el->elem.tca.info.tcaww[ counter ].breakPnt[ ibkpts ].lat;
		    lon[ *np ] = el->elem.tca.info.tcaww[ counter ].breakPnt[ ibkpts ].lon;
		    (*np)++;
		}
	    }
            cgr_range ( sys_M, np, lat, lon,
                        &qpoly, sys_D, &roll, &nout, 
	                dx, dy, &xll, &yll, 
                        &xur, &yur, &ier );
	    break;

	default:
	    *iret = -4;
            er_lmsg( &loglev, grp, iret, NULL, &ier1, strlen(grp));

	    break;
    }
}

static void enlargePolygon(int *npts, float *dx, float *dy, int pixels)
/************************************************************************
enlargePolygon

This routine was developed to expand the size of a polygon by x pixels.
The points are defined in device coordinates and clockwise order.

This routine makes the use of Vectors to enlarge the polygon.
 1. Calculate the Average Vector V at each Vertex Point through 
    vector normalization of the U Vectors at each vertex.  The U vector is
    calculated by differencing each point from the previous point working
    clockwise.
 2. Calculate the Vector perpendicular to each Vertex U.  Call it NU
 3. Calculate the Vector Orthogonal to V by taking the cross
    product of -k and V.  Normalize this vector as VO.
 4. The correct length of Vector Orthoganal VO is calculated by taking the
    pixel size and dividing by the dot product (dotp) of NU and VO.  
    Add this vector to each Vertex point.  We used the dot-product rule here of 
    cos(theta) = U * V, as long as U and V are normalized, where cos(theta) =
    pixels/dotp. 
 
 enlargePolygon ( np, dx, dy, pixels)
 
 Input and Output parameters :
   *np          int          Number of points
   *dx          float        Array of device coordinates
   *dy          float        Array of device coordinates
   *pixels      int          Number of Pixels to enlarge
**
 Log:
   L. Hinson/AWC    01/12    Created
*************************************************************************/
{
  int i, np;
  float vectorUi[MAXPTS*2];
  float vectorUj[MAXPTS*2];
  float vectorVi[MAXPTS*2];
  float vectorVj[MAXPTS*2];
  float vectorNUi[MAXPTS*2];
  float vectorNUj[MAXPTS*2];
  float vectorOi[MAXPTS*2];
  float vectorOj[MAXPTS*2];
  float ui, uj, uni, unj, vi, vj, oi, oj;
  float dotp, mag, magn;
  np = *npts;
  for (i = 1; i < np; i++) {
    ui = dx[i] - dx[i-1];
    uj = dy[i] - dy[i-1];
    mag = hypot(ui,uj);
    vectorUi[i-1] = ui/mag;
    vectorUj[i-1] = uj/mag;
    uni = uj;
    unj = -1 * ui;
    magn = hypot(uni,unj);
    vectorNUi[i-1] = uni/magn;
    vectorNUj[i-1] = unj/magn;    
  }
  ui = dx[0] - dx[np-1];
  uj = dy[0] - dy[np-1];
  mag = hypot(ui,uj);
  vectorUi[np-1] = ui/mag;
  vectorUj[np-1] = uj/mag;
  uni = uj;
  unj = -1*ui;
  magn = hypot(uni,unj);
  vectorNUi[np-1] = uni/magn;
  vectorNUj[np-1] = unj/magn;
  
  vi = (vectorUi[0] + vectorUi[np-1])/2;
  vj = (vectorUj[0] + vectorUj[np-1])/2;
  vectorVi[0] = vi;
  vectorVj[0] = vj;
  for (i = 1; i<np; i++) {
    vi = (vectorUi[i] + vectorUi[i-1])/2;
    vj = (vectorUj[i] + vectorUj[i-1])/2;
    vectorVi[i] = vi;
    vectorVj[i] = vj;
  }
  
  for (i = 0; i < np; i++) {
    oi = vectorVj[i];
    oj = -1*vectorVi[i];
    mag = hypot(oi,oj);
    vectorOi[i] = oi/mag;
    vectorOj[i] = oj/mag;
  }
  
  for (i = 0; i<np; i++) {
    dotp = vectorNUi[i]*vectorOi[i] + vectorNUj[i]*vectorOj[i];
    dx[i] += vectorOi[i]*pixels/dotp;
    dy[i] += vectorOj[i]*pixels/dotp;
  }
}
    
    
    
