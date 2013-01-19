#include "cvgcmn.h"
#include "vgstruct.h"
#include "drwids.h"

#define	NSIDES	6
#define	NLINES	72
#define	RADDIST	250.0F
#define	FILTER	0.3F

#define	DEBUGVGF	"rebun_debug.vgf"

void cvg_rebun ( int *ifips, int fipsin[], int *expand, int *debug,
	char *bndnam, int *npout, float *latout, float *lonout, 
        int *iret )
/************************************************************************
 * cvg_rebun								*
 *                                                                      *
 * This function accepts as input a list of county FIPS codes and	*
 * generates from these codes an NSIDES-sided polygon generally 	*
 * representing the area covered by the counties.			*
 * 'expand' is a flag indication whether to adjust the sides of the	*
 * resulting line to include all county centroids.			*
 *									*
 * Note:	GINITP, GSDEVA and GSMPRJ must be called before 	*
 *		calling this routine.					*
 *                                                                      *
 * cvg_rebun ( int nfips, int fipsin, int expand, int debug, 		*
 *             char bndnam, int npout, float latout, float lonout, 	*
 *             int iret )						*
 *                                                                      *
 * Input parameters:                                                    *
 *  *ifips   int    number of county FIPS codes (number of counties)	*
 *  fipsin[] int    array of integer fips codes				*
 *  *expand  int    expansion flag (0-FALSE,1-TRUE)			*
 *  *debug   int    debug     flag (0-FALSE,1-TRUE)			*
 *  *bndnam  char   name of the bounds file to use (ex. CNTY_BNDS)	*
 *                                                                      *
 * Output parameters:                                                   *
 *  *npout   int    number of points on output line			*
 *  *latout  float  output line latitudes				*
 *  *lonout  float  output line longitudes				*
 *  *iret    int    return code						*
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Program sequence:							*
 * 1) Get centroids for all FIPS; compute centroid of centroids (cofc).	*
 * 2) For all 360 dirs from cofc, determine farthest intersection.	*
 * 3) Generate an NLINES-pt polygon using every 360/NLINES intersection	*
 * 4) Compute the angle at each point and remove the one closest to 180	*
 * 5) Repeat 4) until NSIDES is reached.				*
 * 6) Repeat 3)-5) 360/NLINES times using an offset set of intersections*
 * 7) Now have 360/NLINES number of potential polygons; choose the one	*
 *    with fewest number of centroids outside.				*
 * 8) If no centroids outside, then done.				*
 * 9) If expand applied, move polygon segments closest to outside	*
 *    centroids to include the outside centroid.			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	11/02						*
 * W.D.Plummer/NCEP	12/02	chg call sequence of cgr_segdist	*
 * T. Piper/SAIC	02/04	Removed unused variables intrxmn and	*
 *								intrymn	*
 * m.gamazaychikov/SAIC	08/04	Extracted from rebun			*
 * J. Wu/SAIC		09/04	Use cgr_reducePts() to reduce points	*
 * A. Hardy/NCEp	 4/05	Made input ints pointers,changed intrxmx*
 *				and intrymx FLT_MAX -> RMISSD		*
 * H. Zeng/SAIC		04/05	changed cgr_rolseg arguments		*
 * J. Wu/SAIC		06/05	remove reference to LLLMXPT		*
 * J. Wu/SAIC		02/06	add parameters in cgr_reducePts		*
 * J. Wu/SAIC		10/06	change calling seq. of cgr_reducePts	*
 * E. Safford/SAIC	12/06	add coord_sys to reduceopt		* 
 ***********************************************************************/
{
int    	ii, done, offset, found, mindistptr, ier;
char	fips[MAX_CNTY][8], tag[MAX_CNTY][16];
float	cenlat[MAX_CNTY], cenlon[MAX_CNTY];
char	vgf[64], strout[128];
float	cclat, cclon, mindist;

float	lllat, lllon, urlat, urlon;

int	nsides, nlines, nlin, nl, nf, minp, maxp;
int	npts, nout, np, intrsct, one=1, nred;
float	filter, filt, *xlat, *ylon;

float	linex[360][2], liney[360][2], angle, dist, dir;
float	intrxmx[360], intrymx[360];
float	xint, yint, distmin[360], distmax[360];

float	xpoly[NLINES], ypoly[NLINES];
float	xout[NLINES], yout[NLINES];
float	xred[NLINES], yred[NLINES];

int	inout[MAX_CNTY], nin, maxnin, nvert, nrvert, nxvert, prefoffset;
float	tdist, mintdist, zlat0, zlon0, zlat1, zlon1, xz[2], yz[2];

float	px, py, xint0, yint0, xint1, yint1;

VG_DBStruct     el_lin[360/NLINES+1];

float	minlat, minlon, maxlat, maxlon;
int	rol_ll, rol_lr, rol_ul, rol_ur;
int	nfips;

char	reduceopt[256], tmpopt[128];
/*---------------------------------------------------------------------*/

    /*
    if ( debug >= 0 )  cvg_crvgf ( DEBUGVGF, &ier );
    */

    nfips = *ifips;
    nsides = NSIDES;

    clo_init ( &ier );

    gqbnd ( sys_M, &lllat, &lllon, &urlat, &urlon, &ier, strlen(sys_M) );
    clo_bsarea ( &lllat, &lllon, &urlat, &urlon, &ier );

    /*
     *  Convert integer fips to characters & get centroids.
     *  Compute centroid of centroids.
     */
    cclat = 0.0F;
    cclon = 0.0F;
    for ( ii = 0; ii < nfips; ii++ )  {
	cst_inch ( fipsin[ii], fips[ii], &ier );
	sprintf ( tag[ii], "<FIPS>%s", fips[ii] );
        clo_bstype ( bndnam, &ier );
        clo_bstag ( tag[ii], &ier );
	clo_bgcent ( &(cenlat[ii]), &(cenlon[ii]), &ier );
	cclat += cenlat[ii];
	cclon += cenlon[ii];
    }
    cclat /= (float)nfips;
    cclon /= (float)nfips;

    /*
     *  Check if (cclat,cclon) is located within a county whose FIPS 
     *  code is in the list.
     */
    clo_tqbnd ( bndnam, cclat, cclon, strout, &ier );
    if ( ier == 0 )  {
        clo_bginfo ( bndnam, 0, strout, &ier );
        found = G_FALSE;
	ii = 0;
	while ( ii < nfips && found == G_FALSE )  {
	    if ( strstr ( strout, tag[ii] ) != (char*)NULL )  {
		found = G_TRUE;
	        break;
	    }	
	    ii++;
	}
    }
    if ( ier != 0 || found == G_FALSE )  {
        /*
	 *  (cclat,cclon) not in a listed county; assign closest listed
	 *  county centroid as (cclat,cclon).
	 */
	mindist = FLT_MAX;
	for ( ii = 0; ii < nfips; ii++ )  {
	    clo_dist ( &cclat, &cclon, &one, &(cenlat[ii]), &(cenlon[ii]), 
	    		&dist, &ier );
	    if ( dist < mindist )  {
	        mindist = dist;
		mindistptr = ii;
	    }
	}
	cclat = cenlat[mindistptr];
	cclon = cenlon[mindistptr];
    }

    sprintf ( vgf, "rebundle.vgf" );

    maxnin = 0;
    mintdist = FLT_MAX;
    prefoffset = IMISSD;

    nlines = NLINES;

    /*
     *  Use RADDIST statute miles converted to meters.
     */
    dist = RADDIST * SM2M;
    for ( ii = 0; ii < 360; ii++ )  {
	linex[ii][0] = cclat;
	liney[ii][0] = cclon;
	angle = (float)ii;
	clo_dltln ( &cclat, &cclon, &dist, &angle,
		&(linex[ii][1]), &(liney[ii][1]), &ier );

	intrxmx[ii] = RMISSD;
	intrymx[ii] = RMISSD;
	distmin[ii] =  FLT_MAX;
	distmax[ii] = 0.0F;

    }

    
    /*
     *  Allocate memory space.
     */
    clo_qmxpts ( "BOUNDS", &maxp, &ier );
    G_MALLOC ( xlat, float, (maxp+1), "CVG_REBUN" );
    G_MALLOC ( ylon, float, (maxp+1), "CVG_REBUN" );
    minp = 0;
    filter = 0.0F;

    for ( nf = 0; nf < nfips; nf++ )  {

	clo_bstype ( bndnam, &ier );
        clo_bstag ( tag[nf], &ier );

	done = G_FALSE;
	ier = 0;
	while ( done == G_FALSE && ier == 0 )  {

	  clo_bgnext ( &minp, &maxp, &filter, &npts, xlat, ylon, &ier );

	  if ( ier == 0 )  {

	    clo_bgrange ( &minlat, &minlon, &maxlat, &maxlon, &ier );

	    xlat[npts] = xlat[0];
	    ylon[npts] = ylon[0];
	    npts++;

            for ( nlin = 0; nlin < 360; nlin++ )  {

	      if ( ( G_MAX(linex[nlin][0],linex[nlin][1]) < minlat ) ||
	           ( G_MAX(liney[nlin][0],liney[nlin][1]) < minlon ) ||
	           ( G_MIN(linex[nlin][0],linex[nlin][1]) > maxlat ) ||
	           ( G_MIN(liney[nlin][0],liney[nlin][1]) > maxlon ) ) {
	      }
	      	/*
		 *  Do nothing; ranges do not intersect.
		 */
	      else  {

		cgr_rolseg ( linex[nlin], liney[nlin], &minlat, &minlon, 
			&rol_ll, &ier );
		cgr_rolseg ( linex[nlin], liney[nlin], &minlat, &maxlon, 
			&rol_lr, &ier );
		cgr_rolseg ( linex[nlin], liney[nlin], &maxlat, &minlon, 
			&rol_ul, &ier );
		cgr_rolseg ( linex[nlin], liney[nlin], &maxlat, &maxlon, 
			&rol_ur, &ier );

		if ((rol_ll==0 && rol_lr==0 && rol_ul==0 && rol_ur==0)||
		    (rol_ll==1 && rol_lr==1 && rol_ul==1 && rol_ur==1)){
		}
	      	  /*
		   *  Do nothing; ranges do not intersect.
		   */
		else  {

	      	  /*
		   *  We really want to avoid this expensive loop.
		   *  Bnd area & ray might intersect; chk all segments
		   *  and save intersection if most distant.
		   */
		  for ( np = 0; np < npts-1; np++ )  {

		    cgr_segint ( sys_M, linex[nlin], liney[nlin],
				 sys_M, &(xlat[np]), &(ylon[np]), 
				 sys_M, &xint, &yint, &intrsct, &ier );

		    if ( intrsct == G_TRUE )  {
			clo_dist ( &cclat, &cclon, &one, &xint, &yint, &dist, &ier);
			if ( dist > distmax[nlin] )  {
			    distmax[nlin] = dist;
			    intrxmx[nlin] = xint;
			    intrymx[nlin] = yint;
			}
			if ( dist < distmin[nlin] )  {
			    distmin[nlin] = dist;
			}

		    }
		  }
	        }
	      }
	    }
	  }
	  else  {
	      done = G_TRUE;
	  }
	    
        }

    }

    for ( offset = 0; offset < (360/nlines); offset++ )  {

      el_lin[offset].hdr.vg_class = CLASS_LINES;
      el_lin[offset].hdr.vg_type = LINE_ELM;
      cvg_initelm ( &el_lin[offset] );
      el_lin[offset].hdr.maj_col = 5;
      el_lin[offset].hdr.closed = 0;
      el_lin[offset].elem.lin.info.lintyp = 1;
      el_lin[offset].elem.lin.info.width = 2;
      el_lin[offset].elem.lin.info.numpts = 0;

      npts = 0;
      for ( nlin = 0; nlin < nlines; nlin++ )  {

	nl = nlin * ( 360 / nlines ) + offset;
/*
if(debug>=0 && offset == 3 ) {
printf("nlin=%d, nl=%d, distmax[nl]=%6.2f\n", nlin, nl, distmax[nl] );
}
*/
	if ( distmax[nl] > 0.0F || distmax[nl] < 0.0F )  {
	    xpoly[nlin] = (intrxmx[nl]+
			   intrxmx[((nl-5)+360)%360]+
			   intrxmx[((nl+5)+360)%360])/3.0F;
	    ypoly[nlin] = (intrymx[nl]+
			   intrymx[((nl-5)+360)%360]+
			   intrymx[((nl+5)+360)%360])/3.0F;
	    el_lin[offset].elem.lin.latlon[nl]    = xpoly[nl];
	    el_lin[offset].elem.lin.latlon[nl+nl] = ypoly[nl];
	    npts++;
	}
	else  {
	}
      }
      el_lin[offset].elem.lin.info.numpts = npts;
      el_lin[offset].hdr.recsz = sizeof(VG_HdrStruct) + sizeof(LineInfo) + 
		el_lin[offset].elem.lin.info.numpts*2*sizeof(float);

/*
if(debug>=0 && offset == 3 ) {
el_lin[offset].hdr.closed = 1;
for ( np = 0; np < npts; np++ )  {
el_lin[offset].elem.lin.latlon[np]      = xpoly[np];
el_lin[offset].elem.lin.latlon[np+npts] = ypoly[np];
printf("np=%d, lat=%6.2f, lon=%6.2f\n", np, xpoly[np], ypoly[np] );
}
el_lin[offset].hdr.maj_col = 2;
cvg_writef( &(el_lin[offset]), -1, el_lin[offset].hdr.recsz, DEBUGVGF, &loc, &ier );
}
*/

      filt = FILTER;
      cv_rduc ( &npts, xpoly, ypoly, &filt, &nout, xout, yout, &ier );
/*
if(debug>=0)printf("offset=%d, npts=%d, nout=%d\n", 
offset, npts, nout );
*/

      el_lin[offset].elem.lin.info.numpts = nout;
      for ( np = 0; np < nout; np++ )  {
        el_lin[offset].elem.lin.latlon[np]      = xout[np];
        el_lin[offset].elem.lin.latlon[np+nout] = yout[np];
      }
      el_lin[offset].hdr.maj_col = 3;
      el_lin[offset].hdr.closed = 1;
      el_lin[offset].hdr.recsz = sizeof(VG_HdrStruct) + sizeof(LineInfo) + 
		el_lin[offset].elem.lin.info.numpts*2*sizeof(float);

/*
if(debug>=0 && offset == 3 ) {
el_lin[offset].hdr.maj_col = 3;
cvg_writef( &(el_lin[offset]), -1, el_lin[offset].hdr.recsz, DEBUGVGF, &loc, &ier );
for ( np = 0; np < nout; np++ )  {
printf("np=%d, lat=%6.2f, lon=%6.2f\n", np, xout[np], yout[np] );
}
}
*/
    
    /*
     *  Reduce the points to "nsides" using angle-based algorithm
     */
    sprintf(  reduceopt, "<alg_choice>1</alg_choice><reduce_num>%d</reduce_num>",
              nsides );
    sprintf ( tmpopt, "<coord_sys>%s</coord_sys>", sys_M );	  
    strcat ( reduceopt, tmpopt );
	
    cgr_reducePts ( reduceopt, nout, xout, yout, NULL, 
			&nred, xred, yred, NULL, &ier );

    if ( ier == 0 ) {
        nout = nred;
        for ( np = 0; np < nout; np++ )  {
	    xout[np] = xred[np];
	    yout[np] = yred[np];
	  }
        }

    xout[nout] = xout[0];
    yout[nout] = yout[0];
    nout++;

    for ( np = 0; np < nout; np++ )  {
        el_lin[offset].elem.lin.latlon[np     ] = xout[np];
        el_lin[offset].elem.lin.latlon[np+nout] = yout[np];
    }
    el_lin[offset].elem.lin.info.numpts = nout;

    el_lin[offset].hdr.maj_col = offset+1;
    el_lin[offset].hdr.closed = 1;
    el_lin[offset].hdr.recsz = sizeof(VG_HdrStruct) + sizeof(LineInfo) + 
		el_lin[offset].elem.lin.info.numpts*2*sizeof(float);

    cgr_inpoly ( sys_M, &nfips, cenlat, cenlon, sys_M, &nout, xout, yout,
	inout, &ier );
    nin = 0;
    tdist = 0.0F;
    for ( ii = 0; ii < nfips; ii++ )  {
        if ( inout[ii] == 1 )  {
        nin++;
        }
        else  {
            cgr_dist ( nout, xout, yout, cenlat[ii], cenlon[ii], &dist, &nrvert, &ier );
            tdist += dist;
        }
    }

    if ( nin > maxnin || ( nin == maxnin && tdist < mintdist ) )  {

        mintdist = tdist;
        prefoffset = offset;
        maxnin = nin;
    }

    if ( nin == nfips )  break;

    }

    *npout = el_lin[prefoffset].elem.lin.info.numpts;
/*
if(debug>=0)printf("PREFOFFSET=%d, npout=%d\n", prefoffset, *npout );
*/

    for ( ii = 0; ii < *npout; ii++ )  {
        latout[ii] = el_lin[prefoffset].elem.lin.latlon[ii];
        lonout[ii] = el_lin[prefoffset].elem.lin.latlon[ii+(*npout)];
    }


    if ( (maxnin != nfips) && (prefoffset != IMISSD) && (*expand == G_TRUE) )  {

        npts = *npout;

        cgr_inpoly ( sys_M, &nfips, cenlat, cenlon, sys_M, &npts, 
	    latout, lonout, inout, &ier );

        for ( ii = 0; ii < nfips; ii++ )  {
          if ( inout[ii] == 0 )  {
            cgr_inpoly ( sys_M, &nfips, &(cenlat[ii]), &(cenlon[ii]), 
			 sys_M, &npts, latout, lonout, &(inout[ii]), &ier );
            if ( inout[ii] == 0 )  {
	      cgr_segdist ( &npts, latout, lonout, &(cenlat[ii]), &(cenlon[ii]),
		&dist, &nrvert, &nxvert, &px, &py, &ier );

              clo_dist ( &(cenlat[ii]), &(cenlon[ii]), &one, &px, &py, &dist, &ier );
              clo_direct ( &(cenlat[ii]), &(cenlon[ii]), &px, &py, &dir, &ier );
	      nvert = G_MIN ( nrvert, nxvert );
	      dist *= 1.05F;
	      clo_dltln ( &(latout[nvert]), &(lonout[nvert]), &dist, &dir, 
			&zlat0, &zlon0, &ier );
	      clo_dltln ( &(latout[nvert+1]), &(lonout[nvert+1]), &dist, &dir, 
			&zlat1, &zlon1, &ier );

	      xz[0] = zlat0; xz[1] = zlat1;
	      yz[0] = zlon0; yz[1] = zlon1;
	      cgr_segint ( sys_M, xz, yz, 
	       sys_M, &(latout[(nvert-1+npts-1)%(npts-1)]), 
		      &(lonout[(nvert-1+npts-1)%(npts-1)]), 
	       sys_M, &xint0, &yint0, &intrsct, &ier );
	      cgr_segint ( sys_M, xz, yz, 
	       sys_M, &(latout[(nvert+1+npts-1)%(npts-1)]), 
		      &(lonout[(nvert+1+npts-1)%(npts-1)]), 
	       sys_M, &xint1, &yint1, &intrsct, &ier );
	      latout[nvert] = xint0;
	      lonout[nvert] = yint0;
	      latout[(nvert+1)%(npts-1)] = xint1; 
	      lonout[(nvert+1)%(npts-1)] = yint1;
	      latout[(npts-1)] = latout[0];
	      lonout[(npts-1)] = lonout[0];
	      }
	    }
	}

    }

    /*
     *  Free memory space.
     */
    G_FREE ( xlat, float );
    G_FREE ( ylon, float );
    
    *iret = 0;

    return;

}
