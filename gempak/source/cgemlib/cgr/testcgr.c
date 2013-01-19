#include "geminc.h"
#include "gemprm.h"
#include "cgrcmn.h"

int main ( void )
/************************************************************************
 * TESTCGR								*
 *									*
 * This program tests the GEMLIB "CGR" functions.			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 4/97						*
 * D.W.Plummer/NCEP	 7/97	Added CGR_DIST				*
 * D. Keiser/GSC	 7/97	Added cgr_intrsct			*
 * D.W.Plummer/NCEP	 7/97	Added cgr_polyint & cgr_segint		*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * I. Durham/GSC	 5/98	Removed call to cgr_intrsct		*
 * C. Lin/EAI(S.Danz/AWC)6/98	change calling sequence to crg_inpoly	*
 * D.W.Plummer/NCEP	 6/98	Added CGR_LINDIST			*
 * D.W.Plummer/NCEP	 1/99	Added CGR_ROLSEG			*
 * D.W.Plummer/NCEP	 9/99	Change to CGR_SEGINT calling sequence	*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * M. Li/SAIC		08/01	Added CGR_INTERSECT			*
 * M. Li/SAIC		09/01	Added xout, yout, and cleanup		*
 * S. Jacobs/NCEP	11/01	Added CGR_CENTROID			*
 * A. Hardy/SAIC	 4/02   Added CGR_PTONLN			*
 * W.D.Plummer/NCEP	12/02	Add CGR_SEGDIST				*
 * R. Tian/SAIC		 5/03	Added CGR_INSERT			*
 * D.W.Plummer/NCEP	08/03	Added CGR_RANGE				*
 * D.W.Plummer/NCEP	08/03	Added CGR_REORDER and CGR_POLYUNION	*
 * D.W.Plummer/NCEP     02/04   Added CGR_POLYINTERP                    *
 * D.W.Plummer/NCEP	 1/04	Calling seq chg to CGR_RANGE		*
 * J. Wu/SAIC		09/04	Added cgr_reducePts			*
 * B. Yin/SAIC          10/04   Added smear type in CGR_POLYSMEAR       *
 * E. Safford/SAIC	10/04	added cgr_polydiff			*
 * H. Zeng/SAIC		12/04	added cgr_ordrccw			*
 * D.W.Plummer/NCEP	01/05	added cgr_lineinterp			*
 * H. Zeng/SAIC		04/05	change cgr_rolseg arguments		*
 * J. Wu/SAIC           05/05   Added  cgr_sphpolyarea       		*
 * R. Tian/SAIC         07/05   Added  cgr_vectxprod       		*
 * R. Tian/SAIC         07/05   Added  cgr_qrol       			*
 * B. Yin/SAIC		12/05	Added cgr_linepolyint			*
 * B. Yin/SAIC		01/06	Added cgr_linepoly			*
 * B. Yin/SAIC		01/06	Added cgr_linelen			*
 * J. Wu/SAIC           02/06   Added size check in cgr_reducepts       *
 * E. Safford/SAIC	04/06	added cgr_reduceLinePoly		*
 * S.Danz/AWC            2/06   Added cgr_bisectpt                      *
 * S.Danz/AWC            2/06   Added cgr_inpolywn                      *
 * S.Danz/AWC            2/06   Added cgr_csegint                       *
 * S.Danz/AWC            2/06   Added cgr_objint                        *
 * S.Danz/AWC            2/06   Added cgr_objinpoly                     *
 * E. Safford/SAIC	09/06	added cgr_segintwn			*
 * J. Wu/SAIC           10/06   modified cgr_reducepts       		*
 * D.W.Plummer/NCEP	12/06	Added tol to qrol calling seq		*
 * J. Wu/SAIC           01/07   add float input for reducePct/reduceDst	*
 ***********************************************************************/
{
int		ii, cont, mode, istat, ginit, ier;
int		iret, numsub, i;
char		select[LLSCRN], filnam[LLSCRN];
char		*defdir;
char		buffer[LLBSIZ];
FILE		*fp;

double		dbl_x[10], dbl_y[10], dbl_qx[10];
float		x[10], y[10], qx[100], qy[100];
int		irol;
float		px[100], py[100];
float		px2[100], py2[100];
float		px3[100], py3[100];
float		nx, ny;
float		x1, y1, x2, y2, fx, fy, xint, yint;
int		inout[1];
int		np, npts=0, npts2=0, npts3=0;
float		dist;
int		nv;
int		bpnt1[20], apnt1[20], bpnt2[20], apnt2[20];
int		intrsct;
int		concave;
float		xs1[2], ys1[2], xs2[2], ys2[2];
float		xout[20], yout[20];
char		sys1[2], sys2[2], sys3[2];
int		lor, aob;

int         	iunit, itype, iside;
float       	xsize, ysize;
char        	device[13], dfilnam[73];
char        	proj[80];

float       	lllat, lllon, urlat, urlon;
char        	pro[80], *ptr;
float       	prjang1, prjang2, prjang3;

int		tl, nearest_vrt, next_vrt;
float		*tx, *ty, xclo, yclo;

float		dx[100], dy[100], xcent, ycent, area;

float		ppx[100], ppy[100], qqx[100], qqy[100],
		ttx[200], tty[200];
float		dens, crvscl;
int		npp, nqq, ntt, widx[100];
char		sys[2], sys_1[2], sys_2[2], sysout[2];
int		rollout, nsys;
float		xll, yll, xur, yur;
char		syss[] = {"MWLPVNDS"};

float		pxout[400], pyout[400];
int		npo, indx[100], maxnpo;

int		nmap, nout, qpoly;
float		pct;
float		xmap1[10], ymap1[10], xmap2[10], ymap2[10];

int		reducepts, rnin, rnout;
float		rxin[500], ryin[500], rxout[500], ryout[500];
int		algchoice;
float		reducepct, reducedst;

char            smear_type[32];

float		radius, *polylat, *polylon, polyarea, crad;

int		closed, rol;

int		nptsLine, nptsPoly;
float		*xLine, *yLine, *xPoly, *yPoly;
Boolean		intersct;

Boolean		*inoutFlag;
float		*xOut, *yOut, tolerance;

char		sizeopt[64], reduceopt[256], pts_sys[8];
float		rlat[500], rlon[500];

float		linelen;

float		tol;
/*---------------------------------------------------------------------*/
    cont = G_FALSE;

    while ( cont == G_FALSE ) {
        printf ( "\n\n" );
        printf ( "   1 = CGR_INPOLY    2 = CGR_DIST      3 = CGR_POLYINT\n");
        printf ( "   4 = CGR_SEGINT    5 = CGR_LINDIST   6 = CGR_ROLSEG \n");
        printf ( "   7 = CGR_QSOL      8 = CGR_INTERSECT 9 = CGR_CENTROID\n");
        printf ( "  10 = CGR_PTONLN    11 = CGR_SEGDIST  12 = CGR_INSERT\n");
        printf ( "  13 = CGR_RANGE     14 = CGR_REORDER  15 = CGR_POLYUNION\n");
        printf ( "  16 = CGR_POLYSMEAR 17 = CGR_POLYLINK 18 = CGR_POLYINTERP\n");
        printf ( "  19 = CGR_REDUCEPTS 20 = CGR_POLYDIFF 21 = CGR_ORDRCCW\n");
        printf ( "  22 = CGR_LINEINTERP23 = CGR_SPHPOLYAREA\n");
        printf ( "  24 = CGR_VECTXPROD 25 = CGR_QROL     26 = CGR_LINEPOLYINT\n");
        printf ( "  27 = CGR_LINEPOLY  28 = CGR_LINELEN\n" );
        printf ( "  29 = CGR_REDUCELINEPOLY 30 = CGR_INPOLYWN\n");
        printf ( "  31 = CGR_CSEGINT   32 = CGR_BISECTPT 33 = CGR_OBJINT\n");
        printf ( "  34 = CGR_OBJINPOLY 35 = CGR_CONCAVE  36 = CGR_SEGINTWN\n");
        printf ( "  50 = Read in polygon or line #1 from file\n" );
        printf ( "  51 = Read in polygon or line #2 from file\n" );
        printf ( "       (files contain a list of paired points only)\n" );
        printf ( "  90 = GINITP (needed for #1, #8, #13, #27)\n" );
        printf ( "  91 = GSDEVA and GSMPRJ (str/90;-105;0 proj)\n" );
        printf ( "  92 = GSDEVA and GSMPRJ (ced/0;0;0 proj)\n\n" );
        printf ( "\n" );
        printf ( "Select a subroutine number or type EXIT: " );
        scanf ( " %s", select );
        switch ( select[0] ) {
    	    case 'e':
    	    case 'E':
    		cont = G_TRUE;
		numsub = -1;
    	    default:
    		numsub = atoi ( select );
    		break;
        }

/*---------------------------------------------------------------------*/
        if ( numsub == 1 ) {

	    printf ( "Enter the x coordinate:  " );
	    scanf ( " %f", &nx );
	    printf ( "Enter the y coordinate:  " );
	    scanf ( " %f", &ny );

	    printf ( "Enter coordinate system:  " );
	    scanf ( " %s", sys );

	    np = 1;
	    cgr_inpoly ( sys, &np, &nx, &ny, sys_1, &npts, px, py, 
						   inout, &iret );
	    for ( i = 0; i < np; i++ )  {
	        if ( inout[i] )  {
		    printf("Point (%f,%f) is inside polygon.\n", nx, ny );
	        }
	        else {
		    printf("Point (%f,%f) is not inside polygon.\n", nx, ny );
	        }
	    }

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 2 ) {

	    printf ( "Enter the x coordinate (normalized):\n" );
	    scanf ( " %f", &nx );
	    printf ( "Enter the y coordinate (normalized):\n" );
	    scanf ( " %f", &ny );

	    cgr_dist ( npts, px, py, nx, ny, &dist, &nv, &iret );

	    printf ( "\nCGR_DIST: iret = %d\n\n", iret );
	    printf("Point (%f,%f) is closest to vertex %d\n",
							nx, ny, nv );
	    printf("with the closest segment distance of %f\n", dist );

        }
/*---------------------------------------------------------------------*/
	if ( numsub == 3 ) {

	    if ( npts == 0 || npts2 == 0 ) {
	        printf ( "One or both polygons has insufficient points.\n" ) ;
	    }
	    else {
	        printf ( "Using the two polygons entered.\n" );

		cgr_polyint( sys_N, &npts, px, py, sys_N, 
			&npts2, px2, py2, &intrsct, &iret );

		if ( intrsct == 1 ) 
	            printf ( "POLYGONS INTERSECT\n" );
		else
	            printf ( "POLYGONS DO NOT INTERSECT\n" );
	    }

	}
/*---------------------------------------------------------------------*/
	if ( numsub == 4 ) {

	    printf ( "Enter sys x y x y for 1st segment (eg, N 0 0 1 1)\n" );
	    scanf ( " %s %f %f %f %f", 
		    sys1, &(xs1[0]), &(ys1[0]), &(xs1[1]), &(ys1[1]) );
	    printf ( "Enter sys x y x y for 2nd segment (eg, N 0 0 1 1)\n" );
	    scanf ( " %s %f %f %f %f", 
		    sys2, &(xs2[0]), &(ys2[0]), &(xs2[1]), &(ys2[1]) );
	    printf ( "Enter sys intersecting point \n" );
	    scanf ( " %s", sys3 );

            	cgr_segint( sys1, &(xs1[0]), &(ys1[0]), sys2, &(xs2[0]), &(ys2[0]), 
				sys3, &xint, &yint, &intrsct, &iret );

		if ( intrsct == 1 ) 
	            printf ( "SEGMENTS (%5.2f,%5.2f)-(%5.2f,%5.2f) and (%5.2f,%5.2f)-(%5.2f,%5.2f) INTERSECT.\nPoint of intersection is (%5.2f,%5.2f)\n", xs1[0], ys1[0], xs1[1], ys1[1], xs2[0], ys2[0], xs2[1], ys2[1], xint, yint );
		else
	            printf ( "SEGMENTS (%5.2f,%5.2f)-(%5.2f,%5.2f) and (%5.2f,%5.2f)-(%5.2f,%5.2f) DO NOT INTERSECT.\nPoint of intersection is (%5.2f,%5.2f)\n", xs1[0], ys1[0], xs1[1], ys1[1], xs2[0], ys2[0], xs2[1], ys2[1], xint, yint );

	}
/*---------------------------------------------------------------------*/
        if ( numsub == 5 ) {

	    printf ( "Enter the 1st x y coordinate pair of line:\n" );
	    scanf ( " %f %f", &x1, &y1 );
	    printf ( "Enter the 2nd x y coordinate pair of line:\n" );
	    scanf ( " %f %f", &x2, &y2 );
	    printf ( "Enter the x y coordinate pair of fixed point:\n" );
	    scanf ( " %f %f", &fx, &fy );

	    cgr_lindist ( x1, y1, x2, y2, fx, fy, 
			  &xint, &yint, &dist, &iret );

	    printf ( "\nCGR_LINDIST: iret = %d\n\n", iret );
	    printf("Point (%f,%f) is the intersecting point\n",
							xint, yint );
	    printf("with a distance of %f\n", dist );

        }
/*---------------------------------------------------------------------*/
	if ( numsub == 6 ) {

	    printf ( "Enter first (x,y) pair of line segment:\n" );
	    scanf ( " %f %f", &(x[0]), &(y[0]) );
	    printf ( "Enter second (x,y) pair of line segment:\n" );
	    scanf ( " %f %f", &(x[1]), &(y[1]) );
	    printf ( "Enter test (x,y) pair:\n" );
	    scanf ( " %f %f", &(qx[0]), &(qy[0]) );

	    cgr_rolseg( x, y, &(qx[0]), &(qy[0]), &irol, &iret );

	    printf ( "\nCGR_ROLSEG: iret = %d\n", iret );

	    if ( irol == 1 )
	        printf ( "\nPoint is right of (or on) line segment.\n" );
	    else
	        printf ( "\nPoint is left of line segment.\n" );
	}
/*---------------------------------------------------------------------*/

        if ( numsub == 7 ) {

	    if ( npts != 0 )  {

	        printf ( "Enter the x and y coordinates:\n" );
	        scanf ( " %f %f", &nx, &ny );

	        cgr_qsol ( npts, px, py, nx, ny, &lor, &aob, &iret );

	        printf ( "\nCGR_QSOL: iret = %d\n\n", iret );
	        if ( lor == -1 )  printf("Point is left and ");
	        if ( lor ==  1 )  printf("Point is right and ");
	        if ( aob == -1 )  printf("above\n");
	        if ( aob ==  1 )  printf("below\n");
	        if ( lor == 0 || aob == 0 )  printf("Point is on the line.\n");

	    }
	    else
		printf("Must input line points for line #1\n");

        }
/*---------------------------------------------------------------------*/
     	if ( numsub == 8 ) {

            printf ( "Enter sys and number of points for polygon 1\n" );
            scanf ( " %s %d", sys1, &npts );

	    printf ( "Enter %d pairs of x, y for polygon 1\n", npts);
	    for (i = 0; i < npts; i++) {
	        scanf ( "%f %f", &(px[i]), &(py[i]) ); 
	    }

	    printf ( "Enter sys and number of points for polygon 2\n" );
            scanf ( " %s %d", sys2, &npts2 );

            printf ( "Enter %d pairs of x, y for polygon 2\n", npts2);
            for (i = 0; i < npts2; i++) {
                scanf ( "%f %f", &(px2[i]), &(py2[i]) );
            }

	    printf ( "Enter expected number of intersection points:\n");
	    scanf("%d", &intrsct);
	    printf ( "Enter sys for the intersection points\n" );
	    scanf ( " %s", sys3 );

            cgr_intersect ( sys1, &npts, px, py, sys2, &npts2, px2, py2, &intrsct, sys3, 
			    &nv, xout, yout, bpnt1, apnt1, bpnt2, apnt2, &iret );

            printf ( "\nCGR_INTERSECT: iret = %d\n\n", iret );
            if ( iret == 0 ) {
		printf("The polygons have %d intersection points\n", nv);
		for (i = 0; i < nv; i++) {
		    printf("    point %d -- (%f, %f)\n", i+1, xout[i], yout[i]);
		    printf("    between point %d and point %d of polygon1 \n", bpnt1[i], apnt1[i]);
		    printf(" &  between point %d and point %d of polygon2 \n", bpnt2[i], apnt2[i]);
 
		}
	    }
	    else {
		printf(" No intersection!\n");
	    } 

        }
/*---------------------------------------------------------------------*/
     	if ( numsub == 9 ) {

	    printf ( "Enter the number of points for the polygon (<=100)\n" );
            scanf ( "%d", &npts );

	    printf ( "Enter %d pairs of x, y for the polygon\n", npts);
	    for (i = 0; i < npts; i++) {
	        scanf ( "%f %f", &(dx[i]), &(dy[i]) ); 
	    }

	    cgr_centroid ( dx, dy, &npts, &xcent, &ycent, &area, &iret );

            printf ( "\nCGR_CENTROID: iret = %d\n\n", iret );

	    printf ( "X Centroid      = %12.5f\n", xcent );
	    printf ( "Y Centroid      = %12.5f\n", ycent );
	    printf ( "Area of Polygon = %12.5f\n\n", area );

        }
/*---------------------------------------------------------------------*/
     	if ( numsub == 10 ) {

            printf ( "Enter the 1st x y coordinate pair of line:\n" );
	    scanf ( " %f %f", &x1, &y1 );
	    printf ( "Enter the 2nd x y coordinate pair of line:\n" );
	    scanf ( " %f %f", &x2, &y2 );
	    printf ( "Enter the x y coordinate pair of test point:\n" );
	    scanf ( " %f %f", &fx, &fy );

	    cgr_ptonln ( x1, y1, x2, y2, fx, fy, &iside, &iret );

            printf ( "\nCGR_PTONLN: iret = %d\n\n", iret );
	    if ( iside == 0 ) {
	        printf ( "iside = %d  - Point is not on the line. \n", iside);
	    }
	    else if ( iside == 1 ) {
	        printf ( "iside = %d  - Point is on the open ray", iside);
	        printf ( " left of the first point. \n");
	    }
	    else if ( iside == 2 ) {
	        printf ( "iside = %d  - Point is on the line ", iside);
	        printf ( "inbetween the two points. \n");
	    }
	    else if ( iside == 3 ) {
	        printf ( "iside = %d  - Point is on the open ray", iside);
	        printf ( " right of the second point. \n");
	    }

	}
/*---------------------------------------------------------------------*/
     	if ( numsub == 11 ) {

	    printf ( "Enter the line to test (1 or 2):\n" );
	    scanf ( " %d", &tl );
	    printf ( "Enter the x y coordinate pair of test point:\n" );
	    scanf ( " %f %f", &fx, &fy );

	    switch ( tl )  {
		case 	1:
		    tx = px;
		    ty = py;
	    	    np = npts;
		    break;
		case	2:
		    tx = px2;
		    ty = py2;
	    	    np = npts2;
		    break;
	    }
	    cgr_segdist ( &np, tx, ty, &fx, &fy,
                          &dist, &nearest_vrt, &next_vrt,
                          &xclo, &yclo, &iret );

            printf ( "\nCGR_SEGDIST: iret = %d\n\n", iret );
	    printf ( "\tdistance = %f\n\tnearest vertex = %d, next vertex = %d\n\tclosest point is (%f,%f)\n",	
		dist, nearest_vrt, next_vrt, xclo, yclo );

	}
/*---------------------------------------------------------------------*/
        if ( numsub == 12 ) {
            ntt = 200;
	    dens = 5.0F;
	    crvscl = 30.0F;

            printf ( "Enter an ordered sequence of points.\n");
            printf ( "Number of points ( < 100 ):");
            scanf  ( "%d", &npp);
            printf ( "Enter point coordinates x/y seperated by space.\n");
            for ( i = 0; i < npp; i++) {
                printf ( "Point %d: ", i+1);
                scanf  ( "%f %f", &ppx[i], &ppy[i]);
            }

            printf ( "Enter a sequence of points intended for insertion.\n");
            printf ( "Number of points ( < 100 ):");
            scanf  ( "%d", &nqq);
            printf ( "Enter point coordinates x/y seperated by space.\n");
            for ( i = 0; i < nqq; i++) {
                printf ( "Point %d: ", i+1);
                scanf  ( "%f %f", &qqx[i], &qqy[i]);
            }

            cgr_insert ( ppx, ppy, npp, qqx, qqy, nqq, dens, crvscl,
			 ttx, tty, &ntt, widx, &iret );

	    if ( iret != 0 ) {
		printf ( "Could not perform parametric curve fit.\n");
	    }
	    else {
                printf ( "New sequence of points:\n");
                for ( i = 0; i < ntt; i++ ) {
                    printf( "Point %d:\t%f\t%f\n", i+1, ttx[i], tty[i]);
                }

                printf ( "Inserted position: ");
                for ( i = 0; i < nqq; i++ ) {
                    printf ( "%d ", widx[i] );
                }
                printf ( "\n" );
	    }
        }
/*---------------------------------------------------------------------*/
        if ( numsub == 13 ) {

	  if ( npts < 1 )  {
	    printf("\nMust first input a set of points via option 50.\n");
	  }
	  else  {

	    printf ( "Enter the coordinate system for range "
		    "(A for all - %s): ", syss);
	    scanf ( " %s", sysout );
	    cst_lcuc ( sysout, sysout, &ier );

            printf ( "Enter flag for polygon pole circumscription check (0-false or 1-true): ");
            scanf  ( "%d", &qpoly);

	    if ( strcmp(sysout,"A") != 0 )  {
                cgr_range ( sys_1, &npts, px, py, &qpoly, sysout,
			    &rollout, &nout, px, py, 
			    &xll, &yll, &xur, &yur, &iret );
		printf("IRET=%d, rollout=%d, nout=%d, range (%s) = "
			"(%8.3f,%8.3f) - (%8.3f,%8.3f)\n", 
			iret, rollout, nout, sys_1, xll, yll, xur, yur );
	    }
	    else  {

		nsys = sizeof(syss) / sizeof(char) - 1;
		sysout[1] = '\0';
		for ( ii = 0; ii < nsys; ii++ )  {
		    sysout[0] = syss[ii];
                    cgr_range ( sys_1, &npts, px, py, &qpoly, sysout,
			    &rollout, &nout, px2, py2, 
			    &xll, &yll, &xur, &yur, &iret );
		    printf("IRET=%d, rollout=%d, nout=%d, range (%s) = "
			    "(%8.3f,%8.3f) - (%8.3f,%8.3f)\n", 
			    iret, rollout, nout, sysout, xll, yll, xur, yur );
		}

	    }

	  }

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 14 ) {

	    if ( npts > 0 )  {
	        printf("Previous polygon #1:\n");
	        for ( ii = 0; ii < npts; ii++ )
		    printf("ii=%d, (%6.2f,%6.2f)\n", ii, px[ii], py[ii] );

	        cgr_reorder ( &npts, px, py, indx, &ier );

	        printf("IRET=%d\n", iret );
	        for ( ii = 0; ii < npts; ii++ )  {
		    ppx[ii] = px[indx[ii]];
		    ppy[ii] = py[indx[ii]];
	        }
	        for ( ii = 0; ii < npts; ii++ )  {
		    px[ii] = ppx[ii];
		    py[ii] = ppy[ii];
	        }
	        for ( ii = 0; ii < npts; ii++ )
		    printf("ii=%d, (%6.2f,%6.2f)\n", 
			ii, px[ii], py[ii] );
	    }

	    if ( npts2 > 0 )  {
	        printf("Previous polygon #2:\n");
	        for ( ii = 0; ii < npts2; ii++ )
		    printf("ii=%d, (%6.2f,%6.2f)\n", ii, px2[ii], py2[ii] );

	        cgr_reorder ( &npts2, px2, py2, indx, &ier );

	        printf("IRET=%d\n", iret );
	        for ( ii = 0; ii < npts2; ii++ )  {
		    ppx[ii] = px2[indx[ii]];
		    ppy[ii] = py2[indx[ii]];
	        }
	        for ( ii = 0; ii < npts2; ii++ )  {
		    px2[ii] = ppx[ii];
		    py2[ii] = ppy[ii];
	        }
	        for ( ii = 0; ii < npts2; ii++ )
		    printf("ii=%d, (%6.2f,%6.2f)\n", 
			ii, px2[ii], py2[ii] );
	        }

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 15 ) {

/* 	    printf ( "Enter process type (0-UNION, 1-INTERSECT):"); */
/* 	    scanf ( " %d", &itype ); */

	    itype = 0;
	    if ( npts <= 2 || npts2 <= 2 )  {
		printf("Two valid polygons must be input via options 50 and 51\n");
	    }
	    else  {

	    printf ( "Processing will be computed from input polygons via option 50 and 51.\n");

	    maxnpo = 2 * sizeof(px)/sizeof(float) +
		     2 * sizeof(px2)/sizeof(float);
	    printf ( "Maximum number of output points limited to %d.\n",maxnpo);

	    cgr_init ( &ier );

	    cgr_polyunion ( &npts, px, py, &npts2, px2, py2, &maxnpo,
		        &npo, pxout, pyout, &iret );

	    printf("IRET=%d\n\n", iret );

	    cgr_done ( &ier );

	    if ( npo != 0 )  {
	      printf("The %s polygon:\n", itype==0?"UNION":"INTERSECT" );
	      for ( ii = 0; ii < npo; ii++ )  {
		printf("vertex # %2d - ( %0.1f, %0.1f )\n", 
			ii, pxout[ii], pyout[ii] );
	      }
	      for ( ii = 0; ii < npo; ii++ )  printf("%0.1f, ", pxout[ii] );
	      for ( ii = 0; ii < npo; ii++ )  printf("%0.1f, ", pyout[ii] );
	      printf("\n");
	    }
	    else  {
	      printf("The two polygons do not intersect.\n");
	    }

	    }

	    printf("\nMEMORY INFORMATION:\n\tBytes Allocated - %d\n\tBytes Freed - %d\n\t(net %d bytes)\n", nBytesAlloc, nBytesFree, nBytesAlloc-nBytesFree );

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 16 ) {

            printf ( "Enter smear type ( original or rubberband ): ");
            scanf ( " %s", smear_type );

            if ( strcasecmp ( smear_type, "original" ) == 0 ) {
	       printf ( "Enter number of mapping points: ");
	       scanf ( " %d", &nmap );
	    }

	    for ( ii = 0; ii < nmap; ii++ )  {
	        printf ( "MAP POINT #%d - Enter x and y coord of poly1: ",ii);
	        scanf ( " %f %f", &(xmap1[ii]), &(ymap1[ii]) );
	        printf ( "MAP POINT #%d - Enter x and y coord of poly2: ",ii);
	        scanf ( " %f %f", &(xmap2[ii]), &(ymap2[ii]) );
	    }

	    printf ( "Processing will be computed from input polygons.\n");

	    maxnpo = 2 * sizeof(px)/sizeof(float) +
		     2 * sizeof(px2)/sizeof(float);
	    printf ( "Maximum number of output points limited to %d.\n",maxnpo);

	    cgr_init ( &ier );

	    cgr_polysmear ( smear_type, &npts, px, py, &npts2, px2, py2,
			    &nmap, xmap1, ymap1, xmap2, ymap2,
			    &maxnpo, &npo, pxout, pyout, &iret );
										             printf("IRET=%d\n\n", iret );

	    cgr_done ( &ier );

	    printf("The SMEARED polygon:\n");
	    for ( ii = 0; ii < npo; ii++ )  {
	        printf("vertex # %2d - ( %0.1f, %0.1f )\n",
	                ii, pxout[ii], pyout[ii] );
	    }
	    for ( ii = 0; ii < npo; ii++ )  printf("%0.1f, ", pxout[ii] );
	    for ( ii = 0; ii < npo; ii++ )  printf("%0.1f, ", pyout[ii] );
	    printf("\n");

	    printf("\nMEMORY INFORMATION:\n\tBytes Allocated - %d\n\tBytes Freed - %d\n\t(net %d bytes)\n", nBytesAlloc, nBytesFree, nBytesAlloc-nBytesFree );

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 17 ) {

	    if ( npts <= 2 || npts2 <= 2 )  {
		printf("Two valid polygons must be input via options 50 and 51\n");
	    }
	    else  {

	    printf ( "Processing will be computed from input polygons via option 50 and 51.\n");

	    maxnpo = 2 * sizeof(px)/sizeof(float) +
		     2 * sizeof(px2)/sizeof(float);
	    printf ( "Maximum number of output points limited to %d.\n",maxnpo);

	    cgr_init ( &ier );

	    cgr_polylink ( &npts, px, py, &npts2, px2, py2, &maxnpo,
		        &npo, pxout, pyout, &iret );

	    cgr_done ( &ier );

	    printf("IRET=%d\n\n", iret );
	    if ( npo != 0 )  {
	      printf("The LINKED polygon:\n");
	      for ( ii = 0; ii < npo; ii++ )  {
		printf("vertex # %2d - ( %0.1f, %0.1f )\n", 
			ii, pxout[ii], pyout[ii] );
	      }
	      for ( ii = 0; ii < npo; ii++ )  printf("%0.1f, ", pxout[ii] );
	      for ( ii = 0; ii < npo; ii++ )  printf("%0.1f, ", pyout[ii] );
	      printf("\n");
	    }

	    }

	    printf("\nMEMORY INFORMATION:\n\tBytes Allocated - %d\n\tBytes Freed - %d\n\t(net %d bytes)\n", nBytesAlloc, nBytesFree, nBytesAlloc-nBytesFree );

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 18 ) {

	    printf ( "Enter interpolation percentage (e.g., 0.5): ");
	    scanf ( " %f", &pct ); 

	    printf ( "Enter number of mapping points: ");
	    scanf ( " %d", &nmap ); 

	    for ( ii = 0; ii < nmap; ii++ )  {
		printf ( "MAP POINT #%d - Enter x and y coord of poly1: ",ii);
		scanf ( " %f %f", &(xmap1[ii]), &(ymap1[ii]) );
		printf ( "MAP POINT #%d - Enter x and y coord of poly2: ",ii);
		scanf ( " %f %f", &(xmap2[ii]), &(ymap2[ii]) );
	    }

	    if ( npts <= 2 || npts2 <= 2 )  { 
	        printf("Two valid polygons (number of points > 2) must be input via options 50 and 51\n");
	    }
	    else  {

	      printf ( "Processing will be computed from input polygons via option 50 and 51.\n");

              maxnpo = 2 * sizeof(px )/sizeof(float) +
	               2 * sizeof(px2)/sizeof(float);
	      printf ( "Maximum number of output points limited to %d.\n",maxnpo);

	      cgr_init ( &ier );

	      cgr_polyinterp ( &npts, px, py, &npts2, px2, py2, &pct, 
		            &nmap, xmap1, ymap1, xmap2, ymap2, 
			    &maxnpo, &npo, pxout, pyout, &iret );
	      printf("IRET=%d\n\n", iret );

              cgr_done ( &ier ); 

              printf("The INTERPOLATED polygon:\n");
              for ( ii = 0; ii < npo; ii++ )  { 
		printf("vertex # %2d - ( %0.2f, %0.2f )\n", 
			ii, pxout[ii], pyout[ii] ); 
	      }
              for ( ii = 0; ii < npo; ii++ )  printf("%0.2f, ", pxout[ii] );
              for ( ii = 0; ii < npo; ii++ )  printf("%0.2f, ", pyout[ii] );
              printf("\n");

              printf("\nMEMORY INFORMATION:\n\tBytes Allocated - %d\n\tBytes Freed - %d\n\t(net %d bytes)\n", nBytesAlloc, nBytesFree, nBytesAlloc-nBytesFree );

            }

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 19 ) {
	    printf ( "Enter the coordinate system for the points\n" );
            scanf ( " %s", pts_sys );

	    printf ( "Enter the number of points for the polygon (<=500)\n" );
            scanf ( "%d", &rnin );

	    printf ( "Enter %d pairs of x, y for the polygon\n", rnin);
	    for (i = 0; i < rnin; i++) {
	        scanf ( "%f %f", &(rlat[i]), &(rlon[i]) ); 
	    }
	    
	    gtrans( sys_M, sys_D, &rnin, rlat, rlon, rxin, ryin, &ier,
	            strlen(sys_M), strlen(sys_D) );

	    printf ( "Enter the choice of reduction algorithm:\n" );
	    printf ( "(Any input other than 1, 2, 3 will be defaulted to 3 here)\n" );
	    printf ( "	1 - Based on angle\n" );
	    printf ( "	2 - Based on size increase\n" );
	    printf ( "	3 - Based on percentage of size increase + distance\n" );
            scanf ( "%d", &algchoice );
            
	    if ( algchoice < 1 || algchoice > 3 ) {
	        algchoice = 3;
	    }
	    
	    sprintf ( reduceopt, "<alg_choice>%d</alg_choice>", algchoice );
	    
	    if ( algchoice == 1 || algchoice == 2 ) {	    
	        printf ( "Enter the desired number of points after reduction:\n" );
                scanf ( "%d", &reducepts );
	        
		sprintf ( sizeopt, "<reduce_num>%d</reduce_num>", reducepts );
                strcat( reduceopt, sizeopt );
	    }
	    else {
	        printf ( "Enter the maximum percentage allowed:\n" );
                scanf ( "%f", &reducepct );
		
		printf ( "Enter the maximum distance allowed:\n" );
                scanf ( "%f", &reducedst );
		
		sprintf ( sizeopt, "<incr_pct>%10.2f</incr_pct>", reducepct );
                strcat( reduceopt, sizeopt );
		sprintf ( sizeopt, "<incr_dst>%10.2f</incr_dst>", reducedst );
                strcat( reduceopt, sizeopt );
	    }
	    
	    sprintf ( sizeopt, "<coord_sys>%s</coord_sys>", pts_sys );
	    strcat ( reduceopt, sizeopt );
	    	    
	    cgr_reducePts ( reduceopt, rnin, rxin, ryin, 
	                    NULL, &rnout, rxout, ryout, NULL, &iret );
	    
            printf("The original polygon:\n");
            for ( ii = 0; ii < rnin; ii++ )  { 
		printf("Vertex # %3d - ( %0.2f, %0.2f )\n", ii, rxin[ii], ryin[ii] ); 
	    }
	    
            printf ( "\n\nCGR_REDUCEPTS: iret = %d\n\n", iret );
	    
	    if ( iret != 0 ) {
	        printf ( "No reduction performed because ");	        
	        if ( iret == 1 ) {
		    printf ( "the number of points after reduction\nshould be");
		    printf ( " less than the original number of points in polygon.\n");
		}
		else if ( iret == -1 ) {
		    printf ( "the number of points after reduction\nshould be no less than 3.\n");
		}
		else if ( iret == -2 ) {
		    printf ( "at least 3 points are required to\nform a polygon.\n");
		}
		else if ( iret == -3 ) {
		    printf ( "Bad algorithm choice, must be 1, 2, or 3\n");
		}
		else if ( iret == -4 ) {
		    printf ( "Bad increase percentage or distance.\n  Both must be > 0\n");
		}
	    }
	    else {
	        printf ( "The number of points has been reduced from %d to %d\n\n",
		          rnin, rnout );	        	        
                printf("The reduced polygon:\n");
                for ( ii = 0; ii < rnout; ii++ )  { 
		    printf ( "Vertex # %3d - ( %0.2f, %0.2f )\n", 
		            ii, rxout[ii], ryout[ii] ); 
	        }	    
	    }	    	    

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 20 ) {

	    if ( npts <= 2 || npts2 <= 2 )  {
		printf("Two valid polygons must be input via options 50 and 51\n");
	    }
	    else  {

	    printf ( "Processing will be computed from input polygons via option 50 and 51.\n");

	    maxnpo = 2 * sizeof(px)/sizeof(float) +
		     2 * sizeof(px2)/sizeof(float);
	    printf ( "Maximum number of output points limited to %d.\n",maxnpo);

	    cgr_init ( &ier );

	    cgr_polydiff ( &npts, px, py, &npts2, px2, py2, &maxnpo,
		        &npo, pxout, pyout, &iret );

	    printf("IRET=%d\n\n", iret );

	    cgr_done ( &ier );

	    if ( npo != 0 )  {
	      printf("The DIFF polygon:\n" );
	      for ( ii = 0; ii < npo; ii++ )  {
		printf("vertex # %2d - ( %0.1f, %0.1f )\n", 
			ii, pxout[ii], pyout[ii] );
	      }
	      for ( ii = 0; ii < npo; ii++ )  printf("%0.1f, ", pxout[ii] );
	      for ( ii = 0; ii < npo; ii++ )  printf("%0.1f, ", pyout[ii] );
	      printf("\n");
	    }
	    else if( iret == 1 ) {
	      printf("Polygon 1 is completely contained within polygon 2.\n");
            }
	    else  {
	      printf("Error on processing.\n");
	    }

	    }

	    printf("\nMEMORY INFORMATION:\n\tBytes Allocated - %d\n\tBytes Freed - %d\n\t(net %d bytes)\n", nBytesAlloc, nBytesFree, nBytesAlloc-nBytesFree );

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 21 ) {

	    if ( npts > 0 )  {
	        printf("Original list of points #1:\n");
	        for ( ii = 0; ii < npts; ii++ )
		    printf("ii=%d, (%6.2f,%6.2f)\n", ii, px[ii], py[ii] );

	        cgr_ordrccw ( npts, px, py, &ier );

	        printf("IRET=%d\n", iret );
	        for ( ii = 0; ii < npts; ii++ )
		    printf("ii=%d, (%6.2f,%6.2f)\n", 
			ii, px[ii], py[ii] );
	    }

	    if ( npts2 > 0 )  {
	        printf("Previous polygon #2:\n");
	        for ( ii = 0; ii < npts2; ii++ )
		    printf("ii=%d, (%6.2f,%6.2f)\n", ii, px2[ii], py2[ii] );

	        cgr_ordrccw ( npts2, px2, py2, &ier );

	        printf("IRET=%d\n", iret );
	        for ( ii = 0; ii < npts2; ii++ )
		    printf("ii=%d, (%6.2f,%6.2f)\n", 
			ii, px2[ii], py2[ii] );
	        }

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 22 ) {

	    printf ( "Enter interpolation percentage (e.g., 0.5): ");
	    scanf ( " %f", &pct ); 

	    if ( npts <= 2 || npts2 <= 2 )  { 
	        printf("Two valid lines (number of points > 2) must be input via options 50 and 51\n");
	    }
	    else  {

	      printf ( "Processing will be computed from input lines via option 50 and 51.\n");

              maxnpo = 2 * sizeof(px )/sizeof(float) +
	               2 * sizeof(px2)/sizeof(float);
	      printf ( "Maximum number of output points limited to %d.\n",maxnpo);

	      cgr_init ( &ier );

	      cgr_lineinterp ( &npts, px, py, &npts2, px2, py2, &pct, 
			    &maxnpo, &npo, pxout, pyout, &iret );
	      printf("IRET=%d\n\n", iret );

              cgr_done ( &ier ); 

              printf("The INTERPOLATED line:\n");
              for ( ii = 0; ii < npo; ii++ )  { 
		printf("point # %2d - ( %0.2f, %0.2f )\n", 
			ii, pxout[ii], pyout[ii] ); 
	      }
              for ( ii = 0; ii < npo; ii++ )  printf("%0.2f, ", pxout[ii] );
              for ( ii = 0; ii < npo; ii++ )  printf("%0.2f, ", pyout[ii] );
              printf("\n");

              printf("\nMEMORY INFORMATION:\n\tBytes Allocated - %d\n\tBytes Freed - %d\n\t(net %d bytes)\n", nBytesAlloc, nBytesFree, nBytesAlloc-nBytesFree );

            }

        }
/*---------------------------------------------------------------------*/
     	if ( numsub == 23 ) {

	    printf ( "Enter the radius of the sphere ( any non-positive " );
	    printf ( "number will be \ndefaulted as the earth's radius - 3956.5466 mile )\n" );
            scanf ( "%f", &crad );
 	    if ( crad <= 0 ) {
	        printf ( "\nWarning: the Earth's radius will be used!\n\n" );
	    }

	    printf ( "Enter the number of points for the polygon\n" );
            scanf ( "%d", &npts );

	    G_MALLOC ( polylat, float, npts, "CGRSPHPLOYAREA");
	    G_MALLOC ( polylon, float, npts, "CGRSPHPLOYAREA");
	    printf ( "Enter %d pairs of lat/lons for the polygon\n", npts );
	    for (i = 0; i < npts; i++) {
	        scanf ( "%f %f", &(polylat[i]), &(polylon[i]) ); 
	    }
	    
	    radius = crad;
	    cgr_sphpolyarea ( &npts, polylat, polylon, &radius, &polyarea, &iret );

            printf ( "\nCGR_SPHPOLYAREA: iret = %d\n\n", iret );
	    if ( crad > 0 ) {
	        printf ( "Area of Polygon = %f\n\n", polyarea );
	    }
	    else {
	        printf ( "Area of Polygon = %f sq. meters (%f sq. miles)\n\n", 
		          polyarea, polyarea * M2SM * M2SM );
            }
	    
	    G_FREE ( polylat, float );
	    G_FREE ( polylon, float );

        }
/*---------------------------------------------------------------------*/
	if ( numsub == 24 ) {
	    printf ( "Enter three dimensional vector A: " );
	    scanf ( "%lf %lf %lf", &dbl_x[0], &dbl_x[1], &dbl_x[2] );
	    printf ( "Enter three dimensional vector B: " );
	    scanf ( "%lf %lf %lf", &dbl_y[0], &dbl_y[1], &dbl_y[2] );
	    cgr_vectxprod ( dbl_x, dbl_y, dbl_qx, &iret );
	    printf ( "Cross Product Vector: %lf %lf %lf\n", 
		    dbl_qx[0], dbl_qx[1], dbl_qx[2] );
	}
/*---------------------------------------------------------------------*/
	if ( numsub == 25 ) {
	    printf ( "Enter the line to test (1 or 2):\n" );
	    scanf ( " %d", &tl );
	    printf ( "Tell if the line is closed (1 = yes, 0 = no):\n" );
	    scanf ( " %d", &closed );
	    printf ( "Enter the x y coordinate pair of test point:\n" );
	    scanf ( " %f %f", &fx, &fy );
	    printf ( "Enter the acceptable tolerance:\n" );
	    scanf ( " %f", &tol );

	    switch ( tl )  {
		case 	1:
		    tx = px;
		    ty = py;
	    	    np = npts;
		    break;
		case	2:
		    tx = px2;
		    ty = py2;
	    	    np = npts2;
		    break;
	    }
	    cgr_qrol ( &np, tx, ty, &closed, &fx, &fy, &tol, &rol, &iret );

            printf ( "\nCGR_QROL: iret = %d\n\n", iret );
	    switch ( rol ) {
	        case 1:
		    printf ( "Right to the line.\n" );
		break;

	        case 0:
		    printf ( "On the line.\n" );
		break;

	        case -1:
		    printf ( "Left to the line.\n" );
		break;
	    }
	}
/*---------------------------------------------------------------------*/
	if ( numsub == 26 ) {
	    printf ( "Enter the Coordinate system of the line to test:\n" );
	    scanf ( " %s", sys_1 );
	    printf ( "Enter the number of points in the line:\n" );
	    scanf ( " %d", &nptsLine );

	    G_MALLOC ( xLine, float, nptsLine, "CGRLINEPOLYINT" );
	    G_MALLOC ( yLine, float, nptsLine, "CGRLINEPOLYINT" );

	    printf ( "Enter the x y coordinate pairs of points in the line:\n" );
	 
            for ( ii = 0; ii < nptsLine; ii++ ) { 
		printf( "Point %d\n", ii + 1 );
	        scanf ( " %f %f", &xLine[ii], &yLine[ii] );
	    }

	    printf ( "Enter the Coordinate system of the polygon to test:\n" );
	    scanf ( " %s", sys_2 );
	    printf ( "Enter the number of points in the polygon:\n" );
	    scanf ( " %d", &nptsPoly );

	    G_MALLOC ( xPoly, float, nptsPoly, "CGRLINEPOLYINT" );
	    G_MALLOC ( yPoly, float, nptsPoly, "CGRLINEPOLYINT" );

	    printf ( "Enter the x y coordinate pairs of points in the polygon:\n" );

            for ( ii = 0; ii < nptsPoly; ii++ ) { 
		printf( "Point %d\n", ii + 1 );
	        scanf ( " %f %f", &xPoly[ii], &yPoly[ii] );
	    }

	   if ( nptsLine >= 2 && nptsPoly >=3 ) {
	      cgr_linepolyint( sys_1, nptsLine, xLine, yLine, 
			       sys_2, nptsPoly, xPoly, yPoly,
			       &intersct, &ier );
	      printf ( "CRG_LINEPOLYINT: %d\n", ier );
	   
	      if ( intersct ) {
	         printf ( "The line intersects the polygon.\n" );
	      }
	      else {
	         printf ( "The line does not intersect the polygon.\n" );
	      }
	   }
	   else {
		printf ( "The line or the polygon are not complete.\n" );
	   }

	   G_FREE ( xLine, float );
	   G_FREE ( yLine, float );
	   G_FREE ( xPoly, float );
	   G_FREE ( yPoly, float );

	}
/*---------------------------------------------------------------------*/
	if ( numsub == 27 ) {

	    if ( npts < 2 || npts2 < 3) {

		printf ( "\nPlease run option 50 and 51 (input in map coordinates) to read a line and a polygon from files.\n" );

	    }

	    else {

	        cgr_linepoly ( npts, px, py, npts2, px2, py2,
			       &nout, &xOut, &yOut, &inoutFlag, &ier );

	        printf ( "CRG_LINEPOLY: %d\n", ier );
	   
	        if ( ier == 0 ) {
		
		    printf ( "Lat/x\t Lon/y   In/Out\n" );

		    for ( ii = 0; ii < nout; ii++ ) {

		        printf ( "%6.2f\t %6.2f    %s\n", xOut[ ii ], yOut[ ii ],
		    	         ( inoutFlag[ ii ] ) ? "In" : "Out" );

		   }
	        }

	   	G_FREE ( xOut, float );
	   	G_FREE ( yOut, float );
		G_FREE ( inoutFlag, Boolean );

	    }

	}
/*---------------------------------------------------------------------*/
        if ( numsub == 28 ) {
	   
	    printf ( "Enter the number of points on the line:\n" );
	    scanf ( " %d", &nptsLine );

	    G_MALLOC ( xLine, float, nptsLine, "CGRLINELEN" );
	    G_MALLOC ( yLine, float, nptsLine, "CGRLINELEN" );

	    printf ( "Enter the lat/lon pairs of points on the line:\n" );

	    for ( ii = 0; ii < nptsLine; ii++ ) {

	        printf( "Point %d\n", ii + 1 );
		scanf ( " %f %f", &xLine[ii], &yLine[ii] );

	    }

            if ( nptsLine >= 2  ) {

	        cgr_linelen( xLine, yLine, nptsLine, &linelen, &ier );
		printf ( "\nCRG_LINELEN: %d\n", ier );

		if ( ier == 0 ) {

		    printf ( "The line length is: %10.2f NM\n", linelen );

		}

	    }
	    else {

	        printf ( "The number of points are less than 2.\n" );

	    }

	    G_FREE ( xLine, float );
	    G_FREE ( yLine, float );

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 29 ) {
	    printf ( "Enter the number of points for the line/polygon (<=500)\n" );
            scanf ( "%d", &rnin );

	    printf ( "Enter %d pairs of x, y for the line/polygon\n", rnin);
	    for (i = 0; i < rnin; i++) {
	        scanf ( "%f %f", &(rxin[i]), &(ryin[i]) ); 
	    }

	    printf ( "Enter the tolerance factor (> 0.0F): \n" );
            scanf ( "%f", &tolerance );

	    cgr_reduceLinePoly ( rnin, rxin, ryin, tolerance, &rnout, 
	    	rxout, ryout, &iret );

            printf("The original line/polygon:\n");
            for ( ii = 0; ii < rnin; ii++ )  { 
		printf("Vertex # %3d - ( %0.1f, %0.1f )\n", ii, rxin[ii], ryin[ii] ); 
	    }
	    
            printf("The reduced line/polygon:\n");
            for ( ii = 0; ii < rnout; ii++ )  { 
		printf("Vertex # %3d - ( %0.1f, %0.1f )\n", ii, rxout[ii], ryout[ii] ); 
	    }

            printf ( "\n\nCGR_REDUCELINEPOLY: iret = %d\n\n", iret );
	   
        }
/*---------------------------------------------------------------------*/
	if ( numsub == 30 ) {

	    printf ( "Enter the x coordinate:  " );
	    scanf ( " %f", &nx );
	    printf ( "Enter the y coordinate:  " );
	    scanf ( " %f", &ny );

	    np = 1;
	    cgr_inpolywn ( np, &nx, &ny, npts, px, py, 0, inout, &iret );
	    for ( i = 0; i < np; i++ )  {
	        if ( inout[i] )  {
		    printf("Point (%f,%f) is inside polygon.\n", nx, ny );
	        }
	        else {
		    printf("Point (%f,%f) is not inside polygon.\n", nx, ny );
	        }
	    }
	}
/*---------------------------------------------------------------------*/
	if ( numsub == 31 ) {

	    printf ( "Enter x y x y for 1st segment (eg, 0 0 1 1)\n" );
	    scanf ( " %f %f %f %f", 
		    &(xs1[0]), &(ys1[0]), &(xs1[1]), &(ys1[1]) );
	    printf ( "Enter x y x y for 2nd segment (eg, 0 0 1 1)\n" );
	    scanf ( " %f %f %f %f", 
		    &(xs2[0]), &(ys2[0]), &(xs2[1]), &(ys2[1]) );

            cgr_csegint( &(xs1[0]), &(ys1[0]), &(xs2[0]), &(ys2[0]), 
			&xint, &yint, &intrsct, &iret );

	    if ( intrsct == 1 ) 
	       printf ( "SEGMENTS (%5.2f,%5.2f)-(%5.2f,%5.2f) and (%5.2f,%5.2f)-(%5.2f,%5.2f) INTERSECT.\nPoint of intersection is (%5.2f,%5.2f)\n", xs1[0], ys1[0], xs1[1], ys1[1], xs2[0], ys2[0], xs2[1], ys2[1], xint, yint );
	    else
	       printf ( "SEGMENTS (%5.2f,%5.2f)-(%5.2f,%5.2f) and (%5.2f,%5.2f)-(%5.2f,%5.2f) DO NOT INTERSECT.\nPoint of intersection is (%5.2f,%5.2f)\n", xs1[0], ys1[0], xs1[1], ys1[1], xs2[0], ys2[0], xs2[1], ys2[1], xint, yint );
	}
/*---------------------------------------------------------------------*/
	if ( numsub == 32 ) {

            printf ( "Enter the points ABC to define the angle:\n" );
            printf ( "     Enter the x y coordinate pair for point A: " );
	    scanf ( " %f %f", &(x[0]), &(y[0]) );
            printf ( "     Enter the x y coordinate pair for point B: " );
	    scanf ( " %f %f", &(x[1]), &(y[1]) );
            printf ( "     Enter the x y coordinate pair for point C: " );
	    scanf ( " %f %f", &(x[2]), &(y[2]) );
	    printf ( "Enter the distance from B to compute the result " );
	    scanf ( " %f", &dist );

            cgr_bisectpt(x, y, dist, &x1, &y1, &iret);
            printf ("Location of point along the bisecting line (%.2f, %.2f)\n", x1, y1);
	}
/*---------------------------------------------------------------------*/
	if ( numsub == 33 ) {

		cgr_objint( npts, px, py, NULL, npts2, px2, py2, NULL, &intrsct, &iret );

		if ( iret == 0 ) {
		    if ( intrsct == 1 ) 
	                printf ( "OBJECTS INTERSECT\n" );
		    else
	                printf ( "OBJECTS DO NOT INTERSECT\n" );
		} else {
                    printf ( "\nCGR_OBJINT: iret = %d\n\n", iret );
		}
	}
/*---------------------------------------------------------------------*/
	if ( numsub == 34 ) {

	    if ( npts == 0 || npts2 == 0 ) {
	        printf ( "One or both objects have insufficient points.\n" ) ;
	    }
	    else {
	        printf ( "Using the two objects entered.\n" );

		cgr_objinpoly( npts, px, py, NULL, npts2, px2, py2, NULL, &intrsct, &iret );

                if (iret == 0) {
		    if ( intrsct == 1 ) 
	                printf ( "The first object is completely inside the second\n" );
		    else
	                printf ( "The first object is NOT completely inside the second\n" );
		} else {
                    printf ( "\nCGR_OBJINPOLY: iret = %d\n\n", iret );
		}
	    }
	}
/*---------------------------------------------------------------------*/
	if ( numsub == 35 ) {

	    np = 1;
	    cgr_concave ( npts, px, py, &concave, &iret );
		if ( iret == 0 ) {
		    if ( concave == 1 ) 
	                printf ( "OBJECTS CONCAVE\n" );
		    else
	                printf ( "OBJECTS NOT CONCAVE\n" );
		} else {
                    printf ( "\nCGR_CONCAVE: iret = %d\n\n", iret );
		}
	}
/*---------------------------------------------------------------------*/
	if ( numsub == 36 ) {

	    printf ( "Enter two lat lon pairs for 1st segment (eg, 34.77 -108.90 30.25 -101.01)\n" );
	    scanf ( " %f %f %f %f", &(xs1[0]), &(ys1[0]), &(xs1[1]), &(ys1[1]) );

	    printf ( "Enter two lat lon pairs for 2nd segment\n" );
	    scanf ( " %f %f %f %f", &(xs2[0]), &(ys2[0]), &(xs2[1]), &(ys2[1]) );

            cgr_segintwn( &(xs1[0]), &(ys1[0]), &(xs2[0]), &(ys2[0]), 
				&xint, &yint, &intrsct, &iret );

	    if ( intrsct == 1 ) 
	        printf ( "\nSEGMENTS (%5.2f,%5.2f)-(%5.2f,%5.2f) and (%5.2f,%5.2f)-(%5.2f,%5.2f) INTERSECT.\nPoint of intersection is (%5.2f,%5.2f)\n", xs1[0], ys1[0], xs1[1], ys1[1], xs2[0], ys2[0], xs2[1], ys2[1], xint, yint );
	    else
	        printf ( "\nSEGMENTS (%5.2f,%5.2f)-(%5.2f,%5.2f) and (%5.2f,%5.2f)-(%5.2f,%5.2f) DO NOT INTERSECT.\nPoint of intersection is (%5.2f,%5.2f)\n", xs1[0], ys1[0], xs1[1], ys1[1], xs2[0], ys2[0], xs2[1], ys2[1], xint, yint );

	    printf("  iret = %d\n", iret );
	}
/*---------------------------------------------------------------------*/
        if ( numsub == 50 ) {

	    printf ( "Enter filename with polygon/line coords:\n");
	    scanf ( " %s", filnam );

	    printf ( "Enter the coordinate system of pts in file:\n");
	    scanf ( " %s", sys_1 );

	    defdir = NULL;
	    fp = (FILE *)cfl_ropn ( filnam, defdir, &iret );

	    if ( iret != 0 )  {
		printf("Error opening file %s\n", filnam );
	    }
	    else {

		npts = 0;
		while ( !feof(fp) ) {
		    cfl_rdln ( fp, sizeof(buffer), buffer, &iret );
		    if ( iret == 0 ) {
		        sscanf( buffer, "%f %f", &(px[npts]), &(py[npts]) );
		        printf("pt #%d -- x=%f, y=%f\n", 
			    npts, px[npts], py[npts] );
			npts++ ;
		    }
		}
		printf("There are %d points in line/polygon.\n", npts );

	    }

	    cfl_clos ( fp, &iret );

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 51 ) {

	    printf ( "Enter filename with polygon coords:\n");
	    scanf ( " %s", filnam );

	    printf ( "Enter the coordinate system of pts in file:\n");
	    scanf ( " %s", sys_2 );

	    defdir = NULL;
	    fp = (FILE *)cfl_ropn ( filnam, defdir, &iret );

	    if ( iret != 0 )  {
		printf("Error opening file %s\n", filnam );
	    }
	    else {

		npts2 = 0;
		while ( !feof(fp) ) {
		    cfl_rdln ( fp, sizeof(buffer), buffer, &iret );
		    if ( iret == 0 ) {
		        sscanf( buffer, "%f %f", &(px2[npts2]), &(py2[npts2]) );
		        printf("pt #%d -- x=%f, y=%f\n", 
			    npts2, px2[npts2], py2[npts2] );
			npts2++ ;
		    }
		}
		printf("There are %d points in line/polygon #2.\n", npts2 );

	    }

	    cfl_clos ( fp, &iret );

        }
/*---------------------------------------------------------------------*/
        if ( numsub == 52 ) {

	    printf ( "Enter filename with polygon coords:\n");
	    scanf ( " %s", filnam );

	    printf ( "Enter the coordinate system of pts in file:\n");
	    scanf ( " %s", sys_2 );

	    defdir = NULL;
	    fp = (FILE *)cfl_ropn ( filnam, defdir, &iret );

	    if ( iret != 0 )  {
		printf("Error opening file %s\n", filnam );
	    }
	    else {

		npts3 = 0;
		while ( !feof(fp) ) {
		    cfl_rdln ( fp, sizeof(buffer), buffer, &iret );
		    if ( iret == 0 ) {
		        sscanf( buffer, "%f %f", &(px3[npts3]), &(py3[npts3]) );
		        printf("pt #%d -- x=%f, y=%f\n", 
			    npts3, px3[npts3], py3[npts3] );
			npts3++ ;
		    }
		}
		printf("There are %d points in line/polygon #3.\n", npts3 );

	    }

	    cfl_clos ( fp, &iret );

        }
/*---------------------------------------------------------------------*/
            if ( numsub == 90 ) {

                mode = 1;
                ginitp ( &mode, &istat, &ier );

                ginit = 1;

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 91 ) {

              if ( ginit == 0 )  {
                printf("Must run GINITP first.\n" );
              }
              else  {
                strcpy ( device, "GN" );
		printf("device=%s\n", device );

                iunit = 1;
                strcpy ( dfilnam, "TESTCGR" );
                itype = 1;
                xsize = 500.0F;
                ysize = 500.0F;

		printf("gsdeva\n");
		printf("device=%s\n", device );
		printf("iunit=%d\n", iunit );
		printf("dfilnam=%s\n", dfilnam );
		printf("itype=%d\n", itype );
		printf("x,ysize=%f %f\n", xsize, ysize );
                gsdeva (device, &iunit, dfilnam, &itype, &xsize, &ysize, &iret,
                        strlen(device), strlen(dfilnam));

                lllat = 10.0F;
                lllon = -120.0F;
                urlat = 50.0F;
                urlon = -50.0F;
                strcpy ( proj, "str/90;-105;0" );
                strcpy ( pro, strtok ( proj, "/" ) );
                prjang1 = 0.0F;  prjang2 = 0.0F;  prjang3 = 0.0F;
                ptr = strtok(NULL,";");
                if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang1 );
                ptr = strtok(NULL,";");
                if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang2 );
                ptr = strtok(NULL,";");
                if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang3 );

                gsmprj ( pro, &prjang1, &prjang2, &prjang3,
                         &lllat, &lllon, &urlat, &urlon, &iret, strlen(proj));

              }
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 92 ) {

              if ( ginit == 0 )  {
                printf("Must run GINITP first.\n" );
              }
              else  {
                strcpy ( device, "XW" );
		printf("device=%s\n", device );

                iunit = 1;
                strcpy ( dfilnam, "TESTCGR" );
                itype = 1;
                xsize = 500.0F;
                ysize = 500.0F;

		printf("gsdeva\n");
		printf("device=%s\n", device );
		printf("iunit=%d\n", iunit );
		printf("dfilnam=%s\n", dfilnam );
		printf("itype=%d\n", itype );
		printf("x,ysize=%f %f\n", xsize, ysize );
                gsdeva (device, &iunit, dfilnam, &itype, &xsize, &ysize, &iret,
                        strlen(device), strlen(dfilnam));

                lllat = -90.0F;
                lllon = -180.0F;
                urlat =  90.0F;
                urlon = 180.0F;
                strcpy ( proj, "ced/0;0;0" );
                strcpy ( pro, strtok ( proj, "/" ) );
                prjang1 = 0.0F;  prjang2 = 0.0F;  prjang3 = 0.0F;
                ptr = strtok(NULL,";");
                if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang1 );
                ptr = strtok(NULL,";");
                if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang2 );
                ptr = strtok(NULL,";");
                if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang3 );

                gsmprj ( pro, &prjang1, &prjang2, &prjang3,
                         &lllat, &lllon, &urlat, &urlon, &iret, strlen(proj));

              }
            }
/*---------------------------------------------------------------------*/
    }
    return(0);
}
