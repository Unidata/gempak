#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "clocmn.h"
#include "gpc.h"
#include "proto_gpc.h"

#define MAXSTN          1000

extern	CLO_t		clo;

int main (void)
/************************************************************************
 * TESTCLO								*
 *									*
 * This program tests the GEMLIB "CLO" functions.			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 4/98	Create					*
 * S. Jacobs/NCEP	 5/98	Added CLO_DLTLN				*
 * D.W.Plummer/NCEP     12/98   Rename clo_direct to clo_tdirect        *
 * D.W.Plummer/NCEP     12/98   Added clo_findcity, _closest, _tclosest	*
 * D.W.Plummer/NCEP      1/99   Added clo_tinltln; rm clo_rolseg	*
 * D.W.Plummer/NCEP      1/99   Added clo_lonin as option #5		*
 * D.W.Plummer/NCEP      2/99   Added options #11, #90 and #91		*
 * D.W.Plummer/NCEP      2/99   Added option #12			*
 * D.W.Plummer/NCEP      3/99   Added CLO_FINDCNTY and CLO_FINDSTN	*
 * D.W.Plummer/NCEP      3/99   Corrected printf statement		*
 * D.W.Plummer/NCEP      4/99   Updated for MARINE and COASTAL types	*
 * D.W.Plummer/NCEP      5/99   Added CLO_TRACK				*
 * D.W.Plummer/NCEP      7/99   Added CLO_FROM				*
 * D.W.Plummer/NCEP      8/99   Added CLO_DIRECT			*
 * A. Hardy/GSC          8/99   Added CLO_CMPWDS                      	*
 * D.W.Plummer/NCEP      9/99   Change calling sequence of clo_from	*
 * M. Li/GSC		10/99	Added CLO_CMPDIR as option #19		*
 * M. Li/GSC		10/99	Added iret to clo_direct, clo_cmpwds	*
 * M. Li/GSC		10/99	Modified clo_dist, clo_direct, 		*
 *				clo_dltln, clo_compass, clo_cmpwds to 	*
 *				be callable by FORTRAN			*
 * M. Li/GSC		10/99	Added multi-point cal. to clo_dist	*
 * A. Hardy/GSC		12/99   Added lat/lon flag to clo_from          *
 * A. Hardy/GSC		01/00   Added clo_format                        *
 * S. Law/GSC		07/00	Changed clo_track parameters		*
 * D.W.Plummer/NCEP	 8/00	Updated for revised CLO library		*
 * A. Hardy/GSC		08/00   Added clo_dddec;renamed clo_format      *
 *                              to clo_ddenc                            *
 * A. Hardy/GSC		 9/00   Fixed units range in clo_dddec,clo_ddenc*
 * A. Hardy/GSC		 9/00   Change calling sequence of clo_dddec    *
 * D.W.Plummer/NCEP	 4/01	Added new functions for bounds filling	*
 * D.W.Plummer/NCEP	 4/01	Increase size of type_prompt		*
 * F. J. Yen/NCEP	 8/01	Modified calling sequence for clo_cmpdir* 
 * D.W.Plummer/NCEP	 8/01	Rm clo_bqinfo (use cst_gtag instead)	*
 * M. Li/SAIC		 8/01	Added clo_reorder			*
 * D.W.Plummer/NCEP	10/01	Changed meaning of flag for clo_from	*
 * A. Hardy/SAIC         4/02   Added clo_cetr				*
 * J. Wu/SAIC            8/02   remove clo_cetr 			*
 * D.W.Plummer/NCEP	 9/02	Add clo_bstag and clo_bgcent		*
 * m.gamazaychikov/SAIC  9/02	Change calling sequence to clo_reorder	*
 * D.W.Plummer/NCEP	11/02	Added clo_bgrange			*
 * F. J. Yen/NCEP	01/04	Added VOR option for flag in clo_from	* 
 * B. Yin/SAIC		 9/04	Added clo_clip/clo_clipget/clo_clipdone	* 
 * m.gamazaychikov/SAIC 10/04   Added clo_fastates                      *
 * F. J. Yen/NCEP	 1/05	Added clo_blasso			*
 * F. J. Yen/NCEP	 2/05	Changed prompt for clo_blasso tag values*
 * J. Wu/SAIC            6/05   add clo_qmxpts 				*
 * B. Yin/SAIC		 9/05	add clo_stngall				*
 * B. Yin/SAIC		 1/06	add the type list for clo_from		*
 * E. Safford/SAIC	02/06	add clo_snapOnePt			*
 * H. Zeng/SAIC		02/06	added clo_findcwa			*
 * J. Wu/SAIC           10/06   add clo_cleanFmLine 			*
 * D.W.Plummer/NCEP	12/06	add clo_tmatch				*
 ***********************************************************************/
{
    int		ii, i, iret, cont, ginit;
    int		ij, nprt, nholes;
    int		numsub;
    int		num;
    int		nstn;
    int		srchtyp;
    char	select[4], type_prompt[1024];

    char	stn[12], cmpdir[8];
    float	lat, lon, lat2, lon2, lat1, lon1, distm, dir, distsm;
    float	ddistm[100];
    int		np, loctyp;

    char	qstate[8], qid[32];
    char	info[2048];
    float	plat, plon, flat[100], flon[100];
    int		qnum, maxlen, nret;
    int		npairs, npx, nclose, order[100];

    float       latx[MAXSTN], lonx[MAXSTN];
    int         cy_fips[MAXSTN];
    char        desc[MAXSTN][32], statex[MAXSTN][3];
    char        st_fips[MAXSTN][9], wfo[MAXSTN][20];
    char        id[MAXSTN*10];
    int		ncnty, npts;
    char	strout[128];

    float	latll, lonll, latur, lonur;
    int		nltln;

    float	*lons, odir;
    int		mode, istat, ier;
    int		iunit, itype, isubtyp;
    float	xsize, ysize;
    char	device[13], dfilnam[73];
    char	proj[80];

    float	lllat, lllon, urlat, urlon;
    char	pro[80], *ptr;
    float	prjang1, prjang2, prjang3;

    int		inc, ntimes;
    float	spd, lattrk[24], lontrk[24];

    fdttms_t	time1, time2, timetrk[24];

    char        wds[4], cmpwds[20];
    int         flag, nhotx;

    char	locnam[32], qwfo[12];

    char        stnamin[256], stnamout[256], faarea[8], sep[2], ptype[8];

    int         format, icol, nexp;

    int		nbnd, nparts[MAXSTN];
    char	*pin, in[80];
    char	*pbn, bn[32];
    char	bndname[MAXSTN*sizeof(bn)], bndinfo[MAXSTN*sizeof(in)];
    float	clat[MAXSTN], clon[MAXSTN];

    char	strdcd[80];

    float       latb[LLMXPT], lonb[LLMXPT];
    int		minpts, maxpts, maxnum, mxp;
    int         indx[LLMXPT];
    float	filt;

    char	bounds[80], name[80], sysp[10];
    float	*xp, *yp;
    int		nclip = 0, maxppts = 0, which;

    int         npo, nfips, fips[100];
    float       xo[LLMXPT], yo[LLMXPT];
    char        btags[10500];
    gpc_polygon union_poly;

    int		nstns, stnnm[MAXSTN], elv[MAXSTN], pri[MAXSTN];
    char	stnid[MAXSTN][9], country[MAXSTN][3], c10[MAXSTN][20];
    float	snapLat, snapLon;

    char	fmLine[STD_STRLEN * 2], cleanFmLine[STD_STRLEN * 2];

    float	tol;

/*---------------------------------------------------------------------*/
    in_bdta(&ier);

    ginit = 0;

    type_prompt[0] = '\0';

    cont = G_FALSE;

    while ( cont == G_FALSE ) {
	printf ( "\n\n" );
	printf ( "   1 = CLO_INIT      2 = CLO_TDIRECT   3 = CLO_COMPASS\n" );
	printf ( "   4 = CLO_DIST      5 = CLO_LONIN     6 = CLO_DLTLN\n" );
	printf ( "   7 = CLO_STNGALL   8 = CLO_CLOSEST   9 = CLO_TCLOSEST\n" );
	printf ( "  10 = CLO_TINLTLN  11 = CLO_TQBND    12 = CLO_BINPOLY\n" );
	printf ( "  13 = CLO_FINDNUM  14 = CLO_FINDSTN  15 = CLO_TRACK\n" );
        printf ( "  16 = CLO_FROM     17 = CLO_DIRECT   18 = CLO_CMPWDS \n" );
	printf ( "  19 = CLO_CMPDIR   20 = CLO_DDENC    21 = CLO_SORTSTN\n" );  
	printf ( "  22 = CLO_FINDDESC 23 = CLO_SORTBND  24 = CLO_REORDER\n" );
	printf ( "  25 = CLO_WHICH    26 = CLO_DDDEC    27 = CLO_BSTYPE\n" );
	printf ( "  28 = CLO_BSAREA   29 = CLO_BGNEXT   30 = CLO_BSTAG\n" );
	printf ( "  31 = CLO_BGCENT   32 = CLO_BGRANGE  33 = CLO_CLIP\n" );
	printf ( "  34 = CLO_CLIPGET  35 = CLO_CLIPDONE 36 = CLO_FASTATES\n" );
	printf ( "  37 = CLO_BLASSO   38 = CLO_QMXPTS   39 = CLO_SNAPONEPT\n" );
	printf ( "  40 = CLO_FINDCWA  41 = CLO_CLEANFMLINE 42 = CLO_TMATCH\n" );
	printf ( "  90 = GINITP (needed for #s11,12,29,33,36,37)\n" );
	printf ( "  91 = GSDEVA and GSMPRJ (needed for #s11,12,29,33,36,37)\n\n" );
	printf ( "\n" );
	printf ( "Select a subroutine number or type EXIT: " );
	scanf ( " %s", select );
	switch ( select[0] ) {
		case 'e':
		case 'E':
			cont = G_TRUE;
		default:
			numsub = atoi ( select );
			break;
	}

/*---------------------------------------------------------------------*/
	    if ( numsub == 1 ) {

		clo_init ( &iret );

    		type_prompt[0] = '\0';
		strcat( type_prompt, "\nEnter the location type (not all types are valid options for all CLO functions):\n" );
		for ( ii = 0; ii < clo.nloc; ii++ )  {
		    strcat( type_prompt, "'" );
		    strcat( type_prompt, clo.loc[ii].name );
		    strcat( type_prompt, "'" );
		    if ( ii < clo.nloc-1 )  strcat( type_prompt, "," );
		    if ( (ii+1)%5 == 0 )  strcat( type_prompt, "\n" );
		}
		strcat( type_prompt, "\n\n" );

		printf ( "\nCLO_INIT: iret = %d\n\n", iret );
		printf ( "\nNumber of locs = %d\n\n", clo.nloc );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 2 ) {

		printf ( type_prompt );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );
		printf ( "Enter latitude and longitude:\n" );
		scanf ( " %f %f", &lat, &lon );

		clo_tdirect ( locnam, lat, lon, stn, &distm, &dir, &iret );

		printf ( "\nCLO_TDIRECT: iret = %d\n", iret );

		if ( iret == 0 )  {
		    distsm = distm * 6.213712e-04F;
		    clo_compass( &dir, cmpdir, &i, &iret );
		    printf ( "\tclosest station\t\t\t= %s\n\tdistance (meters)\t\t= %-.2f\n\tdistance (statute miles)\t= %-.2f\n\tdirection (degrees from N)\t= %-.2f\n\tdirection (16-pt compass)\t= %-s\n", stn, distm, distsm, dir, cmpdir );
		}
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 3 ) {

		printf ( "Enter direction (0-360 degrees from N):\n" );
		scanf ( " %f", &dir );
		
		clo_compass( &dir , cmpdir, &i,  &iret );

		printf ( "\nCLO_COMPASS: iret = %d\n", iret );

		printf ( "\nCompass direction is %s\n", cmpdir );
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 4 ) {

		printf ( "Enter lat/lon pair of point #1:\n" );
		scanf ( " %f %f", &lat, &lon );
		printf ( "Enter number of lat/lon pairs other than point#1(<100):\n" );
		scanf ( " %d", &npx );

		if ( npx != 0 )  {
		    npairs = npx;
		    printf ( "Enter %d pairs of lat/lon :\n", npairs );
		    for ( i = 0 ; i < npairs ; i++ )  {
                        printf ( "Enter lat/lon pair of point #%d:\n", i+2 );
		        scanf ( " %f %f", &(flat[i]), &(flon[i]) );
		    }
		}
		
		clo_dist( &lat, &lon, &npx, flat, flon, ddistm, &ier );               
		if ( npx != 0 )  {
		    npairs = npx;
		    for ( i = 0 ; i < npairs ; i++ )  {
		     printf ( "\nDistance between point#1 and #%d is %f meters\n", i+2, ddistm[i] );   
		    }
		}                               
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 5 ) {

		printf ( type_prompt );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

		printf ( "Enter longitude bounds :\n" );
		scanf ( " %f %f", &lon1, &lon2 );

		loctyp = clo_which ( locnam );

                npts = clo.loc[loctyp].stn.nstn;
                lons = (float *)malloc( (size_t)npts * sizeof(float) );

		for ( ii = 0; ii < npts; ii++ )  
		    lons[ii] = clo.loc[loctyp].stn.station[ii].lon;
		
		if ( npts > 0 )  {

		    clo_lonin( lons, npts, lon1, lon2, MAXHOT, 
			       hotlist, &nhot, &iret );

		    printf ( "\nCLO_LONIN: iret = %d\n", iret );

		    printf("There are %d longitudes between %f and %f :\n", 
			    nhot, lon1, lon2 );
		    for ( i = 0; i < nhot; i++ )
                        printf("%3d  %f\n", i, lons[hotlist[i]] );

		}

		free ( lons );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 6 ) {

		printf ( "Enter a latitude longitude pair:\n" );
		scanf ( " %f %f", &lat, &lon );
		printf ( "Enter the distance in meters:\n" );
		scanf ( " %f", &distm );
		printf ( "Enter direction (0-360 degrees from N):\n" );
		scanf ( " %f", &dir );

		clo_dltln ( &lat, &lon, &distm, &dir, &lat2, &lon2, &iret );

		printf ( "\nCLO_DLTLN: iret = %d\n", iret );

		printf ( "\nNew lat/lon position: %.5f %.5f\n", lat2, lon2 );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 7 ) {

		printf ( type_prompt );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

		printf ( "Enter test point lat/lon :\n" );
		scanf ( " %f %f", &plat, &plon );

		printf ( "Enter number of closest points :\n" );
		scanf ( " %d", &nclose );

		clo_tclosest( locnam, plat, plon, nclose, &iret );

		clo_stngall ( locnam, nclose, &nstns, latx, lonx, 
				desc, statex, stnid, stnnm, country, 
				elv, pri, c10, &iret);

		printf ( "\nCLO_STNGALL: iret = %d\n", iret );
			
		if ( iret == 0 ) {

		   printf ( "\n # stnid \tstnnm \tstate \tcnty \tdesc\n" );
		   printf ( "   lat \t\tlon \tele \tpri \tcolumn10\n" );
		   for ( ii = 0; ii < nstns; ii++ )  {
		       printf("%2d %-8s \t%6d \t%2s \t%2s \t%-32s\n",
			ii+1, stnid[ii], stnnm[ii], statex[ii], country[ii], desc[ii]);
		       printf("   %-6.2f \t%-6.2f \t%-5d \t%-2d \t%-20s\n",
			latx[ii], lonx[ii], elv[ii], pri[ii], c10[ii] );

		   }
		}


	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 8 ) {

		printf ( "Enter number of lat/lon pairs (< 100; 0 to repeat same set of test points):\n" );
		scanf ( " %d", &npx );

		if ( npx != 0 )  {
		    npairs = npx;
		    printf ( "Enter %d pairs of lat/lon :\n", npairs );
		    for ( i = 0 ; i < npairs ; i++ )  {
		        scanf ( " %f %f", &(flat[i]), &(flon[i]) );
		    }
		}

		printf ( "Enter test point lat/lon :\n" );
		scanf ( " %f %f", &plat, &plon );

		printf ( "Enter number of closest points :\n" );
		scanf ( " %d", &nclose );

		clo_closest( flat, flon, npairs, plat, plon, nclose, 
			      order, &iret );

		printf ( "\nCLO_CLOSEST: iret = %d\n", iret );

		printf("\nOUTPUT ORDER (test point %f %f):\n", plat, plon );
		for ( i = 0 ; i < nclose ; i++ )  {
			printf("%3d %3d %8.2f %8.2f\n", 
				i, order[i], flat[order[i]], flon[order[i]] );
		}

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 9 ) {

		printf ( type_prompt );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

		printf ( "Enter test point lat/lon :\n" );
		scanf ( " %f %f", &plat, &plon );

		printf ( "Enter number of closest points :\n" );
		scanf ( " %d", &nclose );

		clo_tclosest( locnam, plat, plon, nclose, &iret );

		printf ( "\nCLO_TCLOSEST: iret = %d\n", iret );

		loctyp = clo_which ( locnam );

		switch ( clo.loc[loctyp].format )  {

		    case 0 		:

			clo_tgid ( locnam, MAXSTN, 
				   sizeof(id), &nhotx, id, &iret );

			clo_tgltln ( locnam, MAXSTN, 
				     &nhotx, latx, lonx, &iret );

			if ( nhotx > 0 )  {
			    i = 0;
			    printf("%2d %-8s - %8.2f %8.2f\n",
			        i, strtok( id, ";" ), latx[i], lonx[i] );
			    for ( i = 1; i < nhotx; i++ )  {
			        printf("%2d %-8s - %8.2f %8.2f\n",
				    i, strtok( NULL, ";" ), latx[i], lonx[i] );
			    }
			}

			break;

		    case 1 :

			clo_stngall ( "COUNTY", MAXSTN, &ncnty, latx, lonx, 
				desc, statex, st_fips, cy_fips, country, elv, pri,
				wfo, &iret);
			
			for ( i = 0; i < ncnty; i++ )  {
			    printf("%2d %-8s - %32s %2s %8.2f %8.2f %10s\n",
				i, st_fips[i], desc[i], statex[i], 
				latx[i], lonx[i], wfo[i] );
			}

			break;

		}

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 10 ) {

		printf ( type_prompt );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

		printf ( "Enter test point lower left :\n" );
		scanf ( " %f %f", &latll, &lonll );
		printf ( "Enter test point upper right :\n" );
		scanf ( " %f %f", &latur, &lonur );

		clo_tinltln( locnam, latll, latur, lonll, lonur, &iret );

		printf ( "\nCLO_TINLTLN: iret = %d\n", iret );

		clo_tgid ( locnam, MAXSTN, sizeof(id), &nltln, id, &iret );
		clo_tgltln( locnam, MAXSTN, &nltln, latx, lonx, &iret );

		printf("\nLOCATIONS INSIDE = %d\n", nltln );
		if ( nltln > 0 )  {
		    i = 0;
		    printf("%-3d %-8s - %8.2f %8.2f\n",
		        i, strtok( id, ";" ), latx[i], lonx[i] );
		    for ( i = 1; i < nltln; i++ )  {
		        printf("%-3d %-8s - %8.2f %8.2f\n",
		            i, strtok( NULL, ";" ), latx[i], lonx[i] );
		    }
		}


	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 11 ) {

	      if ( ginit == 0 )  {
		printf("Must run GINITP first.\n" );
	      }
	      else  {

		printf ( type_prompt );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

		printf ( "Enter number of test points (<100):\n" );
		scanf ( " %d", &npts );

		printf ( "Enter test point lat lon pair(s) :\n" );
		for ( i = 0; i < npts; i++ )
		    scanf ( " %f %f", &(flat[i]), &(flon[i]) );

		for ( i = 0; i < npts; i++ )  {

		    clo_tqbnd( locnam, flat[i], flon[i], strout, &iret );

		    printf("IRET=%d, LAT=%6.2f, LON=%7.2f, BOUND = %s\n",
		        iret, flat[i], flon[i], strout );

		    clo_bginfo( locnam, 0, strout, &iret );

		    printf("INFORMATION=>%s\n", strout );

	        }

	      }
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 12 ) {

	      if ( ginit == 0 )  {
		printf("Must run GINITP first.\n" );
	      }
	      else  {

		printf ( type_prompt );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

		printf ( "Enter number of points in poly (<100):\n" );
		scanf ( " %d", &npts );

		printf ( "Enter polygon point lat lon pair(s) :\n" );
		for ( i = 0; i < npts; i++ )
		    scanf ( " %f %f", &(flat[i]), &(flon[i]) );

		clo_binpoly( locnam, npts, flat, flon, &iret );

		printf("CLO_BINPOLY IRET = %d\n", iret );

		clo_bgall ( locnam, MAXSTN, &nbnd, bndname, clat, clon,
			    nparts, bndinfo, &iret );

		printf("CLO_BGALL IRET = %d\n", iret );

		printf("Total number of bounds = %d\n", nbnd );
		pbn = (char *)cst_split ( bndname, '|', sizeof(bn), bn, &ier );
		pin = (char *)cst_split ( bndinfo, '|', sizeof(in), in, &ier );
		for ( ii = 0; ii < nbnd; ii++ )  {
		    printf("%3d - %32s %8.2f %8.2f %s\n",
				ii, bn, clat[ii], clon[ii], in );
		    pbn = (char *)cst_split ( pbn, '|', sizeof(bn), bn, &ier );
		    pin = (char *)cst_split ( pin, '|', sizeof(in), in, &ier );
		}

	      }
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 13 ) {

		printf ( type_prompt );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

		printf ( "Enter station number (eg. 72405):\n" );
		scanf ( " %d", &qnum );

		maxlen = sizeof(info);
		clo_findnum( locnam, qnum, maxlen, &nret, info, &iret );

		printf ( "\nCLO_FINDNUM: iret = %d\n", iret );

		printf ( "\nNumber of stations returned = %d\n", nret );
		printf ( "INFO=%s\n", info );

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 14 ) {

		printf ( type_prompt );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

		printf ( "Enter station identifier (eg. MSN or DCA):\n" );
		scanf ( " %s", qid );
		printf ( "Enter state PO abbr (*=all):\n" );
		scanf ( " %s", qstate );
		if ( strcmp(qstate,"*") == 0 )  qstate[0] = '\0';
		printf ( "Enter search type (1=exact,2=firstpart,3=index)\n" );
		scanf ( " %d", &srchtyp );

		maxlen = sizeof(info);
		clo_findstn( locnam, qid, qstate, srchtyp, maxlen, 
				&nret, info, &iret );

		printf ( "\nCLO_FINDSTN: iret = %d\n", iret );

		printf ( "\nNumber of stations returned = %d\n", nret );
		for ( i = 0 ; i < nret ; i++ )  {
		    if ( i == 0 )  
			printf("%s\n", strtok( info, ";" ) );
		    else
			printf("%s\n", strtok( NULL, ";" ) );
		}

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 15 ) {

		printf ( "Enter lat,lon pair #1 and time #1 (YYMMDD/HHMM):\n");
		scanf ( " %f %f %s", &lat1, &lon1, time1 );
		printf ( "Enter lat,lon pair #2 and time #2 (YYMMDD/HHMM):\n");
		scanf ( " %f %f %s", &lat2, &lon2, time2 );

		printf ( "Enter track increment in minutes (eg., 30 or 60):\n");
		scanf ( " %d", &inc );
		printf ( "Enter number of times (max 24):\n");
		scanf ( " %d", &ntimes );

		clo_track( lat1, lon1, time1, lat2, lon2, time2, inc, ntimes,
			   &spd, &dir, lattrk, lontrk, timetrk, &iret );

		printf ( "\nCLO_TRACK: iret = %d\n", iret );

		printf ( "\nSpeed = %f mps   Direction (from N) = %f deg\n", spd, dir );
		printf ( "\nTrack :\n" );
		for ( i = 0; i < ntimes; i++ )  {
		    printf ( "Lat = %6.2f   Lon = %6.2f  Time = %s\n", 
			lattrk[i], lontrk[i], timetrk[i] );
		}

	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 16 || numsub == 41 ) {

		printf ( "Enter number of lat-lon pairs:\n");
		scanf ( " %d", &npts );

		printf ( "How you you want the output? :\n");
		if ( numsub == 16 ) {
		    printf ( "    0 - compass direction prepend\n");
		    printf ( "    1 - compass direction postpend\n");
		    printf ( "    2 - VOR coordinate\n");
		}
		else {
		    printf ( "    4 - FROM line\n");
		    printf ( "    5 - BOUNDED BY line\n");
		}
		
		scanf ( " %d", &flag );

		for ( i = 0; i < npts; i++ )  {
		    printf ( "Enter lat,lon pair #%-d:\n", i );
		    scanf ( " %f %f", &(flat[i]), &(flon[i]) );
		}

		printf ( "Enter type of from line");
	        printf ( " (%d=SIGINTL_ELM, %d=SIGNCON_ELM,\n", SIGINTL_ELM,
			  SIGNCON_ELM );
		printf ( "  %d=SIGCONV_ELM, %d=SIGOUTL_ELM, %d=GFA_ELM)\n", 
			   SIGCONV_ELM, SIGOUTL_ELM, GFA_ELM );
		scanf ( " %d", &itype );

		printf ( "Enter subtype of from line (%d=isolated, %d=line, %d=area)\n", SIGTYP_ISOL, SIGTYP_LINE, SIGTYP_AREA );
		scanf ( " %d", &isubtyp );
		
		clo_from( itype, isubtyp, npts, flag, flat, flon, 
			  sizeof(fmLine), fmLine, &iret );
		
		if ( numsub == 16 ) {
		    printf ( "\nCLO_FROM: iret = %d\n", iret );
		    printf ( "\nFROM line = <%s>\n", fmLine );
                }

                if ( flag == 4 ) {
		    clo_cleanFmLine( fmLine, 0, cleanFmLine, &iret );
                }
		else if ( flag == 5 ) {
		    clo_cleanFmLine( fmLine, 1, cleanFmLine, &iret );		    
		}
		
		if ( numsub == 41 ) {
		    printf ( "\nCLO_CLEANFMLINE: iret = %d\n", iret );
		    printf ( "\nFROM line = <%s>\n", fmLine );
		    printf ( "\nCleaned FROM line = <%s>\n", cleanFmLine );
                }
		
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 17 ) {

		printf ( "Enter first latitude and longitude:\n" );
		scanf ( " %f %f", &lat1, &lon1 );
		printf ( "Enter second latitude and longitude:\n" );
		scanf ( " %f %f", &lat2, &lon2 );

		clo_direct( &lat1, &lon1, &lat2, &lon2, &dir, &iret );

                printf ( "\nCLO_DIRECT: iret = %d\n", iret ); 

		printf ( "\nCLO_DIRECT : direction (degrees) = %f\n", dir );

	    }
/*---------------------------------------------------------------------*/
            if ( numsub == 18 ) {

                printf ( "Enter a 16-point compass direction e.g. N, NNE...\n");
                scanf ( "%s", wds );

                clo_cmpwds( wds, &i, cmpwds, &num, &iret );
                
                printf ( "\nCLO_CMPWDS: iret = %d\n", iret ); 
                printf ( "\nCLO_CMPWDS : direction = %s\n", cmpwds );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 19 ) {

                printf ( "Enter a 16-point compass direction e.g. N, NNE...\n");
                scanf ( "%s", wds );

                clo_cmpdir( wds, &odir, &iret );
                
                printf ( "\nCLO_CMPDIR: iret = %d\n", iret ); 
                printf ( "\nCLO_CMPDIR : direction = %6.1f\n", odir );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 20 ) {
                printf ( type_prompt );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

		printf ( "Enter the format code : 1st digit - rounding (5-10),\n" );
		printf ( "2nd digit - units (0-3), " );
		printf ( "3rd digit - direction(0-2),\n" ); 
		printf ( "4th digit - deg/dec(0-3)\n" );
		scanf ( "%d", &format );
		printf ( "Enter latitude and longitude:\n" );
		scanf ( " %f %f", &lat, &lon );
                printf ( "\nFORMAT: format= %d \n",format);

		clo_ddenc ( locnam, format, lat, lon, strout, &ier);

                printf ( "\nCLO_DDENC: iret = %d\n", iret ); 
                printf ( "\nCLO_DDENC: location = %s \n",strout);

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 21 ) {
                printf ( type_prompt );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

		printf ( "Enter the sort control :\n\t%d = STN_ID\n\t%d = STN_NM\n\t%d = STN_DESC\n\t%d = STN_ST\n\t%d = STN_LAT\n\t%d = STN_LON\n\t%d = STN_ELV\n\t%d = STN_PRI\n\t%d = COL10\n", STN_ID, STN_NM, STN_DESC, STN_ST, STN_LAT, STN_LON, STN_ELV, STN_PRI, STN_COL10 );
		scanf ( "%d", &icol );

		clo_sortstn ( locnam, icol, &ier);

                printf ( "\nCLO_SORTSTN: iret = %d\n", iret ); 
                printf ( "\nCLO_SORTSTN: station list:\n" );

		loctyp = clo_which ( locnam );

		nstn = clo.loc[loctyp].stn.nstn;
		for ( ii = 0; ii < nstn; ii++ )  {
		    printf("%8s %6d %24s %2s %2s %6.2f %7.2f %4d %2d\n",
		    clo.loc[loctyp].stn.station[ii].id,
		    clo.loc[loctyp].stn.station[ii].nm,
		    clo.loc[loctyp].stn.station[ii].desc,
		    clo.loc[loctyp].stn.station[ii].state,
		    clo.loc[loctyp].stn.station[ii].cntry,
		    clo.loc[loctyp].stn.station[ii].lat,
		    clo.loc[loctyp].stn.station[ii].lon,
		    clo.loc[loctyp].stn.station[ii].elv,
		    clo.loc[loctyp].stn.station[ii].pri );
		}
            }
/*---------------------------------------------------------------------*/
	    if ( numsub == 22 ) {

		printf ( type_prompt );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

		printf ( "Enter station descriptor (eg. MADISON):\n" );
		scanf ( " %s", qid );
		printf ( "Enter state PO abbr (*=all):\n" );
		scanf ( " %s", qstate );
		if ( strcmp(qstate,"*") == 0 )  qstate[0] = '\0';
		printf ( "Enter search type (1=exact,2=firstpart,3=index)\n" );
		scanf ( " %d", &srchtyp );

		maxlen = sizeof(info);
		clo_finddesc( locnam, qid, qstate, srchtyp, maxlen, 
				&nret, info, &iret );

		printf ( "\nCLO_FINDDESC: iret = %d\n", iret );

		printf ( "\nNumber of stations returned = %d\n", nret );
		for ( i = 0 ; i < nret ; i++ )  {
		    if ( i == 0 )  
			printf("%s\n", strtok( info, ";" ) );
		    else
			printf("%s\n", strtok( NULL, ";" ) );
		}

	    }
/*---------------------------------------------------------------------*/
            if ( numsub == 23 ) {
                printf ( type_prompt );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

		printf ( "Enter the sort control :\n\t%d = BND_NAME\n\t%d = BND_STREC\n\t%d = BND_CLAT\n\t%d = BND_CLON\n\t%d = BND_MNLAT\n\t%d = BND_MXLON\n", BND_NAME, BND_STREC, BND_CLAT, BND_CLON, BND_MNLAT, BND_MXLON );
		scanf ( "%d", &icol );

		clo_sortbnd ( locnam, icol, &ier);

                printf ( "\nCLO_SORTBND: iret = %d\n", iret ); 
                printf ( "\nCLO_SORTBND: list:\n" );

		loctyp = clo_which ( locnam );

		nstn = clo.loc[loctyp].bnd.nbnd;
		for ( ii = 0; ii < nstn; ii++ )  {
		    printf("%32s %9ld %6.2f %7.2f %6.2f %7.2f %6.2f %7.2f %2d\n",
		    clo.loc[loctyp].bnd.bound[ii].name,
		    clo.loc[loctyp].bnd.bound[ii].strec,
		    clo.loc[loctyp].bnd.bound[ii].cenlat,
		    clo.loc[loctyp].bnd.bound[ii].cenlon,
		    clo.loc[loctyp].bnd.bound[ii].minlat,
		    clo.loc[loctyp].bnd.bound[ii].minlon,
		    clo.loc[loctyp].bnd.bound[ii].maxlat,
		    clo.loc[loctyp].bnd.bound[ii].maxlon,
		    clo.loc[loctyp].bnd.bound[ii].nparts );
		}
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 24 ) {

                printf ( "Enter number of lat-lon pairs:\n");
                scanf ( " %d", &npts );

                for ( i = 0; i < npts; i++ )  {
                    printf ( "Enter lat,lon pair #%-d:\n", i );
                    scanf ( " %f %f", &(flat[i]), &(flon[i]) );
                }

                clo_reorder( npts, flat, flon, indx, &iret );

                printf ( "\nCLO_REORDER: iret = %d\n", iret );
                if ( iret == 0 ) {
                    for (ii = 0; ii < npts; ii++) {
                        latb[ii] = flat[indx[ii]];
                        lonb[ii] = flon[indx[ii]];
                        printf(" point %d: lat = %f, lon = %f\n", ii, latb[ii], lonb[ii]);
                    }
                } 

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 25 ) {

		printf ( type_prompt );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

                printf ( "\nCLO_WHICH = %d\n", clo_which ( locnam ) ); 

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 26 ) {
                printf ( type_prompt );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

		printf ( "Enter the format code : \n" );
		printf ( "1st digit - rounding (5-10), " );
		printf ( "2nd digit - units (0-3),\n" );
		printf ( "3rd digit - direction(0-2), " ); 
		printf ( "4th digit - deg/dec(0-3)\n" );
		scanf ( "%d", &format );
		printf ( "Enter the location string (eg. 50NM ENE TAMPA ");
		printf ( "or 20SM SW BWI or DCA): \n");
		scanf  ("\n%[^\n]s", strdcd );
		cst_lcuc ( strdcd, strdcd, &ier );
		printf ( "Enter the number of points to be returned\n");
		scanf ( "%d", &nexp);

		clo_dddec ( locnam, &format, strdcd, &nexp, flat, flon, 
		            &nret, &ier);

                printf ( "\nCLO_DDDEC: ier = %d\n", ier); 

                printf ( "\nNumber of lat/lon pairs returned = %d\n", nret );
		for ( i = 0 ; i < nret ; i++ )  {
                    printf ( "CLO_DDDEC: stn %d : lat, lon  = %f  %f \n", 
		               i+1, flat[i], flon[i]);
		}
		if ( nret == 0 ) 
		       printf ( "CLO_DDDEC: stn %d : lat, lon  = %f  %f \n", 
				nret , flat[i], flon[i]);
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 27 ) {
                printf ( type_prompt );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

		clo_bstype ( locnam, &ier );

                printf ( "\nCLO_BSTYPE: ier = %d\n", ier); 

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 28 ) {
		printf ( "Enter area in form: latll lonll latur lonur :\n" );
		scanf ( " %f %f %f %f", &latll, &lonll, &latur, &lonur );

		clo_bsarea ( &latll, &lonll, &latur, &lonur, &ier );

                printf ( "\nCLO_BSAREA: ier = %d\n", ier); 

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 29 ) {

		np = 0;
		maxpts = sizeof(latb) / sizeof(float);
		mxp = 0;

		printf ( "Enter minimum number of points\n" );
		scanf ( " %d", &minpts );

		printf ( "Enter maximum number of points (%d)\n", maxpts );
		scanf ( " %d", &maxpts );

		printf ( "Enter filter factor (0->1)\n" );
		scanf ( " %f", &filt );
		printf("filt=%f\n", filt );

		ier = 0;
		while ( ier == 0 )  {
		    clo_bgnext ( &minpts, &maxpts, &filt, 
				 &npts, latb, lonb, &ier );
		    if ( ier == 0 )  {
			printf("Number of points returned = %d\n", npts );
			mxp = G_MAX ( mxp, npts );
		        np++;
			printf ( "Type 'n' for next, 'e' for exit: " );
			scanf ( " %s", select );
			if ( strcmp(select,"e")==0 ) ier = -1;
		    }
		}

                printf ( "\nCLO_BGNEXT: ier = %d\n", ier); 
                printf ( "\nTotal number of polygons returned = %d\n", np );
                printf ( "\nMaximum number of points = %d\n", mxp );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 30 ) {
                printf ( "Enter tag (eg., <STATE>WI)\n" );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

		clo_bstag ( locnam, &ier );

                printf ( "\nCLO_BSTAG: ier = %d\n", ier); 

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 31 ) {

		clo_bgcent ( &plat, &plon, &ier );

                printf ( "\nCLO_BGCENT: ier = %d\n", ier); 
		printf("\tCentroid: (%6.2f,%6.2f)\n", plat, plon );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 32 ) {

		clo_bgrange ( &latll, &lonll, &latur, &lonur, &ier );

                printf ( "\nCLO_BGRANGE: ier = %d\n", ier); 
		printf("\tRange: (%6.2f,%6.2f) - (%6.2f,%6.2f)\n", 
			latll, lonll, latur, lonur );

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 33 ) {

		printf ( "Enter the bounds name:\n" );
		scanf ( " %s", bounds );

		printf ( "Enter the clipping area tag:\n" );
		scanf ( " %s", name );

		printf ( "Enter the polygon points coordinate system:\n" );
		scanf ( " %s", sysp );

		printf ( "Enter the number of polygon points:\n" );
		scanf ( " %d", &np );

		if ( np > 0 ) {
		   xp = malloc ( np * sizeof ( float ) );
		   yp = malloc ( np * sizeof ( float ) );
		}

		printf ( "Enter %d pairs of (x, y):\n", np );
		for ( ii = 0 ; ii < np ; ii++ )  {
                    printf ( "Enter (x, y) of point #%d:\n", ii + 1 );
		    scanf ( " %f %f", &(xp[ii]), &(yp[ii]) );
		}
		
		maxppts = 0;
		clo_clip ( &np, xp, yp, sysp, bounds, name, &nclip, &maxppts, &ier );

                printf ( "\nCLO_CLIP: ier = %d\n", ier ); 

		if ( ier == 0 ) {
		   printf ( "\tNumber of resulting clipped polygons: %d\n", nclip );
		   printf ( "\tMaximum number of points in any clipped polygon: %d\n", maxppts );
		}

		if ( np > 0 ) {
		   free ( xp );
		   free ( yp );
		}
            }
/*---------------------------------------------------------------------*/
            if ( numsub == 34 ) {

		if ( maxppts > 0 ) {

		   xp = malloc ( maxppts * sizeof ( float ) );
		   yp = malloc ( maxppts * sizeof ( float ) );

		   printf ( "Enter which contour you want to see:(1 - %d)\n", nclip );
		   scanf ( " %d", &which );

		   --which;
		   clo_clipget ( &which, &np, xp, yp, &ier );

                   printf ( "\nCLO_CLIPGET: ier = %d\n\n", ier); 

		   if ( ier == 0 ) {
		      printf ( "Number of points in returned polygon: %d\n", np );
		   
		      for ( ii = 0; ii < np; ii++ ) {
                          printf ( "(x, y) of point #%d: ", ii + 1 );
		          printf ( "\t%f \t%f\n", xp[ ii ], yp[ ii ] );
		      }
		   }

		   free ( xp );
		   free ( yp );
		}

		else {
		   printf ( "\nNo clipped polygons! Please run CLO_CLIP first!\n" );
		}

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 35 ) {

		clo_clipdone ( &ier );

		maxppts = 0;
		nclip   = 0;

                printf ( "\nCLO_CLIPDONE: ier = %d\n", ier); 

            }
/*---------------------------------------------------------------------*/
            if ( numsub == 36 ) {
              if ( ginit == 0 )  {
                printf("Must run GINITP first.\n" );
              }
              else  {

                printf ( "Enter FA area:\n" );
                scanf ( "%s", faarea );

                printf ( "Enter state string separated by separator:\n" );
                scanf ( "%s", stnamin );

                printf( "Enter separator:\n" );
                scanf ( "%s", sep );

                printf( "Enter product type (AIRMET or SIGMET):\n" );
                scanf ( "%s", ptype );

                clo_fastates ( faarea, stnamin, sep[0], ptype, stnamout, &ier);

                printf ( "\nCLO_FASTATES: ier = %d\n", ier);
                printf ( "\nOrdered State String: %s \n", stnamout);

              }
            }
/*---------------------------------------------------------------------*/
	    if (numsub == 37) {
	        if ( ginit == 0 )  {
                  printf("Must run GINITP first.\n" );
                }
            else  {
		printf ( "Enter the bounds name:\n" );
                scanf ( " %s", bounds );

                printf ( "Enter the bounds search tag ( <FIPS> ) :\n" );
                scanf ( " %s", name );

		printf ( "Enter number of tags in string:\n");
		scanf ( " %d", &npts);

		printf ( "Enter string of tag values separated by ';' \n");
		scanf ( " %s", btags);
               /* 
                * Structure initializations.
                */

                union_poly.num_contours = 0;
                union_poly.hole         = (int*)NULL;
                union_poly.contour      = (gpc_vertex_list*)NULL;


		printf ( "  bounds=%s  tag=%s  num of tags=%d\n", bounds,
			name, npts );
		printf ( "  tags=%s\n", btags );
                clo_blasso ( bounds, name, &npts, btags, &union_poly, &iret );
		printf (" \nCLO_BLASSO:  iret = %d\n", iret );
		if ( iret != -1 ) {
		  if ( union_poly.num_contours == 0 ) {
		      printf (" \nNo. of polygons=%d\n",
				 union_poly.num_contours );
		  }
		  else {
		    /*
		     * Determine the number of polygons that are holes.
		     */
		    nholes = 0;
		    for ( ii = 0; ii < union_poly.num_contours; ii++ ) {
		        if ( union_poly.hole[ii] == 1 ) nholes++;
		    }
		    printf("\nNo. of polygons=%d     No. of polygons that are"
		           " holes=%d\n", union_poly.num_contours, nholes);

                    for ( ii = 0; ii < union_poly.num_contours; ii++ ) {
		        printf ("  Polygon %4d has %5d vertices with hole-flag"
				"=%d\n", ii, union_poly.contour[ii].num_vertices,
			        union_poly.hole[ii]);
		    }
		    printf (" To print vertices of all polygons type 'y'\n" );
		    printf ("   or to print vertices of a particular polygon,"
			    " type the polygon number\n" );
		    printf ("   or to not print vertices, type 'n'\n" );
		    scanf ( " %s", select );
		    switch ( select[0] ) {
		        case 'y':
		        case 'Y':
			    /*
			     * Print vertices of all polygons
			     */
			    if ( union_poly.num_contours > 100 ) {
			        printf (" More than 100 polygons; only the first"
					" 100 will be printed\n" );
			        nprt = 100;
			    }
			    else {
			    	nprt = union_poly.num_contours;
			    }
			    for ( ij = 0; ij < nprt; ij++ ) {
                                gpc_gvlist ( &union_poly.contour[ij], &npo,
					xo, yo, &ier);
                                printf ("Polygon %4d   Hole_flag=%d   No. vertic"
				      "es=%4d\n", ij, union_poly.hole[ij], npo );

                   	        for ( ii = 0; ii < npo; ii++ ) {
                       	            printf ("  %9.3f %9.3f\n", xo[ii], yo[ii]);
			        }
			    }

		        case 'n':
		        case 'N':
			    /*
			     * Don't print any vertices
			     */
			    break;

		        default:
			    /*
			     * Print vertices of selected polygons
			     */
		    	    numsub = atoi ( select );
			    if ( numsub == 0 )  {
				if ( select[0] != '0' ) {
				    numsub = -1;
				}
			    }
			    while ( select[0] != 'n' )  {
			      if ( numsub >= 0 &&
				         numsub < union_poly.num_contours  ) { 
                                gpc_gvlist ( &union_poly.contour[numsub], &npo,
					xo, yo, &ier);
			        printf ("Vertices for polygon %d:\n", numsub);
                   	        for ( ii = 0; ii < npo; ii++ ) {
                       	          printf ("  %9.3f %9.3f\n", xo[ii], yo[ii]);
			        }
			      }
			      else {
				printf ("Invalid polygon number\n");
			      }			
			      printf (" Enter another polygon number to print"
				    " or 'n' for none.\n" );
			      scanf ( " %s", select );
			      numsub = atoi ( select );
			      if ( numsub == 0 )  {
				  if ( select[0] != '0' ) {
				      numsub = -1;
				  }
			      }
			    }			
			    break;
		        }
		    }
		  }
                gpc_free_polygon ( &union_poly );
	        }
            }
/*---------------------------------------------------------------------*/
	    if ( numsub == 38 ) {

		clo_init ( &ier );
		
		printf ( "\nEnter the type (STATION or BOUNDS):\n" );
		scanf ( " %s", ptype );
		clo_qmxpts ( ptype, &mxp, &iret );
		
		printf ( "\nCLO_QMXPTS: iret = %d\n\n", iret );
		if ( iret == 0 ) {
		    printf ( "\nMax points allowed for %s = %d\n\n", ptype, mxp );
                }
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 39 ) {
		clo_init ( &ier );
	        printf( "\nEnter the lat and lon of the point to be snapped:\n");
		scanf( " %f %f", &lat, &lon );

		clo_snapOnePt( lat, lon, &snapLat, &snapLon, &iret );
		printf("\n CLO_SNAPONEPT:  iret = %d\n", iret );
		printf("        point lat: %f, lon: %f\n", lat, lon );
		printf("     snaps to lat: %f, lon: %f\n", snapLat, snapLon );
		printf("\n");
	    }
/*---------------------------------------------------------------------*/
	    if ( numsub == 40 ) {

		printf ( "Enter the location table name:\n");
		scanf ( "%s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

		printf ( "Enter CWA name (eg. IND):\n" );
		scanf ( "%s",  qwfo );

		maxnum = 100;
		clo_findcwa (locnam, qwfo, maxnum, &nfips, fips, &iret);

		printf ( "\nCLO_FINDCWA: iret = %d\n", iret );
		printf ( "\nNumber of counties returned = %d\n", nfips );
		for (ii=0;ii<nfips;ii++) printf("COUNTY %d is %d\n",ii,fips[ii]);

	    }
/*---------------------------------------------------------------------*/
/*	    #41 co-located with #16
	    if ( numsub == 40 ) {
	    }*/
/*---------------------------------------------------------------------*/
	    if ( numsub == 42 ) {

		printf ( type_prompt );
		scanf ( " %s", locnam );
		cst_lcuc ( locnam, locnam, &ier );

		printf ( "Enter test point lat/lon :\n" );
		scanf ( " %f %f", &plat, &plon );

		printf ( "Enter tolerance :\n" );
		scanf ( " %f", &tol );

		clo_tmatch( locnam, plat, plon, tol, &iret );

		printf ( "\nCLO_TMATCH: iret = %d\n", iret );

		if ( iret == 0 )  {

		  loctyp = clo_which ( locnam );

		  switch ( clo.loc[loctyp].format )  {

		    case 0 		:

			clo_tgid ( locnam, MAXSTN, 
				   sizeof(id), &nhotx, id, &iret );

			clo_tgltln ( locnam, MAXSTN, 
				     &nhotx, latx, lonx, &iret );

			if ( nhotx > 0 )  {
			    i = 0;
			    printf("%2d %-8s - %8.2f %8.2f\n",
			        i, strtok( id, ";" ), latx[i], lonx[i] );
			    for ( i = 1; i < nhotx; i++ )  {
			        printf("%2d %-8s - %8.2f %8.2f\n",
				    i, strtok( NULL, ";" ), latx[i], lonx[i] );
			    }
			}

			break;

		    case 1 :

			clo_stngall ( "COUNTY", MAXSTN, &ncnty, latx, lonx, 
				desc, statex, st_fips, cy_fips, country, elv, pri,
				wfo, &iret);
			
			for ( i = 0; i < ncnty; i++ )  {
			    printf("%2d %-8s - %32s %2s %8.2f %8.2f %10s\n",
				i, st_fips[i], desc[i], statex[i], 
				latx[i], lonx[i], wfo[i] );
			}

			break;

		  }

		}

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
/*
		printf ( "Enter DEVICE (XW, GN, etc.):\n" );
		scanf ( " %s", device );
*/
		strcpy ( device, "GN" );
		printf("device=%s\n", device );

		iunit = 1;
		strcpy ( dfilnam, "TESTCLO" );
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
                        12, 72);

/*
		printf ( "Enter GAREA in form latll;lonll;latur;lonur :\n" );
		scanf ( " %f;%f;%f;%f", &lllat, &lllon, &urlat, &urlon );
		printf ( "Enter PROJ in form pro/ang1;ang2;ang3 :\n" );
		scanf ( " %s", proj );
		strcpy ( pro, strtok ( proj, "/" ) );
		prjang1 = 0.0F;  prjang2 = 0.0F;  prjang3 = 0.0F;
		ptr = strtok(NULL,";");
		if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang1 );
		ptr = strtok(NULL,";");
		if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang2 );
		ptr = strtok(NULL,";");
		if ( ptr != '\0' )  sscanf( ptr, "%f", &prjang3 );
*/
		printf("done\n");
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
	}
        return 0;
}
