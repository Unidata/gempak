#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

#include "cascmn.h"
#include "proto_sigavgf.h"

#define  JET_LINE    6
#define  LMULT       1
#define  LDIR        1
#define  LSIZE       1.20F
#define  LWIDTH      7
#define  LENBUF      256

void tangwnd ( float *plat, float *plon, int np, float *windir, 
               int *iret );

void sigajet ( char *fhour, int numjet, jets_t *ptrj, int itime[], 
               char *chlvl, char *flvl, int *iret ) 
/************************************************************************
 * sigajet                                                              *
 *                                                                      *
 * This program encodes the High or Mid Level Significant Weather ASCII *
 * jet information into VG file format.   				*
 *                                                                      *
 * sigajet ( fhour, numjet, ptrj, itime, chlvl, flvl, iret )		* 
 *                                                                      *
 * Input parameters:                                                    *
 *	*fhour		char 	   Forecast hour			*
 *      numjet          int	   Number of jet lines			*
 *      *ptrj		jets_t     Pointer to JETS link list		*
 *      itime[]		int	   Issued date/time 			*
 *      *chlvl		char	   Chart level 				*
 *      *flvl		char	   Flight level format			*
 *				   TRUE  = new format			*
 *				   FALSE = old format			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int        Return code                      	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC     3/02	Created					*
 * A. Hardy/SAIC     5/02       Round flight levels nearest 10		*
 * A. Hardy/SAIC    11/02       Added check for winds w/o heights	*
 * R. Tian/SAIC	     5/03	Re-deisgned the wind dir calc algorithm	*
 * R. Tian/SAIC	     6/03	Separate jet and wind points		*
 * M. Li/SAIC	     7/04	Encode jet element			*
 * M. Li/SAIC	     7/04	Removed grpch				*
 * M. Li/SAIC	     1/05	Added chlvl				*
 * T. Piper/SAIC    01/05	Removed unused variables difspd & lstspd*
 * M. Li/SAIC	    10/05	Added flvl				*
 * S. Danz/AWC      07/06	Switched to new cvg_writeD function	*
 * L. Hinson/AWC    06/12       Revised Jet Hash Logic to plot hashes   *
 *                                only if 10 m/s change in speed.       *
 * L. Hinson/AWC    06/12       Fixed Flight Levels calc to round up    * 
 * L. Hinson/AWC    08/12       Initialize "pivot" to 0 for each jet    *
 ***********************************************************************/
 {
    int   	    ii, ij, jj, kk, ier, len;
    FILE  	    *fptr;
    jets_t          *ptr;

    int		    numerr, leverr, wflg, tmptm, gpnum, tmpmod; 
    int             sizrec, start;
    int		    txtclr;
    float           xmxlat, xmnlat, xmxlon, xmnlon, xmxwnd;
    float           flight, tmplv1, tmplv2, tmplv3, tmpspd, speed, tmplvf;
    float           angle, angle3, flat, flon, dist;
    char            level1[12], level2[12], level3[12];
    char 	    ofname[LENBUF], errgrp[8], jetstr[40];
    char            ctime[LENBUF], grpch;
    int        	    mode, istat, iunit, itype;
    char            device[8], filnam[20];
    float           xsize, ysize;
    float           windir[MAXPTS], initspeed;
    int		    njp, jpi[MAXPTS], nwnd, jwi[MAXPTS], iwnd[MAXPTS];
    int		    nn, inz, nbp, nhp, pivot;
    Boolean	    delta;

    VG_DBStruct     el;
/*---------------------------------------------------------------------*/
     *iret  = 0;
     fptr   = NULL;
     ier    = 0;
     leverr = 0;
     start  = 0;
     gpnum  = 0;
     grpch  = 0;
     wflg   = G_TRUE;
     strcpy ( errgrp, "SIGAVGF" );

    /*
     * Init GEMPLT and set DEVICE.
     */

     mode = 1;
     ginitp ( &mode, &istat, &ier );

     iunit = itype = 1;
     xsize = ysize = 1.0F;
     strcpy ( device, "GN" );
     strcpy ( filnam, "CGR_TANGENT" );
     gsdeva ( device, &iunit, filnam, &itype, &xsize, &ysize, &ier,
              strlen(device), strlen(filnam) );

    /* 
     * Create output filename.
     */

     if ( strcmp ( chlvl, "SWM" ) == 0 ) {
         strcpy ( ofname, "mjet_");
     }
     else {
	 strcpy ( ofname, "jet_");
     }

     for ( ij = 0; ij < 4; ij++ ){
	 tmptm = itime[ij];
         cst_inch ( tmptm, ctime, &ier );
	 cst_lstr ( ctime, &len, &ier );
	 if ( len == 1 ) strcat ( ofname, "0");
         strcat ( ofname, ctime );
     }
     strcat ( ofname, "_");
     strcat ( ofname, fhour);
     strcat ( ofname, "_final.vgf");

    /* 
     * Open output file and write the VG file header information. 
     */

     cvg_crvgf ( ofname, &ier );
     cvg_open ( ofname, wflg, &fptr, &ier);
     if ( ( ier != 0 ) || ( fptr == NULL ) ) {
         numerr = -5;
         er_lmsg ( &leverr, errgrp, &numerr, ofname, &ier,
                   strlen(errgrp), strlen(ofname) );
		   exit(1);
     }

    /*
     * Loop through the jet element list and write each one to VG file.
     */

     ptr = ptrj;
     start = sizeof ( VG_HdrStruct ) + sizeof ( FileHeadType );


     for ( ii = 0; ii < numjet; ii++) {
       pivot = 0;

       /* 
	* Determine the maximum and minimum lat. and lon.
	*/

	for ( njp = nwnd = jj = 0; jj < ptr->npt; jj++ ) {
            if ( ptr->speed[jj] == SIGRLMS && ptr->level[jj] == SIGRLMS ) {
	        jpi[njp++] = jj;

                if ( jj == 0 ) {
                    xmxlat = ptr->lat[jj];
                    xmnlat = ptr->lat[jj];
                    xmxlon = ptr->lon[jj];
                    xmnlon = ptr->lon[jj];
	        }
                else {
                    xmxlat = G_MAX ( xmxlat, ptr->lat[jj] );
                    xmnlat = G_MIN ( xmnlat, ptr->lat[jj] );
                    xmxlon = G_MAX ( xmxlon, ptr->lon[jj] );
                    xmnlon = G_MIN ( xmnlon, ptr->lon[jj] );
	        }
	    }
	    else {
		jwi[nwnd++] = jj;
	    }
         }

	/*
	 * Get tangent wind direction for each point.
	 */
         tangwnd ( ptr->lat, ptr->lon, ptr->npt, windir, &ier );


        /*
	 * Check for wind barb and hash.
	 */

	 for ( jj = 0; jj < nwnd; jj++ ) {
	    iwnd[jj] = 0;
	    kk = jwi[jj];
	    if ( ptr->level[kk] == SIGRLMS ) {
                iwnd[jj] = -1;
            }
            else {
                if ( ptr->levabv[kk] != SIGRLMS || ptr-> levblw[kk] != SIGRLMS )
                    iwnd[jj] = 1;

	       /*
		* Look for the maximum wind speed.
		*/
	
		if ( jj == 0 ) {
		    xmxwnd = ptr->speed[kk];
                    pivot = jj;
		}
		else {
                    if ( ptr->speed[kk] > xmxwnd ) {
                      xmxwnd = ptr->speed[kk];
                      pivot = jj;
                    }                    
		}
            }
         }

	/*
	 * Find the first non-zero value in the flag array iwnd.
 	 */
	
	 inz = 0;
	 for ( jj = 0; jj < nwnd; jj++ ) {
	    if ( iwnd [jj] != 0 ) {
		inz = jj;
		break;
	    } 
         }
         if (inz == 0) { inz = pivot; }
	/*
	 * Barb at the maximum wind(s).
	 */
	
	 for ( jj = 0; jj < nwnd; jj++ ) {
	    kk = jwi[jj];
	    if ( iwnd[jj] == 0 &&  ptr->speed[kk] == xmxwnd ) iwnd[jj] = 1;
	 }
         /**********************************
          Revised Coding...Go forward along the jet
         **********************************/
         kk = jwi[inz];
         initspeed = ptr->speed[kk];
         for ( jj = inz+1; jj < nwnd; jj++ ) {
            kk = jwi[jj];
            /* Here we use hash marks if the speed diff is 10 m/s (20 kts)
               or more */
            if ( (ptr->speed[kk] - initspeed) <= -9.0 ) {	    
	      if ( (jj + 1) < nwnd ) {
	        if ( fabs(ptr->speed[jwi[jj+1]] - ptr->speed[kk]) > 9.0 ) {
                  iwnd[jj] = -1;
		} else {
		  iwnd[jj] = 1;
		}
	      } else {
	        iwnd[jj] = -1;
	      }
            } else {
              iwnd[jj] = 1;
            }
            initspeed = ptr->speed[kk];
         }
         initspeed = ptr->speed[jwi[inz]];
         /***********************************
          Revised Coding...Go reverse along the jet
         ***********************************/
         if (inz >= 1) {
           for ( jj = inz-1; jj >= 0; jj-- ) {
             kk = jwi[jj];
             if ( (ptr->speed[kk] - initspeed) <= -9.0 ) {
	       if ( ( jj - 1 ) >= 0 ) {
	         if ( fabs(ptr->speed[jwi[jj-1]] - ptr->speed[kk]) > 9.0 ) {
		   iwnd[jj] = -1;
		 } else {
		   iwnd[jj] = 1;
		 }
	       } else {
	         iwnd[jj] = -1;
	       }
             } else {
               iwnd[jj] = 1;                 
             }
             initspeed = ptr->speed[kk];
           }
         }

	 	
	/* 
	 * Fill in VG header information.
	 */

	 sizrec = (int)(sizeof( VG_HdrStruct ) + sizeof( JetType ));

	 if ( ( xmxlat < 0.0F ) || ( xmnlat < 0.0F ) ) {
             el.hdr.maj_col  = 19;
             el.hdr.min_col  = 19;
	 }
	 else {
             el.hdr.maj_col  = 1;
             el.hdr.min_col  = 1;
	 }

         el.hdr.delete   = 0;
         el.hdr.vg_type  = JET_ELM;
         el.hdr.vg_class = CLASS_MET;
         el.hdr.smooth   = 2;
         el.hdr.version  = 0;
         el.hdr.filled   = 0;
         el.hdr.closed   = 0;
         el.hdr.recsz    = sizrec; 
         el.hdr.grptyp   = grpch;
         el.hdr.grpnum   = gpnum;
         el.hdr.range_min_lat = xmnlat;
         el.hdr.range_min_lon = xmnlon;
         el.hdr.range_max_lat = xmxlat;
         el.hdr.range_max_lon = xmxlon;

	/* 
	 * Fill in jet line information.
	 */

	 el.elem.jet.line.splcol          = el.hdr.maj_col; 
	 el.elem.jet.line.spl.info.numpts = njp;
	 el.elem.jet.line.spl.info.spltyp = JET_LINE;  
	 el.elem.jet.line.spl.info.splstr = LMULT; 
	 el.elem.jet.line.spl.info.spldir = LDIR;
	 el.elem.jet.line.spl.info.splsiz = LSIZE;
	 el.elem.jet.line.spl.info.splwid = LWIDTH;
         for ( ij = 0; ij < njp; ij++ ) {
             el.elem.jet.line.spl.latlon[ij] = ptr->lat[jpi[ij]];
             el.elem.jet.line.spl.latlon[ij+njp] = ptr->lon[jpi[ij]];
        }

	/* 
	 * Begin creating the jet TEXT string.
         * Determine the jet flight level.
         */

	 nbp = 0;
	 nhp = 0;
         for ( jj = 0; jj < nwnd; jj++ ) {
	     kk = jwi[jj]; 
	    /* 
	     * Write out BARB and FLIGHT LEVEL information.
	     */

             if ( ptr->speed[kk] != SIGRLMS ) {
                 if ( ptr->level[kk] != SIGRLMS ) {

	             flight = ptr->level[kk];
                     tmplv1 = pr_hgmf (&flight);
		     tmplvf = tmplv1;
		     tmplv1 = tmplv1/ 100.0F;
                     tmpmod = ( (int) tmplv1 % 10 );
                     if (  tmpmod >= 5 ) {
                         tmplv1 = tmplv1 + (float)( 10 - tmpmod );
                     }
                     else {
                         tmplv1 = tmplv1 - (float)tmpmod;
                     }

		    /*
		     * Calculate flight level deltas.
		     */

		     delta = False;
		     if ( ptr->levabv[kk] != SIGRLMS || ptr->levblw[kk] != SIGRLMS ) {
			 cst_lcuc ( flvl, flvl, &ier );
			 if ( strcmp ( flvl, "FALSE" ) == 0 ) {
			     if ( ptr->levabv[kk] != SIGRLMS ) {
			         tmplv2 = pr_hgmf ( &(ptr->levabv[kk]) );
			         tmplv2 = (tmplv2 - tmplvf)/100.F;
			         cst_inch ( (int) tmplv2, level2, &ier );
			     }
			     else {
			         strcpy ( level2, "0" );
			     }

			     if ( ptr->levblw[kk] != SIGRLMS ) {
                                 tmplv3 = pr_hgmf ( &(ptr->levblw[kk]) );
                                 tmplv3 = (tmplv3 - tmplvf)/100.F;
                                 cst_inch ( (int) tmplv3, level3, &ier );
                             }
			     else {
			         strcpy ( level3, "0" );
                             }
			 }
			/*
			 * New format for flight level delta.
  			 */
			 else {
			     if ( ptr->levabv[kk] != SIGRLMS ) {
                                 tmplv2 = pr_hgmf ( &(ptr->levabv[kk]) );
                                 tmplv2 = tmplv2/100.F + 0.5;
                                 cst_inch ( (int) tmplv2, level3, &ier );
                             }
                             else {
                                 cst_inch ( (int) tmplv1, level3, &ier );
                             }

                             if ( ptr->levblw[kk] != SIGRLMS ) {
                                 tmplv3 = pr_hgmf ( &(ptr->levblw[kk]) );
                                 tmplv3 = tmplv3/100.F + 0.5;
                                 cst_inch ( (int) tmplv3, level2, &ier );
                             }
                             else {
                                 cst_inch ( (int) tmplv1, level2, &ier );
                             }
                         }
			 delta = True;
		     }
			
	             cst_inch ( (int) tmplv1, level1, &ier ); 

                     strcpy ( jetstr, "FL");
	             strcat ( jetstr, level1);
		     if ( delta ) {
			 strcat ( jetstr, "\n" );
			 strcat ( jetstr, level2 );
			 strcat ( jetstr, "/" );
			 strcat ( jetstr, level3 );
		     }

	             if ( ( xmxlat < 0.0F ) || ( xmnlat < 0.0F ) ) {
			 txtclr = 19;
	             }
	             else {
			 txtclr = 31;
	             }

		     if ( kk != 0 ) {
			 angle = windir[kk];
		         dist = 300000.0F;

			 if ( ptr->lat[kk] > 0.0F ) {
			     angle3 =  angle - 90.0F;
			 }
			 else {
			     angle3 = angle + 90.0F;
			 }

			 angle =  270.0F - angle ;
			 if ( angle < 0.0F ) {
			    angle += 360.0F;
			 }
	                 clo_dltln ( &ptr->lat[kk], &ptr->lon[kk], &dist,
		                     &angle3, &flat, &flon, &ier);
		     }
		 }

                /* 
	         * Fill in WIND BARB/HASH information. 
	         */

		 angle = windir[kk];

		 if ( iwnd[jj] == 1 ) {
		     
	            /*
                     * Wind barb text attributes.
                     */

		     strcpy ( el.elem.jet.barb[nbp].spt.text, jetstr );

                     el.elem.jet.barb[nbp].sptcol            = txtclr;
                     el.elem.jet.barb[nbp].spt.info.sptxtyp  = 5;
                     el.elem.jet.barb[nbp].spt.info.turbsym  = 4;
                     el.elem.jet.barb[nbp].spt.info.sztext   = 1.25F;
                     el.elem.jet.barb[nbp].spt.info.itxfn    = 2;
                     el.elem.jet.barb[nbp].spt.info.iwidth   = 1;
                     el.elem.jet.barb[nbp].spt.info.rotn     = angle + 1000.0F;
                     el.elem.jet.barb[nbp].spt.info.ialign   = 0;
                     el.elem.jet.barb[nbp].spt.info.ithw     = 1;
                     el.elem.jet.barb[nbp].spt.info.txtcol   = txtclr;
                     el.elem.jet.barb[nbp].spt.info.filcol   = 31;
                     el.elem.jet.barb[nbp].spt.info.lincol   = 31;
                     el.elem.jet.barb[nbp].spt.info.offset_x = 0;
                     el.elem.jet.barb[nbp].spt.info.offset_y = -4;
                     el.elem.jet.barb[nbp].spt.info.lat      = flat;
                     el.elem.jet.barb[nbp].spt.info.lon      = flon;

                    /*
                     * Fill in wind barb information.
                     */
                     tmpspd = ptr->speed[kk];
                     speed  = pr_mskn ( &tmpspd );

                     el.elem.jet.barb[nbp].wndcol          = 2;
                     el.elem.jet.barb[nbp].wnd.info.numwnd = 1;
                     el.elem.jet.barb[nbp].wnd.info.width  = 3;
                     el.elem.jet.barb[nbp].wnd.info.size   = 1.40F;
                     el.elem.jet.barb[nbp].wnd.info.wndtyp = 114;
                     el.elem.jet.barb[nbp].wnd.info.hdsiz  = 0.0F;

                     el.elem.jet.barb[nbp].wnd.data.latlon[0] = ptr->lat[kk];
                     el.elem.jet.barb[nbp].wnd.data.latlon[1] = ptr->lon[kk];
		     el.elem.jet.barb[nbp].wnd.data.spddir[0] = speed;
                     el.elem.jet.barb[nbp].wnd.data.spddir[1] = angle;

                     nbp++;

		 }
		 else if ( iwnd[jj] == -1 ) {

		    /*
		     * Fill in wind hashs information. 
		     */

		     tmpspd = ptr->speed[kk];
		     speed  = pr_mskn ( &tmpspd );

		     el.elem.jet.hash[nhp].wndcol          = 5;
		     el.elem.jet.hash[nhp].wnd.info.numwnd = 1;
                     el.elem.jet.hash[nhp].wnd.info.width  = 2;
                     el.elem.jet.hash[nhp].wnd.info.size   = 1.0F;
		     el.elem.jet.hash[nhp].wnd.info.wndtyp = 1;
                     el.elem.jet.hash[nhp].wnd.info.hdsiz  = 0.0F;
		     
		     el.elem.jet.hash[nhp].wnd.data.latlon[0] = ptr->lat[kk];
                     el.elem.jet.hash[nhp].wnd.data.latlon[1] = ptr->lon[kk];
		     el.elem.jet.hash[nhp].wnd.data.spddir[0] = RMISSD;
                     el.elem.jet.hash[nhp].wnd.data.spddir[1] = angle;
	
		     nhp++;
		 }

	     }
         }
	 
   	 el.elem.jet.nbarb = nbp;
	 el.elem.jet.nhash = nhp; 	

	/*
         * Write jet element to VG file.
         */

	 cvg_writeD ( &el, start, sizrec, fptr, &ier);
         start = sizrec + start;

         ptr = ptr->next;
     }

    /* 
     * Close output file. 
     */
     if ( fptr != NULL ) cvg_clos ( fptr, &ier );
     
     return;
}

#define MAXOUT          50000

void tangwnd ( float *plat, float *plon, int np, float *windir, 
               int *iret )
/************************************************************************
 * tangwnd                                                              *
 *                                                                      *
 * This function performs parametric curve fit to a sequence of JET	*
 * points, and computes the tangent wind direction at each point.	*
 *                                                                      *
 * tangwnd ( plat, plon, np, windir, iret ) 	            		*
 *                                                                      *
 * Input parameters:                                                    *
 *      *plat           float   latitude of points                      *
 *      *plon           float   longitude of points                     *
 *      np              int     number of points                        *
 *                                                                      *
 * Output parameters:                                                   *
 *      *windir         float   tangent wind direction                  *
 *      *iret           int     Return code                             *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          5/03                                           *
 * R. Tian/SAIC		 6/03	Modified to handle duplicate points	*
 ***********************************************************************/
{
    char proj[8];
    float minlat, maxlat;
    float lllat, lllon, urlat, urlon;
    float angle1, angle2, angle3;
    float px[MAXPTS], py[MAXPTS];
    float xcv[MAXOUT], ycv[MAXOUT];
    float dens, crvscl;
    int istrt, iend, maxpts, nout;
    float distance;
    int nearest, index[MAXPTS];
    float lat[3], lon[3], dir;
    int i, j, n, ier;

/*---------------------------------------------------------------------*/
    *iret = 0;

    minlat = 9999.0F;
    maxlat = -9999.0F;
    for ( i = 0; i < np; i++ ) {
        minlat = G_MIN ( minlat, plat[i] );
        maxlat = G_MAX ( maxlat, plat[i] );
    }

    /*
     * Set map projection.
     */
    if ( minlat >= 0.0F ) {
        /*
         * Use North STR projection.
         */
        strcpy ( proj, "STR" );
        angle1 = 90.0F;
        angle2 = -90.0F;
        angle3 = 0.0F;
        lllat = -15.0F;
        lllon = -135.0F;
        urlat = -15.0F;
        urlon = -135.0F;
    }
    else if (maxlat < 0.0F ) {
        /*
         * Use South STR projection.
         */
        strcpy ( proj, "STR" );
        angle1 = -90.0F;
        angle2 = -90.0F;
        angle3 = 0.0F;
        lllat = 15.0F;
        lllon = -135.0F;
        urlat = 15.0F;
        urlon = -135.0F;
    }
    gsmprj ( proj, &angle1, &angle2, &angle3,
             &lllat, &lllon, &urlat, &urlon, &ier, strlen(proj) );
    /*
     * Transform coordinate from MAP to DEVICE, and perform
     * parametric curve fit.
     */
    gtrans ( sys_M, sys_D, &np, plat, plon, px, py, &ier,
             strlen(sys_M), strlen(sys_D) );

    dens = 5.0F;
    crvscl = 25.0F;
    istrt = 0;
    iend = MAXOUT;
    maxpts = MAXOUT;
    cv_prmt ( &np, px, py, &dens, &maxpts, &crvscl, &istrt, &iend, &nout,
              xcv, ycv, &ier );
    if ( ier != 0 ) {
        *iret = -1;
        return;
    }

    /*
     * Find the nearest point in (xcv, ycv) for each point in sequence
     * (px, py). 
     */
    for ( i = 0; i < np; i++ ) {
        cgr_dist ( nout, xcv, ycv, px[i], py[i], &distance,
                   &nearest, &ier );
        index[i] = nearest;
    }
    
    /*
     * Compute the direction.
     */
    for ( i = 0; i < np; i++ ) {
	j = index[i];

	if (j == 0 ) {
	    n = 2;
	    if ( xcv[j] == xcv[j+1] && ycv[j] == ycv[j+1] ) {
    	        gtrans ( sys_D, sys_M, &n, &xcv[j+1], &ycv[j+1], lat, lon, &ier,
             	         strlen(sys_D), strlen(sys_M) );
	    }
	    else {
    	        gtrans ( sys_D, sys_M, &n, &xcv[j], &ycv[j], lat, lon, &ier,
             	         strlen(sys_D), strlen(sys_M) );
	    }
	    clo_direct ( &lat[1], &lon[1], &lat[0], &lon[0], &dir, &ier );
	}
	else if ( j == nout - 1 ) {
	    n = 2;
	    if ( xcv[j] == xcv[j-1] && ycv[j] == ycv[j-1] ) {
    	        gtrans ( sys_D, sys_M, &n, &xcv[j-2], &ycv[j-2], lat, lon, &ier,
             	         strlen(sys_D), strlen(sys_M) );
	    }
	    else {
    	        gtrans ( sys_D, sys_M, &n, &xcv[j-1], &ycv[j-1], lat, lon, &ier,
             	         strlen(sys_D), strlen(sys_M) );
	    }
	    clo_direct ( &lat[1], &lon[1], &lat[0], &lon[0], &dir, &ier );

	}
	else {
	    n = 2;
	    if ( xcv[j] == xcv[j+1] && ycv[j] == ycv[j+1] ) {
    	        gtrans ( sys_D, sys_M, &n, &xcv[j-1], &ycv[j-1], lat, lon, &ier,
             	         strlen(sys_D), strlen(sys_M) );
	    }
	    else {
    	        gtrans ( sys_D, sys_M, &n, &xcv[j], &ycv[j], lat, lon, &ier,
             	         strlen(sys_D), strlen(sys_M) );
	    }
	    clo_direct ( &lat[1], &lon[1], &lat[0], &lon[0], &dir, &ier );

	}

	/*
	 * 'dir' is relative to North, wind direction is relative to South.
	 */
	dir += 180.0F;
	if (dir >= 360.0F ) {
	    dir -= 360.0F;
	}
	windir[i] = dir;
    }
}
