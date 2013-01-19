#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "crgcmn.h"

void crg_setsigmet ( VG_DBStruct *el, int joffset, int elnum, int *iret )
/************************************************************************
 * crg_setsigmet                                                        *
 *                                                                      *
 * This function sets the range for a sigmet element.			*
 *                                                                      *
 * crg_setsigmet ( el, joffset, elnum, iret ) 				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*el		VG_DBStruct	Element containing circle	*
 * 	joffset		int		File position of the element	*
 *	elnum		int		Element number			*
 *									*
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          07/02   initial coding                          *
 * H. Zeng/EAI          07/02   modified for very large iso. SIGMET     *
 ***********************************************************************/
{
    float	llx, lly, urx, ury, ccx, ccy, dist, ang1, ang2;
    float       new_llx, new_lly, new_urx, new_ury, new_ccx, new_ccy;
    float	lat[MAX_SIGMET*2+3], lon[MAX_SIGMET*2+3];
    float	s1lat[2], s1lon[2], s2lat[2], s2lon[2];
    float	x1[2], y1[2], x2[2], y2[2];
    float	xint, yint, new_dist;
    float	dirs[]={ 0.0F, 180.0F, 90.0F, 270.0F };
    int 	ii, kk, ier, np, npx, vg_subtype, two, intrsct;
    SigmetType  *psig;
/*---------------------------------------------------------------------*/

    *iret = 0;

    psig       = &(el->elem.sig);
    vg_subtype = psig->info.subtype;
    np         = psig->info.npts;
    dist       = psig->info.distance * NM2M;

    /*
     *  get bounds
     */
    crg_gbnd (sys_M, sys_D, np, &(psig->latlon[0]),
              &(psig->latlon[np]), &llx, &lly, &urx, &ury, 
              &ccx, &ccy);


    /*
     * For line or isolated SIGMET, range should be expanded 
     * because of the distance.
     */
    if ( vg_subtype == SIGTYP_ISOL && !G_DIFFT(dist, 0.0F, GDIFFD) ) {

         npx = 4;
         for ( ii = 0; ii < npx; ii++ ) {
	     clo_dltln ( &(psig->latlon[0]), &(psig->latlon[np]), 
                         &dist, &(dirs[ii]), &(lat[ii]), &(lon[ii]), &ier );

         }
         crg_gbnd (sys_M, sys_D, npx, &(lat[0]), &(lon[0]), 
                   &new_llx, &new_lly, &new_urx, &new_ury, 
                   &new_ccx, &new_ccy );

         /*
          * Calculate the distance between (new_ccx, new_ccy) and
          * (new_llx, new_lly).
          */
         new_dist = (float)sqrt( (double)((new_ccx - new_llx) * (new_ccx - new_llx) 
                           + (new_ccy - new_lly) * (new_ccy - new_lly) ) );

         /*
          * modify the range according to new_dist.
          */
         llx = new_ccx - new_dist;
         urx = new_ccx + new_dist;
         ury = new_ccy + new_dist;
         lly = new_ccy - new_dist;

    }
    else if ( vg_subtype == SIGTYP_LINE && !G_DIFFT(dist, 0.0F, GDIFFD) ) {

	 switch ( psig->info.sol )  {

	     case    SIGLINE_NOF:
	     case    SIGLINE_SOF:
	     case    SIGLINE_EOF:
	     case    SIGLINE_WOF:

		 npx = 0;
		 for ( ii = 0; ii < np; ii++ )  {
		     clo_dltln ( &(psig->latlon[ii]), &(psig->latlon[ii+np]),
				 &dist, &(dirs[psig->info.sol-1]),
				 &(lat[npx]), &(lon[npx]), &ier );
		     npx++;
		 }
                 crg_gbnd (sys_M, sys_D, npx, &(lat[0]), &(lon[0]), 
                           &new_llx, &new_lly, &new_urx, &new_ury, 
                           &new_ccx, &new_ccy );

	         break;

	     case    SIGLINE_ESOL:

		 lat[0] = psig->latlon[0];
		 lon[0] = psig->latlon[np];

		 clo_direct ( &(psig->latlon[1]), &(psig->latlon[np+1]),
			      &(psig->latlon[0]), &(psig->latlon[np  ]),
			      &ang1, &ier );

		 ang1 -= 90.0F;
		 clo_dltln ( &(psig->latlon[0]), &(psig->latlon[np]), &dist, 
			     &ang1, &(lat[2*np+1]), &(lon[2*np+1]), &ier );
		 ang1 = ang1 - 180.0F;
		 clo_dltln ( &(psig->latlon[0]), &(psig->latlon[np]), &dist, 
			     &ang1, &(lat[1]), &(lon[1]), &ier );

		 ang2 = ang1;

		 two = 2;
		 for ( ii = 1; ii < np-1; ii++ )  {

		  clo_direct ( &(psig->latlon[ii-1]), &(psig->latlon[np+ii-1]),
			       &(psig->latlon[ii]), &(psig->latlon[np+ii]), 
			       &ang1, &ier );
		  ang1 = (float)fmod ( ((double)ang1+270.0), 360.0);
		  clo_dltln ( &(psig->latlon[ii]), &(psig->latlon[np+ii]), 
			      &dist, &ang1, &(s1lat[1]), &(s1lon[1]), &ier );
		  clo_direct ( &(psig->latlon[ii+1]), &(psig->latlon[np+ii+1]),
			       &(psig->latlon[ii]), &(psig->latlon[np+ii]),
			       &ang2, &ier );
		  ang2 = (float)fmod ( ((double)ang2+90.0), 360.0);
		  clo_dltln ( &(psig->latlon[ii]), &(psig->latlon[np+ii]), 
			      &dist, &ang2, &(s2lat[0]), &(s2lon[0]), &ier );

		  if ( G_ABS(ang1-ang2) > 1.F )  {

		    clo_dltln ( &(psig->latlon[ii-1]), &(psig->latlon[np+ii-1]), 
				&dist, &ang1, &(s1lat[0]), &(s1lon[0]), &ier );
		    clo_dltln ( &(psig->latlon[ii+1]), &(psig->latlon[np+ii+1]), 
				&dist, &ang2, &(s2lat[1]), &(s2lon[1]), &ier );

		    gtrans ( sys_M, sys_N, &two, s1lat, s1lon, x1, y1, 
		             &ier, strlen(sys_M), strlen(sys_N) );
		    gtrans ( sys_M, sys_N, &two, s2lat, s2lon, x2, y2, 
		             &ier, strlen(sys_M), strlen(sys_N) );
		    cgr_segint( sys_N, x1, y1, sys_N, x2, y2,
			        sys_M, &xint, &yint, &intrsct, &ier );

		  }
		  else  {

		    xint = (s1lat[1] + s2lat[0]) / 2.0F;
		    yint = (s1lon[1] + s2lon[0]) / 2.0F;

		  }

		  kk = ii + 1;
		  lat[kk] = xint;
		  lon[kk] = yint;

		  ang1 = (float)fmod ( ((double)ang1+180.0), 360.0 );
		  ang2 = (float)fmod ( ((double)ang2+180.0), 360.0 );

		  clo_dltln ( &(psig->latlon[ii]), &(psig->latlon[np+ii]), 
			      &dist, &ang1, &(s1lat[1]), &(s1lon[1]), &ier );
		  clo_dltln ( &(psig->latlon[ii]), &(psig->latlon[np+ii]), 
			      &dist, &ang2, &(s2lat[0]), &(s2lon[0]), &ier );

		  if ( G_ABS(ang1-ang2) > 1.F )  {

		    clo_dltln ( &(psig->latlon[ii-1]), &(psig->latlon[np+ii-1]), 
				&dist, &ang1, &(s1lat[0]), &(s1lon[0]), &ier );
		    clo_dltln ( &(psig->latlon[ii+1]), &(psig->latlon[np+ii+1]), 
				&dist, &ang2, &(s2lat[1]), &(s2lon[1]), &ier );

		    gtrans ( sys_M, sys_N, &two, s1lat, s1lon, x1, y1, 
		             &ier, strlen(sys_M), strlen(sys_N) );
		    gtrans ( sys_M, sys_N, &two, s2lat, s2lon, x2, y2, 
		             &ier, strlen(sys_M), strlen(sys_N) );
		    cgr_segint( sys_N, x1, y1, sys_N, x2, y2,
			        sys_M, &xint, &yint, &intrsct, &ier );

		  }
		  else  {

		    xint = (s1lat[1] + s2lat[0]) / 2.0F;
		    yint = (s1lon[1] + s2lon[0]) / 2.0F;

		  }

		  kk = 2*np - ii + 1;
		  lat[kk] = xint;
		  lon[kk] = yint;

		  ang1 = (float)fmod ( ((double)ang1+180.0), 360.0 );
		  ang2 = (float)fmod ( ((double)ang2+180.0), 360.0 );

		  ang1 = ang2;

		 } /* the end of for (... */

		 clo_direct ( &(psig->latlon[np-2]), &(psig->latlon[2*np-2]),
			      &(psig->latlon[np-1]), &(psig->latlon[2*np-1]),
			      &ang2, &ier );

		 ang2 -= 90.0F;
		 clo_dltln ( &(psig->latlon[np-1]), &(psig->latlon[2*np-1]), 
			     &dist, &ang2, &(lat[np]), &(lon[np]), &ier );

		 ang2 = (float)fmod ( ((double)ang2+180.0), 360.0);
		 clo_dltln ( &(psig->latlon[np-1]), &(psig->latlon[2*np-1]), 
			     &dist, &ang2, &(lat[np+2]), &(lon[np+2]), &ier );

		 lat[np+1] = psig->latlon[np-1];
		 lon[np+1] = psig->latlon[2*np-1];

		 lat[2*np+2] = lat[0];
		 lon[2*np+2] = lon[0];

		 npx = 2*np + 3;
                 crg_gbnd (sys_M, sys_D, npx, &(lat[0]), &(lon[0]), 
                           &new_llx, &new_lly, &new_urx, &new_ury, 
                           &new_ccx, &new_ccy );

		break;

	 } /* the end of switch ... */

         /*
          * compare two set of ranges and get the union of them.
          */
         llx = ( llx <= new_llx ) ? llx : new_llx; 
         urx = ( urx >= new_urx ) ? urx : new_urx; 
         ury = ( ury >= new_ury ) ? ury : new_ury; 
         lly = ( lly <= new_lly ) ? lly : new_lly; 

    } /* the end of else if ... */


    llx -= (float)EXTRA_SM;
    urx += (float)EXTRA_SM;
    ury += (float)EXTRA_SM;
    lly -= (float)EXTRA_SM;

    /*
     *  Store the device coordinates in the range array.
     */
    crg_save(elnum, joffset, llx, lly, urx, ury, &ier);


}
