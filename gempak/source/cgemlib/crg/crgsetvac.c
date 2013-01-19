#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "crgcmn.h"

void crg_setvac ( VG_DBStruct *el, int joffset, int elnum, int *iret )
/************************************************************************
 * crg_setvac                                                           *
 *                                                                      *
 * This function sets the range for an ash cloud  element.		*
 *                                                                      *
 * crg_setvac ( el, joffset, elnum, iret ) 				*
 *                                                                      *
 * Input parameters:                                                    *
 *	*el		VG_DBStruct	Element containing ash cloud	*
 * 	joffset		int		File position of the element	*
 *	elnum		int		Element number			*
 *									*
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/XTRIA	09/03   initial coding				*
 * H. Zeng/XTRIA	11/03   expanded the range for ASH CLOUD	*
 * H. Zeng/XTRIA	01/04   added new types for ash cloud		*
 * H. Zeng/SAIC		08/04	removed NOTAVBL&ENDVAA			*
 * H. Zeng/SAIC		04/05	added more types			*
 ***********************************************************************/
{
    float	llx, lly, urx, ury, ccx, ccy, dist, ang1, ang2;
    float       new_llx, new_lly, new_urx, new_ury, new_ccx, new_ccy;
    float	lat[MAX_ASH*2+3], lon[MAX_ASH*2+3];
    float	s1lat[2], s1lon[2], s2lat[2], s2lon[2];
    float	x1[2], y1[2], x2[2], y2[2];
    float	xint, yint;
    int 	ii, kk, ier, np, npx, vg_subtype, two, intrsct;
    AshType     *pvac;
/*---------------------------------------------------------------------*/

    *iret = 0;

    pvac       = &(el->elem.ash);
    vg_subtype = pvac->info.subtype;
    np         = pvac->info.npts;
    dist       = pvac->info.distance * NM2M;

    /*
     *  get bounds
     */
    crg_gbnd (sys_M, sys_D, np, &(pvac->latlon[0]),
              &(pvac->latlon[np]), &llx, &lly, &urx, &ury, 
              &ccx, &ccy);


    /*
     * For line Ash Cloud, range should be expanded 
     * because of the distance.
     */
    if ( vg_subtype == ASHCLD_LINE && !G_DIFFT(dist, 0.0F, GDIFFD) ) {

	 switch ( pvac->info.sol )  {

	     default: /* ASHCLD_LINE always uses ESOL line option. */

		 lat[0] = pvac->latlon[0];
		 lon[0] = pvac->latlon[np];

		 clo_direct ( &(pvac->latlon[1]), &(pvac->latlon[np+1]),
			      &(pvac->latlon[0]), &(pvac->latlon[np  ]),
			      &ang1, &ier );

		 ang1 -= 90.0F;
		 clo_dltln ( &(pvac->latlon[0]), &(pvac->latlon[np]), &dist, 
			     &ang1, &(lat[2*np+1]), &(lon[2*np+1]), &ier );
		 ang1 = ang1 - 180.0F;
		 clo_dltln ( &(pvac->latlon[0]), &(pvac->latlon[np]), &dist, 
			     &ang1, &(lat[1]), &(lon[1]), &ier );

		 ang2 = ang1;

		 two = 2;
		 for ( ii = 1; ii < np-1; ii++ )  {

		  clo_direct ( &(pvac->latlon[ii-1]), &(pvac->latlon[np+ii-1]),
			       &(pvac->latlon[ii]), &(pvac->latlon[np+ii]), 
			       &ang1, &ier );
		  ang1 = (float)fmod ( ((double)ang1+270.0), 360.0);
		  clo_dltln ( &(pvac->latlon[ii]), &(pvac->latlon[np+ii]), 
			      &dist, &ang1, &(s1lat[1]), &(s1lon[1]), &ier );
		  clo_direct ( &(pvac->latlon[ii+1]), &(pvac->latlon[np+ii+1]),
			       &(pvac->latlon[ii]), &(pvac->latlon[np+ii]),
			       &ang2, &ier );
		  ang2 = (float)fmod ( ((double)ang2+90.0), 360.0);
		  clo_dltln ( &(pvac->latlon[ii]), &(pvac->latlon[np+ii]), 
			      &dist, &ang2, &(s2lat[0]), &(s2lon[0]), &ier );

		  if ( G_ABS(ang1-ang2) > 1.F )  {

		    clo_dltln ( &(pvac->latlon[ii-1]), &(pvac->latlon[np+ii-1]), 
				&dist, &ang1, &(s1lat[0]), &(s1lon[0]), &ier );
		    clo_dltln ( &(pvac->latlon[ii+1]), &(pvac->latlon[np+ii+1]), 
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

		  clo_dltln ( &(pvac->latlon[ii]), &(pvac->latlon[np+ii]), 
			      &dist, &ang1, &(s1lat[1]), &(s1lon[1]), &ier );
		  clo_dltln ( &(pvac->latlon[ii]), &(pvac->latlon[np+ii]), 
			      &dist, &ang2, &(s2lat[0]), &(s2lon[0]), &ier );

		  if ( G_ABS(ang1-ang2) > 1.F )  {

		    clo_dltln ( &(pvac->latlon[ii-1]), &(pvac->latlon[np+ii-1]), 
				&dist, &ang1, &(s1lat[0]), &(s1lon[0]), &ier );
		    clo_dltln ( &(pvac->latlon[ii+1]), &(pvac->latlon[np+ii+1]), 
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

		 clo_direct ( &(pvac->latlon[np-2]), &(pvac->latlon[2*np-2]),
			      &(pvac->latlon[np-1]), &(pvac->latlon[2*np-1]),
			      &ang2, &ier );

		 ang2 -= 90.0F;
		 clo_dltln ( &(pvac->latlon[np-1]), &(pvac->latlon[2*np-1]), 
			     &dist, &ang2, &(lat[np]), &(lon[np]), &ier );

		 ang2 = (float)fmod ( ((double)ang2+180.0), 360.0);
		 clo_dltln ( &(pvac->latlon[np-1]), &(pvac->latlon[2*np-1]), 
			     &dist, &ang2, &(lat[np+2]), &(lon[np+2]), &ier );

		 lat[np+1] = pvac->latlon[np-1];
		 lon[np+1] = pvac->latlon[2*np-1];

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

    } /* the end of if ... */
    else if ( vg_subtype == ASHCLD_NOTSEEN ||
	      vg_subtype >= ASHCLD_OTHERS     ) {
   
         /*
          * For no shape Ash Cloud types, range should be expanded 
          * because of the large text box.
          */
         llx -= 200;
         urx += 200;
         ury += 100;
         lly -= 100;
    }

	 
    llx -= (float)EXTRA_SM;
    urx += (float)EXTRA_SM;
    ury += (float)EXTRA_SM;
    lly -= (float)EXTRA_SM;

    /*
     *  Store the device coordinates in the range array.
     */
    crg_save(elnum, joffset, llx, lly, urx, ury, &ier);


}
