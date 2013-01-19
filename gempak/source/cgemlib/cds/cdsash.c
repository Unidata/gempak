#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

void cds_ash ( VG_DBStruct *el, int indx, int *iret )
/************************************************************************
 * cds_ash								*
 *									*
 * This function displays VAA ash cloud elements to the output device.	*
 *									*
 * cds_ash (el, indx, iret)						*
 *									*
 * Input parameters:							*
 * 	*el		VG_DBStruct	Pointer to VG record structure	*
 *	indx		int		Index into user attribute table *
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 6/03	Copied from cds_sig			*
 * H. Zeng/XTRIA	10/03   removed ASHCLD_ISOL			*
 * H. Zeng/XTRIA	10/03   added handling of NOTSEEN type		*
 * H. Zeng/XTRIA	11/03   added fill-pattern for area&line	*
 * H. Zeng/XTRIA	01/04   added new ash cloud types		*
 * H. Zeng/XTRIA	03/04	used half-width distance		*
 * H. Zeng/SAIC		08/04	removed NOTAVBL&ENDVAA			*
 * H. Zeng/SAIC		04/05	added more types			*
 * T. Piper/SAIC	12/05	redone with new Setting_t structure	*
 ***********************************************************************/
{
    int		ii, kk, npts, np, intrsct, ier;
    int		color, width, lintyp, lthw, lwhw, two;
    int         iltypx, ilthwx, iwidthx, iwhwx, icolrx;
    int		itxfnx, itxhwx, itxwidx, ibrdrx, irrotnx, ijustx;
    int		ibrdr, irrotn, iwdth, kialign, kitxfn, kithw, kturbsym;
    int		ifilled, iftyp, jftyp, iftypx;
    float	lat[MAX_ASH*2+3], lon[MAX_ASH*2+3];
    float	dist, ang1, ang2, szfil, szfilx;
    float	s1lat[2], s1lon[2], s2lat[2], s2lon[2];
    float	x1[2], y1[2], x2[2], y2[2];
    float	xint, yint;
    float       sztextx, rotn, txtsz;
    AshType	*pash;
    SptxType	*pspt;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Save plot attributes.
     */
    gqcolr ( &icolrx, &ier );
    gqline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gqtext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
             &ijustx, &ier );
    gqfill ( &szfilx, &iftypx, &ier );
   
    /*
     * setup basic information
     */
    pash  = &(el->elem.ash);
    width =  (int) (( (  cdsUattr[indx].info.ash->linwid == 0 ) ?
		(float)pash->info.linwid :
		(float)cdsUattr[indx].info.ash->linwid) * cdsLineWdth);
    lintyp = (  cdsUattr[indx].info.ash->lintyp == 0 ) ?
		pash->info.lintyp : cdsUattr[indx].info.ash->lintyp;

    lthw  = 0;
    lwhw  = 0;
    np    = pash->info.npts;

    gsline (&lintyp, &lthw, &width, &lwhw, &ier);

    color = (cdsColor == 0) ? 
       	 ( ( cdsUattr[indx].maj_col == 0 ) ?
	     el->hdr.maj_col : cdsUattr[indx].maj_col ) : cdsColor;
    gscolr (&color, &ier);

    switch ( pash->info.subtype )  {

      case	ASHCLD_LINE:		/* line		*/

	for ( ii = 0; ii < np; ii++ )  {
	    lat[ii] = pash->latlon[ii];
	    lon[ii] = pash->latlon[ii+np];
	}

	gline ( sys_M, &np, lat, lon, &ier, strlen(sys_M) );

	if ( !G_DIFF(pash->info.distance, 0.0F) )  {

	    lintyp = 2;
    	    gsline (&lintyp, &lthw, &width, &lwhw, &ier);

            /*
             * Get half-width distance, converted it to statute mile.
	     */

	    dist = (pash->info.distance / 2.0F) * NM2M;


	    lat[0] = pash->latlon[0];
	    lon[0] = pash->latlon[np];

	    clo_direct ( &(pash->latlon[1]), &(pash->latlon[np+1]),
				 &(pash->latlon[0]), &(pash->latlon[np  ]),
				 &ang1, &ier );

	    ang1 -= 90.0F;
	    clo_dltln ( &(pash->latlon[0]), &(pash->latlon[np]), &dist, 
				&ang1, &(lat[2*np+1]), &(lon[2*np+1]), &ier );
	    ang1 = ang1 - 180.0F;
	    clo_dltln ( &(pash->latlon[0]), &(pash->latlon[np]), &dist, 
				&ang1, &(lat[1]), &(lon[1]), &ier );

	    ang2 = ang1;

	    two = 2;
	    for ( ii = 1; ii < np-1; ii++ )  {

		clo_direct ( &(pash->latlon[ii-1]), &(pash->latlon[np+ii-1]),
				  &(pash->latlon[ii]), &(pash->latlon[np+ii]), 
				  &ang1, &ier );
		ang1 = (float)fmod ( ((double)ang1+270.0), 360.0);
		clo_dltln ( &(pash->latlon[ii]), &(pash->latlon[np+ii]), 
				 &dist, &ang1, &(s1lat[1]), &(s1lon[1]), &ier );
		clo_direct ( &(pash->latlon[ii+1]), &(pash->latlon[np+ii+1]),
				  &(pash->latlon[ii]), &(pash->latlon[np+ii]),
				  &ang2, &ier );
		ang2 = (float)fmod ( ((double)ang2+90.0), 360.0);
		clo_dltln ( &(pash->latlon[ii]), &(pash->latlon[np+ii]), 
				 &dist, &ang2, &(s2lat[0]), &(s2lon[0]), &ier );

		if ( G_ABS(ang1-ang2) > 1.F )  {

		    clo_dltln ( &(pash->latlon[ii-1]), &(pash->latlon[np+ii-1]), 
				   &dist, &ang1, &(s1lat[0]), &(s1lon[0]), &ier );
		    clo_dltln ( &(pash->latlon[ii+1]), &(pash->latlon[np+ii+1]), 
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

		clo_dltln ( &(pash->latlon[ii]), &(pash->latlon[np+ii]), 
				 &dist, &ang1, &(s1lat[1]), &(s1lon[1]), &ier );
		clo_dltln ( &(pash->latlon[ii]), &(pash->latlon[np+ii]), 
				 &dist, &ang2, &(s2lat[0]), &(s2lon[0]), &ier );

		if ( G_ABS(ang1-ang2) > 1.F )  {

		    clo_dltln ( &(pash->latlon[ii-1]), &(pash->latlon[np+ii-1]), 
			   &dist, &ang1, &(s1lat[0]), &(s1lon[0]), &ier );
		    clo_dltln ( &(pash->latlon[ii+1]), &(pash->latlon[np+ii+1]), 
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

	    }

	    clo_direct ( &(pash->latlon[np-2]), &(pash->latlon[2*np-2]),
				 &(pash->latlon[np-1]), &(pash->latlon[2*np-1]),
				 &ang2, &ier );

	    ang2 -= 90.0F;
	    clo_dltln ( &(pash->latlon[np-1]), &(pash->latlon[2*np-1]), 
				&dist, &ang2, &(lat[np]), &(lon[np]), &ier );

	    ang2 = (float)fmod ( ((double)ang2+180.0), 360.0);
	    clo_dltln ( &(pash->latlon[np-1]), &(pash->latlon[2*np-1]), 
				&dist, &ang2, &(lat[np+2]), &(lon[np+2]), &ier );

	    lat[np+1] = pash->latlon[np-1];
	    lon[np+1] = pash->latlon[2*np-1];

	    lat[2*np+2] = lat[0];
	    lon[2*np+2] = lon[0];

	    npts = 2*np + 3;

            /*
             * Check if fill is set.
             */

            ifilled = ( cdsUattr[indx].filled == 0 ) ?
		        (int) el->hdr.filled : cdsUattr[indx].filled;
            if ( ifilled >= 1 && cdsFill == 1 ) {

	         if  ( ifilled == 1 )
	               iftyp = 2;
	         else
	               iftyp = ifilled - 1;

	         szfil = 1.0F;
	         gsfill ( &szfil, &iftyp, &ier );

	         gfill(sys_M, &npts, lat, lon, &ier, strlen(sys_M));
       
	         jftyp = 1;
	         gsfill ( &szfil, &jftyp, &ier );
	    }

	    gline ( sys_M, &npts, lat, lon, &ier, strlen(sys_M) );

	}

	break;

      case	ASHCLD_AREA:		/* area		*/

	for ( ii = 0; ii < np; ii++ )  {
	    lat[ii] = pash->latlon[ii];
	    lon[ii] = pash->latlon[ii+np];
	}
	lat[np] = pash->latlon[0];
	lon[np] = pash->latlon[np];
	np++;

        /*
         * Check if fill is set.
         */

        ifilled = ( cdsUattr[indx].filled == 0 ) ?
		    (int) el->hdr.filled : cdsUattr[indx].filled;
        if ( ifilled >= 1 && cdsFill == 1 ) {

	     if  ( ifilled == 1 )
	           iftyp = 2;
	     else
	           iftyp = ifilled - 1;

	     szfil = 1.0F;
	     gsfill ( &szfil, &iftyp, &ier );

	     gfill(sys_M, &np, lat, lon, &ier, strlen(sys_M));
       
	     jftyp = 1;
	     gsfill ( &szfil, &jftyp, &ier );
	}

	gline ( sys_M, &np, lat, lon, &ier, strlen(sys_M) );

	break;

      case	ASHCLD_NOTSEEN:		/* not seen type     */

	/*
	 *  Plot special text for this type.
	 */
        pspt = &(pash->spt);
        lat[0] = pash->latlon[0];
        lon[0] = pash->latlon[np];

	ibrdr  = 111;
	irrotn = ( (int)pspt->info.rotn / 1000 ) + 1;
	rotn   = (float) fmod ( (double)pspt->info.rotn, 1000. );

	kialign = pspt->info.ialign;

	if (pspt->info.sptxtyp == 0) kialign += 2;

        txtsz  = (float)pspt->info.sztext * cdsTxtSiz;

        iwdth  = (int)((float)pspt->info.iwidth * cdsSptWdth);

	kitxfn = pspt->info.itxfn;

	kithw  = pspt->info.ithw;

        gstext (&kitxfn, &kithw, &txtsz, &iwdth, &ibrdr, &irrotn, 
		&kialign, &ier);	

	if (pspt->info.sptxtyp == 0) {

	    gtext ( sys_M, 
		   &(lat[0]),
		   &(lon[0]),
		   pspt->text,
		   &rotn,
		   &(pspt->info.offset_x),
		   &(pspt->info.offset_y),
		   &ier, 
		   strlen(sys_M), strlen(pspt->text));
	}
	else {

	    kturbsym = pspt->info.turbsym;
	    gtxsy ( sys_M, 
		   &(pspt->info.sptxtyp), 
		   &kturbsym,
		   &kialign,
		   &(pspt->info.offset_x),
		   &(pspt->info.offset_y),
		   &rotn,
		   &(lat[0]),
		   &(lon[0]),
		   pspt->text, &ier,
		   strlen(sys_M), strlen(pspt->text)); 
	}

	break;

      default:		/* types other than above */

	/*
	 *  Plot special text for all other types.
	 */
        pspt = &(pash->spt);
        lat[0] = pash->latlon[0];
        lon[0] = pash->latlon[np];

	ibrdr  = 111;
	irrotn = ( (int)pspt->info.rotn / 1000 ) + 1;
	rotn   = (float) fmod ( (double)pspt->info.rotn, 1000. );

	kialign = pspt->info.ialign;

	if (pspt->info.sptxtyp == 0) kialign += 2;

        txtsz  = (float)pspt->info.sztext * cdsTxtSiz;

        iwdth  = (int)((float)pspt->info.iwidth * cdsSptWdth);

	kitxfn = pspt->info.itxfn;

	kithw  = pspt->info.ithw;

        gstext (&kitxfn, &kithw, &txtsz, &iwdth, &ibrdr, &irrotn, 
		&kialign, &ier);	

	if (pspt->info.sptxtyp == 0) {

	    gtext ( sys_M, 
		   &(lat[0]),
		   &(lon[0]),
		   pspt->text,
		   &rotn,
		   &(pspt->info.offset_x),
		   &(pspt->info.offset_y),
		   &ier, 
		   strlen(sys_M), strlen(pspt->text));
	}
	else {

	    kturbsym = pspt->info.turbsym;
	    gtxsy ( sys_M, 
		   &(pspt->info.sptxtyp), 
		   &kturbsym,
		   &kialign,
		   &(pspt->info.offset_x),
		   &(pspt->info.offset_y),
		   &rotn,
		   &(lat[0]),
		   &(lon[0]),
		   pspt->text, &ier,
		   strlen(sys_M), strlen(pspt->text)); 
	}

	break;

    }


    /*
     *  Restore the saved plot attribute values
     */
    gsfill ( &szfilx, &iftypx, &ier );
    gstext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
             &ijustx, &ier );
    gsline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gscolr ( &icolrx, &ier );

}
