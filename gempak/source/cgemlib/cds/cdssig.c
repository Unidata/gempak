#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

void cds_sig ( VG_DBStruct *el, int indx, int *iret )
/************************************************************************
 * cds_sig								*
 *									*
 * This function displays SIGMETs to the output device.			*
 *									*
 * cds_sig (el, indx, iret)						*
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
 * D.W.Plummer/NCEP	 7/99	Copied from cds_line			*
 * D.W.Plummer/NCEP	 9/99	Compute circle from element distance;	*
 *				Compute line extension area		*
 * H. Zeng/EAI           9/99   Preserve plot attributes                *
 * F. J. Yen/NCEP 	10/99   Handled user attribute table            *
 * M. Li/GSC		10/99	Modified clo_direct and clo_dltln codes	*
 * D.W.Plummer/NCEP	12/99	Added plotting of sequence number	*
 * M. Li/GSC		 1/00	Used string variables in gtrans		*
 * S. Law/GSC		05/00	changed to use MAX_SIGMET for lat/lon	*
 * H. Zeng/EAI          06/00   increased the sizes of lat&lon arrays   *
 * A. Hardy/GSC         11/00   renamed coordinate system declarations	*
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 * T. Piper/SAIC        12/05   redone with new Setting_t structure     *
 ***********************************************************************/
{
    int		ii, kk, npts, np, intrsct, ier;
    int		mtype, color, width, lintyp, lthw, lwhw, mkhw, two;
    int         iltypx, ilthwx, iwidthx, iwhwx, icolrx, imarkx, imkhwx, imkwidx;
    char	str[4];
    float	lat[MAX_SIGMET*2+3], lon[MAX_SIGMET*2+3];
    float	size, dist, dir, ang1, ang2;
    float	dirs[]={ 0.0F, 180.0F, 90.0F, 270.0F };
    float	s1lat[2], s1lon[2], s2lat[2], s2lon[2];
    float	x1[2], y1[2], x2[2], y2[2];
    float	xint, yint;
    float       szmarkx;
    float	lbllat, lbllon, rotat=0.0F;
    int		ixoff=0, iyoff=2;
    int		itxfn_s, itxhw_s, itxwid_s, ibrdr_s, irrotn_s, ijust_s;
    float	sztext_s;
    int		itxfn, itxhw, itxwid, ibrdr, irrotn, ijust;
    float	sztext;
    SigmetType	*psig;
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Save plot attributes.
     */
    gqcolr ( &icolrx, &ier );
    gqline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gqmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &ier );
   
    /*
     * setup basic information
     */
    psig  = &(el->elem.sig);
    width =  (int) (( (  cdsUattr[indx].info.sig->linwid == 0 ) ?
		(float)psig->info.linwid :
		(float)cdsUattr[indx].info.sig->linwid) * cdsLineWdth);
    lintyp = (  cdsUattr[indx].info.sig->lintyp == 0 ) ?
		psig->info.lintyp : cdsUattr[indx].info.sig->lintyp;

    lthw  = 0;
    lwhw  = 0;
    mtype = 1;
    mkhw  = 0;
    size  = 1.0F;
    np    = psig->info.npts;

    gsline (&lintyp, &lthw, &width, &lwhw, &ier);

    color = (cdsColor == 0) ? 
       	 ( ( cdsUattr[indx].maj_col == 0 ) ?
	     el->hdr.maj_col : cdsUattr[indx].maj_col ) : cdsColor;
    gscolr (&color, &ier);

    switch ( psig->info.subtype )  {

      case	SIGTYP_ISOL:		/* isolated	*/

	/*
	 *  Plot marker w/ surrounding circle
	 */

        lat[0] = psig->latlon[0];
        lon[0] = psig->latlon[np];
        gsmrkr ( &mtype, &mkhw, &size, &width, &ier );
        gmark ( sys_M, &np, lat, lon, &ier, strlen(sys_M) );

	if ( !G_DIFF(psig->info.distance, 0.0F ) )  {

            dir = ( lat[0] >= 0.F ) ? 180.F : 0.F;
	    dist = psig->info.distance * NM2M;
	    clo_dltln ( &lat[0], &lon[0], &dist, &dir, &(lat[1]), &(lon[1]), &ier );
            np = 18;
            gcircl ( sys_M, lat, lon, &(lat[1]), &(lon[1]), &np, 
		     &ier, strlen(sys_M) );

	}

	break;

      case	SIGTYP_LINE:		/* line		*/

	for ( ii = 0; ii < np; ii++ )  {
	    lat[ii] = psig->latlon[ii];
	    lon[ii] = psig->latlon[ii+np];
	}

	gline ( sys_M, &np, lat, lon, &ier, strlen(sys_M) );

	if ( !G_DIFF(psig->info.distance, 0.0F) )  {

	    lintyp = 2;
    	    gsline (&lintyp, &lthw, &width, &lwhw, &ier);

	    dist = psig->info.distance * NM2M;

	    switch ( psig->info.sol )  {

		case	SIGLINE_NOF:
		case	SIGLINE_SOF:
		case	SIGLINE_EOF:
		case	SIGLINE_WOF:

		    npts = 1;
		    for ( ii = 0; ii < np; ii++ )  {
			clo_dltln ( &(psig->latlon[ii]), &(psig->latlon[ii+np]),
				    &dist, &(dirs[psig->info.sol-1]),
				    &(lat[npts]), &(lon[npts]), &ier );
			npts++;
		    }
		    lat[npts] = psig->latlon[np-1];
		    lon[npts] = psig->latlon[2*np-1];
		    npts++;

		    gline ( sys_M, &npts, lat, lon, &ier, strlen(sys_M) );

		break;

		case	SIGLINE_ESOL:

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

		    }

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

		    npts = 2*np + 3;
		    gline ( sys_M, &npts, lat, lon, &ier, strlen(sys_M) );

		break;

	    }

	}

	break;

      case	SIGTYP_AREA:		/* area		*/

	for ( ii = 0; ii < np; ii++ )  {
	    lat[ii] = psig->latlon[ii];
	    lon[ii] = psig->latlon[ii+np];
	}
	lat[np] = psig->latlon[0];
	lon[np] = psig->latlon[np];
	np++;

	gline ( sys_M, &np, lat, lon, &ier, strlen(sys_M) );

	break;

    }

    if ( el->hdr.vg_type == SIGCONV_ELM || el->hdr.vg_type == SIGOUTL_ELM )  {

	if ( el->hdr.vg_type == SIGCONV_ELM ) 
	    sprintf( str, "%d%c", psig->info.seqnum, psig->info.msgid[0] );
	else if ( el->hdr.vg_type == SIGOUTL_ELM )
	    sprintf( str, "%d", psig->info.seqnum );

	np = psig->info.npts;
	lbllat = psig->latlon[0];
	lbllon = psig->latlon[np];
	for ( ii = 1; ii < np; ii++ )  {
	    if ( psig->latlon[ii] > lbllat )  {
	        lbllat = psig->latlon[ii];
	        lbllon = psig->latlon[ii+np];
	    }
	}

	gqtext( &itxfn_s, &itxhw_s, &sztext_s, &itxwid_s, &ibrdr_s, 
		&irrotn_s, &ijust_s, &ier );
	itxfn  = 0;
	itxhw  = 0;
	sztext = 1.5F;
	itxwid = 0;
	ibrdr  = 0;
	irrotn = 0;
	ijust  = 2;
	gstext( &itxfn, &itxhw, &sztext, &itxwid, &ibrdr, &irrotn, &ijust, &ier );
	gtext( sys_M, &lbllat, &lbllon, str, &rotat, &ixoff, &iyoff, &ier, 
	       strlen(sys_M), strlen(str) );
	gstext( &itxfn_s, &itxhw_s, &sztext_s, &itxwid_s, &ibrdr_s, 
		&irrotn_s, &ijust_s, &ier );

    }

    /*
     *  Restore the saved plot attribute values
     */
    gsmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &ier );
    gsline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gscolr ( &icolrx, &ier );

}
