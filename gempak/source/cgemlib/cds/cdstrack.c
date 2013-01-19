#include "geminc.h"
#include "gemprm.h"
#include "cds.h"

void cds_track ( VG_DBStruct *el, int indx, int *iret )
/************************************************************************
 * cds_track								*
 *									*
 * This function displays tracks to the output device.			*
 *									*
 * cds_track (el, indx, iret)						*
 *									*
 * Input parameters:							*
 * 	*el		VG_DBStruct	Pointer to VG record structure	*
 * 	indx		int		Index into user attribute table	*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		05/99	Copied from cds_line			*
 * G. Krueger/EAI	06/99	Fixed doubled labels			*
 * G. Krueger/EAI	09/99	Preserve plot and text attributes	*
 * F. J. Yen/NCEP	10/99	Handled user attribute table		*
 * S. Law/GSC		07/00	Fixed string chopping			*
 * H. Zeng/EAI          08/00   added skip factor for track             *
 * M. Li/GSC		10/00	added skip = -2 , -3 and text attr.	*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 * A. Hardy/GSC         07/01   changed sztext check to 0.0;removed int *
 * M. Li/SAIC           01/03   delete vgstruct.h                       *
 * E. Safford/SAIC	12/03	add check on nip to keep it in range	*
 * T. Piper/SAIC        12/05   redone with new Setting_t structure     *
 ***********************************************************************/
{
    int		curr, ismth, np, nip, nep, ier, xoff, yoff, itime, ii, jj;
    int		skip, last, color, smooth, width, lthw, lwhw, mkhw;
    int         ismtypx, icolrx, iltypx, ilthwx, iwidthx, iwhwx, imarkx;
    int         imkhwx, imkwidx, itxfnx, itxhwx, itxwidx, ibrdrx;
    int         irrotnx, ijustx;
    int		itxfn, itxhw, itxwid, ibrdr, irrotn, ijust;
    int		iltype1, iltype2, imtype1, imtype2;
    char	*pt;
    fdttms_t	time;
    float	*plat, *plon, size, rotn, sztext;
    float       densx, sztextx, szmarkx;
    TrackType	*ptrk;
/*---------------------------------------------------------------------*/

    *iret = 0;
   
    /*
     *  Save plot and text attributes.
     */
    gqsmth ( &ismtypx, &densx, &ier );
    gqcolr ( &icolrx, &ier );
    gqline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gqmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &ier );
    gqtext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
	     &ijustx, &ier );

    /*
     *  Set the smoothing level.
     */
    ismth = ( cdsUattr[indx].smooth == -1 ) ?
		(int) el->hdr.smooth : cdsUattr[indx].smooth;
    smooth = (ismth == 0) ? 0 : 2;
    gssmth (&smooth, &cdsSmthDens[ismth], &ier);


    /*
     * setup basic information
     */
    ptrk  = &(el->elem.trk);
    width = (int)(( ( cdsUattr[indx].info.trk->width == 0 ) ?
	(float)ptrk->info.width : (float)cdsUattr[indx].info.trk->width ) * cdsLineWdth);
    lthw  = 0;
    lwhw  = 0;
    mkhw  = 0;
    size  = 1.25F;
    width = 3;
    np    = ptrk->info.npts;
    nip   = ptrk->info.nipts;
    nep   = np - nip;
    rotn  = 0.0F;
    xoff  = 2;
    yoff  = 2;

    /*
     * draw initial line, marks, and times
     */
    plat = ptrk->latlon;
    plon = &(ptrk->latlon[np]);
    color = (cdsColor == 0) ? 
	( ( cdsUattr[indx].maj_col == 0 ) ?
	     el->hdr.maj_col : cdsUattr[indx].maj_col )  : cdsColor;

    gscolr (&color, &ier);
    iltype1 = ( cdsUattr[indx].info.trk->ltype1 == 0 ) ?
	ptrk->info.ltype1 : cdsUattr[indx].info.trk->ltype1;
    gsline (&iltype1, &lthw, &width, &lwhw, &ier);
    gline  (sys_M, &nip, plat, plon, &ier, strlen(sys_M));
    imtype1 = ( cdsUattr[indx].info.trk->mtype1 == 0 ) ?
	ptrk->info.mtype1 : cdsUattr[indx].info.trk->mtype1;
    gsmrkr (&imtype1, &mkhw, &size, &width, &ier);
    gmark  (sys_M, &nip, plat, plon, &ier, strlen(sys_M));

    /*
     *  Set the text attributes.
     */
    itxfn  = ( cdsUattr[indx].info.trk->itxfn == 0 ) ?
	ptrk->info.itxfn : cdsUattr[indx].info.trk->itxfn;
    itxhw  = ( cdsUattr[indx].info.trk->ithw == 0 ) ?
	ptrk->info.ithw : cdsUattr[indx].info.trk->ithw; 
    sztext = ( ( G_DIFF(cdsUattr[indx].info.trk->sztext, 0.0F) ) ?
	ptrk->info.sztext :
	cdsUattr[indx].info.trk->sztext )  * cdsTxtSiz;;
    itxwid = 1;
    ibrdr  = 111;
    irrotn = 1;
    ijust  = 2;
    gstext( &itxfn, &itxhw, &sztext, &itxwid, &ibrdr, &irrotn, 
	    &ijust, &ier );

    for (curr = 0; curr < ptrk->info.nipts; curr++) {
	pt = strchr (ptrk->info.times[curr], '/');
	if (!pt) {
	    pt = ptrk->info.times[curr];
	}
	else {
	    pt++;
	}
	strcpy (time, pt);
	gtext (sys_M, &(plat[curr]), &(plon[curr]), time, &rotn, &xoff, &yoff, 
	       &ier, strlen(sys_M), strlen(time));
    }

    /*
     * draw extrapolated line, marks, and times
     */
    nep++;	/* include last initial point */
    plat = &(ptrk->latlon[(nip - 1)]);
    plon = &(ptrk->latlon[(np + nip - 1)]);
    color = (cdsColor == 0) ? 
	( ( cdsUattr[indx].min_col == 0 ) ?
	     el->hdr.min_col : cdsUattr[indx].min_col )  : cdsColor;

    gscolr (&color, &ier);
    iltype2 = ( cdsUattr[indx].info.trk->ltype2 == 0 ) ?
	ptrk->info.ltype2 : cdsUattr[indx].info.trk->ltype2;
    gsline (&iltype2, &lthw, &width, &lwhw, &ier);
    gline  (sys_M, &nep, plat, plon, &ier, strlen(sys_M));

    nep--;	/* do not include last initial point */
    plat = &(ptrk->latlon[nip]);
    plon = &(ptrk->latlon[(np + nip)]);
    imtype2 = ( cdsUattr[indx].info.trk->mtype2 == 0 ) ?
	ptrk->info.mtype2 : cdsUattr[indx].info.trk->mtype2;
    gsmrkr (&imtype2, &mkhw, &size, &width, &ier);
    gmark  (sys_M, &nep, plat, plon, &ier, strlen(sys_M));

    plat = ptrk->latlon;
    plon = &(ptrk->latlon[np]);
    skip = ptrk->info.skip; /* skip factor for time labels */
    last = (ptrk->info.npts)-1; /* the index of the last point */

    jj =0;

    /*
     * skip factor = -2, draw label at the top of the hours
     */
    if(skip == -2) {

      for(; curr <= last; curr++ ) {
        pt = strchr (ptrk->info.times[curr], '/');
        if (!pt) {
            pt = ptrk->info.times[curr];
        }
        else {
            pt++;
        }
        strcpy (time, pt);

        sscanf(time, "%d", &itime);
	ii = itime % 100;
       
	if ( ii == 0 ) {
            gtext (sys_M, &(plat[curr]), &(plon[curr]), time, &rotn,
               &xoff, &yoff, &ier, strlen(sys_M), strlen(time));

	    jj++;
	}  /* if  */

      }    /* for */

    }


    /*
     * skip factor = -3, draw label at the top of the hour and half hour
     */
    if(skip == -3) {

      for(; curr <= last; curr++ ) {
        pt = strchr (ptrk->info.times[curr], '/');
        if (!pt) {
            pt = ptrk->info.times[curr];
        }
        else {
            pt++;
        }
        strcpy (time, pt);

        sscanf(time, "%d", &itime);
	ii = (itime - 30) % 100;

        if ( (itime % 100) == 0 || ii == 0 ) {
            gtext (sys_M, &(plat[curr]), &(plon[curr]), time, &rotn,
               &xoff, &yoff, &ier, strlen(sys_M), strlen(time));

	    jj++;
        }  /* if  */

      }    /* for */

    }


    /*
     *  draw time labels for the first and last points.
     *  Add a check for nip to keep it in range.
     */
    if (skip == -1 || jj == 0) {
        curr = (nip < MAX_TRACKS-1) ? nip : MAX_TRACKS-1;

        pt = strchr (ptrk->info.times[curr], '/');
        if (!pt) {
            pt = ptrk->info.times[curr];
        }
        else {
            pt++;
        }

        strcpy (time, pt);
        gtext (sys_M, &(plat[curr]), &(plon[curr]), time, &rotn,
           &xoff, &yoff, &ier, strlen(sys_M), strlen(time));

        pt = strchr (ptrk->info.times[last], '/');
        if (!pt) {
            pt = ptrk->info.times[last];
        }
        else {
            pt++;
        }
        strcpy (time, pt);
        gtext (sys_M, &(plat[last]), &(plon[last]), time, &rotn,
           &xoff, &yoff, &ier, strlen(sys_M), strlen(time));
    }

    
    /*
     * Optionally draw time labels for the middle points.
     * Take skip factor into consideration.
     */
    if(skip >= 0) {     /* A valid skip factor >= 0 */

      curr+=(1+skip);   /* Go to the next candidate point */
      for(; curr < last; curr = curr+1+skip) {
        pt = strchr (ptrk->info.times[curr], '/');
        if (!pt) {
            pt = ptrk->info.times[curr];
        }
        else {
            pt++;
        }
        strcpy (time, pt);

        gtext (sys_M, &(plat[curr]), &(plon[curr]), time, &rotn,
               &xoff, &yoff, &ier, strlen(sys_M), strlen(time));
      }

    }

    /*
     *  Restore the saved plot and text attribute values
     */
    gstext ( &itxfnx, &itxhwx, &sztextx, &itxwidx, &ibrdrx, &irrotnx,
	     &ijustx, &ier );
    gsmrkr ( &imarkx, &imkhwx, &szmarkx, &imkwidx, &ier );
    gsline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gscolr ( &icolrx, &ier );
    gssmth ( &ismtypx, &densx, &ier );

}
