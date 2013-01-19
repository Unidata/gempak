#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "cds.h"

void cds_ccf ( VG_DBStruct *el, int indx, int *iret )
/************************************************************************
 * cds_ccf								*
 *									*
 * This function displays CCFs to the output device.			*
 *									*
 * cds_ccf (el, indx, iret)						*
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
 * D.W.Plummer/NCEP	 7/99	Copied from cds_sig			*
 * S. Law/GSC		02/00	added smoothing				*
 * S. Law/GSC		05/00	added fill, moved color setting to pgen	*
 * A. Hardy/GSC         11/00   renamed output coord. system declaration*
 * J. Wu/GSC            02/01   Modified 'unused1' in VG to 'smooth'	*
 * D.W.Plummer/NCEP	 5/01	Added check for cdsColor		*
 * M. Li/SAIC		01/03	delete vgstruct.h			*
 * H. Zeng/SAIC		02/05	assigned iftyp with a new value		*
 * L. Hinson/AWC        07/09   Add Setting/Uattrib tbl functionality   *
 *                              Add Connector, if needed, from text box *
 *                              to polygon.  Add motion vector/speed    *
 * L. Hinson/AWC        02/10   Fix uninitialized 'ifilled' on Lines    *
 ***********************************************************************/
{
    int		ii, np, ier, istyp, width, lintyp, lthw, lwhw;
    int         iltypx, ilthwx, iwidthx, iwhwx, icolrx, iftypx;
    int		ifilled, iftyp;
    float	szfilx, szfil, lat[MAX_SIGMET], lon[MAX_SIGMET];
    int idx;
    VG_DBStruct el_tmp;
    CCFType	*pccf;
    
    int npts, npls, *inout;
    float *xpt, *ypt, *xpl, *ypl;
    
    float areadir, areaspd, xcent, ycent, minangle, V1x, V1y, V2u, V2v;
    float angle, latmv[2], lonmv[2];
    int i, motionvertexnum, foundmotionvertex;
    char    textLayoutStr[256];
    Boolean textLayoutNIL = False;
    
/*---------------------------------------------------------------------*/

    *iret = 0;

    /*
     *  Save plot attributes.
     */
    gqcolr (&icolrx, &ier);
    gqline (&iltypx, &ilthwx, &iwidthx, &iwhwx, &ier);
    gqfill (&szfilx, &iftypx, &ier);
   
    /*
     * setup basic information
     */
    lintyp = 1;
    lthw  = 0;
    width = 3;
    lwhw  = 0;
    gsline (&lintyp, &lthw, &width, &lwhw, &ier);
    
    pccf  = &(el->elem.ccf);
    np    = pccf->info.npts;
    /*
     * Set the color for the feature.
     */
    if (cdsColor == 0) {
      if ( cdsUattr[indx].maj_col == 0 ) {
        if (pccf->info.prob == (CCFLVL_HIGH + 1)) { /*1 = High, 2= Low */
          gscolr ( &(el->hdr.maj_col), &ier);
	}
	else {
          gscolr ( &(el->hdr.min_col), &ier);
	}
      }
      else {
        /* Use the Uattrib table */
        if (pccf->info.prob == (CCFLVL_HIGH + 1)) {
          gscolr ( &cdsUattr[indx].maj_col, &ier);
          el->hdr.maj_col = cdsUattr[indx].maj_col; /* Colors everything else */
        } else {
          gscolr ( &cdsUattr[indx].min_col, &ier);
          el->hdr.maj_col = cdsUattr[indx].min_col;  /* Colors everything else */
        }
      }
    } else {
      gscolr ( &cdsColor, &ier);
    }

    /*
     *  Set the smoothing level_tmp
     */
    ii = (cdsUattr[indx].smooth == -1) ?
	 (int) el->hdr.smooth : cdsUattr[indx].smooth;
    istyp = (ii == 0) ? 0 : 2;
    gssmth (&istyp, &cdsSmthDens[ii], &ier);

    /*
     * If fill is set, fill the polygon.
     */
    ifilled=0;
    if (cdsUattr[indx].filled == 0) {
      ifilled = (int) el->hdr.filled;
    } else {
      if (pccf->info.subtype == SIGTYP_AREA) {      
        if (pccf->info.cover == CCFLVL_HIGH + 1) {
          ifilled =  cdsUattr[indx].info.ccf->fillhi;
        }
        if (pccf->info.cover == CCFLVL_MEDIUM + 1) {
          ifilled = cdsUattr[indx].info.ccf->fillmed;
        }
        if (pccf->info.cover == CCFLVL_LOW + 1) {
          ifilled = cdsUattr[indx].info.ccf->filllow;
        }
      }
    }
    
    if (ifilled >= 1 && cdsFill == 1) {
	iftyp = ifilled;
	szfil = 1.0F;
	gsfill (&szfil, &iftyp, &ier);

	gfill (sys_M, &np, pccf->latlon, &(pccf->latlon[np]),
	       &ier, strlen (sys_M));

	iftyp = 1;
	gsfill (&szfil, &iftyp, &ier);
    }
    
    if (cdsUattr[indx].info.ccf->linetype == 0) {
      lintyp = el->elem.ccf.info.linetype;
    } else {
      lintyp = cdsUattr[indx].info.ccf->linetype;
    }
        
    switch (pccf->info.subtype)  {

      case	SIGTYP_LINE_HIGH:		/* line		*/
        gsline (&lintyp, &lthw, &width, &lwhw, &ier);
	for (ii = 0; ii < np; ii++)  {
	    lat[ii] = pccf->latlon[ii];
	    lon[ii] = pccf->latlon[ii+np];
	}

	gline (sys_M, &np, lat, lon, &ier, strlen (sys_M));

	break;

      case      SIGTYP_LINE_MED:               /* line         */
        gsline (&lintyp, &lthw, &width, &lwhw, &ier);
        for (ii = 0; ii < np; ii++)  {
	    lat[ii] = pccf->latlon[ii];
	    lon[ii] = pccf->latlon[ii+np];
	}

	gline (sys_M, &np, lat, lon, &ier, strlen (sys_M));

        break;
        
      case	SIGTYP_AREA:		/* area		*/
        gsline (&lintyp, &lthw, &width, &lwhw, &ier);

	for ( ii = 0; ii < np; ii++ )  {
	    lat[ii] = pccf->latlon[ii];
	    lon[ii] = pccf->latlon[ii+np];
	}
	lat[np] = pccf->latlon[0];
	lon[np] = pccf->latlon[np];
	np++;

	gline (sys_M, &np, lat, lon, &ier, strlen (sys_M));

	break;

    }
    
    /* Check to see if textLayoutString for CCF Text contains NIL */
    /* If so, set the variable textLayoutNIL to TRUE. */
    
    if ( cdsUattr[indx].info.ccf->textLayout[0] == '\0' ) {
      strcpy(textLayoutStr, el->elem.ccf.textLayout);
    } else {
      strcpy(textLayoutStr, cdsUattr[indx].info.ccf->textLayout);
    }
    if (strstr(textLayoutStr, "NIL") != NULL) {
      textLayoutNIL = True;
    }

    /*
     * Reset smooth level to 0
     */
    if (istyp > 0) {
	istyp = 0;
	gssmth (&istyp, &cdsSmthDens[0], &ier);
    }
    
    /*
     *  Restore the saved plot attribute values
     */
    gsline ( &iltypx, &ilthwx, &iwidthx, &iwhwx, &ier );
    gscolr ( &icolrx, &ier );
    gsfill (&szfilx, &iftypx, &ier);
        
    if (! (pccf->info.subtype == SIGTYP_LINE_HIGH || pccf->info.subtype == SIGTYP_LINE_MED)) {
      /* Create an arrow if the text box is outside the CCF polygon*/
      cds_getinx( el, &idx, &ier);  /* Get the idx to the arrow size */
      npts = 1;
      G_MALLOC( xpt, float, npts, "cds_ccf: xpt" );
      G_MALLOC( ypt, float, npts, "cds_ccf: ypt" );
      G_MALLOC( inout, int, npts, "cds_ccf: inout" );
      xpt[0] = el->elem.ccf.info.textlat;
      ypt[0] = el->elem.ccf.info.textlon;
      npls = el->elem.ccf.info.npts;
      G_MALLOC( xpl, float, npls, "cds_ccf: xpl" );
      G_MALLOC( ypl, float, npls, "cds_ccf: ypl" );
      for ( ii = 0; ii < npls; ii++ ) {
          xpl[ii] = el->elem.ccf.latlon[ii];
          ypl[ii] = el->elem.ccf.latlon[ii+npls];
      }
      cgr_inpoly ( sys_M, &npts, xpt, ypt, sys_M, &npls, xpl, ypl, inout, &ier );
      /*
       * If the center of the text box is outside of GFA polygon, and TextLayoutNIL
       * not set display an arrowed line.
       */
      if ( inout[0] == 0  && textLayoutNIL == False) {
        el_tmp.hdr.delete   = 0;
        el_tmp.hdr.vg_type  = SPLN_ELM;
        el_tmp.hdr.vg_class = (char)CLASS_LINES;
        el_tmp.hdr.filled   = 0;
        el_tmp.hdr.closed   = 0;
        el_tmp.hdr.smooth   = 0;
        el_tmp.hdr.version  = 0;
        el_tmp.hdr.grptyp   = 0;
        el_tmp.hdr.grpnum   = 0;
        el_tmp.hdr.maj_col  = el->hdr.maj_col;
        el_tmp.hdr.min_col  = el->hdr.min_col;
        el_tmp.hdr.recsz    = 0;
        el_tmp.hdr.range_min_lat = 0;
        el_tmp.hdr.range_min_lon = 0;
        el_tmp.hdr.range_max_lat = 0;
        el_tmp.hdr.range_max_lon = 0;

        el_tmp.elem.spl.info.numpts = 2;
        el_tmp.elem.spl.info.spltyp = 4;
        el_tmp.elem.spl.info.splstr = 0.5;
        el_tmp.elem.spl.info.spldir = 1;
        el_tmp.elem.spl.info.splsiz = 1.0;
        el_tmp.elem.spl.info.splsiz = el->elem.ccf.info.szarrow;
        /* Set the Arrow Size from the settings table... */
        if ( fabs (cdsUattr[idx].info.ccf->szarrow < 0.01) ) {
          el_tmp.elem.spl.info.splsiz = el->elem.ccf.info.szarrow;
        } else {
          el_tmp.elem.spl.info.splsiz = cdsUattr[idx].info.ccf->szarrow;
        }
                        
        el_tmp.elem.spl.latlon[0] = el->elem.ccf.info.textlat;
        el_tmp.elem.spl.latlon[1] = el->elem.ccf.info.arrowlat;
        el_tmp.elem.spl.latlon[2] = el->elem.ccf.info.textlon;
        el_tmp.elem.spl.latlon[3] = el->elem.ccf.info.arrowlon;
        el_tmp.elem.spl.info.splwid = 3;
        el_tmp.hdr.maj_col = el_tmp.hdr.min_col = 31;        
        cds_dspelm(&el_tmp, &ier);
        el_tmp.elem.spl.info.splwid = 1;
        el_tmp.hdr.maj_col = el_tmp.hdr.min_col = 32;
        cds_dspelm(&el_tmp, &ier);

      }
      G_FREE(xpl, float);
      G_FREE(ypl, float);
      G_FREE(xpt, float);
      G_FREE(ypt, float);
      G_FREE(inout, int);
      /*
       * Create the text box element
       */
      cds_ccftxt(el, &el_tmp, &ier);
      if (ier == 3) { /* exit routine if ier set to 3 */
        *iret = 0;    /* on NIL set in layout string */
        return;
      }
      cds_dspelm( &el_tmp, &ier);
      
      /* Create a Motion Vector */
      areadir = el->elem.ccf.info.dir+180.0;
      areaspd = el->elem.ccf.info.spd;
      xcent = el->elem.ccf.info.arrowlon;
      ycent = el->elem.ccf.info.arrowlat;
      minangle = 360.0;
      motionvertexnum = 0;
      foundmotionvertex=0;
      for (i=0; i < npls; i++) {
        V1x = lon[i] - xcent;
        V1y = lat[i] - ycent;
        V2u=-areaspd*sin(areadir*PI/180);
        V2v=-areaspd*cos(areadir*PI/180);
        /* OK...Compute the angle using the ArcCossine of the dot product
	    of two-vectors/divided by their magnitude */
        angle=acos((V1x*V2u+V1y*V2v)/(hypot(V1x,V1y)*hypot(V2u,V2v)))
	     *180.0/PI;
        if (angle < minangle) {  /* Get the minimum angle */
          minangle=angle;
          motionvertexnum=i;
          foundmotionvertex=1;
        }
      }
      if (foundmotionvertex) {
        /* Now do text box to side of motion vector */        
        el_tmp.hdr.vg_class=CLASS_TEXT;
        el_tmp.hdr.vg_type=(char)SPTX_ELM;
        el_tmp.elem.spt.info.lat=lat[motionvertexnum]-1.9*cos(areadir*PI/180);
        el_tmp.elem.spt.info.lon=lon[motionvertexnum]-1.9*sin(areadir*PI/180);
        el_tmp.elem.spt.info.offset_x=0;
        el_tmp.elem.spt.info.offset_y=0;
        el_tmp.elem.spt.info.ialign=-1;
        el_tmp.elem.spt.info.rotn=1000;
        el_tmp.elem.spt.info.sztext=0.70;
        el_tmp.elem.spt.info.itxfn=1;
        el_tmp.elem.spt.info.ithw=2;
        el_tmp.elem.spt.info.iwidth=1;
        el_tmp.elem.spt.info.txtcol=el->hdr.maj_col;
        el_tmp.elem.spt.info.lincol=el->hdr.min_col;
        el_tmp.elem.spt.info.filcol=1;
        el_tmp.elem.spt.info.sptxtyp=5;
        el_tmp.elem.spt.info.turbsym=0;
        sprintf(el_tmp.elem.spt.text, "%d", (int)(el->elem.ccf.info.spd));
        cds_dspelm(&el_tmp, &ier);
        /*  Draw the Motion vector */
        np=2;
        el_tmp.hdr.delete   = 0;
        el_tmp.hdr.vg_class=CLASS_LINES;
        el_tmp.hdr.vg_type= (char) SPLN_ELM;
        el_tmp.hdr.smooth=0;
        el_tmp.hdr.maj_col  = el->hdr.maj_col;
        el_tmp.hdr.min_col  = el->hdr.min_col;
        el_tmp.elem.spl.info.spltyp=6;
        el_tmp.elem.spl.info.splstr=1;
        el_tmp.elem.spl.info.spldir=1;
        el_tmp.elem.spl.info.splsiz=0.5;
        el_tmp.elem.spl.info.splwid=4;
        el_tmp.hdr.closed=(char)1;
        el_tmp.hdr.filled=(char)0;
        el_tmp.hdr.smooth=(char)0;

        el_tmp.elem.spl.info.numpts=np;
        latmv[1]=lat[motionvertexnum];
        lonmv[1]=lon[motionvertexnum];
        latmv[0]=lat[motionvertexnum]-1.1*cos(areadir*PI/180);
        lonmv[0]=lon[motionvertexnum]-1.1*sin(areadir*PI/180);
        for ( ii = 0; ii < np; ii++ ) {
                  el_tmp.elem.spl.latlon[ii]    = latmv[ii];
                  el_tmp.elem.spl.latlon[ii+np] = lonmv[ii];
        }
        cds_dspelm(&el_tmp, &ier);           
      }
    }
}
