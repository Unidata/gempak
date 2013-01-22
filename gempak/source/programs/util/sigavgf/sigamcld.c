#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

#include "cascmn.h"
#include "proto_sigavgf.h"

#define  CLOUD_LINE  3
#define  LMULT       1
#define  LDIR        1
#define  LSIZE       0.800F
#define  LWID        2
#define  LENBUF      256
#define	 MAX_NCTYP   9
#define  MAX_NCDIS   6 

static void siga_hgtcvt ( float level, char *outlvl, int *iret );

void sigamcld ( char *fhour, int nummcld, mcloud_t *ptrm, int itime[], 
               char grpch, int *iret ) 
/************************************************************************
 * sigamcld                                                             *
 *                                                                      *
 * This program encodes the Mid Level Significant Weather ASCII cloud   *
 * information into VG file format.   					*
 *                                                                      *
 * sigamcld ( fhour, nummcld, ptrm, itime, grpch, iret )		* 
 *                                                                      *
 * Input parameters:                                                    *
 *	*fhour		char 	   Forecast hour			*
 *      nummcld         int	   Number of cloud lines		*
 *      *ptrm		mcloud_t   Pointer to MCLOUD link list		*
 *      itime[]		int	   Issued date/time 			*
 *      grpch           char       Group type				*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int        Return code                      	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC       01/05	                                 	*
 * S. Danz/AWC      07/06	Switched to new cvg_writeD function	*
 * M. Li/SAIC	    11/06	Check for longitude at 0 and 180 degree	*
 * L. Hinson/AWC    06/12       Fixed check to see if line is closed    *
 ***********************************************************************/
 {
    int   	    ii, ij, jj, kk, ier, txtlen, len, iera, ierb;
    FILE  	    *fptr;
    mcloud_t         *ptr;

    int		    numerr, leverr, wflg, tmptm, gpnum; 
    int             sizrec, start, cbflag;
    float           xmxlat, xmnlat, xmxlon, xmnlon;
    float           dist0, dist1; 
    float           alat[MAXPTS], alon[MAXPTS];
    float           blat[MAXPTS], blon[MAXPTS];
    float           tmplat[MAXPTS], tmplon[MAXPTS];
    float           xcent, ycent, area, axcent, aycent, aarea;
    float           bxcent, bycent, barea;
    char            level1[12], level2[12], tmpdis[16], tmptyp[16];
    char 	    ofname[LENBUF], errgrp[8], cldstr[132];
    char	    icstr[20], turbstr[20], cbstr[20];
    char            ctime[LENBUF], iclos, ctmp[10];

    char            *typlst[MAX_NCTYP] = {"CI", "CC", "CS", "AC", "AS", 
					  "NS", "SC", "ST", "CU"};
    char            *dislst[MAX_NCDIS] = {"SKC", "SCT", "BKN", "OVC", 
					  "LYR", "BKN;OVC"};
    int		    ncdis[] = { 0, 2, 3, 4, 14, 7 }; 
    int		    dic, tdeg;

    Boolean	    cbonly;

    VG_DBStruct     el;
/*---------------------------------------------------------------------*/
     *iret  = 0;
     fptr   = NULL;
     ier     = 0;
     leverr  = 0;
     start   = 0;
     wflg    = G_TRUE;
     strcpy ( errgrp, "SIGAVGF" );

    /* 
     * Create output filename.
     */

     strcpy ( ofname, "mcld_");

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
     * Loop through the cloud element list and write each one to VG file.
     */

     ptr = ptrm;
     start = sizeof ( VG_HdrStruct ) + sizeof ( FileHeadType );

     for ( ii = 0; ii < nummcld; ii++) {
        ij = 0;
        for ( jj = (ptr->npt - 1); jj >= 0; jj-- ) {
           tmplat[jj] = ptr->lat[ij];
           tmplon[jj] = ptr->lon[ij];
           ij++;
        } 
        for ( jj = 0; jj < ptr->npt; jj++ ) {
           ptr->lat[jj] = tmplat[jj];
           ptr->lon[jj] = tmplon[jj];
   	}

       /* 
	* Determine the maximum and minimum lat. and lon.
	*/

	for ( jj = 0; jj < ptr->npt; jj++ ) {
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

	/*
	 * Check if the line is closed.
	 */

	 if ( G_DIFF ( ptr->lat[0], ptr->lat[ptr->npt -1] ) &&
	      G_DIFF ( ptr->lon[0], ptr->lon[ptr->npt -1] ) ) {
	     iclos = 1;
	 }
	 else {
	     iclos = 0;
	 }

	/* 
	 * Fill in VG header information.
	 */

	 gpnum = ii+1;
	 sizrec = (int)(sizeof( VG_HdrStruct ) + sizeof( SpLineInfo )) +
	         ( ( ptr->npt * 2) * (int)sizeof (float) );

         el.hdr.delete   = 0;
         el.hdr.vg_type  = SPLN_ELM;
         el.hdr.vg_class = CLASS_LINES;
         el.hdr.maj_col  = 6;
         el.hdr.min_col  = 6;
         el.hdr.smooth   = 2;
         el.hdr.version  = 0;
         el.hdr.filled   = 0;
         el.hdr.closed   = iclos;
         el.hdr.recsz    = sizrec; 
         el.hdr.grptyp   = grpch;
         el.hdr.grpnum   = gpnum;
         el.hdr.range_min_lat = xmnlat;
         el.hdr.range_min_lon = xmnlon;
         el.hdr.range_max_lat = xmxlat;
         el.hdr.range_max_lon = xmxlon;

	/* 
	 * Fill in VG element information.
	 */

	 el.elem.spl.info.numpts = ptr->npt;
	 el.elem.spl.info.spltyp = CLOUD_LINE;  
	 el.elem.spl.info.splstr = LMULT; 
	 el.elem.spl.info.spldir = LDIR;
	 el.elem.spl.info.splsiz = LSIZE;
	 el.elem.spl.info.splwid = LWID;
         for ( kk = 0; kk < el.elem.spl.info.numpts; kk++ ) {
             el.elem.spl.latlon[kk] = ptr->lat[kk];
             el.elem.spl.latlon[kk+el.elem.spl.info.numpts] = ptr->lon[kk];
        }

	/* 
	 * Write cloud LINE element to VG file.
	 */

         cvg_writeD ( &el, start, sizrec, fptr, &ier);

	 start = sizrec + start;

	/* 
	 * Begin creating the cloud TEXT string.
         */

	 cbflag = 0;
	 cbonly = True;
	 
   	/*
	 * Non-cb cloud types.
	 */

	 if ( ptr -> ntyp > 0 ) { 
	     for ( jj = 0; jj < ptr -> ntyp; jj++ ) {
                 for ( ij = MAX_NCTYP-1; ij >= 0; ij-- ){
                     if ( ptr -> nctyp[jj] ==  ij ) {
                         if ( jj == 0 ) {
                             strcpy ( cldstr, typlst[ij] );
                         }
                         else {
                             strcat ( cldstr, ";" );
                             strcat ( cldstr, typlst[ij] );
                         }
                     }
                 }
             }

	     strcat ( cldstr, "|" );
         }
	 else {
	     strcpy ( cldstr, "|" );
         }

        /*
         * Non-Cb cloud distribution.
         */

         if ( ptr -> ncld > 0 ) {
             for ( jj = 0; jj < ptr -> ncld; jj++ ) {
                 for ( ij = 0; ij < MAX_NCDIS; ij++ ){
                     if ( ptr -> ncdis[jj] ==  ncdis[ij] ) {
                         if ( jj == 0 ) {
                             strcat ( cldstr, dislst[ij] );
                         }
                         else {
                             strcat ( cldstr, ";" );
                             strcat ( cldstr, dislst[ij] );
                         }
                     }
                 }
             }
         }
	 strcat ( cldstr, "|" );

	/*
	 * Icing.
	 */

	 if ( ptr -> icing == 1 ) {

	    /*
	     * Degree of icing.
	     */

	     if ( ptr -> dic == 5 ) {
		 dic = 5;
	     }
	     else if ( ptr -> dic == 8 ) {
                 dic = 7;
             }
	     else {
                 dic = 0;
             }

	    /*
	     * Top/base of icing.
	     */

	     if ( G_DIFF(ptr->icbase, SIGRLMS) ) {
                 strcpy( level1, "");
             }
             else {
                 siga_hgtcvt ( ptr->icbase, level1, &ier );
             }

	     if ( G_DIFF(ptr->ictop, SIGRLMS) ) {
                 strcpy( level2, "");
             }  
             else {
                 siga_hgtcvt ( ptr->ictop, level2, &ier );
             }

	     if ( dic == 0 ) {
		 strcpy ( ctmp, "" );
	     }
	     else {
		 sprintf ( ctmp, "%d", dic );
	     }

	     if ( strlen ( level1 ) == (size_t)0 && strlen ( level2) == (size_t)0 ) {
	     	sprintf ( icstr, "%s||", ctmp );
	     }
	     else {
	     	sprintf ( icstr, "%s|%s/%s|", ctmp, level2, level1 );
	     }
	 }
	 else {
	     strcpy ( icstr, "||" );
	 }
	 strcat ( cldstr, icstr );

	/*
         * Turbulence.
         */

         if ( ptr -> turb == 1 ) {

             if ( ptr -> tdeg  == 2 ) {
                 tdeg = 4;
             }
             else if ( ptr -> tdeg == 19 || ptr -> tdeg == 12 ) {
                 tdeg = 56;
             }
	     else if ( ptr -> tdeg  == 3 ) {
                 tdeg = 6;
             }
	     else {
		 tdeg  = 0;
	     }

             if ( G_DIFF(ptr->tbase, SIGRLMS) ) {
                 strcpy( level1, "");
             }
             else {
                 siga_hgtcvt ( ptr->tbase, level1, &ier );
             }

             if ( G_DIFF(ptr->ttop, SIGRLMS) ) {
                 strcpy( level2, "");
             }
             else {
                 siga_hgtcvt ( ptr->ttop, level2, &ier );
             }

	     if ( tdeg == 0 ) {
                 strcpy ( ctmp, "" );
             }
             else {
                 sprintf ( ctmp, "%d", tdeg );
             }

             if ( strlen ( level1 ) == (size_t)0 && strlen ( level2) == (size_t)0 ) {
                sprintf ( turbstr, "%s||", ctmp );
             }
             else {
                sprintf ( turbstr, "%s|%s/%s|", ctmp, level2, level1 );
             }
         }
         else {
             strcpy ( turbstr, "||" );
         }
         strcat ( cldstr, turbstr );

	/*
	 * Determine Cb-only flag.
	 */

	 cbflag = ptr -> ncld + ptr -> ntyp + ptr -> tdeg + ptr -> dic;
	 if ( cbflag > 0 ) cbonly = False;

	/*
	 * Cb top/base.
	 */

	 if ( ptr -> fcb == 1 ) {

	    /*
             * Cb top/base.
             */

             if ( G_DIFF(ptr -> cbbase, SIGRLMS) ) {
	     	 if ( cbonly ) {
             	     strcpy( level1, "XXX");
	          }
	          else {
		     strcpy ( level1, "");
	          }
	     }
             else if ( !G_DIFF(ptr -> cbbase, SIGRLMS) ) {
	         siga_hgtcvt ( ptr->cbbase, level1, &ier );
             }

	     if ( G_DIFF(ptr -> cbtop, SIGRLMS) ) {
                 if ( cbonly ) {
                     strcpy( level2, "XXX");
                  }
                  else {
                     strcpy ( level2, "");
                  }
             }
             else if ( !G_DIFF(ptr -> cbtop, SIGRLMS) ) {
                 siga_hgtcvt ( ptr->cbtop, level2, &ier );
             }

            /*
             * Determine CB cloud distribution.
             */

             if ( ptr->cbdis == 12 ) {
                 strcpy( tmpdis, "FRQ");
             }
             else if ( ptr->cbdis == 11 ) {
		 if ( cbonly ) {
                     strcpy( tmpdis, "OCNL\n");
                     strcat( tmpdis, "EMBD");
		 }
		 else {
		    strcpy ( tmpdis, "OCNL;EMBD" );
		 }
             }
             else if ( ptr->cbdis == 9 ) {
		 if ( cbonly ) {
                     strcpy( tmpdis, "ISOL\n");
                     strcat( tmpdis, "EMBD");
		 }
		 else {
		     strcpy ( tmpdis, "ISOL;EMBD" );
		 }
             }
	     else if ( ptr->cbdis == 10 ) {
		 strcpy ( tmpdis, "OCNL" );
	     }
	     else if ( ptr->cbdis == 8 ) {
                 strcpy ( tmpdis, "ISOL" );
             }
	     else {
		 strcpy ( tmpdis, "" );
	     }

            /*
             * Determine cb cloud type. 
             */

             if ( ptr->cbtyp == 9 ) {
                 strcpy( tmptyp, "CB");
             }
	     else {
		 strcpy( tmptyp, "" );
	     }

            /*
             * Assemble cb cloud text string. 
             */

	     if ( cbonly ) {
	         sprintf( cldstr, "%s\n%s\n%s\n%s", tmpdis, tmptyp, 
	                  level2, level1);
	     }
	     else {
		 if ( strlen ( level1 ) == (size_t)0 && strlen ( level2) == (size_t)0 ) {
		     sprintf ( cbstr, "%s", tmpdis );
		 } 
		 else {
		     sprintf ( cbstr, "%s|%s/%s", tmpdis, level2, level1 );
		 }
		 strcat ( cldstr, cbstr );
	     }

	 }
	 else {
	     strcat ( cldstr, "|" );
	 }

        /*
         * Determine the lat/lon position for the text box. 
	 * If line crosses dateline, place box in the larger area.
         */

	 jj = 0;
	 kk = 0;
	 for ( ij = 0; ij < ptr->npt; ij++ ) {
	    if ( ptr->lon[ij] >= 0.0F ) {
	        alat[jj] = ptr->lat[ij]; 
	        alon[jj] = ptr->lon[ij]; 
		jj++;
	    }
	    else {
	        blat[kk] = ptr->lat[ij]; 
	        blon[kk] = ptr->lon[ij]; 
		kk++;
	    }
	 }

	 if ( ( jj > 0 ) && ( kk > 0 ) ) {
	     cgr_centroid (alat, alon, &jj, &axcent, &aycent, &aarea, &iera );
	     cgr_centroid (blat, blon, &kk, &bxcent, &bycent, &barea, &ierb );

	     if ( iera != -1 && ierb != -1 ) {
	         if ( G_ABS (aarea) > G_ABS (barea) ) {
		    xcent = axcent;
		    ycent = aycent;
	         }
	         else {
		    xcent = bxcent;
		    ycent = bycent;
	         }
	     } else if ( iera != -1 && ierb == -1 ) {
		xcent = axcent;
                ycent = aycent;
	     } else if ( iera == -1 && ierb != -1 ) {
                xcent = bxcent;
                ycent = bycent;
             } else {
		xcent = 0.5F * ( xmnlat + xmxlat ); 
		ycent = 0.5F * ( xmnlon + xmxlon ); 

		dist0 = G_ABS(xmnlon) + G_ABS(xmxlon);
	     	dist1 = G_ABS(G_ABS(xmnlon) - 180.0F) + G_ABS(G_ABS(xmxlon) - 180.0F);
		
		if ( dist1 < dist0 ) {
		    if ( ycent > 0 )
			ycent += -180.0F; 
		    else
			ycent += 180.0F;
		}
	
	     }
	 }
	 else {
	     cgr_centroid (ptr->lat, ptr->lon, &ptr->npt, &xcent, &ycent, 
	               &area, &ier );
	 }

	/* 
	 * Fill in cloud TEXT header information. 
	 */

	 cst_lstr ( cldstr, &txtlen, &ier );
         sizrec = (int)(sizeof( VG_HdrStruct ) + sizeof( SpTextInfo )) +
	           txtlen + 1;

         el.hdr.delete   = 0;
         el.hdr.vg_type  = SPTX_ELM;
         el.hdr.vg_class = CLASS_TEXT;
         el.hdr.maj_col  = 31;
         el.hdr.min_col  = 31;
         el.hdr.smooth   = 0;
         el.hdr.version  = 0;
         el.hdr.filled   = 0;
         el.hdr.closed   = 1;
         el.hdr.recsz    = sizrec; 
         el.hdr.grptyp   = grpch;
         el.hdr.grpnum   = gpnum;
         el.hdr.range_min_lat = (float)xcent;
         el.hdr.range_min_lon = (float)ycent;
         el.hdr.range_max_lat = (float)xcent;
         el.hdr.range_max_lon = (float)ycent;

	/* 
	 * Fill in cloud TEXT element information. 
	 */

	 if ( cbonly ) {
	     el.elem.spt.info.sptxtyp = 4;
	     el.elem.spt.info.turbsym  = 4;
	 }
	 else {
	     el.elem.spt.info.sptxtyp = 15;
             el.elem.spt.info.turbsym  = 0;
         }
	 el.elem.spt.info.sztext   = 1.07F;
	 el.elem.spt.info.itxfn    = 2;
	 el.elem.spt.info.iwidth   = 1;
	 el.elem.spt.info.rotn     = 0.0F;
	 el.elem.spt.info.ialign   = 0;
	 el.elem.spt.info.offset_x = 0;
	 el.elem.spt.info.offset_y = 0;
	 el.elem.spt.info.ithw     = 1;
	 el.elem.spt.info.txtcol   = 31;
	 el.elem.spt.info.filcol   = 31;
	 el.elem.spt.info.lincol   = 31;
	 el.elem.spt.info.lat      = (float)xcent;
	 el.elem.spt.info.lon      = (float)ycent;
	 strcpy ( el.elem.spt.text, cldstr );

	/* 
	 * Write cloud TEXT element information.
	 */

         cvg_writeD ( &el, start, sizrec, fptr, &ier);

	/* 
	 * Advance pointer in link list and increment 'start'.
	 */

	 start = sizrec + start;
	 ptr = ptr->next;
     }

    /* 
     * Close output file. 
     */
     if ( fptr != NULL ) cvg_clos ( fptr, &ier );
     
     return;
}


static void siga_hgtcvt ( float level, char *outlvl, int *iret )
/************************************************************************
 * siga_hgtcvt                                                          *
 *                                                                      *
 * This function converts height level from float format to string.     *
 *                                                                      *
 *                                                                      *
 * void  siga_hgtcvt ( level, outlvl, iret )                  		*
 *                                                                      *
 * Input parameters:                                                    *
 * 	level		float	height level in float format		*
 * 	*outlvl		char	height level string			*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*iret        	int 	Return code                             *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		01/05	Created					*
 ***********************************************************************/
{
    int		    ier, tmpmod;
    float           tmplvl;
/*---------------------------------------------------------------------*/
    *iret = 0;
    tmplvl = pr_hgmf (&level);
    tmplvl = tmplvl/ 100.0F;
    tmpmod = ( (int) tmplvl % 10 );
    if (  tmpmod >= 5 ) {
   	tmplvl = tmplvl + (float)( 10 - tmpmod );
    }
    else {
        tmplvl = tmplvl - (float)tmpmod;
    }

    cst_inch ( (int) tmplvl, outlvl, &ier );
}

