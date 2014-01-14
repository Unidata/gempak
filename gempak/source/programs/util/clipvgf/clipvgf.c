#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"

void clip_line ( int npoly, float *px, float *py, int npts, float *plat, 
		 float *plon, int closed, int maxpts, int *ninout, 
		 float *xinout, float *yinout, int *inout, int *iret );
                 
void recalcGFAtLblArwLoc(VG_DBStruct *el);

/************************************************************************
 * clipvgf.c								*
 *									*
 * CONTENTS:								*
 * clipvgf								*
 * clip_line                                                            *
 * recalcGFAtLblArwLoc                                                  *
 ***********************************************************************/
 

int main ( int argc, char **argv )
/************************************************************************
 * clipvgf								*
 *                                                                      *
 * This program clips elements in a VGF file based on a bounds 		*
 * specification.  By default, a simple clipping algorithm is used 	*
 * where element points either inside or outside the polygon are kept 	*
 * or thrown away based on an input flag.  Alternatively, an exact	*
 * algorithm may be requested which clips precisely at the borders.	*
 *                                                                      *
 * The bound definition must be in the format:				*
 * bound_name|<area_tag_name>area_tag_value				*
 * and must be enclosed w/ quotes so the shell will ignore directives.	*
 *									*
 * Examples:								*
 * clipvgf input.vgf "STATE_BNDS|<STATE>WY" keep output.vgf rough	*
 * clipvgf input.vgf "STATE_BNDS|<STATE>WY" keep output.vgf exact	*
 * Where "rough" uses the simple clipping algorithm and "exact" yields	*
 * precise clipping at the bounds borders.				*
 *                                                                      *
 * The following element classes are not processed:			*
 * CLASS_WATCHES, CLASS_TRACKS, CLASS_SIGMETS				*
 *                                                                      *
 * main(argc, argv)                                                     *
 *                                                                      *
 * Input parameters:                                                    *
 *  argc   int      number of parameters of command line                *
 *  argv   char**   parameter array of command line                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 2/02						*
 * D.W.Plummer/NCEP	 5/02	Added exact clipping and label moving	*
 * D.W.Plummer/NCEP	 8/02	Account for text grouped with symbol	*
 * D.W.Plummer/NCEP	 8/02	Create output VGF, even if it is empty	*
 * H. Zeng/XTRIA	02/03	converted CLASS_CIRCLE to CLASS LINES	*
 * D.W.Plummer/NCEP	 8/03	Bug fix - add pt to closed lines	*
 * R. Tian/SAIC		11/04	Added clip jet element			*
 * S. Danz/AWC		07/06	Update to new cvg_writef() parameter    *
 * T. Piper/SAIC	03/07	Added ninout-- for closed line case	*
 * L. Hinson/AWC        07/07   Added clip code for GFA elements        *
 * L. Hinson/AWC        07/07   Add recalcGFAtLblArwLoc function *
                                for GFA elements                        *
 * X.Guo/CWS		10/10   Bug fix - Low level graphic don't make  *
 *                              it all the way north in central US      *
 * L. Hinson/AWC        09/13   Fixed improperly clipped JET_ELM Barbs  *
 *                              and hashes                              *
 ***********************************************************************/
{
int    	ii, jj, ip, ibeg, iend, loc, ne, found, found_txt, joffset, kept, ier;
int    	wrtflg, pagflg;
int	minpts, maxpts, npts, numpts, npoly;
char	vg_class, vg_type;
char	bnd[128], keep[32], bnd_name[64], bnd_tag[64];
char    infile[128], ifname[128], outfile[128];
char	*cptr;
long	ifilesize;
int	more, curpos;
float	flat, flon, filt, px[LLMXPT], py[LLMXPT];
float	plat[LLMXPT], plon[LLMXPT], *ptrlat, *ptrlon;
float	tlat[LLMXPT], tlon[LLMXPT];
float	fltmin, fltmax, flnmin, flnmax;
int	inout[LLMXPT], tinout[LLMXPT];
char	device[8], dfilnam[128], pro[32];
float	xsize, ysize, lllat, lllon, urlat, urlon;
float	prjang1, prjang2, prjang3;
int	mode, istat, iunit, itype;
char	errgrp[8];
int	ninout;
float	xinout[LLMXPT], yinout[LLMXPT];
char	precision[8];
int	tltpts, nbarb, nhash;
int     tmaxpts, tnpts, tnclip;
char    hazList[ STD_STRLEN ];

VG_DBStruct     el, el_t, el_q, el_lin;

FILE    *ifptr;

int	ninxarr, inxarr[100];
/*---------------------------------------------------------------------*/

/*
 *  First check if number of input arguments is correct.
 */
    if ( argc < 5 )  {
	pagflg = G_FALSE;
	strcpy ( errgrp, "CLIPVGF" );
        ip_help ( errgrp, &pagflg, &ier,
                  strlen(errgrp) );
	exit (0);
    }

/*
 *  First input on command line is input vgf file name.
 */
    strcpy ( infile, argv[1] );
    wrtflg = 0;
    cvg_open ( infile, wrtflg, &(ifptr), &ier );
    if ( ier != 0 )  {
	printf("Error opening VGF file %s\n", infile );
	exit (0);
    }
    cfl_inqr ( infile, NULL, &ifilesize, ifname, &ier );

/*
 *  Second input on command line is bounds name.
 */
    clo_init ( &ier );
    strcpy ( bnd, argv[2] );
    cptr = cst_split( bnd, '|', sizeof(bnd_name), bnd_name, &ier );
    clo_bstype ( bnd_name, &ier );
    if ( ier != 0 ) {
      printf("Error finding bounds type %s\n", bnd_name );
      exit (0);
    }
    if ( cptr != (char *)NULL )  {
	strcpy ( bnd_tag, cptr );
        clo_bstag ( bnd_tag, &ier );
    }
    fltmin =  -90.0F; fltmax =  90.0F;
    flnmin = -180.0F; flnmax = 180.0F;
    clo_bsarea ( &fltmin, &flnmin, &fltmax, &flnmax, &ier );
    minpts = 3; maxpts = sizeof(px)/sizeof(float);
    filt = 0.0F;
    clo_bgnext ( &minpts, &maxpts, &filt, &npoly, px, py, &ier );
    if ( ier < 0 )  {
	printf("Error retrieving bound area %s|%s\n", bnd_name, bnd_tag );
	exit (0);
    }

/*
 *  Third input on command line is keep flag.
 */
    strcpy ( keep, argv[3] );

/*
 *  Fourth input on command line is output vgf file name; create it.
 */
    strcpy ( outfile, argv[4] );
    cvg_crvgf ( outfile, &ier );

/*
 *  Fifth input on command line is clip precision = "rough" or "exact"
 */
    if ( argv[5] != (char *)NULL ) 
        strcpy ( precision, argv[5] );
    else
	strcpy ( precision, "ROUGH" );
    cst_lcuc ( precision, precision, &ier );

/*
 *  All input checks out OK; set up GAREA and PROJ for inpoly.
 */
    mode = 1;
    ginitp ( &mode, &istat, &ier );

    strcpy ( device, "GN" );

    iunit = 1;
    strcpy ( dfilnam, "CLIPVGF" );
    itype = 1;
    xsize = 500.0F;
    ysize = 500.0F;

    gsdeva ( device, &iunit, dfilnam, &itype, &xsize, &ysize, &ier,
             strlen(device), strlen(dfilnam));

/*
 *  Something more sophisticated may be needed here in the future
 *  to set up a proper proj and garea based on the clip area.
 *  For instance, the following definitions probably won't work
 *  on a clipping bound equivalent to Antartica.
 */
    lllat = 0.0F;
    lllon = -135.0F;
    urlat = 0.0F;
    urlon = 45.0F;
    strcpy ( pro, "str" );
    prjang1 = 90.0F;  prjang2 = -105.0F;  prjang3 = 0.0F;
    gsmprj ( pro, &prjang1, &prjang2, &prjang3,
             &lllat, &lllon, &urlat, &urlon, &ier, strlen(pro));

/*
 *  Loop through all the elements to set the range records.
 */
    crg_init ( &ier );
    ne = 0;
    more = G_TRUE;
    curpos = 0;
    ifptr = (FILE *) cfl_ropn(ifname, "", &ier);
    while ( ne < MAX_EDITABLE_ELEMS && more == G_TRUE )  {
	cvg_rdrecnoc ( ifname, ifptr, curpos, &el, &ier );
	if ( ier < 0 )  {
	    more = G_FALSE;
	}
	else  {
	    crg_set ( &el, curpos, 1, &ier );
	    curpos += el.hdr.recsz;
	    ne++;
	}
    }
    cfl_clos ( ifptr, &ier );

/*
 *  Loop through all the elements.
 */
    ne = 0;
    more = G_TRUE;
    curpos = 0;
    ifptr = (FILE *) cfl_ropn(ifname, "", &ier);
    while ( ne < MAX_EDITABLE_ELEMS && more == G_TRUE )  {

	cvg_rdrecnoc( ifname, ifptr, curpos, &el, &ier );

	if ( ier < 0 )  {
	    more = G_FALSE;
	}
	else if ( el.hdr.recsz > 0 )  {

	    crg_gginx( el.hdr.grptyp, el.hdr.grpnum, 
			sizeof(inxarr)/sizeof(inxarr[0]), 
			inxarr, &ninxarr, &ier );

/*
 *  Increment file pointer now because element hdrsz may change.
 */
	    curpos += el.hdr.recsz;

	    vg_class = el.hdr.vg_class;
	    vg_type  = el.hdr.vg_type;

	    switch ( (int)vg_class )  {

		case	CLASS_SYMBOLS:
		case	CLASS_TEXT:
		case	CLASS_WINDS:
		case	CLASS_COMSYM:
		case	CLASS_MARKER:

		    switch ( (int)vg_type )  {

		        case	TEXT_ELM:
		        case	TEXTC_ELM:
		            flat = el.elem.txt.info.lat;
		            flon = el.elem.txt.info.lon;
			    break;
		        case	SPTX_ELM:
		            flat = el.elem.spt.info.lat;
		            flon = el.elem.spt.info.lon;
			    break;
		        case	BARB_ELM:
		        case	ARROW_ELM:
		        case	DARR_ELM:
		        case	HASH_ELM:
		            flat = el.elem.wnd.data.latlon[0];
		            flon = el.elem.wnd.data.latlon[1];
			    break;
		        case	WXSYM_ELM:
		        case	CTSYM_ELM:
		        case	ICSYM_ELM:
		        case	PTSYM_ELM:
		        case	PWSYM_ELM:
		        case	SKSYM_ELM:
		        case	SPSYM_ELM:
		        case	TBSYM_ELM:
		        case	MARK_ELM:
		        case	CMBSY_ELM:
		            flat = el.elem.sym.data.latlon[0];
		            flon = el.elem.sym.data.latlon[1];
			    break;
		    }

		    npts = 1;
		    cgr_inpoly ( "M", &npts, &flat, &flon, "M", &npoly, px, py, 
				 inout, &ier );

		    if ( ( inout[0] == 1 && strcmp(keep,"keep") == 0 )  ||
			 ( inout[0] == 0 && strcmp(keep,"keep") != 0 ) )  {

/*
 *  Check if element is TEXT grouped with a SYMBOL.  If this text was going
 *  to be kept but it's symbol was going to be throw away, throw it away also.
 */
		      if ( (int)vg_class == CLASS_TEXT &&
			   (int)el.hdr.grptyp != 0 && ninxarr > 1 )  {

			found = G_FALSE;
			ii = 0;
			while ( ii < ninxarr && found == G_FALSE )  {
			    crg_goffset ( inxarr[ii], &joffset, &ier );
			    cvg_rdrecnoc( ifname, ifptr, joffset, &el_q, &ier );
			    if ( el_q.hdr.vg_class == CLASS_SYMBOLS )  {
				
			      found = G_TRUE;
		              flat = el_q.elem.sym.data.latlon[0];
		              flon = el_q.elem.sym.data.latlon[1];
			      npts = 1;
		    	      cgr_inpoly ( "M", &npts, &flat, &flon, "M", 
					&npoly, px, py, inout, &ier );
		    	      if ((inout[0] == 1 && strcmp(keep,"keep") == 0) ||
                                  (inout[0] == 0 && strcmp(keep,"keep") != 0)) {
		        	cvg_writef( &el, -1, el.hdr.recsz, outfile, 
					   FALSE, &loc, &ier );
		        	kept = G_TRUE;
			      }
			      else  {
		        	kept = G_FALSE;
		              }
			    }

			    ii++;

			}
			if ( found == G_FALSE )  {
		          cvg_writef( &el, -1, el.hdr.recsz, outfile, 
					FALSE, &loc, &ier );
		          kept = G_TRUE;
			}
		      }
		      else  {
/*
 *  non-TEXT -- keep it.
 */
		        cvg_writef( &el, -1, el.hdr.recsz, outfile, FALSE, &loc, &ier );
		        kept = G_TRUE;
		      }

		    }
		    else  {
/*
 *  Element is not to be kept.
 */
		      kept = G_FALSE;
		    }

/*
 *  Check if element was kept and is a SYMBOL element grouped with TEXT;
 *  make sure any text elements are saved off they were going to be thrown away.
 */
		    if ( kept == G_TRUE && (int)vg_class == CLASS_SYMBOLS &&
			   (int)el.hdr.grptyp != 0 && ninxarr > 1 )  {

			ii = 0;
			while ( ii < ninxarr )  {
			    crg_goffset ( inxarr[ii], &joffset, &ier );
			    cvg_rdrecnoc( ifname, ifptr, joffset,
					&el_q, &ier );
			    if ( el_q.hdr.vg_class == CLASS_TEXT )  {
				
			      el_t = el_q;
		    	      switch ( (int)el_t.hdr.vg_type )  {
		        	  case	TEXT_ELM:
		        	  case	TEXTC_ELM:
		            	    flat = el_t.elem.txt.info.lat;
		            	    flon = el_t.elem.txt.info.lon;
			    	    break;
		        	  case	SPTX_ELM:
		            	    flat = el_t.elem.spt.info.lat;
		            	    flon = el_t.elem.spt.info.lon;
			    	    break;
			      }
			      npts = 1;
		    	      cgr_inpoly ( "M", &npts, &flat, &flon, "M", 
					&npoly, px, py, inout, &ier );
		    	      if ( ( kept == G_TRUE )  && 
		    	      ( ( inout[0] == 1 && strcmp(keep,"keep") == 0 )  ||
                              ( inout[0] == 0 && strcmp(keep,"keep") != 0 ) ) ) {
			      }
			      else  {
		      	        cvg_writef ( &el_t, -1, el_t.hdr.recsz, outfile, 
					FALSE, &loc, &ier );
		              }
			    }

			    ii++;

			}

		      }

		    break;

		case	CLASS_CIRCLE:
		case	CLASS_LINES:
		case	CLASS_FRONTS:
                
/*
 * convert a circle element to a line element
 */
		    if ( vg_class == CLASS_CIRCLE ) {

			 cvg_cir2lin ( &el, 10, &el_lin, &ier );
			 el = el_lin;
			 vg_class = el.hdr.vg_class;
			 vg_type  = el.hdr.vg_type;
		    }

		    switch ( (int)vg_type )  {

                        case    LINE_ELM:
                            npts = el.elem.lin.info.numpts;
			    ptrlat = &(el.elem.lin.latlon[   0]);
			    ptrlon = &(el.elem.lin.latlon[npts]);
                            break;

                        case    SPLN_ELM:
                            npts = el.elem.spl.info.numpts;
			    ptrlat = &(el.elem.spl.latlon[   0]);
			    ptrlon = &(el.elem.spl.latlon[npts]);
                            break;

                        case    FRONT_ELM:
                            npts = el.elem.frt.info.numpts;
			    ptrlat = &(el.elem.frt.latlon[   0]);
			    ptrlon = &(el.elem.frt.latlon[npts]);
                            break;

		    }

		    memcpy ( plat, ptrlat, (size_t)npts*sizeof(float) );
		    memcpy ( plon, ptrlon, (size_t)npts*sizeof(float) );
		    if ( el.hdr.closed == 1 )  {
			plat[npts] = plat[0];
			plon[npts] = plon[0];
		        npts++;
		    }

		    if ( strcmp(precision,"EXACT") == 0 )  {

			clip_line ( npoly, px, py, npts, plat, plon, 
				(int)el.hdr.closed, sizeof(xinout)/sizeof(float), 
				&ninout, xinout, yinout, inout, &ier );
		    }
		    else if ( strcmp(precision,"ROUGH") == 0 )  {

		        cgr_inpoly ( "M", &npts, plat, plon, "M", &npoly, px, py,
                                 inout, &ier );
			ninout = npts;
			memcpy ( xinout, plat, (size_t)ninout*sizeof(float) );
			memcpy ( yinout, plon, (size_t)ninout*sizeof(float) );
			
		    }

/*
 *  If element is closed, and some points are to be kept and others are not,
 *  then rotate the locations arrays such that a transition point is the first point.
 */
                    if ( el.hdr.closed == 1 )  {
                        ip = 0;
			ninout--;
                        while ( inout[ip] == inout[0] && ip < ninout )  ip++;
                        if ( ip != ninout )  {
                            if (( inout[0] == 1 && strcmp(keep,"keep") == 0 )  ||
                                ( inout[0] == 0 && strcmp(keep,"keep") != 0 ) ) {
                                memcpy ( tlat, xinout, (size_t)ninout*sizeof(float) );
                                memcpy ( tlon, yinout, (size_t)ninout*sizeof(float) );
                                memcpy ( tinout, inout, (size_t)ninout*sizeof(float) );
                                for ( ii = 0; ii < ninout; ii++ )  {
                                    xinout[ii] = tlat[(ii+ip) % ninout];
                                    yinout[ii] = tlon[(ii+ip) % ninout];
                                    inout[ii] = tinout[(ii+ip) % ninout];
                                }
                            }
                        }
                    }

		    ip = 0;
		    while ( ip < ninout )  {

			ibeg = ip; iend = ip;
			while ( inout[ip] == inout[ibeg] && ip < ninout )  ip++;
			iend = ip - 1;
			numpts = iend - ibeg + 1;

/*
 *  If element is closed, and some points are to be kept and others are not,
 *  then reset the closed flag.
 */
			if ( el.hdr.closed == 1 && numpts != ninout )
				el.hdr.closed = 0;

			if ( numpts > 1 )  {

		         if (( inout[ibeg] == 1 && strcmp(keep,"keep") == 0 )  ||
			     ( inout[ibeg] == 0 && strcmp(keep,"keep") != 0 ) ) {

			  switch ( (int)vg_type )  {

                            case    LINE_ELM:
			  	el.elem.lin.info.numpts = numpts;
				ptrlat = &(el.elem.lin.latlon[     0]);
				ptrlon = &(el.elem.lin.latlon[numpts]);
				el.hdr.recsz = ( (int)((sizeof(float) * 2 * (size_t)numpts) +
                      				  sizeof(VG_HdrStruct) +  
						  sizeof(LineInfo) ));
                                break;

                            case    SPLN_ELM:
			  	el.elem.spl.info.numpts = numpts;
				ptrlat = &(el.elem.spl.latlon[     0]);
				ptrlon = &(el.elem.spl.latlon[numpts]);
				el.hdr.recsz = ( (int)((sizeof(float) * 2 * (size_t)numpts) +
                      				  sizeof(VG_HdrStruct) + 
						  sizeof(SpLineInfo) ));
                                break;

                            case    FRONT_ELM:
			  	el.elem.frt.info.numpts = numpts;
				ptrlat = &(el.elem.frt.latlon[     0]);
				ptrlon = &(el.elem.frt.latlon[numpts]);
				el.hdr.recsz = ( (int)((sizeof(float) * 2 * (size_t)numpts) +
                      				  sizeof(VG_HdrStruct) + 
						  sizeof(FrontInfo) ));
                                break;

                          }

			  memcpy(ptrlat, &(xinout[ibeg]), (size_t)numpts*sizeof(float));
			  memcpy(ptrlon, &(yinout[ibeg]), (size_t)numpts*sizeof(float));

		      	  cvg_writef ( &el, -1, el.hdr.recsz, outfile, 
				       FALSE, &loc, &ier );

			  if ( (int)el.hdr.grptyp != 0 && ninxarr > 1 )  {
			    found = G_FALSE;
			    found_txt = G_FALSE;
			    ii = 0;
			    while ( ii < ninxarr && found == G_FALSE )  {
			      crg_goffset ( inxarr[ii], &joffset, &ier );
			      cvg_rdrecnoc( ifname, ifptr, joffset,
					&el_q, &ier );
			      if ( el_q.hdr.vg_class == CLASS_TEXT )  {
				
			        found_txt = G_TRUE;
				el_t = el_q;
		    		switch ( (int)el_t.hdr.vg_type )  {
		        	  case	TEXT_ELM:
		        	  case	TEXTC_ELM:
		            	    flat = el_t.elem.txt.info.lat;
		            	    flon = el_t.elem.txt.info.lon;
			    	    break;
		        	  case	SPTX_ELM:
		            	    flat = el_t.elem.spt.info.lat;
		            	    flon = el_t.elem.spt.info.lon;
			    	    break;
				}
				npts = 1;
		    		cgr_inpoly ( "M", &npts, &flat, &flon, "M", 
					&npoly, px, py, inout, &ier );
		    	if ( ( inout[0] == 1 && strcmp(keep,"keep") == 0 )  ||
			     ( inout[0] == 0 && strcmp(keep,"keep") != 0 ) )  {
		      		found = G_TRUE;
		      		break;
		        }
			      }
			      ii++;

			    }
			    if ( found == G_FALSE && ii == ninxarr && 
				found_txt == G_TRUE )  {

			      switch ( (int)vg_type )  {
                                case    LINE_ELM:
		      	          flat = ( el.elem.lin.latlon[0] +
					el.elem.lin.latlon[1] ) / 2.0F;
		      	          flon = ( el.elem.lin.latlon[numpts] +
					el.elem.lin.latlon[numpts+1] ) / 2.0F;
				  break;
                                case    SPLN_ELM:
		      	          flat = ( el.elem.spl.latlon[0] +
					el.elem.spl.latlon[1] ) / 2.0F;
		      	          flon = ( el.elem.spl.latlon[numpts] +
					el.elem.spl.latlon[numpts+1] ) / 2.0F;
				  break;
                                case    FRONT_ELM:
		      	          flat = ( el.elem.frt.latlon[0] +
					el.elem.frt.latlon[1] ) / 2.0F;
		      	          flon = ( el.elem.frt.latlon[numpts] +
					el.elem.frt.latlon[numpts+1] ) / 2.0F;
				  break;
			      }

		    	      switch ( (int)el_t.hdr.vg_type )  {
		        	  case	TEXT_ELM:
		        	  case	TEXTC_ELM:
		            	    el_t.elem.txt.info.lat = flat;
		            	    el_t.elem.txt.info.lon = flon;
			    	    break;
		        	  case	SPTX_ELM:
		            	    el_t.elem.spt.info.lat = flat;
		            	    el_t.elem.spt.info.lon = flon;
			    	    break;
			      }

		      	      cvg_writef ( &el_t, -1, el_t.hdr.recsz, outfile, 
					FALSE, &loc, &ier );
			    }

			  }

			 }

			}
		    }
		    break;

		case	CLASS_MET:
		    switch ( (int)vg_type )  {
                        case    JET_ELM:
                            npts = tltpts = el.elem.jet.line.spl.info.numpts;
			    ptrlat = &(el.elem.jet.line.spl.latlon[   0]);
			    ptrlon = &(el.elem.jet.line.spl.latlon[npts]);
		            memcpy ( plat, ptrlat, (size_t)npts*sizeof(float) );
		            memcpy ( plon, ptrlon, (size_t)npts*sizeof(float) );

		            if ( strcmp(precision,"EXACT") == 0 )  {

			        clip_line ( npoly, px, py, npts, plat, plon, 
				(int)el.hdr.closed, sizeof(xinout)/sizeof(float), 
				&ninout, xinout, yinout, inout, &ier );

		            }
		            else if ( strcmp(precision,"ROUGH") == 0 )  {

		                cgr_inpoly ( "M", &npts, plat, plon, "M", &npoly, px, py, inout, &ier );
			        ninout = npts;
			        memcpy ( xinout, plat, (size_t)ninout*sizeof(float) );
			        memcpy ( yinout, plon, (size_t)ninout*sizeof(float) );
		            }

		            ip = 0;
		            while ( ip < ninout )  {

			        ibeg = ip; iend = ip;
			        while ( inout[ip] == inout[ibeg] && ip < ninout )
				    ip++;
			        iend = ip - 1;
			        numpts = iend - ibeg + 1;

			        if ( numpts > 1 )  {

		                    if (( inout[ibeg] == 1 && strcmp(keep,"keep") == 0 )  ||
			                ( inout[ibeg] == 0 && strcmp(keep,"keep") != 0 ) ) {
			                memcpy ( &(el_t.hdr), &(el.hdr), sizeof(VG_HdrStruct) );
                                        el_t.elem.jet.line.splcol = el.elem.jet.line.splcol;
			                memcpy ( &(el_t.elem.jet.line.spl.info), &(el.elem.jet.line.spl.info), sizeof(SpLineInfo) );
                                        el_t.elem.jet.line.spl.info.numpts = numpts;
			                ptrlat = &(el_t.elem.jet.line.spl.latlon[     0]);
			                ptrlon = &(el_t.elem.jet.line.spl.latlon[numpts]);
			                memcpy(ptrlat, &(xinout[ibeg]), (size_t)numpts*sizeof(float));
			                memcpy(ptrlon, &(yinout[ibeg]), (size_t)numpts*sizeof(float));
					nbarb = 0;
					for ( ii = 0; ii < el.elem.jet.nbarb; ii++ ) {
		                            flat = el.elem.jet.barb[ii].wnd.data.latlon[0];
		                            flon = el.elem.jet.barb[ii].wnd.data.latlon[1];
		                            npts = 1;
		                            cgr_inpoly ( "M", &npts, &flat, &flon, "M", &npoly, px, py, tinout, &ier );
		                            if (( tinout[0] == 1 && strcmp(keep,"keep") == 0 )  ||
			                        ( tinout[0] == 0 && strcmp(keep,"keep") != 0 ) ) {
                                                 memcpy ( &(el_t.elem.jet.barb[nbarb]), &(el.elem.jet.barb[ii]), sizeof(BarbAttr) );
						 nbarb++;
					    }
					}
					el_t.elem.jet.nbarb = nbarb;

					nhash = 0;
					for ( ii = 0; ii < el.elem.jet.nhash; ii++ ) {
		                            flat = el.elem.jet.hash[ii].wnd.data.latlon[0];
		                            flon = el.elem.jet.hash[ii].wnd.data.latlon[1];
		                            npts = 1;
		                            cgr_inpoly ( "M", &npts, &flat, &flon, "M", &npoly, px, py, tinout, &ier );
		                            if (( tinout[0] == 1 && strcmp(keep,"keep") == 0 )  ||
			                        ( tinout[0] == 0 && strcmp(keep,"keep") != 0 ) ) {
                                                memcpy ( &(el_t.elem.jet.hash[nhash]), &(el.elem.jet.hash[ii]), sizeof(HashAttr) );
                                                nhash++;
					    }
					}
					el_t.elem.jet.nhash = nhash;
		      	                cvg_writef ( &el_t, -1, el.hdr.recsz, outfile, 
				               FALSE, &loc, &ier );
			            }
			        }
		            }
                            break;
                        case GFA_ELM:
                          /* Get the Hazard Type... */
                          cvg_getFld ( &el, TAG_GFA_AREATYPE, hazList, &ier );
                          npts = el.elem.gfa.info.npts;
                          ptrlat = &(el.elem.gfa.latlon[0]);
                          ptrlon = &(el.elem.gfa.latlon[npts]);
                          memcpy ( plat, ptrlat, (size_t)npts*sizeof(float) );
                          memcpy ( plon, ptrlon, (size_t)npts*sizeof(float) );
                          if ( el.hdr.closed == 1 )  {
                            plat[npts] = plat[0];
                            plon[npts] = plon[0];
                            npts++;
                          }
                          if(strcmp(hazList,"FZLVL")==0) {  /* Is this a Freezing Level? */
                            if ( strcmp(precision,"EXACT") == 0 ) {
                              clip_line ( npoly, px, py, npts, plat, plon,
                                          (int)el.hdr.closed,
                                          sizeof(xinout)/sizeof(float),
                                          &ninout, xinout, yinout, inout, &ier );
                            } else if (strcmp(precision,"ROUGH") == 0 ) {
                              cgr_inpoly ( "M", &npts, plat, plon, "M", &npoly, px, py,
                                           inout, &ier );
                              ninout = npts;
                              memcpy ( xinout, plat, (size_t)ninout*sizeof(float) );
                              memcpy ( yinout, plon, (size_t)ninout*sizeof(float) );
                            }

                            if ( el.hdr.closed == 1 )  {
                              ip = 0;
                              ninout--;
                              while ( inout[ip] == inout[0] && ip < ninout )  ip++;
                              if ( ip != ninout )  {
                                if (( inout[0] == 1 && strcmp(keep,"keep") == 0 )  ||
                                      ( inout[0] == 0 && strcmp(keep,"keep") != 0 ) ) {
                                  memcpy ( tlat, xinout, (size_t)ninout*sizeof(float) );
                                  memcpy ( tlon, yinout, (size_t)ninout*sizeof(float) );
                                  memcpy ( tinout, inout, (size_t)ninout*sizeof(float) );
                                  for ( ii = 0; ii < ninout; ii++ )  {
                                    xinout[ii] = tlat[(ii+ip) % ninout];
                                    yinout[ii] = tlon[(ii+ip) % ninout];
                                    inout[ii] = tinout[(ii+ip) % ninout];
                                  }
                                }
                              }
                            }
                            ip = 0;

                            while ( ip < ninout ) {
                              ibeg = ip; iend = ip;
                              while ( inout[ip] == inout[ibeg] && ip < ninout ) ip++;
                              iend = ip - 1;
                              numpts = iend - ibeg + 1;
                              if (el.hdr.closed == 1 && numpts != ninout )
                                el.hdr.closed = 0;

                              if ( numpts > 1 )  {

                                if (( inout[ibeg] == 1 && strcmp(keep,"keep") == 0 )  ||
                                      ( inout[ibeg] == 0 && strcmp(keep,"keep") != 0 )) {
                                  el.elem.gfa.info.npts = numpts;
                                  ptrlat = &(el.elem.gfa.latlon[     0]);
                                  ptrlon = &(el.elem.gfa.latlon[numpts]);
                                  memcpy(ptrlat, &(xinout[ibeg]),
                                         (size_t)numpts*sizeof(float));
                                  memcpy(ptrlon, &(yinout[ibeg]),
                                         (size_t)numpts*sizeof(float));
                                  /* Recompute Default Text Label & Arrow location */
                                  recalcGFAtLblArwLoc( &el );
                                  el.hdr.recsz = (int) (sizeof(VG_HdrStruct) +
                                      sizeof(int)*2 + sizeof(char)* STD_STRLEN *
                                      el.elem.gfa.info.nblocks ) + sizeof(float)*numpts*2;
                                  cvg_writef ( &el, -1, el.hdr.recsz, outfile,
                                                FALSE, &loc, &ier );
                                }
                              }
                            }
                          } else { /* We have a GFA object (that's not a freezing level) 
                                      to be clipped */
                            /* Use clo_clip to clip the GFA Polygon against the specified 
                               bounds area. The resulting number of clipped areas (tnclips), 
                               and max points (tmaxpts) is returned */
                            clo_clip(&npts, plat, plon, sys_M, bnd_name, bnd_tag, &tnclip,
                                      &tmaxpts, &ier);
                            /* Foreach of the clipped areas, get the clipped area, and write
                               it out to the VGF file */ 
                            for (ii = 0; ii < tnclip; ii++) {
                              clo_clipget(&ii, &tnpts, tlat, tlon, &ier);
                              el.elem.gfa.info.npts = tnpts;
                              ptrlat = &(el.elem.gfa.latlon[     0]);
                              ptrlon = &(el.elem.gfa.latlon[tnpts]);
                              /* Re-Initialize the latlon struct. */
                              for (jj =0; jj < MAXPTS*2; jj++) {
                                el.elem.gfa.latlon[jj] = 0.00;
                              }
                              memcpy(ptrlat, &(tlat[0]),
                                     (size_t)tnpts*sizeof(float));
                              memcpy(ptrlon, &(tlon[0]),
                                     (size_t)tnpts*sizeof(float));
                              /* Recompute Default Text Label & Arrow location */
                              recalcGFAtLblArwLoc( &el );
                              el.hdr.recsz = (int) (sizeof(VG_HdrStruct) +
                                  sizeof(int)*2 + sizeof(char)* STD_STRLEN *
                                  el.elem.gfa.info.nblocks ) + sizeof(float)*tnpts*2;
                              cvg_writef (&el, -1, el.hdr.recsz, outfile,
                                           FALSE, &loc, &ier );
                            }
                            /*  Free up memory left over from the clo routines */
                            clo_clipdone(&ier);
                          }
                          break;
		    }
		    break;
	    }
	}
	ne++;
    }

    cfl_clos ( ifptr, &ier );
    return(0);
}

/*=====================================================================*/

void clip_line ( int npoly, float *px, float *py, int npts, float *plat, 
		 float *plon, int closed, int maxpts,
		 int *ninout, float *xinout, float *yinout, int *inout,
		 int *iret )
/************************************************************************
 * clip_line								*
 *									*
 * This program clips a single line against a single polygon.		*
 *                                                                      *
 * clip_line( npoly, px, py, npts, plat, plon, closed, maxpts, ninout, 	*
 *				xinout, yinout, inout, iret )		*
 *                                                                      *
 * Input parameters:                                                    *
 *  npoly	int	number of points in clipping polygon		*
 *  *px		float	array of x-coords in clipping polygon		*
 *  *py		float	array of y-coords in clipping polygon		*
 *  npts	int	number of points in line to be clipped		*
 *  *plat	float	array of latitudes in line to be clipped        *
 *  *plon	float	array of longitudes in line to be clipped       *
 *  closed	int	flag indicating if line is closed or not	*
 *  maxpts	int	maximum number of points returned		*
 *                                                                      *
 * Output parameters:                                                   *
 *  *ninout	int	number of returned points			*
 *  *xinout	float	array of x points				*
 *  *yinout	float	array of y points				*
 *  *inout	int	array of indicators as to in or out		*
 *  *iret	int	Return code					*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 3/02						*
 * X.Guo/CWS		10/10 	Missed calculating some intersection pts*
 *                              between line and polygon                *
 ***********************************************************************/
{
int	ii, jj, kk, ll, nint, ier;
int	intrsct, tinout[LLMXPT];
float	xint, yint, temp;
float	fxint[LLMXPT], fyint[LLMXPT], dist[LLMXPT];
float	Nplat[LLMXPT], Nplon[LLMXPT], Npx[LLMXPT], Npy[LLMXPT];
float   x11[2], y11[2];
/*---------------------------------------------------------------------*/

    *iret = 0;

    cgr_inpoly ( "M", &npts, plat, plon, "M", &npoly, px, py, tinout, &ier );

    gtrans ( sys_M, sys_N, &npts, plat, plon, Nplat, Nplon, &ier,
		strlen(sys_N), strlen(sys_N) );
    gtrans ( sys_M, sys_N, &npoly, px, px, Npx, Npy, &ier,
		strlen(sys_N), strlen(sys_N) );

    *ninout = 0;
    for ( ii = 0; ii < npts-1; ii++ )  {
	xinout[*ninout] = plat[ii];
	yinout[*ninout] = plon[ii];
	inout[*ninout] = tinout[ii];
	(*ninout)++;
	nint = 0;
	for ( jj = 0; jj < npoly; jj++ )  {
            if ( jj == npoly - 1 ) {
                x11[0] = px[jj];
                x11[1] = px[0];
                y11[0] = py[jj];
                y11[1] = py[0];
            }
            else {
                x11[0] = px[jj];
                x11[1] = px[jj+1];
                y11[0] = py[jj];
                y11[1] = py[jj+1];
            }
            cgr_segint ( sys_M, &(plat[ii%npts]), &(plon[ii%npts]),
                         sys_M, x11, y11,
                         sys_M, &xint, &yint, &intrsct, &ier );
	    if ( intrsct == G_TRUE )  {
		fxint[nint] = xint;
		fyint[nint] = yint;
		nint++;
	    }
	}
	if ( nint > 0 )  {
	    clo_dist ( &(plat[ii]), &(plon[ii]), &nint, fxint, fyint, 
		       dist, &ier );
	    for ( kk = 0; kk < nint-1; kk++ )  {
		for ( ll = 0; ll < nint-kk-1; ll++ )  {
		    if ( dist[ll] > dist[ll+1] )  {
			temp = fxint[ll];
			fxint[ll] = fxint[ll+1];
			fxint[ll+1] = temp;
			temp = fyint[ll];
			fyint[ll] = fyint[ll+1];
			fyint[ll+1] = temp;
			temp = dist[ll];
			dist[ll] = dist[ll+1];
			dist[ll+1] = temp;
		    }
		}
	    }
	    for ( kk = 0; kk < nint; kk++ )  {
		xinout[*ninout] = fxint[kk];
		yinout[*ninout] = fyint[kk];
		inout[*ninout] = inout[(*ninout)-1];
		(*ninout)++;
		xinout[*ninout] = fxint[kk];
		yinout[*ninout] = fyint[kk];
		if ( inout[(*ninout)-1] == 0 )  inout[*ninout] = 1;
		if ( inout[(*ninout)-1] == 1 )  inout[*ninout] = 0;
		(*ninout)++;
	    }
	}
    }
    xinout[*ninout] = plat[npts-1];
    yinout[*ninout] = plon[npts-1];
    inout[*ninout] = tinout[npts-1];
    (*ninout)++;
}

/*=====================================================================*/

void recalcGFAtLblArwLoc(VG_DBStruct *el)
/***********************************************************************
 * recalcGFAtLblArwLoc                                             *
 *                                                                     *
 *  This routine recomputes a location for the text box and associated *
 *  arrow to be positioned at the eastern most vertex of a polygon     *
 *  Its purpose, in the context of this module, was to correct         *
 *  problems with a dangling text label arrow, that may be a result of *
 *  a clipping operation on a polygon that's intersected the boundary  *
 *                                                                     *
 *  Input/Output parameter:                                            *
 *        *el      VG_DBStruct      Pointer to VG record structure     *
 *                                                                     *
 *                                                                     *
 **                                                                    *
 *  Log:                                                               *
 *  L. Hinson/AWC     07/07  Created                                   *
 **********************************************************************/
{
  int i, npts, ier;
  float ernlon=-999.0;
  float ernlat=0.0;
  char slat[20], slon[20];
  npts = el->elem.gfa.info.npts;
  for (i = 0; i < npts; i++)
    if (el->elem.gfa.latlon[i+npts] > ernlon) {
      ernlon=el->elem.gfa.latlon[i+npts];
      ernlat=el->elem.gfa.latlon[i];
    }
  ;
  sprintf(slat, " %5.2f", ernlat);
  sprintf(slon, " %5.2f", ernlon);
  cvg_setFld (el, TAG_GFA_LAT, slat, &ier);
  cvg_setFld (el, TAG_GFA_LON, slon, &ier);
  cvg_setFld (el, TAG_GFA_ARROW_LAT, slat, &ier);
  cvg_setFld (el, TAG_GFA_ARROW_LON, slon, &ier);
} 
