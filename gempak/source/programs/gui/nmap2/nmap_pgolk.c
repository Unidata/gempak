#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "nmap_data.h"

void pgolk_udlist ( VG_DBStruct *el, int *iret );

/*
 * Define table containing outlook categories
 */
#define	OUTLOOK_TABLE	"outlook.tbl"

/*
 * Define table containing outlook group type full names
 */
#define	GRPNAME_TABLE	"otlfilnam.tbl"

/*
 * Define index for changing line group order
 */
static int	_order=0;


/************************************************************************
 * nmap_pgolk.c                                                         *
 *                                                                      *
 * This module creates the outlook product.				*
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 *      pgolk_update()	      create outlook control			*
 *      pgolk_udlist()	      create outlook list for display		*
 *   	pgolk_getfname()      build the outlook text file name		*
 *   	pgolk_check()         check unexpected outlook elements		*
 *      pgolkl_getGroup()     find the group type and name for the prod *
 *                                                                      *
 ***********************************************************************/

/*=====================================================================*/

void pgolk_update ( char *fname, int *iret )
/************************************************************************
 * pgolk_update                                                    	*
 *                                                                      *
 * This function creates an outlook text product from a vgf file.	*
 *                                                                      *
 * void pgolk_update( fname, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  *fname		char	VG Filename				*
 *                                                                      *
 * Output parameters:                                             	*
 *  *iret		int	Return code				*
 *				-1 = unable to convert			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 4/98						*
 * I. Durham/GSC         5/98   Changed underscore decl. to an include  *
 * D.W.Plummer/NCEP	 7/98	Bug fix to GENERAL THUNDERSTORM groups	*
 * E. Safford/GSC	10/98	mod for param change to cvg_rdrec	*
 * D.W.Plummer/NCEP	10/98	mods for ordering grouped lines		*
 * D.W.Plummer/NCEP	 1/99	Added recognition of line type 20	*
 * D.W.Plummer/NCEP	 6/99	Add connect segments for grouped lines	*
 * H. Zeng/EAI          09/99   Added pgofmt_update()                   *
 * M. Li/GSC		10/99	Modified clo_dist code			*
 * M. Li/GSC		10/99	Added multi-point cal. to clo_dist	*
 * D.W.Plummer/NCEP	 8/00	Added checks for lines being flipped	*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * D.W.Plummer/NCEP	01/01	removed dependencies on line type	*
 * D.W.Plummer/NCEP	01/01	Added processing for particular group	*
 * D.W.Plummer/NCEP	01/01	Added SIGNIFICANT SEVERE string		*
 * H. Zeng/EAI          03/01   modified to use ces_gtgid()             *
 * D.W.Plummer/NCEP	04/01	Changes for filled lines w/o text	*
 * E. Safford/SAIC	11/01	mv _pghdlb_refresh -> pgutls_refresh	*
 * D.W.Plummer/NCEP	12/01	add points file processing		*
 * J. Wu/SAIC		01/02	update the current layer only		*
 * D.W.Plummer/NCEP	 3/02	rm points file processing		*
 * E. Safford/SAIC      03/02   use pgolk_getGroup()                    *
 * M. Li/SAIC		04/02   replaced crg_ggnxt with crg_ggnhl	*
 * T. Piper/SAIC        07/02   fixed no text case with one element     *
 * A. Hardy/NCEP	 1/04	added layer check to ungrouped elements *
 * T. Piper/SAIC	04/06	Changed size of str from 16 to MAX_TEXT	*
 * F. J. Yen/NCEP	10/07	Added Day 3-8 Fire Wx with 2 line types *
 * F. J. Yen/NCEP	 8/08	Added check for not EXT_FIRE so will	*
 *				process scalloped lines, too.		*
 ***********************************************************************/
{
int    		ier, ncx, nc, ng, ne, nel, nelm, fpos, cur_layer, el_layer;
int    		totgrp, lowgrp, grpnum, valcat, nxf, nrloop;
int		inxarry[10];
char		grptyp, vg_class, vg_type, str_found;
char		str[MAX_TEXT], grpTypName[20];
float		lat, lon;

VG_DBStruct     el;

char            textstr[80];

static char   	**outl_cats, **outl_cstr;
char	outl_gstr[32] = { "GENERAL THUNDERSTORMS" };

static int	numcats;
static Boolean	tread=False;
int	n, nlin, nlines, npts, index, nlcnt, flip;
FILE    *ftbl;
char	record[80], catabb[16], catstr[64];

int	grparr[10], done[10];
float	latpt, lonpt, distx, dist;

int	np2, icolrs, iltyps, ilthw, iwidths, iwhw, npx;
int	order, icolr, iltyp, iwidth;
float	x[2], y[2];
float   llx, lly, urx, ury;

/*---------------------------------------------------------------------*/

    *iret = 0;

    clo_init ( &ier );
    npx = 1;

/*
 *  Read in outlook category table, if necessary.
 */
    if ( tread == G_FALSE )  {

	ftbl = cfl_tbop( OUTLOOK_TABLE, "pgen", &ier);
        if ( ftbl == NULL ) {
	    *iret = -1;
            return;
        }

	cfl_tbnr( ftbl, &numcats, &ier );

	if ( numcats == 0 )  {
	    fclose ( ftbl );
	    *iret = -1;
	    return;
	}
	else  {
	    outl_cats = (char **) malloc( sizeof(char *) * numcats );
	    outl_cstr = (char **) malloc( sizeof(char *) * numcats );
	}

	for ( n = 0; n < numcats; n++ )  {

	    cfl_trln( ftbl, 80, record, &ier );

	    strcpy( catabb, strtok( record, "|" ) );
	    strcpy( catstr, strtok(   NULL, "|" ) );

	    outl_cats[n] = (char *) malloc( strlen(catabb)+1 );
	    outl_cstr[n] = (char *) malloc( strlen(catstr)+1 );
	    
	    strcpy( outl_cats[n], catabb );
	    strcpy( outl_cstr[n], catstr );

	}

	fclose ( ftbl );

	tread = True;

    }

/*
 *  Determine the grptyp for the selected group type
 */
    pgolk_getGroup ( &grptyp, grpTypName );

    if ( grptyp == GRPTYP_OTHERS )  {
	*iret = -1;
	return;
    }

    if ( strcmp(grpTypName,"OUTLOOK") == 0 )  {
	strcpy( outl_gstr, "GENERAL THUNDERSTORMS" );
    }
    else  {
	strcpy( outl_gstr, "SIGNIFICANT SEVERE" );
    }

/*
 *  Clear the display area.
 */
    pgprd_clear();

/*
 *  Refresh the display to remove previous line extensions
 *  pgutls_refresh requires device coordinates
 */
    gqbnd ( sys_D, &llx, &lly, &urx, &ury, &ier, strlen(sys_D) );
    pgutls_refresh (llx, ury, urx, lly, &ier);

/*
 *  Save and set some line attributes.
 */
    gqcolr ( &icolrs, &ier );
    gqline ( &iltyps, &ilthw, &iwidths, &iwhw, &ier );
    np2 = 2;
    icolr = 18; 
    gscolr ( &icolr, &ier );
    iltyp = 3; iwidth = 3; 
    gsline ( &iltyp, &ilthw, &iwidth, &iwhw, &ier );

/*
 *  Display day, forecaster and initial&expiration time
 */
    pgofmt_update(&ier); 

/*
 *  Get the next available group number to see how many groups there are.
 */
    crg_ggnhl( grptyp, &totgrp, &lowgrp, &ier );

/*
 *  First look for all groups with string identifiers.
 */
    if ( strcmp(grpTypName, "EXT_FIRE") == 0 )  {

/*  Group type is EXT_FIRE, so set nrloop to loop through twice.
 *  so that for the second time through the loop, the scalloped
 *  lines for DRY TSTM will be processed. 
 */
	nrloop = 2;
    }
    else {
	nrloop = 1;
    }
    for ( nxf = 0; nxf < nrloop; nxf++ )  {
    
        cur_layer = pglayer_getCurLayer ();    
        for ( nc = 0; nc < numcats; nc++ )  {

/*
 *  Process only the group numbers that exist.
 */
	    for ( ng = lowgrp; ng <= totgrp; ng++ )  {

/*
 *  Determine how many elements there are in this group.
 */
	        crg_gginx( grptyp, ng, sizeof(inxarry)/sizeof(inxarry[0]),
		       inxarry, &nelm, &ier );
	
	        if ( nelm != 0 )  {

/*
 *  Search for text string; if string exists and matches
 *  outl_cats[nc], then process group, otherwise skip.
 */
		    str[0] = '\0';
		    for ( ne = 0; ne < nelm; ne++ )  {

		        crg_gtyp( inxarry[ne], &vg_class, &vg_type, &ier );
		        crg_goffset( inxarry[ne], &fpos, &ier );
                        el_layer = crg_getLayer( fpos );
		    
		        if ( el_layer == cur_layer &&
		               (int)vg_class == CLASS_TEXT && fpos > 0 )  {

/*
 *  Found group with a string identifier
 */
			    cvg_rdrec( fname, fpos, &el, &ier );

			    if ((int) vg_type == SPTX_ELM) {
                              strcpy (str, el.elem.spt.text);
			      lat = el.elem.spt.info.lat;
			      lon = el.elem.spt.info.lon;
                            }
                            else {
                              strcpy (str, el.elem.txt.text);
			      lat = el.elem.txt.info.lat;
			      lon = el.elem.txt.info.lon;
                            }
                        }
		    }

		    if ( strcmp(str, outl_cats[nc]) == 0 )  {

/*
 *  Found group with a string identifier; Order the lines such that they 'link'
 *  together logically.  First, gather lines in this group into grparr and
 *  determine which line has the eastmost starting point.
 */
		        nlines = 0;
	                for ( nel = 0; nel < nelm; nel++ )  {
	    	            crg_gtyp( inxarry[nel], &vg_class, &vg_type, &ier );
		            crg_goffset( inxarry[nel], &fpos, &ier);
	    	            el_layer = crg_getLayer( fpos );
		            if ( el_layer == cur_layer &&
		                  (int)vg_type == SPLN_ELM && fpos > 0 )  {
			      cvg_rdrec( fname, fpos, &el, &ier );
			      grparr[nlines] = inxarry[nel];
			      nlines++;
		            }
		        }

		        if ( nlines > 0 )  {
		            order = (_order+1) % nlines;
		            crg_goffset( grparr[order], &fpos, &ier );
	    	            el_layer = crg_getLayer( fpos );
		    
		            if ( el_layer == cur_layer ) {
		              cvg_rdrec( fname, fpos, &el, &ier );
		              npts = el.elem.spl.info.numpts;
    		              flip = el.elem.spl.info.spldir;
        	              switch ( flip )  {
	    	                case -1:	/* flipped arrow	*/
		    	          latpt = el.elem.spl.latlon[npts-1];
		    	          lonpt = el.elem.spl.latlon[2*npts-1];
	        	          break;
	    	                default:	/* un-flipped arrow	*/
		    	          latpt = el.elem.spl.latlon[0];
		    	          lonpt = el.elem.spl.latlon[npts];
	        	          break;
        	              }
	                    }
		        }

/*  If it is the first time through the loop and it is not extended fire wx
 *  (nrloop!=2) process all lines. If it is the first time through the loop
 *  and it is extended fire wx (nrloop=2), process non-scalloped lines only. 
 *  If it is the second time through the loop (which will only happen for
 *  extended fire wx), then process the dry tstm scalloped lines
 *  (ie, el.elem.spl.info.spltyp is 3). 
 */
		        if ( ( nxf == 0 && nrloop != 2 ) ||
			     ( nxf == 0 && nrloop == 2 &&
				      el.elem.spl.info.spltyp != 3 ) ||
			     ( nxf == 1 && el.elem.spl.info.spltyp == 3 ) ) {
		            if ( nxf == 0 ) {
		              sprintf(textstr, "%s\nLABEL: %5.2f %7.2f\n", 
			            outl_cstr[nc], lat, lon );
		            }
		            else {
		              sprintf(textstr,
					"%s DRYTSTM\nLABEL: %5.2f %7.2f\n", 
			    		outl_cstr[nc], lat, lon );
		            }
	    	            pgprd_putstr ( textstr, &ier );

/*
 *  Now loop thru each line and print out line closest to previous line endpoint.
 */
	                    for ( nlcnt = 0; nlcnt < nlines; nlcnt++ )
							done[nlcnt] = 0;
	                    for ( nlcnt = 0; nlcnt < nlines; nlcnt++ )  {

		              dist = FLT_MAX;
		              index = 0;
	                      for ( nlin = 0; nlin < nlines; nlin++ )  {
			        if ( done[nlin] != 1 )  {
		                  crg_goffset( grparr[nlin], &fpos, &ier);
			          el_layer = crg_getLayer( fpos );
			    
			          if ( el_layer == cur_layer ) {
			            cvg_rdrec( fname, fpos, &el, &ier );
			            npts = el.elem.spl.info.numpts;
    		    	            flip = el.elem.spl.info.spldir;
        	    	            switch ( flip )  {
	    	      	              case -1:	    /* flipped arrow    */
			                clo_dist ( &latpt, &lonpt, &npx, 
				              &(el.elem.spl.latlon[npts-1]),
				              &(el.elem.spl.latlon[2*npts-1]), 
				              &distx, &ier );
	        		        break;
	    	      	              default:	    /* un-flipped arrow */
			                clo_dist ( &latpt, &lonpt, &npx, 
				              &(el.elem.spl.latlon[0]),
				              &(el.elem.spl.latlon[npts]), 
				              &distx, &ier );
	        		        break;
        	    	            }
			            if ( distx < dist )  {
				      dist = distx;
				      index = nlin;
			            }
			          }
			        }
		              }

		              if ( nlcnt > 0 )  {
			        sprintf(textstr, "...CONT...\n");
	    		        pgprd_putstr ( textstr, &ier );
		              }

		              crg_goffset( grparr[index], &fpos, &ier);
		              cvg_rdrec( fname, fpos, &el, &ier );

		              if ( nlcnt != 0 )  {
		                x[0] = latpt;  
		                y[0] = lonpt;
			        npts = el.elem.spl.info.numpts;
    		    	        flip = el.elem.spl.info.spldir;
			        switch ( flip )  {
			          case -1:	/* flipped arrow	*/
		                    x[1] = el.elem.spl.latlon[npts-1];  
		                    y[1] = el.elem.spl.latlon[2*npts-1];
			            break;
			          default:	/* un-flipped arrow	*/
		                    x[1] = el.elem.spl.latlon[0];  
		                    y[1] = el.elem.spl.latlon[npts];
			            break;
			        }
		                gline ( sys_M, &np2, x, y, &ier, strlen(sys_M) );
		              }

		              pgolk_udlist ( &el, &ier );

		              done[index] = 1;
		              npts = el.elem.spl.info.numpts;
    		              flip = el.elem.spl.info.spldir;
		              switch ( flip )  {
		                case -1:	/* flipped arrow	*/
		                  latpt = el.elem.spl.latlon[0];
		                  lonpt = el.elem.spl.latlon[npts];
			          break;
		                default:	/* un-flipped arrow	*/
		                  latpt = el.elem.spl.latlon[npts-1];
		                  lonpt = el.elem.spl.latlon[2*npts-1];
		                  break;
		              }
	                    }

		            sprintf(textstr, "$$\n" );
	    	            pgprd_putstr ( textstr, &ier );
		        }
		    }
		    else  {

/*
 *  Check if string is valid
 */
		        if ( strlen(str) != (size_t)0 )  {

		            valcat = G_FALSE;
    		            for ( ncx = 0; ncx < numcats; ncx++ )  {
			      if ( strcmp(str, outl_cats[ncx]) == 0 )  
			        valcat = G_TRUE;
		            }

		            if ( valcat == G_FALSE )  {
			      sprintf(textstr, "Invalid label '%s' found.\n",
					str );
	    		      pgprd_putstr ( textstr, &ier );
		            }
		        }
		    }
	        }
	    }
        }
    }

/*
 *  Next find all groups without a text identifier 
 *  (GENERAL THUNDERSTORMS -or- SIGNIFICANT SEVERE)
 */    
    for ( ng = lowgrp; ng <= totgrp; ng++ )  {

	crg_gginx( grptyp, ng, sizeof(inxarry)/sizeof(inxarry[0]),
		       inxarry, &nelm, &ier );
	
	if ( nelm != 0 )  {

/*
 *  Search for text string; if string exists skip this group
 */
	    ne = 0;
	    str_found = G_FALSE;
	    while ( ne < nelm && str_found == G_FALSE )  {

	        crg_gtyp( inxarry[ne], &vg_class, &vg_type, &ier );

	        if ( (int)vg_class == CLASS_TEXT )  str_found = G_TRUE;

	        ne++;

	    }

	    if ( str_found == G_FALSE )  {

/*
 *  Found group without a string identifier;
 *  Order the lines such that they 'link' together logically.
 *  First, gather lines in this group into grparr and
 *  determine which line has the eastmost starting point.
 */
		nlines = 0;
		lonpt = -180.0F;
	        for ( nel = 0; nel < nelm; nel++ )  {
	    	    crg_gtyp( inxarry[nel], &vg_class, &vg_type, &ier );
		    crg_goffset( inxarry[nel], &fpos, &ier);
	    	    el_layer = crg_getLayer( fpos );
		    if ( el_layer == cur_layer && 
		         (int)vg_type == SPLN_ELM && fpos > 0 )  {
			cvg_rdrec( fname, fpos, &el, &ier );
			grparr[nlines] = inxarry[nel];
			nlines++;
		    }
		}

		if ( nlines > 0 )  {
		    order = (_order+1) % nlines;
		    crg_goffset( grparr[order], &fpos, &ier );
		    cvg_rdrec( fname, fpos, &el, &ier );
		    npts = el.elem.spl.info.numpts;
    		    flip = el.elem.spl.info.spldir;
		    switch ( flip )  {
		      case -1:			/* flipped arrow	*/
		          latpt = el.elem.spl.latlon[npts-1];
		          lonpt = el.elem.spl.latlon[2*npts-1];
		        break;
		      default:			/* un-flipped arrow	*/
		          latpt = el.elem.spl.latlon[0];
		          lonpt = el.elem.spl.latlon[npts];
		        break;
		    }
		}

/*
 *  Now loop thru each line and print out line closest
 *  to previous line endpoint.
 */
	        for ( nlcnt = 0; nlcnt < nlines; nlcnt++ )  done[nlcnt] = 0;
	        for ( nlcnt = 0; nlcnt < nlines; nlcnt++ )  {

		    dist = FLT_MAX;
		    index = 0;
	            for ( nlin = 0; nlin < nlines; nlin++ )  {
			if ( done[nlin] != 1 )  {
		            crg_goffset( grparr[nlin], &fpos, &ier);
			    cvg_rdrec( fname, fpos, &el, &ier );
			    npts = el.elem.spl.info.numpts;
    		            flip = el.elem.spl.info.spldir;
		            switch ( flip )  {
		              case -1:		/* flipped arrow	*/
			        clo_dist ( &latpt, &lonpt, &npx, 
				    &(el.elem.spl.latlon[npts-1]),
				    &(el.elem.spl.latlon[2*npts-1]), 
				    &distx, &ier );
		                break;
		              default:		/* un-flipped arrow	*/
			        clo_dist ( &latpt, &lonpt, &npx, 
				    &(el.elem.spl.latlon[0]),
				    &(el.elem.spl.latlon[npts]), 
				    &distx, &ier );
		                break;
		            }
			    if ( distx < dist )  {
				dist = distx;
				index = nlin;
			    }
			}
		    }

		    if ( nlcnt == 0 )  {

    			if ( strcmp(grpTypName,"OUTLOOK") == 0 )  {
			  strcpy( outl_gstr, "GENERAL THUNDERSTORMS" );
    			}
    		  	else  {
			  strcpy( outl_gstr, "SIGNIFICANT SEVERE" );
    			}
		        if ( nlines == 1 )  {
	    		  if ( el.hdr.filled )  {
	        	    strcpy( outl_gstr, "SIGNIFICANT SEVERE" );
	    		  }
	    		  else  {
	        	    strcpy( outl_gstr, "GENERAL THUNDERSTORMS" );
	    		  }
	    		}

	        	sprintf(textstr, "%s\nLABEL: -1 -1\n", outl_gstr );
	    		pgprd_putstr ( textstr, &ier );
		    }
		    else  {
			sprintf(textstr, "...CONT...\n");
	    		pgprd_putstr ( textstr, &ier );
		    }

		    crg_goffset( grparr[index], &fpos, &ier);
		    cvg_rdrec( fname, fpos, &el, &ier );

		    if ( nlcnt != 0 )  {
		        x[0] = latpt;  
		        y[0] = lonpt;
			npts = el.elem.spl.info.numpts;
    		        flip = el.elem.spl.info.spldir;
		        switch ( flip )  {
		          case -1:		/* flipped arrow	*/
		            x[1] = el.elem.spl.latlon[npts-1];  
		            y[1] = el.elem.spl.latlon[2*npts-1];
		            break;
		          default:		/* un-flipped arrow	*/
		            x[1] = el.elem.spl.latlon[0];  
		            y[1] = el.elem.spl.latlon[npts];
		            break;
		        }
		        gline ( sys_M, &np2, x, y, &ier, strlen(sys_M) );
		    }

		    pgolk_udlist ( &el, &ier );

		    done[index] = 1;
		    npts = el.elem.spl.info.numpts;
    		    flip = el.elem.spl.info.spldir;
		    switch ( flip )  {
		      case -1:		/* flipped arrow	*/
		        latpt = el.elem.spl.latlon[0];
		        lonpt = el.elem.spl.latlon[npts];
		        break;
		      default:		/* un-flipped arrow	*/
		        latpt = el.elem.spl.latlon[npts-1];
		        lonpt = el.elem.spl.latlon[2*npts-1];
		        break;
		    }
	        }

	        sprintf(textstr, "$$\n" );
	    	pgprd_putstr ( textstr, &ier );

	    }
        }
    }

/*
 *  Process all ungrouped lines of type SPLN_ELM 
 *  (GENERAL THUNDERSTORMS -or- SIGNIFICANT SEVERE)
 */

    for ( nel = 0; nel < MAX_EDITABLE_ELEMS; nel++ )  {

	crg_gtyp( nel, &vg_class, &vg_type, &ier );
	crg_ggrp( nel, &grptyp, &grpnum, &ier );
	crg_goffset( nel, &fpos, &ier);
        el_layer = crg_getLayer( fpos );

	if ( (int)vg_type == SPLN_ELM && fpos > 0 &&
		grpnum == 0 && (el_layer == cur_layer) )  {

	    cvg_rdrec( fname, fpos, &el, &ier );

    	    if ( strcmp(grpTypName,"OUTLOOK") == 0 )  {
		strcpy( outl_gstr, "GENERAL THUNDERSTORMS" );
    	    }
            else  {
		strcpy( outl_gstr, "SIGNIFICANT SEVERE" );
    	    }
	    if ( el.hdr.filled )  {
	        strcpy( outl_gstr, "SIGNIFICANT SEVERE" );
	    }
	    else  {
	        strcpy( outl_gstr, "GENERAL THUNDERSTORMS" );
	    }

	    sprintf(textstr, "%s\nLABEL: -1 -1\n", outl_gstr );
	    pgprd_putstr ( textstr, &ier );

	    pgolk_udlist ( &el, &ier );

	    sprintf(textstr, "$$\n" );
	    pgprd_putstr ( textstr, &ier );

	}
    }

/*
 *  Restore color and line attributes
 */
    gscolr ( &icolrs, &ier );
    gsline ( &iltyps, &ilthw, &iwidths, &iwhw, &ier );

/*
 *  Flush buffer
 */
    geplot ( &ier );

/*
 *  Increment order counter.
 */
    _order++;

}

/*=====================================================================*/

void pgolk_udlist ( VG_DBStruct *elmnt, int *iret )
/************************************************************************
 * pgolk_udlist                                                    	*
 *                                                                      *
 * This function takes an element and processes the vertices to 	*
 * compute the closest station, etc.					*
 *                                                                      *
 * pgolk_udlist ( elmnt, iret )						*
 *									*
 * Input parameters:                                                    *
 *  *elmnt	VG_DBStruct	VG special line element			*
 *                                                                      *
 * Output parameters:                                             	*
 *  *iret	int		Return code				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 4/98						*
 * E. Safford/GSC	10/98	light clean up				*
 * D.W.Plummer/NCEP     12/98   Rename clo_direct to clo_tdirect   	*
 * D.W.Plummer/NCEP      5/99	Specify "ANCHOR" in call to clo_tdirect	*
 * M. Li/GSC		10/99	Replaced clo_compass code		*
 * A. Hardy/GSC		01/00   Change clo_tdirect calling sequence     *
 ***********************************************************************/
{
float		distm, dir;
char		cmpdir[4], stn[8], textstr[80], type[7];
int    		distsm5, ii, ier, npts, flip, indxlat, indxlon, icmp;
/*---------------------------------------------------------------------*/

    *iret = 0;

    npts = elmnt->elem.spl.info.numpts;
    flip = elmnt->elem.spl.info.spldir;

    for ( ii = 0; ii < npts; ii++ )  {

        switch ( flip )  {

	    case -1:		/* flipped arrow	*/

	        indxlat = (npts-1) - ii;
	        indxlon = (2*npts-1) - ii;
	        break;

	    default:		/* un-flipped arrow	*/

	        indxlat = ii;
	        indxlon = ii+npts;
	        break;

        }
	strcpy(type,"ANCHOR");

	clo_tdirect( type, elmnt->elem.spl.latlon[indxlat], 
		     elmnt->elem.spl.latlon[indxlon], 
		     stn, &distm, &dir, &ier );

	clo_compass( &dir, cmpdir, &icmp, &ier );

/*
 *  Convert and round distance to nearest 5 statute miles.
 */
	distsm5 = G_NINT( distm * M2SM / 5.0F ) * 5;

	if ( distsm5 <= 5 )  {
	    strcpy( cmpdir, "-" );
	    distsm5 = 0;
	}

	sprintf(textstr, "%5.2f %7.2f %-2d %-4s %s\n", 
		elmnt->elem.spl.latlon[indxlat], 
		elmnt->elem.spl.latlon[indxlon], 
		distsm5, cmpdir, stn );
	pgprd_putstr ( textstr, &ier );

    }

    if ( elmnt->hdr.closed )  {

/*
 *  If line is closed, repeat first point
 */
        switch ( flip )  {

	    case -1:		/* flipped arrow	*/

	        indxlat = (npts-1);
	        indxlon = (2*npts-1);
	        break;

	    default:		/* un-flipped arrow	*/

	        indxlat = 0;
	        indxlon = npts;
	        break;

        }

	clo_tdirect( type, elmnt->elem.spl.latlon[indxlat], 
		     elmnt->elem.spl.latlon[indxlon], 
		     stn, &distm, &dir, &ier );

	clo_compass( &dir, cmpdir, &icmp, &ier );

/*
 *  Convert and round distance to nearest 5 statute miles.
 */
	distsm5 = G_NINT( distm * M2SM / 5.0F ) * 5;

	if ( distsm5 <= 5 )  {
	    strcpy( cmpdir, "-" );
	    distsm5 = 0;
	}

	sprintf(textstr, "%5.2f %7.2f %-2d %-4s %s\n", 
		elmnt->elem.spl.latlon[indxlat], 
		elmnt->elem.spl.latlon[indxlon], 
		distsm5, cmpdir, stn );
	pgprd_putstr ( textstr, &ier );

    }
}

/*=====================================================================*/

void pgolk_getfname ( char *text, char *fname, int *iret )
/************************************************************************
 * pgolk_getfname                                                  	*
 *                                                                      *
 * Build filename from outlook text string.				*
 *                                                                      *
 * void pgolk_getfname(text, fname, iret)				*
 *                                                                      *
 * Input parameters:                                                    *
 *  *text	char	Outlook text string.				*
 *                                                                      *
 * Output parameters:                                                   *
 *  *fname	char	Filename					*
 *  *iret	int	Return Code					*
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 4/98						*
 * M. Li/GSC		01/01	Added group type name			*
 * E. Safford/SAIC	04/02	get grp type from pgolk_getGroup()	*
 * B. Yin/SAIC		01/04	get grp type full name from a table     *
 * G. Grosshans/SPC	10/04	changed filename format			*
 ***********************************************************************/
{
char	day[16], fcstr[64], vtimes[64], time[20], buffer[64];
char	*ptr1, *ptr2, grpname[20], grptyp, *col1, *col2;
int	len, ier, ier_otbl, ier_rtbl, found, bufsiz, ii;
FILE	*fp;
/*---------------------------------------------------------------------*/

    *iret = 0;

    strcpy( fname, "outlook_default.dat" );

    ptr1 = text;
    ptr2 = strchr( ptr1, '\n' );

    if ( ptr2 != NULL )  {

        len = (int)(ptr2 - ptr1);
        strncpy( day, ptr1, len );
        day[len] = '\0';

        ptr1 = ptr2 + 1;
        ptr2 = strchr( ptr1, '\n' );

	if ( ptr2 != NULL )  {

            len = (int)(ptr2 - ptr1);
            strncpy( fcstr, ptr1, len );
            fcstr[len] = '\0';

            ptr1 = ptr2 + 1;
            ptr2 = strchr( ptr1, '\n' );

	    if ( ptr2 != NULL )  {

                len = (int)(ptr2 - ptr1);
                strncpy( vtimes, ptr1, len );
                vtimes[len] = '\0';

		strcpy( time, strtok(vtimes, " ") );

                pgolk_getGroup ( &grptyp, grpname );
    		cst_uclc(grpname, grpname, &ier);
                
/*
 * open the table that contains abbreviated group types 
 * and their full names
 */
		fp = cfl_tbop( GRPNAME_TABLE, "pgen", &ier_otbl );
		
		found = 0;

/*
 * read the table and find the goup type full name
 */
		if( ier_otbl == 0 )  {
		    
		    ier_rtbl = 0;
		    bufsiz = sizeof( buffer );
		    
		    while( ( found == 0 ) && ( ier_rtbl == 0 ) ) {

		        cfl_trln( fp, bufsiz, buffer, &ier_rtbl );
			if( feof( fp ) )
			    break;
			
			ii = 0;

/*
 *  skip leading blank space 
 */
			while( buffer[ ii ] == '\0' || buffer[ ii ] == '\t' 
			       || buffer[ ii ] == ' ' )
			     ii++;

/*
 *  get first column
 */
			col1 = &buffer[ ii ];
			while( buffer[ ii ] != '\0' && buffer[ ii ] != '\t' 
			       && buffer[ ii ] != ' ' )
			    ii++;
			buffer[ ii ] = '\0';

/*
 *  skip blank space and get second column
 */
			 while( buffer[ ii ] == '\0' || buffer[ ii ] == '\t' 
			        || buffer[ ii ] == ' ' )
			     ii++;
			 col2 = &buffer[ ii ];

			 while( buffer[ ii ] != '\0' && buffer[ ii ] != '\t'
				&& buffer[ ii ] != ' ' && buffer[ ii ] != '\n' )
			     ii++;
			 buffer[ ii ] = '\0';

			 if( strcasecmp( grpname, col1 ) == 0 )
			     found = 1;
		    }

		    cfl_clos( fp, &ier_otbl );
		
		}
	
		if( found == 1 )  {
		   if( strcmp( col2, "-" ) == 0 )
		      sprintf( fname, "outlook_%s_%s.dat", day, time );
		   else
	              sprintf( fname, "%soutlook_%s_%s.dat", col2, day, time );
		}
		else
                   sprintf( fname, "%soutlook_%s_%s.dat", grpname, day, time );
	    }
	}
    }
}

/*=====================================================================*/

void pgolk_check ( int maxlen, char *msgs, int *iret )
/************************************************************************
 * pgolk_check                                                    	*
 *                                                                      *
 * This function checks the outlook VG file for elements with group	*
 * types other than the expected group type. If any are found, they are	*
 * listed in the returned msgs array and iret is set to the number of	*
 * erroneous elements found.						*
 *                                                                      *
 * void pgolk_check( maxlen, msgs, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *  maxlen		int	Maximum length of msgs in characters	*
 *                                                                      *
 * Output parameters:                                             	*
 *  *msgs		char	Message containing warning info		*
 *  *iret		int	Return code				*
 *				 n = found n invalid grptyps		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 5/01	create					*
 * J. Wu/GSC		 5/01	update parameter list			*
 * E. Safford/SAIC	03/02 	use pgolk_getGroup for expected grptyp  *
 * M. Li/SAIC		10/02	Get vg_class, and vg_type		*
 * J. Wu/SAIC		07/04	add filter param. to crg_get()		*
 ***********************************************************************/
{
int    		nel, grpnum, grpid, cur_layer, layer, offset, ier;
char		grptyp, grptyp_exp, vg_class, vg_type, str[80];
char		grpTypName[20], grpnam[20];
float		dummy1, dummy2, dummy3, dummy4;
filter_t	filter;
/*---------------------------------------------------------------------*/

    *iret = 0;
    cur_layer = pglayer_getCurLayer();

    pgolk_getGroup ( &grptyp_exp, grpTypName );


    msgs[0] = '\0';

    sprintf ( str, "\nObjects found with group type other than %s:\n",
	grpTypName ) ;
    if ( strlen(msgs) + strlen(str) < (size_t)maxlen )  strcat ( msgs, str );

/*
 *  Check all elements for expected group type.
 */
    for ( nel = 0; nel < MAX_EDITABLE_ELEMS; nel++ )  {

	crg_ggrp( nel, &grptyp, &grpnum, &ier );
        crg_get ( nel, &layer, filter, &dummy1, &dummy2, &dummy3, &dummy4, &ier);
	crg_goffset( nel, &offset, &ier );
	crg_gtyp( nel, &vg_class, &vg_type, &ier );

	if ( offset >= 0 && grptyp > 0 && grptyp != grptyp_exp && 
						layer == cur_layer )  {
	    (*iret)++;

	    grpid = (int)grptyp;
	    ces_gtgnam ( grpid, grpnam, &ier );
	    sprintf( str, "Element %d is group %s (class=%d,type=%d)\n", 
		nel, grpnam, (int)vg_class, (int)vg_type );
	    if ( strlen(msgs) + strlen(str) < (size_t)maxlen )  strcat ( msgs, str );

	}
    }

    if ( *iret != 0 )  {
	sprintf( str, "Total number of mismatched elements = %d", *iret );
	if ( strlen(msgs) + strlen(str) < (size_t)maxlen )  strcat ( msgs, str );
    }
}

/*=====================================================================*/

void pgolk_getGroup ( char *grptyp_exp, char grpname_exp[] )
/************************************************************************
 * pgolk_getGroup                                                       *
 *                                                                      *
 * This function determines what the expected group type and            *
 * corresponding group name are for the current layer.  This expected   *
 * group type is taken from the first grouped element that is found on  *
 * the current layer.  If no grouped elements are located, then the     *
 * default of 7 and outlook will be returned.                           *
 *                                                                      *
 * void pgolk_getGroup ( grptyp_exp, grpname_exp )  			*
 *                                                                      *
 * Input parameters:                                                    *
 *                      None                                            *
 * Output parameters:                                                   *
 *  *grptyp_exp         char    expected group type for this layer      *
 *  grpname_exp[]       char    name of the expected group type         *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/SAIC      03/02   initial coding                          *
 * B. Yin/SAIC		01/04   Added ability to get grp name from layer*
 * J. Wu/SAIC		07/04	add filter param. to crg_get()		*
 ***********************************************************************/
{
int             nel, el_grpnum, grpid, cur_layer, layer, ier;
char            grptyp, el_grptyp, grpTypName[20];
float           dummy1, dummy2, dummy3, dummy4;
filter_t	filter;
/*---------------------------------------------------------------------*/

    cur_layer = pglayer_getCurLayer();

/*
 * Get the group type name of current layer
 */
    grpid = pglayer_getDefGrp( cur_layer );
    if ( grpid != -1 )  {
       ces_gtgnam( grpid, grpname_exp, &ier );
       if ( ier == 0 )  {
	  *grptyp_exp = (char)grpid;
          return;
       }
    }       

/*
 *  Use Outlook as the default type in case no grouped elements are
 *  found in the file.
 */
    strcpy (grpTypName, "OUTLOOK");
    ces_gtgid(grpTypName, &grpid, &ier);
    grptyp = (char)grpid;

/*
 *  Determine the expected group type by searching the range
 *  records for the first grouped element in this layer.  This group
 *  type will be used as the expected group type for all other
 *  elements.
 */
    for ( nel = 0; nel < MAX_EDITABLE_ELEMS; nel++ )  {
        crg_ggrp( nel, &el_grptyp, &el_grpnum, &ier );
        crg_get ( nel, &layer, filter, &dummy1, &dummy2, &dummy3, &dummy4, &ier);

        if ( el_grptyp > 0 && el_grpnum > 0 && layer == cur_layer )  {
            grptyp = el_grptyp;
            ces_gtgnam ( grptyp, grpTypName, &ier);
            break;
        }
    }

    *grptyp_exp = grptyp;
    strcpy ( grpname_exp, grpTypName );
}
