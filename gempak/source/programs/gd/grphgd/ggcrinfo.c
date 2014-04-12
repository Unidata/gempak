#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"

void gg_udlist ( VG_DBStruct el, char *label, FILE *ggfpt, int *plt_ext, int *iret );
void gg_udsymb ( VG_DBStruct el, char *label, FILE *ggfpt, int *iret  );
void gg_udwind ( VG_DBStruct el, FILE *ggfpt, int *iret  );

/************************************************************************
 * ggcrinfo.c                                                         	*
 *                                                                      *
 * This module creates the grid-to-graph contour information.		*
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 *      gg_update()	      create grid-to-graph contour info cntrl	*
 *      gg_udlist()	      create grid-to-graph contour info (lines)	*
 *      gg_udsymb()	      create grid-to-graph contour info (symbs)	*
 *      gg_rdvgf()  	      create info file name off vgf file name   *
 ***********************************************************************/

/*=====================================================================*/

void gg_update ( char *vname, char *iname, int *cur_layer, char *catmap,
                 int *plt_ext, int *iret )
/************************************************************************
 * gg_update                                                            *
 *                                                                      *
 * This function gets graph-to-grid contour information from a VGF file	*
 * and writes the information into a ".info" ascii file.  This function *
 * could be called either from a stand alone graph-to-grid plogram, 	*
 * GRPHGD, or from NMAP2 in PGEN mode.  If it is called from GRPHGD,	*
 * the flag plt_ext is always set to FALSE, and cur_layer to 0.  If it 	*
 * is called from NMAP2, cur_layer is set to currently accessed layer	*
 * and plt_ext could be set to either FALSE or TRUE.  If plt_ext is	*
 * set to FALSE, the extesion plotting is skipped.  If it is TRUE, 	*
 * only plotting of the extensions takes place, without writing out of	*
 * the ".info" file.							*
 *                                                                      *
 * void gg_update( vname, iname, cur_layer, catmap, plt_ext, iret )     *
 *                                                                      *
 * Input parameters:                                                    *
 *  *fname              char    VGF Filename                            *
 *  *iname              char    .info filename                          *
 *  *cur_layer          int     product generation layer to access      *
 *  *catmap             char    CATMAP parameter string                 *
 *  *plt_ext            int     flag for plotting extensions            *
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret               int     Return code                             *
 *                              -1 = unable to convert                  *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      8/98                                           *
 * D.W.Plummer/NCEP      9/98   Increased size of inxarry to 100        *
 * E. Safford/GSC       10/98   updated params to cvg_rdrec             *
 * D.W.Plummer/NCEP     12/98   Added visual line extensions            *
 * H. Zeng/EAI          10/99   Modified for new graph-to-grid window   *
 * D.W.Plummer/NCEP      8/00   increase labelstr and string check      *
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * H. Zeng/EAI          03/01   modified to use ces_gtgid()             *
 * E. Safford/SAIC      11/01   mv _pghdlb_refresh -> pgutls_refresh    *
 * J. Wu/SAIC           01/02   update the current layer only           *
 * M. Li/SAIC           04/02   replaced crg_ggnxt with crg_ggnhl       *
 * H. Zeng/XTRIA        02/03   added CIRCLE_ELM case                   *
 * D.W.Plummer/NCEP     09/03   Rm chk for lines w/ RMISSD valued labls *
 * T. Lee/SAIC          08/04   write out vector elements               *
 * H. Zeng/SAIC         03/05   Added categorical mapping string        *
 * H. Zeng/SAIC         04/05   Added more group types                  *
 * M. Li/SAIC           10/05   Process unlabeled lines                 *
 * m.gamazaychikov/SAIC	12/05   Changed CS for improved info file fnctly*
 * m.gamazaychikov/SAIC 02/06   Added check to m/sure iname is writable *
 * D.W.Plummer/NCEP	02/06	Don't call crg_build if .DEFAULT.vgf	*
 * G.McFadden/SAIC	08/07	Added group WHFT			*
 * F. J. Yen/NCEP	01/08	Added groups EXT_FIRE and EXT_SVR	*
 * S. Jacobs/NCEP	 4/13	Added groups ENHxx for enhanced thunder	*
 ***********************************************************************/
{
int             ii, ier, ng, ne, nn, nint, nel, nelm, fpos, el_layer;
int             totgrp, lowgrp, found, numfnd, grpid, ier2; 
int             inxarry[100], ngtyp[100], ngnum[100];
char            grptyp, vg_class, vg_type;

VG_DBStruct     el, el_lin;
                                                                                                                            
char            labelstr[128];

float           fnum, cint[200];
                                                                                                                            
 char		*which_groups[] = { "LABEL", "HIGH", "LOW", "OUTLOOK",
                                   "HAILOTLK", "TORNOTLK", "WINDOTLK",
                                   "TOTL_SVR", "FIREOUTL", "CATG_SVR",
                                   "TSTMOLK", "WHFT", "EXT_FIRE",
				   "EXT_SVR", "ENH20", "ENH00", "ENH04",
				   "ENH12", "ENH16" };
int             ngrp, numgrp;

FILE           *ggfpt;
/*---------------------------------------------------------------------*/

    *iret = 0;
    nint = 0;

    ggfpt = (FILE *)cfl_wopn( iname, &ier);
     
    if ( ier < 0 && iname[0] != '\0') {
       er_wmsg ( "CFL", &ier, iname, &ier2, 3, strlen (iname) );
      *iret = -1;
       return;
    }

   /*
    *  Generate range records only if VGF is not .DEFAULT.vgf.
    *  i.e., if this function is being called from NMAP.
    */
    if ( strstr(vname,".DEFAULT.vgf") == (char *)NULL )  
	crg_build ( vname, cur_layer, &ier );

    numgrp = sizeof(which_groups)/sizeof(which_groups[0]);

    for ( ngrp = 0; ngrp < numgrp; ngrp++ )  {

      /*
       *  Determine the grptyp for "LABEL".
       */
      ces_gtgid(which_groups[ngrp], &grpid, &ier2);
      grptyp = (char)grpid;

      if ( grptyp == GRPTYP_OTHERS || ier2 != 0 )  {
        *iret = -1;
        return;
      }
                                                                                                                            
      /*
       *  Get the next available group number to see how many groups there are.
       */
      crg_ggnhl ( grptyp, &totgrp, &lowgrp, &ier );
                                                                                                                            
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
             *  Search for text string within the group;
             *  if string exists then process group, otherwise skip.
             *  only do this for one text string
             */
            found = 0;
            for ( ne = 0; ne < nelm && found == 0; ne++ )  {
                                                                                                                            
                crg_gtyp( inxarry[ne], &vg_class, &vg_type, &ier );
                crg_goffset( inxarry[ne], &fpos, &ier );
                el_layer = crg_getLayer( fpos );

                if ( el_layer == *cur_layer &&
                     (int)vg_class == CLASS_TEXT && fpos > 0 )  {
                                                                                                                            
                    found = 1;
                                                                                                                            
                    /*
                     *  Found group with a string identifier
                     */
                    cvg_rdrec( vname, fpos, &el, &ier );
                                                                                                                            
                    if ((int) vg_type == SPTX_ELM) {
                        strcpy (labelstr, el.elem.spt.text);
                    }
                    else {
                        strcpy (labelstr, el.elem.txt.text);
                    }
                                                                                                                            
                    in_catmmap ( labelstr, &fnum, &ier );
                                                                                                                            
                    /*
                     *  Found a string that is a valid number.
                     */
                    if ( ier == 0 )  {
                      numfnd = 0;
                      for ( ii = 0; ii < nint; ii++ )  {
                        if ( G_ABS(fnum-cint[ii]) < 0.005F )  numfnd = 1;
                      }
                      if ( numfnd == 0 )  {
                        cint[nint] = fnum;
                        (nint)++;
                      }
                    }
                    else  {
                        found = 0;
                    }
                                                                                                                            
                }
                                                                                                                            
                if ( found == 1 )  {
                                                                                                                            
                    /*
                     *  Process each LINE from group
                     */
                    for ( nel = 0; nel < nelm; nel++ )  {
                                                                                                                            
                        if ( nel != ne )  {
                                                                                                                            
                            crg_gtyp( inxarry[nel], &vg_class, &vg_type, &ier );
                                                                                                                            
                            crg_goffset( inxarry[nel], &fpos, &ier);
                                                                                                                            
                            el_layer = crg_getLayer( fpos );
                                                                                                                            
                            if ( (int)vg_type == LINE_ELM  &&
                                 el_layer == *cur_layer && fpos > 0 )  {
                                                                                                                            
                                cvg_rdrec(vname, fpos, &el, &ier);
                                                                                                                            
                                if ( el.elem.lin.info.lintyp >= 0 )  {
                                                                                                                            
                                    gg_udlist ( el, labelstr, ggfpt, plt_ext, &ier );
                                                                                                                            
                                }
                            }
                            else if ( (int)vg_type == SPLN_ELM  &&
                                 el_layer == *cur_layer && fpos > 0 )  {
                                                                                                                            
                                cvg_rdrec(vname, fpos, &el, &ier);
                                                                                                                            
                                if ( el.elem.spl.info.spltyp >= 0 )  {
                                                                                                                            
                                    gg_udlist ( el, labelstr, ggfpt, plt_ext, &ier );
                                                                                                                            
                                }
                            }
                            else if ( (int)vg_type == CIRCLE_ELM  &&
                                 el_layer == *cur_layer && fpos > 0 )  {
                                                                                                                            
                                /*
                                 * If circle is encountered, convert it
                                 * to a line first and treat it as line
                                 * element.
                                 */
                                cvg_rdrec(vname, fpos, &el, &ier);
                                cvg_cir2lin ( &el, 10, &el_lin, &ier );
                                el = el_lin;
                                                                                                                            
                                if ( el.elem.lin.info.lintyp >= 0 )  {
                                                                                                                            
                                    gg_udlist ( el, labelstr, ggfpt, plt_ext, &ier );
                                                                                                                            
                                }
                            }
                            else if ( ( (int)vg_type == SPSYM_ELM  ||
                                        (int)vg_type == MARK_ELM )  &&
                                      el_layer == *cur_layer && fpos > 0 )  {
                                                                                                                            
                                cvg_rdrec(vname, fpos, &el, &ier);
                                gg_udsymb ( el, labelstr, ggfpt, &ier );
                                                                                                                            
                            }
                        }
                    }
                }
            }
        }
      }
                                                                                                                            
    }   /* end loop over group types    */
                                                                                                                            
                                                                                                                            
    /*
     * Process unlabeled lines
     */
                                                                                                                            
    strcpy ( labelstr, "UNLABELED" );
    in_catmmap ( labelstr, &fnum, &ier );
                                                                                                                            
    /*
     *  Found a string that is a valid number.
     */
    numfnd = 0;
    for ( ii = 0; ii < nint; ii++ )  {
        if ( G_ABS(fnum-cint[ii]) < 0.005F )  numfnd = 1;
    }
    if ( numfnd == 0 )  {
        cint[nint] = fnum;
        (nint)++;
    }
                                                                                                                            
    for ( ngrp = 0; ngrp <= numgrp; ngrp++ )  {
                                                                                                                            
      /*
       *  Determine the grptyp for "LABEL" or ungrouped lines.
       */
      if ( ngrp == numgrp ) {
          grpid = 0;
      }
      else {
          ces_gtgid(which_groups[ngrp], &grpid, &ier2);
      }
      grptyp = (char)grpid;
                                                                                                                            
      if ( grptyp == GRPTYP_OTHERS )  {
        *iret = -1;
        return;
      }
                                                                                                                            
      /*
       *  Get the next available group number to see how many groups there are.
       */
      crg_ggnhl ( grptyp, &totgrp, &lowgrp, &ier );
                                                                                                                            
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
             *  Search for text string within the group;
             *  if string exists then process group, otherwise skip.
             *  only do this for one text string
             */
            for (ii = 0; ii < nelm; ii++ ) {
                ngtyp[ii] = -999;
                ngnum[ii] = -999;
            }
            nn = 0;
                                                                                                                            
            /*
             * search lines grouped with text.
             */
            for ( ne = 0; ne < nelm; ne++ )  {
                                                                                                                            
                crg_gtyp( inxarry[ne], &vg_class, &vg_type, &ier );
                crg_goffset( inxarry[ne], &fpos, &ier );
                el_layer = crg_getLayer( fpos );
                                                                                                                            
                if ( el_layer == *cur_layer &&
                        (int)vg_class == CLASS_TEXT && fpos > 0 )  {
                                                                                                                            
                    cvg_rdrec( vname, fpos, &el, &ier );
                    ngtyp[nn] = el.hdr.grptyp;
                    ngnum[nn] = el.hdr.grpnum;
                    nn++;
                }
            }
                                                                                                                            
                                                                                                                            
            found = 0;
            for ( ne = 0; ne < nelm; ne++ )  {
                                                                                                                            
                crg_gtyp( inxarry[ne], &vg_class, &vg_type, &ier );
                crg_goffset( inxarry[ne], &fpos, &ier );
                el_layer = crg_getLayer( fpos );
                cvg_rdrec( vname, fpos, &el, &ier );
                                                                                                                            
                if ( nn > 0 ) {
                    for ( ii = 0; ii < nn; ii++ ) {
                        if ( el.hdr.grptyp == ngtyp[ii] && el.hdr.grpnum == ngnum[ii] ) {
                            found = 1;
                            break;
                        }
                    }
                }
                                                                                                                            
                if ( found == 0 ) {
                                                                                                                            
                     if ( (int)vg_type == LINE_ELM  &&
                                       el_layer == *cur_layer && fpos > 0 )  {
                         if ( el.elem.lin.info.lintyp >= 0 )  {
                             gg_udlist ( el, labelstr, ggfpt, plt_ext, &ier );
                                                                                                                            
                         }
                     }
                     else if ( (int)vg_type == SPLN_ELM  &&
                                       el_layer == *cur_layer && fpos > 0 )  {
                         if ( el.elem.spl.info.spltyp >= 0 )  {
                              gg_udlist ( el, labelstr, ggfpt, plt_ext, &ier );
                                                                                                                            
                         }
                     }
                     else if ( (int)vg_type == CIRCLE_ELM  &&
                                      el_layer == *cur_layer && fpos > 0 )  {
                          /*
                           * If circle is encountered, convert it
                           * to a line first and treat it as line
                           * element.
                           */
                          cvg_cir2lin ( &el, 10, &el_lin, &ier );
                          el = el_lin;
                                                                                                                            
                          if ( el.elem.lin.info.lintyp >= 0 )  {
                              gg_udlist ( el, labelstr, ggfpt, plt_ext, &ier );
                                                                                                                            
                          }
                     }
                }
                                                                                                                            
            }
         }
      }
                                                                                                                            
    }   /* end loop over group types    */
                                                                                                                            
                                                                                                                            
                                                                                                                            
    /*
     *  check vector elements.
     */
                                                                                                                            
    for ( nel = 0; nel < MAX_EDITABLE_ELEMS; nel++ )  {
        crg_gtyp( nel, &vg_class, &vg_type, &ier );
        crg_goffset( nel, &fpos, &ier);
        el_layer = crg_getLayer( fpos );
        if ( fpos < 0 ) continue;
        if ( ( el_layer == *cur_layer &&
             ( (int)vg_type == BARB_ELM || (int)vg_type == ARROW_ELM ) ) ) {
            cvg_rdrec( vname, fpos, &el, &ier );
            gg_udwind ( el, ggfpt, &ier );
        }
    }
                                                                                                                            

    cfl_clos ( ggfpt, &ier );

    /*
     *  Flush buffer
     */
    geplot ( &ier );
                                                                                                                            
}
                                                                                                                            

/*=====================================================================*/

void gg_udsymb ( VG_DBStruct el, char *label, FILE *ggfpt, int *iret )
/************************************************************************
 * gg_udsymb                                                    	*
 *                                                                      *
 * This function takes a symbol element and writes some info out.	*
 *                                                                      *
 * gg_udsymb ( el, label, ggfpt, iret )				*
 *									*
 * Input parameters:                                                    *
 *  el		VG_DBStruct	VG symbol element			*
 *  *label	char		Contour label value			*
 *  *ggfpt      FILE            Pointer the ".info" ascii file          *
 *                                                                      *
 * Output parameters:                                             	*
 *  *iret	int		Return code				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	12/02						*
 * D.W.Plummer/NCEP	09/03	Add check for NULL file pointer		*
 * M. Li/SAIC		10/05	From pgggc_udsymb			*
 ***********************************************************************/
{
int	icolr, npts;
/*---------------------------------------------------------------------*/

    *iret = 0;

    icolr = (int)el.hdr.maj_col;

    npts = 1;
    if ( ggfpt != (FILE *)NULL )
        fprintf(ggfpt, "%d %s %7.2f %7.2f %d\n", 
		npts, label, el.elem.sym.data.latlon[0], 
		el.elem.sym.data.latlon[1], icolr );

}

/*=====================================================================*/

void gg_udlist ( VG_DBStruct el, char *label, FILE *ggfpt, int *plt_ext,
                 int *iret )
/************************************************************************
 * gg_udlist                                                    	*
 *                                                                      *
 * This function takes a line element and writes some info out.		*
 *                                                                      *
 * gg_udlist ( el, label, ggfpt, plt_ext, iret )			*
 *									*
 * Input parameters:                                                    *
 *  el		VG_DBStruct	VG special line element			*
 *  *label	char		Contour label value			*
 *  *ggfpt      FILE            Pointer the ".info" ascii file          *
 *  *plt_ext    int     	flag for plotting extensions            *
 *                                                                      *
 * Output parameters:                                             	*
 *  *iret	int		Return code				*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 8/98						*
 * D.W.Plummer/NCEP	 8/98	Added check and output for closed curves*
 * D.W.Plummer/NCEP	12/98	Added visual line extensions		*
 * D.W.Plummer/NCEP	 2/99	Added line color to output text product	*
 * H. Zeng/EAI          10/99   Modified for new graph-to-grid window   *
 * D.W.Plummer/NCEP	11/99	Added checks for when to write data	*
 * A. Hardy/GSC         11/00   renamed coordinate system declarations  *
 * J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 * D.W.Plummer/NCEP	09/03	Only plot extnsions if !writing to file	*
 * H. Zeng/SAIC		04/05	added special line type processing	*
 * D.W.Plummer/NCEP	09/05	add 'extend' param in ggapsm call seq	*
 * M. Li/SAIC		10/05	From pgggc_udlist			*
 * m.gamazaychikov/SAIC	12/05	Added plt_ext to CS, removed wrinfo	*
 * m.gamazaychikov/SAIC 01/05   Fixed plt_ext check for closed lines    *
 * F. J. Yen/NCEP	01/08   Added vgtype and subtyp to header line  *
 ***********************************************************************/
{
int    		ii, ier, npts, nrec, np, np2, ivgtyp, isubtp;
float		x[1000], y[1000], x1[1000], y1[1000];
int		ismth, iclsd;
float		fnxmin, fnymin, fnxmax, fnymax, *lat_lon;
int		nxmax, nymax, start_pt, end_pt, incr;
int		icolrs, icolr;
int		iltyps, iwidths, iltyp, iwidth, ilthw, iwhw;
int		extend=G_TRUE;

/*---------------------------------------------------------------------*/

    *iret = 0;

    switch ( el.hdr.vg_type ) {

      case LINE_ELM :

	npts = el.elem.lin.info.numpts;
        lat_lon = el.elem.lin.latlon;
	isubtp = el.elem.lin.info.lintyp;

	start_pt = 0;
        end_pt   = npts - 1;
        incr     = 1;

        break;

      case SPLN_ELM :

	npts = el.elem.spl.info.numpts;
        lat_lon = el.elem.spl.latlon;
	isubtp = el.elem.spl.info.spltyp;

        if ( el.elem.spl.info.spldir < 0 ) {

	  start_pt = npts - 1;
          end_pt   =  0;
          incr     = -1;
        }
        else {

	  start_pt = 0;
          end_pt   = npts - 1;
          incr     = 1;
        }

        break;

      default:

        break;
    }

    if ( npts <= 1 )  return;

    nrec = npts;
    if ( (int)el.hdr.closed == 1 )  nrec++;

    ismth = (int)el.hdr.smooth;
    iclsd = (int)el.hdr.closed;
    icolr = (int)el.hdr.maj_col;
    ivgtyp = (int)el.hdr.vg_type;

    if ( *plt_ext == 0 )  {

        fprintf(ggfpt, "%d %s %d %d %d %d %d\n", nrec, label, ismth, iclsd,
		icolr, ivgtyp, isubtp );

        /*
         * Loop through start_pt to end_pt inclusive.
         */
        for ( ii = start_pt; ii != (end_pt+incr); ii+=incr )  {

	    fprintf(ggfpt, "%7.2f %7.2f\n", lat_lon[ii], lat_lon[ii+npts] );
        }
    }

    if ( (int)el.hdr.closed == 1 )  {

        if ( *plt_ext == 0 )  {
            fprintf(ggfpt, "%7.2f %7.2f\n", lat_lon[start_pt], 
		    lat_lon[start_pt+npts] );
	}

    }
    else  {

      if ( *plt_ext != 0 )  {

	gtrans ( sys_M, sys_N, &npts, &(lat_lon[0]),
	         &(lat_lon[npts]), x, y, &ier, 
	         strlen(sys_M), strlen(sys_N) );

	for ( ii = 0; ii < npts; ii++ )  {
	    x[ii] = x[ii] * 100.0F;
	    y[ii] = y[ii] * 100.0F;
	}

        gqbnd ( sys_N, &fnxmin, &fnymin, &fnxmax, &fnymax, &ier, 
	        strlen(sys_N) );
	nxmax = (int)(fnxmax * 100.0F);
	nymax = (int)(fnymax * 100.0F);
	ggapsm ( x, y, &npts, &nxmax, &nymax, &ismth, &iclsd, &extend,
	         x1, y1, &np, &ier );

	for ( ii = 0; ii < np; ii++ )  {
	    x1[ii] = x1[ii] / 100.0F;
	    y1[ii] = y1[ii] / 100.0F;
	}

	/*
	 *  Save color and line attributes
	 */
	gqcolr ( &icolrs, &ier );
	gqline ( &iltyps, &ilthw, &iwidths, &iwhw, &ier );

	/*
	 *  Set new color and line attributes
	 */
	icolr = 18;
	gscolr ( &icolr, &ier );
	iltyp = 1;
	iwidth = 3;
	gsline ( &iltyp, &ilthw, &iwidth, &iwhw, &ier );

	/*
	 *  Plot new endpoint segments
	 */
	np2 = 2;
	gline ( sys_N, &np2, &(x1[0]), &(y1[0]), &ier, strlen(sys_N) );
	gline ( sys_N, &np2, &(x1[np-2]), &(y1[np-2]), &ier, strlen(sys_N) );

	/*
	 *  Restore color and line attributes
	 */
	gscolr ( &icolrs, &ier );
	gsline ( &iltyps, &ilthw, &iwidths, &iwhw, &ier );

      }

    }

}

/*=====================================================================*/

void gg_udwind ( VG_DBStruct el, FILE *ggfpt, int *iret )
/************************************************************************
 * gg_udwind								*
 *									*
 * This function takes a vector element and writes some info out.	*
 *									*
 * gg_udwind ( el, ggfpt, iret )					*
 *									*
 * Input parameters:                                                    *
 *  el		VG_DBStruct	VG symbol element			*
 *  *ggfpt      FILE            Pointer the ".info" ascii file          *
 *									*
 * Output parameters:                                             	*
 *  *iret	int		Return code				*
 *									*
 **									*
 * Log:									*
 * T. Lee/SAIC		 8/04	Created					*
 * M. Li/SAIC		10/05	From pgggc_udwind			*
 ***********************************************************************/
{
int	icolr, npts, itype;
/*---------------------------------------------------------------------*/

    *iret = 0;

    npts = 1;
    icolr = el.hdr.maj_col;
    itype = (int)el.hdr.vg_type;
    if ( ggfpt != (FILE *)NULL )  {
        fprintf(ggfpt, "%d %d %d\n%7.2f %7.2f %7.2f %7.2f\n", 
		npts, itype, icolr,
		el.elem.wnd.data.latlon[0], 
		el.elem.wnd.data.latlon[1], 
		el.elem.wnd.data.spddir[1],
		el.elem.wnd.data.spddir[0]);
    }
}
/*=====================================================================*/

void gg_rdvgf  ( char *vname, char *iname, char *catmap, int *iret  )
/************************************************************************
 * gg_rdvgf                                                             *
 *                                                                      *
 * This function takes a VGF file name and opens an info file.  If vname*
 * is not specified, function returns with iret = 0. If vname is 	*
 * specified but does not exist functions returns witn an error code.	*
 * If iname is not specified, it's created by striping the VGF filename	*
 * off its extension and appending ".info" extension to the filename. 	*
 * Before calling function GG_UPDATE parameter plt_ext is set to FALSE	*
 * to indicate not to plot line extensions.  Also cur_layer is set to 0.*
 *                                                                      *
 * gg_rdvgf  ( vname, iname, catmap, iret )                             *
 *                                                                      *
 * Input parameters:                                                    *
 *  *vname      char            VGF file name                           *
 *  *iname      char            info file name                          *
 *  *catmap     char            CATMAP parameter string		        *
 *                                                                      *
 * Output parameters:                                                   *
 *  *iname      char            info file name                          *
 *  *iret       int             Return code                             *
 *                              0 = normal		                *
 *                             -1 = VGF does not exist	                *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * m.gamazaychikov/SAIC 12/05   Created                                 *
 ***********************************************************************/
{
    int         ier, ipos, plt_ext, cur_layer, ierm;
    char        newfil[133];
    long        size;
/*---------------------------------------------------------------------*/
                                                                                               
    *iret = 0;
    plt_ext = G_FALSE;
    cur_layer = 0;
    ierm = -13;
                                                                                               
    if ( vname[0] == '\0' ) {
        return;
    }

    cfl_inqr ( vname, NULL, &size, newfil, &ier );
 
    if ( ier != 0 )  {
        er_wmsg ( "GRPHGD", &ierm, " ", &ier, strlen("GRPHGD"), strlen(" ") );
        *iret = -1;
        return;
    }

    if ( iname[0] == '\0' ) {
        cst_nocc ( vname, '.', 1, 0, &ipos, &ier );
        if  ( ier == 0 )  {
             strncpy ( iname, vname, ipos+1 );
             iname[ipos+1] = '\0';
             strcat ( iname, "info");
             iname[ipos+5] = '\0';
        }
    }

    gg_update ( vname, iname, &cur_layer, catmap, &plt_ext, &ier);

}
/*=====================================================================*/
