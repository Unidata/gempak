#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"

/*=====================================================================*/

int main ( int argc, char **argv )
/************************************************************************
 *                                                                      *
 * This module converts a vgf file to ascii on standard output.		*
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
 * D.W.Plummer/NCEP	 1/02						*
 * D.W.Plummer/NCEP	 3/02	Remove call to ces_gtrtbl		*
 ***********************************************************************/
{
int    	ii, flag, ne, members[256], ier;
int    	wrtflg, numing, pagflg;
char	vg_class, vg_type;
char	attr[256], grp[256], hdr[256];
char    fname[128], onfname[128];
long	filesize;
int	more, curpos;
char	errgrp[12];

VG_DBStruct     el, el_q;

FILE    *fptr;
/*---------------------------------------------------------------------*/

    /*
     *  First check if number of input arguments is correct.
     */
    if ( argc != 2 )  {
        pagflg = G_FALSE;
        strcpy ( errgrp, "VGFTOASCII" );
        ip_help ( errgrp, &pagflg, &ier, strlen(errgrp) );
	exit (0);
    }

    /*
     *  First input on command line is input vgf file name.
     */
    strcpy ( fname, argv[1] );
    wrtflg = 0;
    cvg_open ( fname, wrtflg, &(fptr), &ier );
    if ( ier != 0 )  {
	printf("Error opening VGF file %s\n", fname );
	exit (0);
    }

    cfl_inqr ( fname, NULL, &filesize, onfname, &ier );

    /*
     *  Loop through all the elements, looking for lines only.
     */
    ne = 0;
    more = G_TRUE;
    curpos = 0;
    while ( ne < MAX_EDITABLE_ELEMS && more == G_TRUE )  {

	cvg_rdhdr ( fname, fptr, curpos, (int)filesize, &el, &flag, &ier );

	if ( ier < 0 )  {
	    more = G_FALSE;
	}
	else if ( ( el.hdr.recsz > 0 ) && !el.hdr.delete )  {

	    vg_class = el.hdr.vg_class;
	    vg_type  = el.hdr.vg_type;

	    if ( (int)vg_class == CLASS_LINES )  {

	      /*
	       *  Dump element header.
	       */
	      sprintf ( hdr, 
		    "<VG_TYPE>%d<VG_CLASS>%d<FILLED>%d<CLOSED>%d<SMOOTH>%d<VERSION>%d<MAJ_COL>%d<MIN_COL>%d<GRPTYP>%d<GRPNUM>%d", 
		    el.hdr.vg_type,
		    el.hdr.vg_class,
		    el.hdr.filled,
		    el.hdr.closed,
		    el.hdr.smooth,
		    el.hdr.version,
		    el.hdr.maj_col,
		    el.hdr.min_col,
		    el.hdr.grptyp, 
		    el.hdr.grpnum );
	      printf("%s\n", hdr );

	      /*
	       *  Check for any grouped text.
	       */
	      strcpy ( grp, "<GROUPED TEXT>" );
	      if ( el.hdr.grptyp != 0 )  {
		cvq_scangrp ( fname, el.hdr.grptyp, el.hdr.grpnum, 
				sizeof(members)/sizeof(int),
				members, &numing, &ier );
		for ( ii = 0; ii < numing; ii++ )  {
			cvg_rdhdr ( fname, fptr, members[ii], (int)filesize, 
				&el_q, &flag, &ier );
		  if ( ( el_q.hdr.recsz > 0 ) && !el_q.hdr.delete ) {
		    if ( (int)el_q.hdr.vg_class == CLASS_TEXT )  {
	        	cvg_rdrec( fname, members[ii], &el_q, &ier );
			if ( (int)el_q.hdr.vg_type == TEXT_ELM )  {
			    strcat ( grp, el_q.elem.txt.text );
			}
			else if ( (int)el_q.hdr.vg_type==SPTX_ELM )  {
			    strcat ( grp, el_q.elem.spt.text );
			}
			break;
		    }
		  }
		}
	      }
	      printf("%s\n", grp );

	      /*
	       *  Dump line info and points.
	       */
	      if ( (int)vg_type == LINE_ELM )  {

	        /*
	         *  Found regular line.
	         */
	        cvg_rdrec( fname, curpos, &el, &ier );

		sprintf ( attr, 
		    "<NUMPTS>%d<LINTYP>%d<LTHW>%d<WIDTH>%d<LWHW>%d", 
		    el.elem.lin.info.numpts,
		    el.elem.lin.info.lintyp,
		    el.elem.lin.info.lthw,
		    el.elem.lin.info.width,
		    el.elem.lin.info.lwhw );
		printf("%s\n", attr );

		for ( ii = 0; ii < el.elem.lin.info.numpts; ii++ )  {
		    printf("%6.2f %6.2f\n", el.elem.lin.latlon[ii],
			el.elem.lin.latlon[ii+el.elem.lin.info.numpts] );
		}

	      }
	      else if ( (int)vg_type == SPLN_ELM )  {

	        /*
	         *  Found special line.
	         */
	        cvg_rdrec( fname, curpos, &el, &ier );

		sprintf ( attr, 
		    "<NUMPTS>%d<SPLTYP>%d<SPLSTR>%d<SPLDIR>%d<SPLSIZ>%8.2f<SPLWID>%d", 
		    el.elem.spl.info.numpts,
		    el.elem.spl.info.spltyp,
		    el.elem.spl.info.splstr,
		    el.elem.spl.info.spldir,
		    el.elem.spl.info.splsiz,
		    el.elem.spl.info.splwid );
		printf("%s\n", attr );

		for ( ii = 0; ii < el.elem.spl.info.numpts; ii++ )  {
		    printf("%6.2f %6.2f\n", el.elem.spl.latlon[ii],
			el.elem.spl.latlon[ii+el.elem.spl.info.numpts] );
		}

	      }

	    }

	    /*
	     *  Increment file pointer and get next element.
	     */
	    curpos += el.hdr.recsz;

	}

	ne++;

    }

    return 0;

}
