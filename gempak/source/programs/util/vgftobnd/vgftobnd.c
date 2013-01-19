#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"

#define	RANGE	( 150.0F )

/*=====================================================================*/

int main ( int argc, char **argv )

/************************************************************************
 *                                                                      *
 * This module converts a vgf file to a bounds file.			*
 *                                                                      *
 * Note: This program depends on:					*
 *                                                                      *
 * 1) It and all libraries have been compiled with the pgprm.h 	 	*
 *    parameter MAXPTS set to LLMXPT if more points than the current	*
 *    value of MAXPTS is expected.					*
 * 2) All lines must be grouped as LABEL with text indicating the 	*
 *    group name.							*
 * 3) One other text string must also be grouped with optional info in	*
 *    the <tag>value format: 						*
 *    <PARM1>VALUE1<PARM2>VALUE1...<PARMn>VALUEn			*
 *    At the least, "<NAME>group_name" from 2) will be used.		*
 * 4) There may be multiple groups w/ unique names and info.		*
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
 * D.W.Plummer/NCEP	 4/01						*
 * D.W.Plummer/NCEP	08/03	Bug fix for longitude range calculation	*
 ***********************************************************************/

{
int    	ii, flag, nc, ng, npts, ier;
int    	grpnum, wrtflg, nptot, np;
int	vlines[MAX_EDITABLE_ELEMS], nvl, nvlines;
int	group_cnt[MAX_EDITABLE_ELEMS];
char	grptyp, vg_class, vg_type;
char	str[128], meta[128], name[128], *cptr;
float	lat, lon, cenlat, cenlon;
float	latll, lonll, latur, lonur;
char	fname[LLMXLN], onfname[LLMXLN];
long	filesize;
int	igrp, more, curpos;
float	*latlon;

VG_DBStruct     el;

FILE    *fptr;
/*---------------------------------------------------------------------*/

    /*
     *  First check if number of input arguments is correct.
     */
    if ( argc < 2 )  {
	printf("Usage: vgftobnd vgf_filename\n");
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
     *  Initialize the group structure.
     */
    ces_gtrtbl ( &ier );
    ces_gtgid ( "LABEL", &igrp, &ier );
    for ( ii = 0; ii < MAX_EDITABLE_ELEMS; ii++ )  group_cnt[ii] = 0;

    /*
     *  Scan the file and determine the groups.
     */
    curpos = 0;
    more = G_TRUE;
    while ( ( more ) && ( curpos < filesize ) )  {

	cvg_rdhdr ( fname, fptr, curpos, (int)filesize, &el, &flag, &ier );

	if ( ier < 0 )  {
	    more = G_FALSE;
	}
	else if ( ( el.hdr.recsz > 0 ) ) {

	    vg_class = el.hdr.vg_class;
	    vg_type  = el.hdr.vg_type;
	    grptyp   = el.hdr.grptyp;
	    grpnum   = el.hdr.grpnum;

	    if ( (int)grptyp == igrp && !el.hdr.delete )  {

		group_cnt[grpnum]++;

	    }

	}

	curpos += el.hdr.recsz;

    }

    /*
     *  Loop through all the groups.
     *  Process only the group numbers that have elements.
     */
    for ( ng = 0; ng < MAX_EDITABLE_ELEMS; ng++ )  {

     if ( group_cnt[ng] != 0 )  {

      /*
       *  Scan the file again looking for a particular group number, 
       *  find string and count the number of valid lines.
       */
      nvlines = 0;
      nptot = 0;
      lat = 0.0;
      lon = 0.0;
      str[0] = '\0';
      name[0] = '\0';
      meta[0] = '\0';
      curpos = 0;
      more = G_TRUE;
      while ( ( more ) && ( curpos < filesize ) )  {

	cvg_rdhdr ( fname, fptr, curpos, (int)filesize, &el, &flag, &ier );

	if ( ier < 0 )  {
	    more = G_FALSE;
	}
	else if ( ( el.hdr.recsz > 0 ) && !el.hdr.delete &&
		  ( (int)el.hdr.grptyp == igrp ) && 
		  ( (int)el.hdr.grpnum == ng ) ) {

	    vg_class = el.hdr.vg_class;
	    vg_type  = el.hdr.vg_type;

	    if ( (int)vg_class == CLASS_TEXT )  {

	        /*
	         *  Found string identifier.
	         */
	        cvg_rdrec( fname, curpos, &el, &ier );

	        if ((int) vg_type == SPTX_ELM) {
                    strcpy (str, el.elem.spt.text);
                }
                else {
                    strcpy (str, el.elem.txt.text);
                }

		/*
		 *  Remove any leading spaces.
		 */
		cst_ldsp ( str, str, &nc, &ier );

		if ( strchr ( str, '<' ) != (char *)NULL &&
		     strchr ( str, '|' ) == (char *)NULL )  {

		    /*
		     *  Save as meta information string.
		     */
		    strcpy ( meta, str );

		}
		else  {

		    /*
		     *  Save as name information string.
		     */
		    cptr = cst_split ( str, '|', 
			sizeof(name)/sizeof(name[0]), name, &ier );
		    if ( cptr != (char *)NULL )  strcpy ( meta, cptr );

		    /*
		     *  Replace any blanks with an underscore.
		     */
		    while ( strchr ( name, ' ' ) != (char *)NULL )  
			cst_rpst ( name, " ", "_", name, &ier );

		}

	    }
	    else if ( (int)vg_class == CLASS_LINES )  {

	        /*
	         *  Found line; check if it is closed.
	         */
	        cvg_rdrec( fname, curpos, &el, &ier );
	        if ( el.hdr.closed )  {
		    vlines[nvlines] = curpos;
		    nvlines++;
		    npts = el.elem.lin.info.numpts;
		    for ( np = 0; np < npts; np++ )  {
			lat += el.elem.lin.latlon[np];
			lon += el.elem.lin.latlon[np+npts];
			nptot++;
		    }
		}

	    }

	}

	curpos += el.hdr.recsz;

    }

    cenlat = lat / nptot;
    cenlon = lon / nptot;

    printf("B00000 %-32s%6d%7d%4d\n", 
	name, (int)(cenlat*100), (int)(cenlon*100), nvlines );

    if ( strlen(meta) == 0 )  sprintf( meta, "NAME=%s", name );
    printf("%s\n", meta );

    latlon = el.elem.lin.latlon;
    for ( nvl = 0; nvl < nvlines; nvl++ )  {

        cvg_rdrec( fname, vlines[nvl], &el, &ier );

	latll =  90.0;
	latur = -90.0;
	lonll =  180.0;
	lonur = -180.0;
	npts = el.elem.lin.info.numpts;
	for ( np = 0; np < npts; np++ )  {
	    latll = G_MIN( latll, latlon[np] );
	    lonll = G_MIN( lonll, latlon[np+npts] );
	    latur = G_MAX( latur, latlon[np] );
	    lonur = G_MAX( lonur, latlon[np+npts] );
	    if ( (latlon[np+npts]>+RANGE && latlon[(np+1)%npts+npts]<-RANGE)  ||
		 (latlon[np+npts]<-RANGE && latlon[(np+1)%npts+npts]>+RANGE) )  {
		lonll = -180.0F;
		lonur = +180.0F;
	    }
	}

	printf("%4d              %9.3f%9.3f%9.3f%9.3f%9.3f%9.3f\n",
	    npts*2, latll, latur, lonll, lonur,
	    el.elem.lin.latlon[0], el.elem.lin.latlon[npts] );

	for ( np = 1; np < npts; np++ )  {
	    printf ( "%9.3f%9.3f", 
		el.elem.lin.latlon[np], el.elem.lin.latlon[np+npts] );
	    if ( np % 4 == 0 )  printf("\n");
	}

	printf("\n");

      }

     }

    }

    return (0);

}
