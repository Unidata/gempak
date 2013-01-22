#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"

int main ( int argc, char **argv )
/************************************************************************
 *									*
 * This program creates the CCF product message.			*
 *									*
 * Usage:								*
 * createccfp YYMMDD/HHHH file1 file2 file3 				*
 *									*
 * CCF VGF forecast filenames are assumed to be in order of forecast	*
 * hours 4, 6 and 8.							*
 *									*
 * The output ASCII information is sent to standard output.		*
 *									*
 * cycle_time is in standard GEMPAK time format YYYYMMDD/HHHH.		*
 *									*
 * main(argc, argv)                                                     *
 *                                                                      *
 * Input parameters:                                                    *
 *  argc   int      number of parameters of command line                *
 *  argv   char**   parameter array of command line                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE						*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 6/02						*
 * R.Tian/SAIC		11/02		Fixed time problem		*
 * R.Tian/SAIC		03/03		Fixed size problem		*
 * T. Piper/SAIC	02/04	Removed unused variable maxnt		*
 * L. Hinson/AWC        07/09   Add support for High/Med coverage lines *
 *                              Write coverage value of 1 or 2 on Lines *
 *                              Drop extra read for separate SPTX elem. *
 * L. Hinson/AWC        11/12   Revised for 4, 6, 8 hour forecast       *
 ***********************************************************************/
{
int         ii, jj, nt, np, more, pagflg, flag, ier;
int         na, nl, fpa[50], fpl[50], wrtflg, curpos;
char        cycle_time[20];
long	    filesize;
char        errgrp[12], fname[LLMXLN], onfname[LLMXLN];
int	    mins, idtarr[5];
VG_DBStruct el;
FILE	    *fptr;
/*---------------------------------------------------------------------*/

  /*
   *  First check if number of input arguments is correct.
   */
  if ( argc < 5 )  {
    pagflg = G_FALSE;
    strcpy ( errgrp, "CREATECCFP" );
    ip_help ( errgrp, &pagflg, &ier, strlen(errgrp) );
    exit (0);
  }

  strcpy ( cycle_time, argv[1] );

  for ( nt = 2; nt < 5; nt++ )  {

    strcpy ( fname, argv[nt] );

    wrtflg = 0;
    cvg_open ( fname, wrtflg, &(fptr), &ier );
    if ( ier != 0 )  {
        printf("Error opening VGF file %s\n", fname );
        exit (0);
    }
    cfl_inqr ( fname, NULL, &filesize, onfname, &ier );

    /*
     *  Display preliminary information
     */
    ti_ctoi ( cycle_time, idtarr, &ier, strlen(cycle_time) );
    printf ( "CCFP %4d%02d%02d_%02d00 ", 
       	     idtarr[0], idtarr[1], idtarr[2], idtarr[3] );
    mins = (nt-1)*120 + 120;
    ti_addm ( idtarr, &mins, idtarr, &ier );
    printf ( "%4d%02d%02d_%02d00",
	     idtarr[0], idtarr[1], idtarr[2], idtarr[3] );

    printf ( "\n");

    /*
     *  Determine number of areas and lines; save their file locations
     */
    na = 0;
    nl = 0;
    curpos = 0;
    more = G_TRUE;
    while ( ( more ) && ( curpos < filesize ) )  {

        cvg_rdhdr ( fname, fptr, curpos, (int)filesize, &el, &flag, &ier );

	if ( ier < 0 )  {
            more = G_FALSE;
        }
        else if ((int)el.hdr.vg_class == CLASS_SIGMETS && 
		 (int)el.hdr.vg_type  == SIGCCF_ELM)  {

            cvg_rdrec ( fname, curpos, &el, &ier );
            if ( el.elem.ccf.info.subtype == SIGTYP_AREA )  {
                fpa[na] = curpos;
                na++;
            }
            if ( el.elem.ccf.info.subtype == SIGTYP_LINE_HIGH ||
                 el.elem.ccf.info.subtype == SIGTYP_LINE_MED ) {
                fpl[nl] = curpos;
                nl++;
            }
        }
 	curpos += el.hdr.recsz;
    }

    /*
     *  Process areas
     */
    for (ii = 0; ii < na; ii++) {

        cvg_rdrec ( fname, fpa[ii], &el, &ier );

	np = el.elem.ccf.info.npts;
        printf ( "AREA %d %d %d %d %d %d %d ", 
		 el.elem.ccf.info.cover, el.elem.ccf.info.prob, 
		 el.elem.ccf.info.growth, el.elem.ccf.info.tops,
		 G_NINT(el.elem.ccf.info.spd), G_NINT(el.elem.ccf.info.dir),
		 (np + 1));

        for (jj = 0; jj < np; jj++) {
            printf ( "%d %d ",
		      (int) (el.elem.ccf.latlon[jj] * 10),
		      -(int) (el.elem.ccf.latlon[np + jj] * 10));
        }

        /*
         *  for areas, repeat last point
         */
        printf ( "%d %d ",
		 (int) (el.elem.ccf.latlon[0] * 10),
		 -(int) (el.elem.ccf.latlon[np] * 10) );
        
        /* Print the lat/lon of the Text Box */
        printf ( "%d %d \n",
                 (int) (el.elem.ccf.info.textlat * 10), 
                 -(int) (el.elem.ccf.info.textlon * 10));
    }

    /*
     *  Process lines
     */
    for ( ii = 0; ii < nl; ii++ )  {

        cvg_rdrec ( fname, fpl[ii], &el, &ier );

        printf ( "LINE %d ", el.elem.ccf.info.subtype );

        np = el.elem.ccf.info.npts;
        printf ( "%d ", np );

        for ( jj = 0; jj < np; jj++ )  {
            printf ( "%d %d ",
                     (int)(el.elem.ccf.latlon[   jj]*10),
                     -(int)(el.elem.ccf.latlon[np+jj]*10) );
        }

	printf ( "\n");

    }

  }

  return (0);

}

/*=====================================================================*/
