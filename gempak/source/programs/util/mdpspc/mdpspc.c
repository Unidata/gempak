#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"
#include "pgprm.h"


#define MAXSTN          1000

/************************************************************************
 * mdp.c								*
 *									*
 * CONTENTS:								*
 * mdp									*
 ***********************************************************************/

int main ( int argc, char **argv )
/************************************************************************
 * mdp									*
 *                                                                      *
 * This program generates the mesoscale discussion latlon pairings.	*
 * Pairings will be written to a file whose filename is based on the	*
 * input filename (filename extension, if exists, is replaced w/ "mdp").*
 *                                                                      *
 * Example:								*
 * mdp input_vgf_file.vgf						*
 * Produces latlon pairings in the ASCII file				*
 * output_vgf_file.mdp							*
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
 * D.W.Plummer/NCEP	 6/02						*
 * D.W.Plummer/NCEP	 8/02	Bug fix for lons > 99.995 and < 100.0	*
 * G. Grosshans/SPC	11/02	Updated to decode multi-scalloped lines *
 * G. Grosshans/SPC	12/02	Updated to compute WFO/State		*
 * S. Jacobs/NCEP	 5/03	Clean up unused variables and headers	*
 * G. Grosshans/SPC     10/03   Updated for precision                   *
 * T. Piper/SAIC	12/05	Updated cst_wrap for CSC		*
 * J. Wu/SAIC		04/06	Added parameter in cst_wrap 		*
 * G. Grosshans/SPC	08/08	Updated for SCN 08-45 to add LAT...LON  *
 * 				and list first point as last point      *
 * 				to indicate a closed polygon.  Add      *
 *				word wrapping for ATTN...WFO...		*
 ***********************************************************************/
{
int		ii, ix, iy, ixfirst, iyfirst, pagflg, ne, ilen,
	        npts, ier, iret, nitems, more, curpos;

long		ifilesize;

float		x, y, flat[MAXLISTITEMS], flon[MAXLISTITEMS];

char		vg_class, vg_type, buffer[2048], bufferfinal[2048],
		bufferfinalwfo[2048],str[20], *cptr, errgrp[12], infile[128],
		ifname[128], outfile[128], info[2048], stpo[4],
		cstl_list[1000], cstl_liststate[1000],
		newLineStr[13]="            "; /* 13 spaces */

char		blank[2]={' '}, device[13], dfilnam[73], pro[80];
int		mode, istat, iunit, itype;
float		xsize, ysize, lllat, lllon, urlat, urlon,
		prjang1, prjang2, prjang3;

VG_DBStruct	el;

FILE		*ifptr, *ofptr;

const int line_len = 66;

/*---------------------------------------------------------------------*/

    /* 
     * Set defaults for gsdeva and gsmprj
     */
    mode = 1;
    strcpy ( device, "GN" );
    iunit = 1;
    strcpy ( dfilnam, "MDPSPC" );
    itype = 1;
    xsize = 500.0F;
    ysize = 500.0F;
    lllat = 10.0F;
    lllon = -120.0F;
    urlat = 50.0F;
    urlon = -50.0F;
    strcpy ( pro, "str" );
    prjang1 =   90.0F;
    prjang2 = -105.0F;
    prjang3 =    0.0F;
    cstl_list[0] = '\0';
    cstl_liststate[0] = '\0';

    in_bdta ( &ier );
    ginitp ( &mode, &istat, &ier);
    gsdeva (device, &iunit, dfilnam, &itype, &xsize, &ysize, &iret,
	    strlen(device), strlen(dfilnam));
    gsmprj ( pro, &prjang1, &prjang2, &prjang3, 
	     &lllat, &lllon, &urlat, &urlon, &iret, strlen(pro));
    clo_init ( &ier );

    /*
     *  Check if number of input arguments is correct.
     */
    if ( argc < 2 )  {
	pagflg = G_FALSE;
	strcpy ( errgrp, "MDPSPC" );
        ip_help ( errgrp, &pagflg, &ier,
                  strlen(errgrp) );
	gendp (&mode, &ier);
	exit (0);
    }

    /*
     *  First input on command line is input vgf file name.
     */
    strcpy ( infile, argv[1] );
    cfl_inqr ( infile, NULL, &ifilesize, ifname, &ier );
    ifptr = (FILE *) cfl_ropn(ifname, "", &ier);
    if ( ier != 0 )  {
	printf("Error opening VGF file %s\n", infile );
	gendp (&mode, &ier);
	exit (0);
    }

    /*
     *  Output filename is input filename w/ "mdp" filename extension.
     */
    strcpy ( outfile, infile );
    cptr = strrchr( outfile, '.' );
    if ( cptr != (char *)NULL )  {
	cptr[0] = '\0';
    }
    strcat( outfile, ".mdp" );
    
    /*
     *  Loop through all the elements until a line is found.
     */
    ne = 0;
    more = G_TRUE;
    curpos = 0;
    buffer[0] = '\0';
    bufferfinal[0] = '\0';
    bufferfinalwfo[0] = '\0';
    strcat ( buffer, "LAT...LON   " );
    while ( ne < MAX_EDITABLE_ELEMS && more == G_TRUE )  {
	cvg_rdrecnoc( ifname, ifptr, curpos, &el, &ier );

	if ( ier < 0 )  {
	    more = G_FALSE;
	}
	else  {

	    curpos += el.hdr.recsz;

	    vg_class = el.hdr.vg_class;
	    vg_type  = el.hdr.vg_type;

            if ( ( (int)vg_class == CLASS_LINES ) &&
	    	 ( el.hdr.vg_type == SPLN_ELM ) &&
		 ( (int)el.elem.spl.info.spltyp == 3 ) )  {

		/*
		 *  Open output file.
		 */
		ofptr = (FILE *)cfl_wopn ( outfile, &ier );
		if ( ier != 0 )  {
		    printf("Error opening/creating output file %s\n",
		    	    outfile );
	            gendp (&mode, &ier);
		    exit (0);
    		}

	       /*
		* Find FIPS bounded by the closed line
		*/

		npts = el.elem.spl.info.numpts;

		/*  FIND WHAT STATES ARE IN MD AREA */

		clo_binpoly ( "CNTY_BNDS", npts, el.elem.spl.latlon,
                              &(el.elem.spl.latlon[npts]), &ier );
		clo_tgltln ( "CNTY_BNDS", MAXLISTITEMS, &nitems,
			      flat, flon, &ier);

		for ( ii = 0; ii < nitems; ii++ )  {
		  clo_bginfo( "CNTY_BNDS", ii, info, &ier );
		  cst_gtag( "STATE", info, "?", stpo, &ier);
		  
		  if (strstr(cstl_liststate, stpo)==NULL) {
		    strcat(cstl_liststate, stpo);
		    strcat(cstl_liststate, " ");		  
		  }
		}


		/*  FIND WHAT WFOs ARE IN MD AREA */
		clo_binpoly ( "CWA_BNDS", npts, el.elem.spl.latlon,
                              &(el.elem.spl.latlon[el.elem.spl.info.numpts]),
			      &ier );
		clo_tgltln ( "CWA_BNDS", MAXLISTITEMS, &nitems,
			      flat, flon, &ier);

		for ( ii = 0; ii < nitems; ii++ )  {
		  clo_bginfo( "CWA_BNDS", ii, info, &ier );
		  cst_gtag( "WFO", info, "?", stpo, &ier);
		  strcat(cstl_list, stpo);
		  strcat(cstl_list, "...");		  
		}

		/*
		 *  Format lats and lons into buffer.
		 */
		if ( (int)vg_type == LINE_ELM )  {
		    npts = el.elem.lin.info.numpts;
		}
		else if ( (int)vg_type == SPLN_ELM )  {
		    npts = el.elem.spl.info.numpts;
		}
		for ( ii = 0; ii < npts; ii++ )  {
		    if ( (int)vg_type == LINE_ELM )  {
		        x = el.elem.lin.latlon[ii];
		        y = -el.elem.lin.latlon[ii+npts];
		    }
		    else if ( (int)vg_type == SPLN_ELM )  {
		        x = el.elem.spl.latlon[ii];
		        y = -el.elem.spl.latlon[ii+npts];
		    }
		    /*
		     * Make sure lats and lons are rounded
		     * to 100ths of deg.
		     */
		    x = ((int)(x*100.0F)) / 100.0F;
		    y = ((int)(y*100.0F)) / 100.0F;
		    if ( y >= 100.0F )  y -= 100.0F;
		    ix = G_NINT(x*100.0F);
		    iy = G_NINT(y*100.0F);
		    sprintf( str, "%04d%04d ", ix, iy );
		    strcat ( buffer, str );
		    if ( ii == 0 ) {
                        ixfirst = ix;
			iyfirst = iy;
                    }
		}
		/*
		 *   Repeat first lat/lon point as last lat/lon point 
		 *   to indicate a closed polygon.
		 */


		sprintf( str, "%04d%04d ", ixfirst, iyfirst);
		strcat ( buffer, str );

		sprintf( str, "\n\n" );
		strcat ( buffer, str );

		/*
		 *  Wrap buffer such that only 6 pairs of lat,lons 
		 *  on one line.
		 *  ilen = 55;
		 *  cst_wrap( buffer, blank, &ilen, "\n", (char *)NULL, buffer, &ier );
		 */
		ilen = 66;
		cst_wrap( buffer, blank, &ilen, "\n", newLineStr, buffer, &ier );

	    }

	}
	ne++;
    }

    /*
     * wrap the ATTN...WFO... line
     */
    strcat ( bufferfinalwfo, "ATTN...WFO..." );
    strcat ( bufferfinalwfo, cstl_list );
    cst_wrap( bufferfinalwfo, "...", &line_len, "\n", (char *)NULL, bufferfinalwfo, &ier );

    /*
     * build the output file string and add the ATTN...WFO... string
     */
    strcat ( bufferfinal,"STATES=" );
    strcat ( bufferfinal, cstl_liststate );
    strcat ( bufferfinal, "\n\n" );
    strcat ( bufferfinal, bufferfinalwfo );
/*
    strcat ( bufferfinal,"ATTN...WFO..." );
    strcat ( bufferfinal, cstl_list );
*/
    strcat ( bufferfinal, "\n\n" );
    strcat ( bufferfinal, buffer );

    /*
     *  Write to output file.
     */
    cfl_writ ( ofptr, (int)strlen(bufferfinal),
    	       (unsigned char *)bufferfinal, &ier );

    /*
     *  close files and exit.
     */
    cfl_clos ( ifptr, &ier );
    cfl_clos ( ofptr, &ier );
    gendp (&mode, &ier);
    return(0);

}
