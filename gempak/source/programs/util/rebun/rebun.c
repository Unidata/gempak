#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"

#define	OUTVGF	"rebun.vgf"

/************************************************************************
 * rebund.c								*
 *									*
 * CONTENTS:								*
 * main   								*
 ***********************************************************************/
 
int main ( int argc, char **argv )
/************************************************************************
 * rebundle								*
 *                                                                      *
 * This program accepts as input a VGF file containing any number of	*
 * watch elements as well as an expansion flag. For each watch element	*
 * in the input VGF file, the function 'rebundle' is called.		*
 * Computed polygons are saved to the VGF file 'rebun.vgf'.		*
 *                                                                      *
 * Usage:								*
 * rebundle input.vgf expand						*
 * input.vgf - VGF file containing one or more watches			*
 * expand    - expansion flag ( 0 - FALSE, 1 - TRUE )			*
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
 * D.W.Plummer/NCEP	11/02						*
 * D.W.Plummer/NCEP	02/03	Prologue correction			*
 * S. Chiswell/UCAR	11/03	Initialized debug			*
 * m.gamazaychikov/SAIC	08/04	Extracted function rebundle into 	*
 *				a separate function			*
 * A. Hardy/NCEP	 4/05	Changed ints to pointers in cvg_rebun,  *
 *                              added check for expand argument		*
 * S. Danz/AWC		07/06	Switch to new cvg_writefD() function    *
 ***********************************************************************/
{
int    	ii, ne, wrtflg, curpos, more, loc, expand, debug=0, ier;
char	vgfile[128], vgfname[128], bndnam[25];
char	vg_type;
long	ifilesize;

int	pagflg, mode, istat, itype, iunit;
char	errgrp[8], device[8], dfilnam[128], pro[32];
float	xsize, ysize, lllat, lllon, urlat, urlon;
float   prjang1, prjang2, prjang3;

int	npout;
float	latout[16], lonout[16];

VG_DBStruct     el_wbx, el_lin;

FILE    *ifptr;
/*---------------------------------------------------------------------*/

    /*
     *  First check if number of input arguments is correct.
     */
    if ( argc < 2 || argc > 3 )  {
	pagflg = G_FALSE;
	strcpy ( errgrp, "REBUN" );
        ip_help ( errgrp, &pagflg, &ier,
                  strlen(errgrp) );
	exit (0);
    }

    /*
     *  Set up GAREA and PROJ.
     */

    mode = 1;
    ginitp ( &mode, &istat, &ier );

    strcpy ( device, "GN" );

    iunit = 1;
    strcpy ( dfilnam, "REBUNDLE" );
    itype = 1;
    xsize = 500.0F;
    ysize = 500.0F;

    gsdeva ( device, &iunit, dfilnam, &itype, &xsize, &ysize, &ier,
             strlen(device), strlen(dfilnam));

    lllat = 0.0F;
    lllon = -150.0F;
    urlat = 80.0F;
    urlon = 40.0F;
    strcpy ( pro, "str" );
    prjang1 = 90.0F;  prjang2 = -105.0F;  prjang3 = 0.0F;
    gsmprj ( pro, &prjang1, &prjang2, &prjang3,
             &lllat, &lllon, &urlat, &urlon, &ier, strlen(pro));

    /*
     *  First input on command line is VGF file containing a watch.
     */
    strcpy ( vgfile, argv[1] );
    wrtflg = 0;
    cvg_open ( vgfile, wrtflg, &(ifptr), &ier );
    if ( ier != 0 )  {
        printf("Error opening VGF file %s\n", vgfile );
        exit (0);
    }
    cfl_inqr ( vgfile, NULL, &ifilesize, vgfname, &ier );

    /*
     *  Second input on command line is expand option.
     */
    
    if ( argv[2] == NULL ) {
        expand = 1;
    }
    else { 
        cst_numb ( argv[2], &expand, &ier );
    }

    if ( ( expand < 0 ) || ( expand > 1 ) ) {
        expand = 1;
    }
    cvg_crvgf ( OUTVGF, &ier );
    /*
    debug = 3;
    */

    ne = 0;
    more = G_TRUE;
    curpos = 0;
    while ( ne < MAX_EDITABLE_ELEMS && more == G_TRUE )  {

        cvg_rdrecnoc ( vgfname, ifptr, curpos, &el_wbx, &ier );

	if ( ier < 0 )  {
	    more = G_FALSE;
	}
	else if ( el_wbx.hdr.recsz > 0 )  {

	    curpos += el_wbx.hdr.recsz;

            vg_type  = el_wbx.hdr.vg_type;

            if ( (int)vg_type == WBOX_ELM )  {

	        strcpy ( bndnam, "CNTY_BNDS" );

                cvg_rebun ( &el_wbx.elem.wbx.info.numcnty, 
		    el_wbx.elem.wbx.info.cn_fips, &expand, &debug,
		    bndnam, &npout, latout, lonout, &ier );

		el_lin.hdr.vg_type = LINE_ELM;
		el_lin.hdr.vg_class = CLASS_LINES;
		cvg_initelm ( &el_lin );
		el_lin.hdr.filled = (char)0;
		el_lin.hdr.grptyp = (char)0;
		el_lin.hdr.grpnum = 0;
		el_lin.hdr.maj_col = 6;
		el_lin.hdr.min_col = 6;
		el_lin.elem.lin.info.lintyp = 1;
		el_lin.elem.lin.info.width = 3;
		el_lin.elem.lin.info.numpts = npout;
                for ( ii = 0; ii < npout; ii++ )  {
/*
	            printf("%2d - %6.2f, %6.2f \n", ii, latout[ii], lonout[ii] );
*/
		    el_lin.elem.lin.latlon[ii      ] = latout[ii];
		    el_lin.elem.lin.latlon[ii+npout] = lonout[ii];
                }

		el_lin.hdr.recsz = sizeof(VG_HdrStruct) + sizeof(LineInfo) + npout*2*sizeof(float);
		cvg_writefD( &el_lin, -1, el_lin.hdr.recsz, OUTVGF, &loc, &ier );

	    }

	}

	ne++;

    }

    return ( 0 );

}
