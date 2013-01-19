#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"

#define		POSSIBLE		0
#define		POSSIBLE_COLOR		6
#define		POSSIBLE_FILL		0

#define		LIKELY			1
#define		LIKELY_COLOR		5
#define		LIKELY_FILL		4

#define		OCCURRING		2
#define		OCCURRING_COLOR		2
#define		OCCURRING_FILL		2

/*=====================================================================*/

int main ( int argc, char **argv )
/************************************************************************
 * main                                                           	*
 *                                                                      *
 * Flood Outlook Product (FOP) VGF to ASCII translator.			*
 *                                                                      *
 * This programs translates a VGF file containing information		*
 * about flood locations and times into an ASCII file.			*
 *                                                                      *
 * The format of the ASCII file is:					*
 * numpts flood_category valid_dates					*
 * lon1, lat1 								*
 * lon2, lat2 								*
 * lon3, lat3 								*
 * .... ....								*
 * lonN, latN								*
 *                                                                      *
 * Notes:								*
 * - Valid flood_categories: 0 (POSSIBLE), 1 (LIKELY), 2 (OCCURRING).	*
 * - Flood categories are identified via the fill type, specifically:	*
 *   solid fill (element filled value 2) - category 2			*
 *   hatch fill (element filled value 4) - category 1			*
 *   no    fill (element filled value 0) - category 0			*
 * - The output ASCII information is sent to standard output.		*
 *                                                                      *
 * main(argc, argv)                                                	*
 *                                                                      *
 * Input parameters:                                                    *
 *  argc   int      number of parameters of command line                *
 *  argv   char**   parameter array of command line       		*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP	 1/02						*
 * D.W.Plummer/NCEP	11/02	Added chk for 'V'/'v' 'No flooding...'	*
 * T. Piper/SAIC	02/04	Removed unused variable vg_type		*
 * B. Yin/SAIC		03/04	Changed css_gtim calling sequences	*
 ***********************************************************************/
{
int	ii, jj, ne, more, curpos, flag, len, npts, icat, ier;
int	wrtflg, members[256], numing, pagflg, itype = 1;
long	filesize;
char    vg_class;
char	vgffile[256], onfname[256];
char	str[80], tstr[80], dattim[20], errgrp[12];
float	flat, flon, *floatptr;
FILE	*fptr;
VG_DBStruct el_text, el_line;

/*---------------------------------------------------------------------*/

    /*
     *  Check for proper number of arguments.
     */
    if ( argc < 2 )  {
        pagflg = G_FALSE;
        strcpy ( errgrp, "RFOP" );
        ip_help ( errgrp, &pagflg, &ier, strlen(errgrp) );
        exit (0);
    }

    /*
     *  First argument is input VGF file name.
     */
    strcpy ( vgffile, argv[1] );

    /*
     *  Open input VGF file.
     */
    wrtflg = 0;
    cvg_open ( vgffile, wrtflg, &(fptr), &ier );

    if ( ier != 0 || fptr == (FILE *)NULL )  {
	printf("Error opening file %s\n", vgffile );
	exit (0);
    }

    cfl_inqr ( vgffile, NULL, &filesize, onfname, &ier );

    /*
     *  Skip the file header and loop thru all elements.
     */

    css_gtim ( &itype, dattim, &ier );
    printf("NWS NATIONAL FLOOD OUTLOOK PRODUCT %s\n", dattim );

    ne = 0;
    more = G_TRUE;
    curpos = 0;
    while ( ne < MAX_EDITABLE_ELEMS && more == G_TRUE )  {

        cvg_rdhdr(vgffile, fptr, curpos, (int)filesize, &el_text, &flag, &ier);

	if ( ier < 0 )  {
	    more = G_FALSE;
	}
	else if ( ( el_text.hdr.recsz > 0 ) && !el_text.hdr.delete )  {

            vg_class = el_text.hdr.vg_class;

	    if ( (int)vg_class == CLASS_TEXT && el_text.hdr.grptyp != 0 )  {

	    	/*
	     	 *  Read text element; save string and lat,lon.
	     	 */
		cvg_rdrec( vgffile, curpos, &el_text, &ier );
		if ( (int)el_text.hdr.vg_type == TEXT_ELM )  {
		    strcpy ( str, el_text.elem.txt.text );
		    flat = el_text.elem.txt.info.lat;
		    flon = el_text.elem.txt.info.lon;
		}
		else if ( (int)el_text.hdr.vg_type == SPTX_ELM )   {
		    strcpy ( str, el_text.elem.spt.text );
		    flat = el_text.elem.spt.info.lat;
		    flon = el_text.elem.spt.info.lon;
		}

		strcpy ( tstr, str );
		cst_ldsp ( tstr, tstr, &len, &ier );
		/*
		 *  Check if string is "No flooding expected" or
		 *  "Not ..."  but keep string starting w/ Nov...!
		 *  (November)
		 */
		if ( ( tstr[0] == 'N' || tstr[0] == 'n' )  &&
		     ( tstr[1] == 'O' || tstr[1] == 'o' )  &&
		     ( tstr[2] != 'V' && tstr[2] != 'v' ) )  {
		    printf ( "%s %d %d\n", str, G_NINT(flat), G_NINT(flon) );
		    exit (0);
		}

		cvq_scangrp ( vgffile, el_text.hdr.grptyp, el_text.hdr.grpnum,
                                sizeof(members)/sizeof(int),
                                members, &numing, &ier );
                for ( ii = 0; ii < numing; ii++ )  {
                        cvg_rdhdr ( vgffile, fptr, members[ii], (int)filesize,
                                &el_line, &flag, &ier );
                  if ( ( el_line.hdr.recsz > 0 ) && !el_line.hdr.delete ) {
                    if ( (int)el_line.hdr.vg_class == CLASS_LINES )  {
                        cvg_rdrec( vgffile, members[ii], &el_line, &ier );
                        if ( (int)el_line.hdr.vg_type == LINE_ELM )  {
			    npts = el_line.elem.lin.info.numpts;
                            floatptr = el_line.elem.lin.latlon;
                        }
                        else if ( (int)el_line.hdr.vg_type==SPLN_ELM )  {
                            npts = el_line.elem.spl.info.numpts;
                            floatptr = el_line.elem.spl.latlon;
                        }

	    	        /*
	     	         *  Translate fill type into flood category.
	     	         */
		        switch ( el_line.hdr.filled )  {
			    case POSSIBLE_FILL	:
			        icat = POSSIBLE;
			        break;
			    case LIKELY_FILL	:
			        icat = LIKELY;
			        break;
			    case OCCURRING_FILL	:
			        icat = OCCURRING;
			        break;
			    default	:
			        icat = IMISSD;
			        break;
		        }

		        printf("%d %d %s\n", npts, icat, str );
		        for ( jj = 0; jj < npts; jj++ )  {
			    printf("%6.2f, %6.2f\n", 
				floatptr[jj+npts], floatptr[jj] );
		        }
		    }
	          }
		}
    	    }
        }
	curpos += el_text.hdr.recsz;
        ne++;
    }
    return 0;
}
