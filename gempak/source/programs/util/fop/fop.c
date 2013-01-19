#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "drwids.h"

#define		CHK_DIST		10000
#define		FILT			0.03
#define		REDUCE			20

#define		POSSIBLE		0
#define		POSSIBLE_COLOR		6
#define		POSSIBLE_LINTYP		2
#define		POSSIBLE_LINWID		3

#define		LIKELY			1
#define		LIKELY_COLOR		5
#define		LIKELY_LINTYP		1
#define		LIKELY_LINWID		3

#define		OCCURRING		2
#define		OCCURRING_COLOR		2
#define		OCCURRING_LINTYP	1
#define		OCCURRING_LINWID	3

/*=====================================================================*/

int main ( int argc, char **argv )
/************************************************************************
 * main                                                           	*
 *                                                                      *
 * Flood Outlook Product (FOP) ASCII to VGF translator.			*
 *                                                                      *
 * This programs translates an ASCII text file containing information	*
 * about flood locations and times into a VGF file containing lines	*
 * and text elements. Each line is grouped with a text element.		*
 *                                                                      *
 * The format of the ASCII file is assumed to be:			*
 * numpts flood_category valid_dates					*
 * lat1 lon1								*
 * lat2 lon2								*
 * lat3 lon3								*
 * .... ....								*
 * latN lonN								*
 *                                                                      *
 * Notes:								*
 * - Valid flood_categories: 0 (possible), 1 (likely), 2 (occurring).	*
 * - The output VGF filename is build from the input ASCII filename by 	*
 *   removing any extension from the ASCII filename and replacing it w/	*
 *   ".vgf". If no extension exists, then the ".vgf" extension is 	*
 *   applied. If the VGF file already exists, then the contents of the 	*
 *   ASCII file are decoded and appended onto the VGF file. 		*
 * - Input data points closer together than CHK_DIST (km) are ignored 	*
 *   since these are likely to be geographical boundaries. The result	*
 *   may be multiple lines derived from a single time. In this case,	*
 *   all lines will be grouped with the appropriate text element.	*
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
 * D.W.Plummer/NCEP	 9/01						*
 * D.W.Plummer/NCEP	10/01	Apply cv_rduc to lines w/ > REDUCE pts	*
 * S. Danz/AWC		07/06	Switch to new cvg_writefD() function    *
 ***********************************************************************/
{
int	ii, jj, icat, npts, nout, len, one=1, np, loc, nsegs, ier;
int	grpid, grpnum, lintyp, linwid;
char	asciifile[256], vgffile[256];
char	buffer[80], *cptr, data[16], date[64];
float	filt;
float	flat[LLMXPT], flon[LLMXPT], dist;
float	tlat[LLMXPT], tlon[LLMXPT];
float	rflat[LLMXPT], rflon[LLMXPT];
FILE	*fptr;
VG_DBStruct el;

/*---------------------------------------------------------------------*/

    /*
     *  Check for proper number of arguments.
     */
    if ( argc < 2 )  {
	printf("Usage: fop textfile\n");
	exit (0);
    }

    /*
     *  First argument is input ASCII file name.
     */
    strcpy ( asciifile, argv[1] );

    /*
     *  Open input ASCII file.
     */
    fptr = cfl_ropn ( asciifile, "./", &ier );

    if ( ier != 0 || fptr == (FILE *)NULL )  {
	printf("Error opening file %s\n", asciifile );
	exit (0);
    }

    /*
     *  Build output VGF file name.
     */
    strcpy ( vgffile, asciifile );
    cptr = strrchr ( vgffile, '.' );
    if ( cptr != (char *)NULL )  {
	strcpy ( (char *)(cptr+1), "vgf" );
    }
    else  {
	strcat ( vgffile, ".vgf" );
    }

    grpnum = 0;
    ces_gtrtbl ( &ier );
    ces_gtgid ( "LABEL", &grpid, &ier );

    /*
     *  Read first line from input ASCII file.
     *  This contains a header record which is not used at this time.
     */
    cfl_rdln ( fptr, sizeof(buffer), buffer, &ier );

    /*
     *  Read second line from input ASCII file.
     *  While the input ASCII file is not empty, process.
     */
    cfl_rdln ( fptr, sizeof(buffer), buffer, &ier );

    while ( ier == 0 )  {

       /*
        *  Remove leading spaces, extra blanks and trailing blanks.
        */
	cst_ldsp ( buffer, buffer, &len, &ier );
	cst_rxbl ( buffer, buffer, &len, &ier );
	if ( len > 0 )  {
	    if ( buffer[len-1] == ' ') {
		buffer[len-1] = CHNULL;
		}
	    else {
		buffer[len] = CHNULL;
	    }
	}
       /*
        *  Examine first line in file.
	*  Check if string indicates 'no' flooding.
        */
	if ( ( buffer[0] == 'N' || buffer[0] == 'n' )  &&
	     ( buffer[1] == 'O' || buffer[1] == 'o' ) )  {

	    /*
	     *  Build single text element and quit.
	     */
	    cptr = strrchr ( buffer, ' ' );
	    cst_crnm ( (char *)(cptr+1), &(tlon[0]), &ier );
	    cptr[0] = '\0';
	    cptr = strrchr ( buffer, ' ' );
	    cst_crnm ( (char *)(cptr+1), &(tlat[0]), &ier );
	    cptr[0] = '\0';

    	    el.hdr.vg_class = CLASS_TEXT;
    	    cvg_initelm ( &el );
    	    el.hdr.vg_type  = SPTX_ELM;
    	    el.hdr.filled   = 0;
    	    el.hdr.closed   = 0;
    	    el.hdr.smooth   = 0;
    	    el.hdr.grptyp   = 0;
    	    el.hdr.grpnum   = 0;
	    strcpy ( el.elem.spt.text, buffer );
	    el.elem.spt.info.rotn = 0.0;
	    el.elem.spt.info.sztext = 1.0;
	    el.elem.spt.info.sptxtyp = 0;
	    el.elem.spt.info.turbsym = 0;
	    el.elem.spt.info.itxfn = 21;
	    el.elem.spt.info.ithw = 2;
	    el.elem.spt.info.iwidth = 1;
	    el.elem.spt.info.txtcol = 3;
	    el.elem.spt.info.lincol = 3;
	    el.elem.spt.info.filcol = 3;
	    el.elem.spt.info.ialign = 0;
	    el.elem.spt.info.lat = tlat[0];
	    el.elem.spt.info.lon = tlon[0];
	    el.elem.spt.info.offset_x = 0;
	    el.elem.spt.info.offset_y = 0;

	    el.hdr.range_min_lat = tlat[0];
	    el.hdr.range_min_lon = tlon[0];
	    el.hdr.range_max_lat = tlat[0];
	    el.hdr.range_max_lon = tlon[0];

	    el.hdr.recsz = ( sizeof(VG_HdrStruct) + 
		sizeof(SpTextInfo) + strlen(buffer) + 1 );

	    cvg_writefD ( &el, -1, el.hdr.recsz, vgffile, &loc, &ier );

	    ier = 1;

	}
	else  {

           /*
            *  Flooding areas indicated; parse accordingly.
            *  First line should have number of points, category and date/time.
            */
	    cptr = cst_split ( buffer, ' ', sizeof(data), data, &ier );
	    cst_numb ( data, &npts, &ier );
	    cptr = cst_split ( cptr, ' ', sizeof(data), data, &ier );
	    cst_numb ( data, &icat, &ier );
	    strcpy ( date, cptr );

           /*
            *  Read in all the (latitude,longitude) points.
            */
	    for ( ii = 0; ii < npts; ii++ )  {
    	        cfl_rdln ( fptr, sizeof(buffer), buffer, &ier );
	        sscanf ( buffer, "%f, %f", &(flon[ii]), &(flat[ii]) );
	    }

	    if ( npts > REDUCE )  {
		filt = FILT;
		cv_rduc ( &npts, flat, flon, &filt, 
			  &nout, rflat, rflon, &ier );
		for ( ii = 0; ii < nout; ii++ )  {
		    flat[ii] = rflat[ii];
		    flon[ii] = rflon[ii];
		}
		npts = nout;
	    }

	    /*
	     *  Assign attributes based on flood category.
	     */
	    switch ( icat )  {
	        case POSSIBLE	:
    		    el.hdr.maj_col  = POSSIBLE_COLOR;
    		    el.hdr.min_col  = POSSIBLE_COLOR;
		    lintyp	    = POSSIBLE_LINTYP;
		    linwid	    = POSSIBLE_LINWID;
		    break;
	        case LIKELY 	:
    		    el.hdr.maj_col  = LIKELY_COLOR;
    		    el.hdr.min_col  = LIKELY_COLOR;
		    lintyp	    = LIKELY_LINTYP;
		    linwid	    = LIKELY_LINWID;
		    break;
	        case OCCURRING	:
    		    el.hdr.maj_col  = OCCURRING_COLOR;
    		    el.hdr.min_col  = OCCURRING_COLOR;
		    lintyp	    = OCCURRING_LINTYP;
		    linwid	    = OCCURRING_LINWID;
		    break;
	    }

           /*
            *  Now remove all sequences of points which are likely to be
	    *  geography, i.e., consecutive points that are close together.
	    *  This process may result in several sections of lines each
	    *  associated with a particular time.  Each section gets a
	    *  separate element but all are grouped together.
            */
	    nsegs = 0;

	    ii = 0;
	    np = 0;
	    tlat[np] = flat[ii];
	    tlon[np] = flon[ii];
	    np++;
	    while ( ii < npts-1 )  {

	        clo_dist ( &(flat[ii]), &(flon[ii]), &one, 
		           &(flat[ii+1]), &(flon[ii+1]), &dist, &ier );

	        /*
	         *  Gather sequence of points far apart.
	         */
	        while ( dist > CHK_DIST && ii < npts-1 )  {
		    tlat[np] = flat[ii+1];
		    tlon[np] = flon[ii+1];
		    np++;
		    ii++;
	            clo_dist ( &(flat[ii]), &(flon[ii]), &one, 
		               &(flat[ii+1]), &(flon[ii+1]), &dist, &ier );
	        }

	        /*
	         *  Process only if number of points exceeds 1.
	         */
	        if ( np > 1 )  {

	            if ( nsegs == 0 )  {

		        grpnum++;

	    	        /*
	     	         *  Build text element if first sequence.
	     	         */
    		        el.hdr.vg_class = CLASS_TEXT;
    		        cvg_initelm ( &el );
    		        el.hdr.vg_type  = SPTX_ELM;
    		        el.hdr.filled   = 0;
    		        el.hdr.closed   = 0;
    		        el.hdr.smooth   = 0;
    		        el.hdr.grptyp   = grpid;
    		        el.hdr.grpnum   = grpnum;
		        strcpy ( el.elem.spt.text, date );
		        el.elem.spt.info.rotn = 0.0;
		        el.elem.spt.info.sztext = 1.0;
		        el.elem.spt.info.sptxtyp = 0;
		        el.elem.spt.info.turbsym = 0;
		        el.elem.spt.info.itxfn = 21;
		        el.elem.spt.info.ithw = 2;
		        el.elem.spt.info.iwidth = 1;
		        el.elem.spt.info.txtcol = el.hdr.maj_col;
		        el.elem.spt.info.lincol = el.hdr.maj_col;
		        el.elem.spt.info.filcol = el.hdr.maj_col;
		        el.elem.spt.info.ialign = 0;
		        el.elem.spt.info.lat = tlat[0];
		        el.elem.spt.info.lon = tlon[0];
		        el.elem.spt.info.offset_x = 0;
		        el.elem.spt.info.offset_y = 0;

		        el.hdr.range_min_lat = tlat[0];
		        el.hdr.range_min_lon = tlon[0];
		        el.hdr.range_max_lat = tlat[0];
		        el.hdr.range_max_lon = tlon[0];

		        el.hdr.recsz = ( sizeof(VG_HdrStruct) + 
			    sizeof(SpTextInfo) + strlen(date) + 1 );

		        cvg_writefD (&el, -1, el.hdr.recsz, vgffile, &loc, &ier);

	            }

		    nsegs++;

	    	    /*
	     	     *  Build line element.
	     	     */
		    for ( jj = 0; jj < np; jj++ )  {
		        el.elem.lin.latlon[jj   ] = tlat[jj];
		        el.elem.lin.latlon[jj+np] = tlon[jj];
		    }

    		    el.hdr.vg_class = CLASS_LINES;
    		    cvg_initelm ( &el );
    		    el.hdr.vg_type  = LINE_ELM;
    		    el.hdr.filled   = 0;
    		    el.hdr.closed   = 0;
		    if ( tlat[0] == tlat[np-1]  &&
		         tlon[0] == tlon[np-1] )  el.hdr.closed = 1;
    		    el.hdr.smooth   = 0;
    		    el.hdr.grptyp   = grpid;
    		    el.hdr.grpnum   = grpnum;

		    el.elem.lin.info.numpts = np;
		    el.elem.lin.info.lintyp = lintyp;
		    el.elem.lin.info.lthw = 0;
		    el.elem.lin.info.width = linwid;
		    el.elem.lin.info.lwhw = 0;
    		    el.hdr.filled   = 0;

		    el.hdr.recsz = ( (sizeof(float) * 2 * np) +
                          sizeof(VG_HdrStruct) + sizeof(LineInfo) );

		    cvg_writefD ( &el, -1, el.hdr.recsz, vgffile, &loc, &ier );

	        }

	        /*
	         *  Skip sequence of points close together.
	         */
	        while ( dist < CHK_DIST && ii < npts-1 )  {
		    ii++;
	            clo_dist ( &(flat[ii]), &(flon[ii]), &one, 
		               &(flat[ii+1]), &(flon[ii+1]), &dist, &ier );
	        }

	        /*
	         *  Start new sequence of points far apart.
	         */
	        np = 0;
	        tlat[np] = flat[ii];
	        tlon[np] = flon[ii];
	        np++;

	    }

	    /*
	     *  Read the next group and process if not at EOF.
	     */
            cfl_rdln ( fptr, sizeof(buffer), buffer, &ier );

	}

    }
    return 0;
}

/*=====================================================================*/
