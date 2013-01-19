#include "gbcmn.h"

void gb_gds ( unsigned char *ptarray )
/************************************************************************
 * gb_gds								*
 *									*
 * This function decodes section 2 (Grid Description Section) of a	*
 * GRIB message.							*
 *									*
 * gb_gds ( ptarray )							*
 *									*
 * Input parameters:							*
 *	*ptarray	unsigned char	Data buffer			*
 **									*
 * Log:									*
 * J. Chou/EAI		02/93						*
 * J. Chou/EAI		07/93						*
 * S. Jacobs/EAI        11/93           Clean up; Added GBDIAG prints   *
 * S. Jacobs/EAI	 1/94		Clean up; Rename variables	*
 * L. Williams/EAI	 7/94		Reformat header			*
 * D.W.Plummer/NCEP	 2/96		Cleanup GBDIAGs and comments	*
 * K. Brill/EMC		 5/96		Break for gds.grid_proj = 201	*
 * K. Brill/EMC		 9/98		Break for gds.grid_proj = 203	*
 * T. Piper/GSC		11/98		Updated prolog			*
 * S. Jacobs/NCEP	12/00	Added prototypes			*
 * S. Gilbert/NCEP          03/06    Chngs to remove compiler warnings  *
 * M. Pyle/EMC           9/08           Break for gds.grid_proj = 205   *
 ***********************************************************************/
{

int	indx;

/*---------------------------------------------------------------------*/

	/*
	 * BYTES 1-3
	 * Get the length of the section.
	 */
	indx = 0;
	gds.length = gb_btoi ( ptarray, indx, 3, FALSE );

	/*
	 * BYTE 4
	 * Get the number of vertical co-ordinate parameters.
	 */
	indx = 3;
	gds.NV = gb_btoi ( ptarray, indx, 1, FALSE );

	/*
	 * BYTE 5
	 * Get the value for PV or PL.
	 */
	indx = 4;
	gds.PV = gb_btoi ( ptarray, indx, 1, FALSE );

	/*
	 * BYTE 6
	 * Get the data representation type.
	 */
	indx = 5;
	gds.grid_proj = gb_btoi ( ptarray, indx, 1, FALSE );

	if ( GBDIAG_GDS == TRUE )  {
	    printf ( " GDS bytes  1 -  3 (gds.length)    = %d\n", gds.length );
	    printf ( " GDS byte        4 (gds.NV)        = %d\n", gds.NV );
	    printf ( " GDS byte        5 (gds.PV)        = %d\n", gds.PV );
	    printf ( " GDS byte        6 (gds.grid_proj) = %d\n", gds.grid_proj );
	}

	switch ( gds.grid_proj ) {
	    case  0:
	    case 10:
	    case 20:
	    case 30:
		    gb_ltln ( ptarray );
		    break;
	    case  1:
		    gb_merc ( ptarray );
		    break;
	    case  5:
		    gb_polr ( ptarray );
		    break;
	    case  3:
	    case  8:
	    case 13:
		    gb_lamb ( ptarray );
		    break;
	    case  4:
	    case 14:
	    case 24:
	    case 34:
		    gb_gaus ( ptarray );
		    break;
	    case 50:
	    case 60:
	    case 70:
	    case 80:
		    gb_sphr ( ptarray );
		    break;
	    case  2:
	    case  6:
	    case  7:
	    case  9:
	    case 90:
	    case 201:
	    case 203:
	    case 205:
		    break;
	    default:
		    printf(" WARNING : Grid projection (GDS byte 6) %d not supported.\n", gds.grid_proj );
		    break;
	} 

}
