#include "geminc.h"
#include "gemprm.h"

void nd_gds ( int *gds, float *gdsarr, int *iret );
void nd_lamb ( int *gds, float *gdsarr, int *irltv, int *nrmflg );
		
void nd_gds ( int *gds, float *gdsarr, int *iret )
/************************************************************************
 * nd_gds								*
 *									*
 * This function decodes section 3 (Grid Definition Section) of a	*
 * GRIB2 message.							*
 *									*
 * nd_gds ( gds )							*
 *									*
 * Input parameters:							*
 *      *gds            int     Unpacked Grid Definition Template       *
 * Output parameters:                                                   *
 *      *gdsarr         float   GDS information array (GRIB projection  *
 *                              number, number of columns of data,      *
 *                              number of rows of data, lat/lon of      *
 *                              corners)                                *
 *	*iret		int	Return status				*
 *				-4 - Grid Definition Temp. not supprted	*
 **									*
 * Log:									*
 * T. Piper/SAIC	4/03	Modified from gb_gds			*
 ***********************************************************************/
{
	int irltv, nrmflg;

/*---------------------------------------------------------------------*/
	*iret = 0;
	
	switch ( gds[12] ) {
/*	For future use
	    case 0:
	    case 1:
	    case 2:
	    case 3:
		    nd_ltln ( gds, gdsarr );
		    break;
	    case 10:
		    nd_merc ( gds, gdsarr ); 
		    break;
	    case 20:
		    nd_polr ( gds, gdsarr );  
		    break;
*/
	    case 30:
		    nd_lamb ( gds, gdsarr, &irltv, &nrmflg );
		    break;
/*	For future use	    
 	    case 40:
	    case 41:
	    case 42:
	    case 43:
		    nd_gaus ( gds, gdsarr );
		    break;
	    case 50:
	    case 51:
	    case 52:
	    case 53:
		    nd_sphr ( gds, gdsarr );
		    break;
*/
	    default:
		    *iret = -4;
		    break;
	} 
}
