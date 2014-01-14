#include "gbcmn.h"

void gb_ftim ( int *itime, int *iaccm, int *iret )
/************************************************************************
 * gb_ftim								*
 *									*
 * This routine computes the forecast time and accumulation period	*
 * from the PDS block.							*
 *									*
 * gb_ftim ( itime, iaccm, iret )					*
 *									*
 * Output parameters:							*
 *	*itime  	int		Date, time, forecast array	*
 *	*iaccm		int		Accumulation time		*
 *	*iret		int		reture code			*
 **									*
 * Log:									*
 * Chiz/Unidata		 3/00   Created from gb_gpds                    *
 * M. Li/GSC		 5/00	Added iret and cleaned up		*
 * S. Jacobs/NCEP	 3/11	Added check for Canadian Ensembles	*
 * M. James/Unidata	 9/13	Added more center checks for fcst hours	*
 ***********************************************************************/
{
	int		ifcst, iafgi, ihhh, imm, iyy,
			itarr[5], imin, ier;

/*---------------------------------------------------------------------*/

	*iret = 0;
	*iaccm = IMISSD;
	switch ( pds.time_range ) {
	    case 10:
		    iafgi = 1;
		    ifcst = pds.time_p1 * 256 + pds.time_p2;
		    break;
	    case 2:
	    case 3:
	    case 5:
		    iafgi = 1;
		    ifcst = pds.time_p2;
		    *iaccm = ifcst - pds.time_p1;
		    break;
	    case 4:
/*
 *		    Special processing for the RFC accumulated
 *		    precipitation. The data is observed not forecast
 *		    and must be handled differently.
 */
		    if  ( pds.center == 7 && pds.process == 154 )  {
			iafgi = 0;
			ifcst = 0;
			*iaccm = pds.time_p2 - pds.time_p1;
		    }
		    else {
			iafgi = 1;
			ifcst = pds.time_p2;
			*iaccm = ifcst - pds.time_p1;
		    }
		    break;
	    case 124:
		    iafgi = 0;
		    ifcst = 0;
		    *iaccm = pds.time_p2 - pds.time_p1;
		    break;
	    case 1:
		    /*
 		     *	Special processing for the AWFA MM5 model.
 		     *	Zero hour forecasts (initializations) are indicated
 		     *	as initialized analysis products.
		     *
		     *	Same for Canadian Ensembles.
		     *  Same for Brazil and other centers.
 		     */
		    if  ( ( pds.center == 57 && pds.process == 10 ) ||
			  ( pds.center == 54 && pds.process == 71 ) ||
			  ( pds.center == 78 ) ||
			  ( pds.center == 146 ) ||
			  ( pds.center == 43 ) ||
			  ( pds.center == 46 ) )  {
		        iafgi = 1;
		        ifcst = pds.time_p1;
		    }
		    else  {
		        iafgi = 0;
		        ifcst = 0;
		    }
		    break;
	    case 0:
	    default:
		    iafgi = 1;
		    ifcst = pds.time_p1;
		    break;
	}

	switch ( pds.time_unit ) {
	    case 0:
		    ihhh = ifcst / 60;
		    imm  = ifcst % 60;
		    break;
	    case 1:
		    ihhh = ifcst;
		    imm  = 0;
		    break;
	    case 2:
		    ihhh = ifcst * 24;
		    imm  = 0;
		    break;
	    case 10:
		    ihhh = ifcst * 3;
		    imm  = 0;
		    *iaccm = *iaccm * 3;
		    break;
	    case 11:
		    ihhh = ifcst * 6;
		    imm  = 0;
		    *iaccm = *iaccm * 6;
		    break;
	    case 12:
		    ihhh = ifcst * 12;
		    imm  = 0;
		    *iaccm = *iaccm * 12;
		    break;
	    case 254:
		    ihhh = ifcst / 3600;
		    imm  = ( ifcst % 3600 ) / 60;
		    break;
	    default:
		    ihhh = 0;
		    imm  = 0;
		    break;
	}

/*
 *	Combine the year and the century.
 */
	iyy  = ( ( pds.century - 1 ) * 100 ) + pds.year;

/*
 *	More special treatment for the RFC observed precipitation
 *	data. Add the accumulation time to the reference time.
 */
	if  ( ( iafgi == 0 ) && ( *iaccm != IMISSD ) )  {
	    itarr[0] = iyy;
	    itarr[1] = pds.month;
	    itarr[2] = pds.day;
	    itarr[3] = pds.hour;
	    itarr[4] = pds.minute;
	    imin = *iaccm * 60;
	    ti_addm ( itarr, &imin, itarr, &ier );
	    iyy        = itarr[0];
	    pds.month  = itarr[1];
	    pds.day    = itarr[2];
	    pds.hour   = itarr[3];
	    pds.minute = itarr[4];
	}

	itime[0] = iyy * 10000 + pds.month * 100 + pds.day;
	itime[1] = pds.hour * 100 + pds.minute;
	itime[2] = iafgi * 100000 + ihhh * 100 + imm;

}
