#include "geminc.h"
#include "gemprm.h"

void nd_ftim ( int *is1, int *pds, int *itime, int *iaccm, int *iret );

void nd_ftim ( int *is1, int *pds, int *itime, int *iaccm, int *iret )
/************************************************************************
 * nd_ftim								*
 *									*
 * This routine computes the forecast time and accumulation period	*
 * from the PDS block.							*
 *									*
 * nd_ftim ( is1, pds, itime, iaccm, iret )				*
 *									*
 * Input parameters:							*
 *	*is1		int	Unpacked Section 1 - Ident Section	*
 *	*pds		int	Unpacked Product Definition Section	*
 * Output parameters:							*
 *	*itime  	int	Date, time, forecast array		*
 *	*iaccm		int	Accumulation time			*
 *	*iret		int	Return code				*
 *				-17 - Error setting date/time		*
 **									*
 * Log:									*
 * T. Piper/SAIC	10/02	Created from gbftim.c			*
 * T. Piper/SAIC	05/03	Modified for correct PoP12 forecast time*
 ***********************************************************************/
{
	int		year, month, day, hour, minute, itarr[5];
	int		iafgi, ifcst, ihhh, imm, imin, ier;
/*---------------------------------------------------------------------*/

	*iret  = 0;
	*iaccm = IMISSD;
	year   = is1[12];
	month  = is1[14];
	day    = is1[15];
	hour   = is1[16];
	minute = 0; 

/*	Type of generating process (see Code Table 4.3)  */
	switch ( pds[11] ) {
	    case 0:  /*  Analysis  */
	    case 1:  /*  Initialization  */
	        iafgi = 0;
	        ifcst = 0;
	        break;
	    case 2:  /*  Forecast  */
	    case 3:  /*  Bias corrected forecast  */
	    case 4:  /*  Ensemble forecast  */
		iafgi = 1;
		ifcst = pds[18];
		break;
	    case 5:  /* Probability forecast */
	    case 6:  /* Forecast error */
	    case 7:  /* Analysis error */
	    case 8:  /* Observation */
	    default:
	        *iret = -17;
		return;
	}

/*	Indicator of unit of time range (see Code Table 4.4)
	for Forecast time.  */

	switch ( pds[17] ) {
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
	    case 13:
		    ihhh = ifcst / 3600;
		    imm  = ( ifcst % 3600 ) / 60;
		    break;
	    default:
		    ihhh = 0;
		    imm  = 0;
		    break;
	}

/*
 *	NOTE:  This copied from GRIB1 - may not be relevant...
 *	More special treatment for the RFC observed precipitation
 *	data.  Add the accumulation time to the reference time.
 */
	if  ( ( iafgi == 0 ) && ( *iaccm != IMISSD ) )  {
	    itarr[0] = year; 
	    itarr[1] = month;
	    itarr[2] = day;
	    itarr[3] = hour;
	    itarr[4] = minute;
	    imin = *iaccm * 60;
	    ti_addm ( itarr, &imin, itarr, &ier );
	    year   = itarr[0];
	    month  = itarr[1];
	    day    = itarr[2];
	    hour   = itarr[3];
	    minute = itarr[4];
	}

	itime[0] = year * 10000 + month * 100 + day;
	itime[1] = hour * 100   + minute;
	itime[2] = iafgi * 100000 + ihhh * 100 + imm;

}
