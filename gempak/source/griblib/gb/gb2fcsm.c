#include "gb2def.h"

int gb2_fcsm(int ifcst, int unit)
/************************************************************************
 * gb2_fcsm			                                   	*
 *		   		                                    *
 * This function converts a forecast time to minutes                    *
 * from the GRIB2 IDS and PDS.							*
 *									*
 * int = gb2_fcsm ( ifcst, unit )					*
 *									*
 * Input parameters:							*
 *	ifcst  	int		Forecast time in units defined by PDT   *
 *      unit    int             Forecast time unit as defined in GRIB2  *
 *                              COde Table 4.4                          *
 *                                                                     *
 * Return value:                                                       *
 *                  Forecast time in minutes                           *
 **									*
 * Log:									*
 * S. Gilbert        1/04                                              *
 ***********************************************************************/
{
      int     min;

      switch ( unit ) {
      	    case 0:                       /* Minute */
                   min=ifcst;
   	      	   break;
   	    case 1:                       /* Hour */
   		   min = ifcst*60;
      		   break;
      	    case 2:                       /* Day */
   	      	   min = ifcst * 60 * 24;
   		   break;
      	    case 10:                      /* 3 Hours */
      		   min = ifcst * 3 * 60;
	      	   break;
      	    case 11:                      /* 6 Hours */
		   min = ifcst * 6 *60;
	      	   break;
      	    case 12:                      /* 12 Hours */
		   min = ifcst * 12 * 60;
	      	   break;
      	    case 13:                      /* Second */
		   min = ifcst / 60;
	      	   break;
      	    default:
		    min = 0;
	      	    break;
      }

      return min;

}
