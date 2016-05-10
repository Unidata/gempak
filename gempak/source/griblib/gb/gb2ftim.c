#include "gbcmn.h"
#include "gb2def.h"

void gb2_ftim ( gribfield *gfld, char *gdattm, int *iaccm, int *iret )
/************************************************************************
 * gb2_ftim								*
 *									*
 * This routine computes the forecast time and accumulation period	*
 * from the GRIB2 IDS and PDS.						*
 *									*
 * gb2_ftim ( gfld, gdattm, iaccm, iret )				*
 *									*
 * Input parameters:							*
 *	*gfld  	struct gribfield        Decoded GRIB2 structure         *
 *									*
 * Output parameters:							*
 *      *gdattm         char            pointer to new GEMPAK date/time *
 *                                      string.  Must be allocated to a *
 *                                      minimum of 20 bytes.            *
 *	*iaccm		int		Accumulation time		*
 *	*iret		int		return code			*
 *                                        -27 = Unrecognized PDT number *
 **									*
 * Log:									*
 * Chiz/Unidata		 3/00   Created from gb_gpds                    *
 * M. Li/GSC		 5/00	Added iret and cleaned up		*
 * S. Gilbert        1/04   Modified from gb_ftim for use with GRIB2.   *
 * J. Wu/SGT         03/15	Add a check for analysis grid idsect[2](R6790) *
 ***********************************************************************/
{
    int    ier;
    int	   ifcst, iafgi, ihhh, imm, ipos, itmp,
	   istart[5], iend[5], template, itime[3];
    int    year, month, day, hour, minute;
    int    endloc[20]={0,0,0,0,0,0,0,0,15,22,16,18,17,31,30,0,0,0,0,0};

/*---------------------------------------------------------------------*/

	*iret = 0;
        iafgi=1;
	*iaccm = IMISSD;

        /*
         *	Initial Reference time from IDS.
         */
	year  = (int)gfld->idsect[5];
	month = (int)gfld->idsect[6];
	day   = (int)gfld->idsect[7];
	hour  = (int)gfld->idsect[8];
	minute = (int)gfld->idsect[9];

        /* 
         *  Calculate Ending forecast Hour and Accumulation time, if
         *  appropriate.
         */
        template = (int)gfld->ipdtnum;
	switch ( template ) {
	    case 0:
	    case 1:
	    case 2:
	    case 3:
	    case 4:
	    case 5:
	    case 6:
	    case 7:
	    case 15:
		    iafgi = 1;
            ifcst = gb2_fcsm((int)gfld->ipdtmpl[8],(int)gfld->ipdtmpl[7]);
		    break;

	    case 8:
	    case 9:
	    case 10:
	    case 11:
	    case 12:
	    case 13:
	    case 14:
		    iafgi = 1;
            /*
             *  Set initial reference time array
             */
            istart[0]=year;
            istart[1]=month;
            istart[2]=day;
            istart[3]=hour;
            istart[4]=minute; 
            /*
             *  Set end of forecast time array
             */
            ipos=endloc[template];
            iend[0]=(int)gfld->ipdtmpl[ipos++];
            iend[1]=(int)gfld->ipdtmpl[ipos++];
            iend[2]=(int)gfld->ipdtmpl[ipos++];
            iend[3]=(int)gfld->ipdtmpl[ipos++];
            iend[4]=(int)gfld->ipdtmpl[ipos];
            /*
             *  Calculate diff of end time to start time to get
             *  hour/minute of ending period.
             */
            ti_mdif(iend,istart,&ifcst,&ier);
            /*
             *  Add 1st forecast time to initial reference time to get
             *  start of forecast period.
             */
            itmp = gb2_fcsm((int)gfld->ipdtmpl[8],(int)gfld->ipdtmpl[7]);
		    ti_addm(istart,&itmp,istart,&ier);
            /*
             *  Calculate diff of end time to start of forecast perioid 
             *  to get accum time.
             */
            ti_mdif(iend,istart,iaccm,&ier);
            *iaccm = *iaccm / 60;
		    break;

	    default:
		    iafgi = 0;
		    ifcst = 0;
                    *iret = -27;
		    break;
	}


	    /*
	     *   Check if it is analysis grid
	     */
	    if ( (int)gfld->idsect[12] == 0 ) {
	        iafgi = 0;
	        ifcst = 0;
	    }

	    /*
         *  If PDT number is less than 20, adjust fcst hour, if necessary.
         */
        ihhh = ifcst/60;
        imm  = ifcst%60;

        itime[0] = year * 10000 + month * 100 + day;
        itime[1] = hour * 100 + minute;
        itime[2] = iafgi * 100000 + ihhh * 100 + imm;

        tg_itoc( itime, gdattm, &ier, 20);

}


