#include "geminc.h"
#include "gemprm.h"

#define	VFCMN_GLOBAL
#include "vfcmn.h"


#define CHG_YR   2001    /* Year, month, day, and hour when to  */
#define CHG_MO   11      /* begin using the new WMO headers.    */ 
#define CHG_DA   7
#define CHG_TM   1200

SpcInfo_t       spcinfo;

void vfread ( int *iret )
/************************************************************************
 * vfread                                                               *
 *                                                                      *
 * This program reads the verification file.				*
 *                                                                      *
 * vfread ( iret )                                             		*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret            int            Return Code                     *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/GSC          7/99   Created                                 *
 * M. Li/GSC		10/99	Rounded the mph to the nearest 5	*
 * A. Hardy/GSC         11/99   Added saving of replacement numbers     *
 * A. Hardy/GSC		 2/00   Extracted from SPCTXT                   *
 * A. Hardy/GSC		 5/01   Initialized iret to 0			*
 * A. Hardy/SAIC	10/01   Added WMO header variable		*
 * A. Hardy/SAIC	10/01   Fixed WMO header variable flag		*
 * E. Safford/SAIC	05/02	fix crash when MOTION string is null	*
 * R. Tian/SAIC		06/02	Modified to meet the SPC requirement	*
 * R. Tian/SAIC		06/02	Removed unused variable ickhr		*
 * S. Jacobs/NCEP	 3/03	Initialize structure before reading	*
 * A. Hardy/NCEP	 6/03   Change hail size from int to float	*
 * A. Hardy/NCEP	10/03   Fixed gust wind speed rounding		*
 * A. Hardy/NCEP	11/03   Moved final count of number of counties *
 ***********************************************************************/
{
  char 	  *cptr, first[256];
  char    buff[256], *arrch, dud[2];
  int     i, ierr, ier, len, wcnt, miles, cntot; 
  size_t  ii;
  float   flmile;
/*-------------------------------------------------------------------*/

    *iret = 0;
    ierr  = 0;
    wcnt  = 0;
    cntot = 0;

    /*
     * Initialize the structure of county information.
     */
    spcinfo.total = 0;
    for ( ii = 0; ii < sizeof(spcinfo.cnty)/sizeof(struct county_info); ii++ ) {
	spcinfo.cnty[ii].ugc[0]   = '\0';
	spcinfo.cnty[ii].state[0] = '\0';
	spcinfo.cnty[ii].cname[0] = '\0';
	spcinfo.cnty[ii].fips[0]  = '\0';
	spcinfo.cnty[ii].wfo[0]   = '\0';
	spcinfo.cnty[ii].ctylat  = RMISSD;
	spcinfo.cnty[ii].ctylon  = RMISSD;
    }

    /*
     * Loop till end of file is reached.
     */

    while ( ierr != 4 ) {

        cfl_rdln ( spcinfo.file_info.ifp, sizeof(buff), buff, &ierr );

	/*
	 *   Find the colon separator.
	 */
	cptr = strtok ( buff, ":" );

	/*
	 *   Copy left hand side string into the array and
	 *   into a string for comparisons.
	 */
	strcpy ( first, cptr );

        /*
         *   Remove leading spaces from first array.
         */
	cptr = strtok ( NULL, "\0" );
        if ( cptr != NULL ) {
	    cst_ldsp (cptr, cptr, &len, &ier );
	}

	if ( strcmp(first,"WATCH NUMBER") == 0 ) {
	    spcinfo.wnum = atoi(cptr);
	}
	else if ( strcmp(first,"TYPE") == 0 ) {
	    strcpy ( spcinfo.wtype, cptr );
	}
	else if ( strcmp(first,"PDS/Normal") == 0 ) {
	    strcpy ( spcinfo.pdsn, cptr );
	}
	else if ( strcmp(first,"ISSUE TIME") == 0 ) {
	    arrch=strtok(cptr," ");
	    spcinfo.itime.month= atoi(arrch);
	    arrch=strtok(NULL," ");
	    spcinfo.itime.day = atoi(arrch);
	    arrch=strtok(NULL," ");
	    spcinfo.itime.year = atoi(arrch);
	    arrch=strtok(NULL," ");
	    strcpy ( spcinfo.itime.hour, arrch);
	}
	else if ( strcmp(first,"VALID TIME") == 0 ) {
	    arrch=strtok(cptr," ");
	    spcinfo.vtime.month= atoi(arrch);
	    arrch=strtok(NULL," ");
	    spcinfo.vtime.day = atoi(arrch);
	    arrch=strtok(NULL," ");
	    spcinfo.vtime.year = atoi(arrch);
	    arrch=strtok(NULL," ");
	    strcpy ( spcinfo.vtime.hour,arrch );
	}
	else if ( strcmp(first,"EXPIRATION TIME") == 0 ) {
	    arrch=strtok(cptr," ");
	    spcinfo.etime.month= atoi(arrch);
	    arrch=strtok(NULL," ");
	    spcinfo.etime.day = atoi(arrch);
	    arrch=strtok(NULL," ");
	    spcinfo.etime.year = atoi(arrch);
	    arrch=strtok(NULL," ");
	    strcpy ( spcinfo.etime.hour, arrch);
	}
	else if ( strcmp(first,"ENDPOINT (ANC,sm)") == 0 ) {
	    arrch=strtok(cptr," ");
	    spcinfo.ancrpt.dist1 = atoi(arrch);
	    arrch=strtok(NULL," ");
	    strcpy ( spcinfo.ancrpt.dirct1, arrch);
	    arrch=strtok(NULL," ");
	    strcpy ( spcinfo.ancrpt.stn1, arrch);
	    arrch=strtok(NULL," ");
	    strcpy ( dud, arrch);
	    arrch=strtok(NULL," ");
	    spcinfo.ancrpt.dist2 = atoi(arrch);
	    arrch=strtok(NULL," ");
	    strcpy ( spcinfo.ancrpt.dirct2, arrch);
	    arrch=strtok(NULL," ");
	    strcpy ( spcinfo.ancrpt.stn2, arrch);
	}
	else if ( strcmp(first,"ENDPOINT (VOR,nm)") == 0 ) {
	    arrch=strtok(cptr," ");
	    spcinfo.vorrpt.dist1 = atoi(arrch);
	    arrch=strtok(NULL," ");
	    strcpy ( spcinfo.vorrpt.dirct1, arrch);
	    arrch=strtok(NULL," ");
	    strcpy ( spcinfo.vorrpt.stn1, arrch);
	    arrch=strtok(NULL," ");
	    arrch=strtok(NULL," ");
	    spcinfo.vorrpt.dist2 = atoi(arrch);
	    arrch=strtok(NULL," ");
	    strcpy ( spcinfo.vorrpt.dirct2, arrch);
	    arrch=strtok(NULL," ");
	    strcpy ( spcinfo.vorrpt.stn2, arrch);
	}
	else if ( strcmp(first,"ATTRIB (ANC,sm)") == 0 ) {
	    arrch=strtok(cptr," ");
	    spcinfo.ancatt.dist = atoi(arrch);
	    if ( spcinfo.ancatt.dist >= 0 ){
		arrch=strtok(NULL," ");
		if ( strcmp ( arrch, "EW") == 0 )  
	            strcpy ( spcinfo.ancatt.dirc, "EAST AND WEST");
		if ( strcmp ( arrch, "NS") == 0 )  
	            strcpy ( spcinfo.ancatt.dirc, "NORTH AND SOUTH");
		if ( strcmp ( arrch, "ESOL") == 0 )  
	            strcpy ( spcinfo.ancatt.dirc, "EITHER SIDE");
	    }
	}
	else if ( strcmp(first,"ATTRIB (VOR,nm)") == 0 ) {
	    arrch=strtok(cptr," ");
	    spcinfo.voratt.dist = atoi(arrch);
	    if ( spcinfo.voratt.dist >= 0 ){
		arrch=strtok(NULL," ");
		if ( strcmp ( arrch, "EW") == 0 )  
	            strcpy ( spcinfo.voratt.dirc, "E/W");
		if ( strcmp ( arrch, "NS") == 0 )  
	            strcpy ( spcinfo.voratt.dirc, "N/S");
		if ( strcmp ( arrch, "ESOL") == 0 )  
	            strcpy ( spcinfo.voratt.dirc, "EITHER SIDE");
	    }
	}
	else if ( strcmp(first,"WATCH CORNER POINT") == 0 ) {
	    ++wcnt;
	    switch ( wcnt ) {
	        case 1:
		    arrch=strtok(cptr," ");
	            spcinfo.wcpnt1.lat = (float)atof(arrch);
		    arrch=strtok(NULL," ");
	            spcinfo.wcpnt1.lon = (float)atof(arrch);
		    break;
		case 2:
		    arrch=strtok(cptr," ");
	            spcinfo.wcpnt2.lat = (float)atof(arrch);
		    arrch=strtok(NULL," ");
	            spcinfo.wcpnt2.lon = (float)atof(arrch);
		    break;
		case 3:
		    arrch=strtok(cptr," ");
	            spcinfo.wcpnt3.lat = (float)atof(arrch);
		    arrch=strtok(NULL," ");
	            spcinfo.wcpnt3.lon = (float)atof(arrch);
		    break;
		case 4:
		    arrch=strtok(cptr," ");
	            spcinfo.wcpnt4.lat = (float)atof(arrch);
		    arrch=strtok(NULL," ");
	            spcinfo.wcpnt4.lon = (float)atof(arrch);
		    break;
		default:
		    break;
	    }
	}
	else if ( strcmp(first,"HAIL SIZE (in)") == 0 ) {
	    spcinfo.hailsz = (float)atof(cptr);
	}
	else if ( strcmp(first,"MAX GUSTS (kts)") == 0 ) {
	    spcinfo.maxgust = atoi(cptr);
	    flmile = (float)(spcinfo.maxgust);
            flmile =  pr_knmh( &flmile);
	    miles  = (int)flmile;

	    /*
	     * Rounding the mph to the nearest 5.
	     */
	    spcinfo.maxmph = ( miles / 5 ) * 5;
            if ( (flmile - (float) spcinfo.maxmph) >= 2.500F ) {
                spcinfo.maxmph += 5;
	    }
	}
	else if ( strcmp(first,"MAX TOPS (100s ft)") == 0 ) {
	    spcinfo.maxtops = atoi(cptr);
	}
	else if ( strcmp(first,"MOTION (deg,kts)") == 0 ) {
	    arrch=strtok(cptr," ");
	    if ( arrch == NULL ) {
	        spcinfo.motion.deg   = 0;
	        spcinfo.motion.speed = 0;
	    }
	    else {
	        spcinfo.motion.deg = atoi(arrch);
		arrch=strtok(NULL," ");
	        spcinfo.motion.speed = atoi(arrch);
	    }
	}
	else if ( strcmp(first,"TIME ZONE") == 0 ) {
	    strcpy ( spcinfo.timzone, cptr );
	}
	else if ( strcmp(first,"REPL WATCH NUMBER") == 0 ) {
	    arrch = strtok ( cptr," ");
	    i = 0;
	    while ( arrch != NULL ) {
	        strcpy ( spcinfo.replcnm[i], arrch );
		arrch = strtok( NULL," " );
                i++;
	    }
	    spcinfo.wwrepnm = i-1;
	}
	else if ( strcmp(first,"STATES INCLUDED") == 0 ) {
	    strcpy ( spcinfo.states, cptr );
	}
	else if ( strcmp(first,"STATUS") == 0 ) {
	    strcpy ( spcinfo.status, cptr );
	}
	else if ( strcmp(first,"FORECASTER") == 0 ) {
	    strcpy ( spcinfo.frcstr, cptr );
	}
	else if ( strcmp(first,"WATCH AREA (sq mi)") == 0 ) {
	    spcinfo.warea = atoi(cptr);
	}  
	else if ( ( cptr == NULL ) && ( ierr != 4) ) {
	    cptr = strtok ( buff, " " );
	    if ( strcmp(cptr,"UGC") != 0 ) {
	        strcpy ( spcinfo.cnty[cntot].ugc, cptr );
	        arrch=strtok(NULL," ");
	        strcpy ( spcinfo.cnty[cntot].state, arrch );
	        arrch=strtok(NULL," ");
	        strcpy ( spcinfo.cnty[cntot].cname, arrch );
	        arrch=strtok(NULL," ");
	        spcinfo.cnty[cntot].ctylat = (float)atof(arrch);
	        arrch=strtok(NULL," ");
	        spcinfo.cnty[cntot].ctylon = (float)atof(arrch);
	        arrch=strtok(NULL," ");
	        strcpy ( spcinfo.cnty[cntot].fips, arrch );
	        arrch=strtok(NULL," ");
	        strcpy ( spcinfo.cnty[cntot].wfo, arrch);
	        cntot++;
	    }  
	}

    }
    spcinfo.total = cntot;
} 
