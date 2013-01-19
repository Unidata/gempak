#include "geminc.h"
#include "gemprm.h"
#include "vfcmn.h"

SpcInfo_t       newinfo;

void vfrdrp ( int *iret )
/************************************************************************
 * vfrdrp                                                               *
 *                                                                      *
 * This program reads the verification file.				*
 *                                                                      *
 * vfrdrp ( iret )                                             		*
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
 * A. Hardy/GSC		 5/01   Initialized iret to 0 			*
 * E. Safford/SAIC	05/02	fix error handling null MOTION string	*
 * A. Hardy/NCEP	 6/03	change hail size to float from int	*
 * A. Hardy/NCEP	10/03	Fixed gust wind speed rounding		*
 * A. Hardy/NCEP	11/03	Moved final total of counties		*
 ***********************************************************************/
{
  char 	  *cptr, first[256];
  char    buff[256], *arrch, dud[2];
  int     i, ierr, ier, len, wcnt, miles, cntot; 
  float   flmile;
/*-------------------------------------------------------------------*/
        *iret  = 0;
	ierr  = 0;
	wcnt  = 0;
	cntot = 0;

       /*
        * Loop till end of file is reached.
	*/

	while ( ierr != 4 ) {

	    cfl_rdln ( newinfo.file_info.ifp, sizeof(buff), buff, &ierr );

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
	        newinfo.wnum = atoi(cptr);
	    }
	    else if ( strcmp(first,"TYPE") == 0 ) {
	        strcpy ( newinfo.wtype, cptr );
	    }
	    else if ( strcmp(first,"PDS/Normal") == 0 ) {
	        strcpy ( newinfo.pdsn, cptr );
	    }
	    else if ( strcmp(first,"ISSUE TIME") == 0 ) {
		arrch=strtok(cptr," ");
	        newinfo.itime.month= atoi(arrch);
		arrch=strtok(NULL," ");
	        newinfo.itime.day = atoi(arrch);
		arrch=strtok(NULL," ");
	        newinfo.itime.year = atoi(arrch);
		arrch=strtok(NULL," ");
	        strcpy ( newinfo.itime.hour, arrch);
	    }
	    else if ( strcmp(first,"VALID TIME") == 0 ) {
		arrch=strtok(cptr," ");
	        newinfo.vtime.month= atoi(arrch);
		arrch=strtok(NULL," ");
	        newinfo.vtime.day = atoi(arrch);
		arrch=strtok(NULL," ");
	        newinfo.vtime.year = atoi(arrch);
		arrch=strtok(NULL," ");
	        strcpy ( newinfo.vtime.hour,arrch );
	    }
	    else if ( strcmp(first,"EXPIRATION TIME") == 0 ) {
		arrch=strtok(cptr," ");
	        newinfo.etime.month= atoi(arrch);
		arrch=strtok(NULL," ");
	        newinfo.etime.day = atoi(arrch);
		arrch=strtok(NULL," ");
	        newinfo.etime.year = atoi(arrch);
		arrch=strtok(NULL," ");
	        strcpy ( newinfo.etime.hour, arrch);
	    }
	    else if ( strcmp(first,"ENDPOINT (ANC,sm)") == 0 ) {
		arrch=strtok(cptr," ");
	        newinfo.ancrpt.dist1 = atoi(arrch);
		arrch=strtok(NULL," ");
	        strcpy ( newinfo.ancrpt.dirct1, arrch);
		arrch=strtok(NULL," ");
	        strcpy ( newinfo.ancrpt.stn1, arrch);
		arrch=strtok(NULL," ");
	        strcpy ( dud, arrch);
		arrch=strtok(NULL," ");
	        newinfo.ancrpt.dist2 = atoi(arrch);
		arrch=strtok(NULL," ");
	        strcpy ( newinfo.ancrpt.dirct2, arrch);
		arrch=strtok(NULL," ");
	        strcpy ( newinfo.ancrpt.stn2, arrch);
	    }
	    else if ( strcmp(first,"ENDPOINT (VOR,nm)") == 0 ) {
		arrch=strtok(cptr," ");
	        newinfo.vorrpt.dist1 = atoi(arrch);
		arrch=strtok(NULL," ");
	        strcpy ( newinfo.vorrpt.dirct1, arrch);
		arrch=strtok(NULL," ");
	        strcpy ( newinfo.vorrpt.stn1, arrch);
		arrch=strtok(NULL," ");
		arrch=strtok(NULL," ");
	        newinfo.vorrpt.dist2 = atoi(arrch);
		arrch=strtok(NULL," ");
	        strcpy ( newinfo.vorrpt.dirct2, arrch);
		arrch=strtok(NULL," ");
	        strcpy ( newinfo.vorrpt.stn2, arrch);
	    }
	    else if ( strcmp(first,"ATTRIB (ANC,sm)") == 0 ) {
		arrch=strtok(cptr," ");
	        newinfo.ancatt.dist = atoi(arrch);
		if ( newinfo.ancatt.dist >= 0 ){
		arrch=strtok(NULL," ");
		if ( strcmp ( arrch, "EW") == 0 )  
	            strcpy ( newinfo.ancatt.dirc, "EAST AND WEST");
		if ( strcmp ( arrch, "NS") == 0 )  
	            strcpy ( newinfo.ancatt.dirc, "NORTH AND SOUTH");
		if ( strcmp ( arrch, "ESOL") == 0 )  
	            strcpy ( newinfo.ancatt.dirc, "EITHER SIDE");
		}
	    }
	    else if ( strcmp(first,"ATTRIB (VOR,nm)") == 0 ) {
		arrch=strtok(cptr," ");
	        newinfo.voratt.dist = atoi(arrch);
		if ( newinfo.voratt.dist >= 0 ){
		arrch=strtok(NULL," ");
		if ( strcmp ( arrch, "EW") == 0 )  
	            strcpy ( newinfo.voratt.dirc, "E/W");
		if ( strcmp ( arrch, "NS") == 0 )  
	            strcpy ( newinfo.voratt.dirc, "N/S");
		if ( strcmp ( arrch, "ESOL") == 0 )  
	            strcpy ( newinfo.voratt.dirc, "EITHER SIDE");
		}
	    }
	    else if ( strcmp(first,"WATCH CORNER POINT") == 0 ) {
		++wcnt;
		switch ( wcnt ) {
		    case 1:
		 	arrch=strtok(cptr," ");
	         	newinfo.wcpnt1.lat = (float)atof(arrch);
		 	arrch=strtok(NULL," ");
	                newinfo.wcpnt1.lon = (float)atof(arrch);
		        break;
		    case 2:
		 	arrch=strtok(cptr," ");
	         	newinfo.wcpnt2.lat = (float)atof(arrch);
		 	arrch=strtok(NULL," ");
	                newinfo.wcpnt2.lon = (float)atof(arrch);
		        break;
		    case 3:
		 	arrch=strtok(cptr," ");
	         	newinfo.wcpnt3.lat = (float)atof(arrch);
		 	arrch=strtok(NULL," ");
	                newinfo.wcpnt3.lon = (float)atof(arrch);
		        break;
		    case 4:
		 	arrch=strtok(cptr," ");
	         	newinfo.wcpnt4.lat = (float)atof(arrch);
		 	arrch=strtok(NULL," ");
	                newinfo.wcpnt4.lon = (float)atof(arrch);
		        break;
		    default:
		        break;
	        }
	    }
	    else if ( strcmp(first,"HAIL SIZE (in)") == 0 ) {
	         newinfo.hailsz = (float)atof(cptr);
	    }
	    else if ( strcmp(first,"MAX GUSTS (kts)") == 0 ) {
	        newinfo.maxgust = atoi(cptr);
		flmile = (float)(newinfo.maxgust);
                flmile = pr_knmh( &flmile);
		miles  = (int)flmile;

	       /*
		* Rounding the mph to the nearest 5.
		*/
	            newinfo.maxmph = ( miles / 5 ) * 5;
                    if ( flmile - (float)(newinfo.maxmph) >= 2.500F ) {
		        newinfo.maxmph += 5;
		    }
	    }
	    else if ( strcmp(first,"MAX TOPS (100s ft)") == 0 ) {
	         newinfo.maxtops = atoi(cptr);
	    }
	    else if ( strcmp(first,"MOTION (deg,kts)") == 0 ) {

		arrch=strtok(cptr," ");

		if ( arrch == NULL ) {
	            newinfo.motion.deg   = 0;
	            newinfo.motion.speed = 0;
		}
		else {
	            newinfo.motion.deg = atoi(arrch);
		    arrch=strtok(NULL," ");
	            newinfo.motion.speed = atoi(arrch);
		}

	    }
	    else if ( strcmp(first,"TIME ZONE") == 0 ) {
	        strcpy ( newinfo.timzone, cptr );
	    }
	    else if ( strcmp(first,"REPL WATCH NUMBER") == 0 ) {
		arrch = strtok ( cptr," ");
		i = 0;
		while ( arrch != NULL ) {
	            strcpy ( newinfo.replcnm[i], arrch );
		    arrch = strtok( NULL," " );
                    i++;
		}
		newinfo.wwrepnm = i-1;
	    }
	    else if ( strcmp(first,"STATES INCLUDED") == 0 ) {
	        strcpy ( newinfo.states, cptr );
	    }
	    else if ( strcmp(first,"STATUS") == 0 ) {
	        strcpy ( newinfo.status, cptr );
	    }
	    else if ( strcmp(first,"FORECASTER") == 0 ) {
	        strcpy ( newinfo.frcstr, cptr );
	    }
	    else if ( strcmp(first,"WATCH AREA (sq mi)") == 0 ) {
	        newinfo.warea = atoi(cptr);
	    }  
	    else if ( ( cptr == NULL ) && ( ierr != 4) ) {
	        cptr = strtok ( buff, " " );
		if ( strcmp(cptr,"UGC") != 0 ) {
	            strcpy ( newinfo.cnty[cntot].ugc, cptr );
		    arrch=strtok(NULL," ");
	            strcpy ( newinfo.cnty[cntot].state, arrch );
		    arrch=strtok(NULL," ");
	            strcpy ( newinfo.cnty[cntot].cname, arrch );
		    arrch=strtok(NULL," ");
	            newinfo.cnty[cntot].ctylat = (float)atof(arrch);
		    arrch=strtok(NULL," ");
	            newinfo.cnty[cntot].ctylon = (float)atof(arrch);
		    arrch=strtok(NULL," ");
	            strcpy ( newinfo.cnty[cntot].fips, arrch );
		    arrch=strtok(NULL," ");
	            strcpy ( newinfo.cnty[cntot].wfo, arrch);
	            cntot++;
		}  
	    }

	}
        newinfo.total = cntot;
} 
