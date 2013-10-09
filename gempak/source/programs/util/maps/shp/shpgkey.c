#include "geminc.h"
#include "gemprm.h"
#include "shpprm.h"

void shp_gkey ( shp_record *onerec, char *reckey, int *iret )
/************************************************************************
 * shp_gkey                                                             *
 *                                                                      *
 * This function gets the record key in order to combine those     	*
 * records with the same key.						*
 *                                                                      *
 * shp_gkey ( onerec, reckey, iret )                           		*
 *                                                                      *
 * Input parameters:                                                    *
 *      *onerec         shp_record  	One shape record		*
 *                                                                      *
 * Output parameters:                                                   *
 *	*reckey		char		Record key         		*
 *      *iret           int             Return code                     *
 *                                        0 = Normal                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC          3/04   	Initial coding                  *
 * R. Tian/SAIC          2/05   	Added MAP_FIRE/RECR/CWAB        *
 * R. Tian/SAIC          4/05   	Added MAP_RFCB			*
 * R. Tian/SAIC          6/05   	Added MAP_TMZN/USST		*
 * S. Jacobs/NCEP	 4/10		Added MAP_NPSA			*
 * S. Jacobs/NCEP	 3/11		Changed keys for MAP_MZCN	*
 * X. Guo/CWS            9/11           Changed SHAPE_LENG to COUNTYNAME*
 * X. Guo/CWS            4/12           Changed AREA to Shape_Area for  *
 *                                      MAP_NPSA                        *
 ***********************************************************************/
{
    char stabbr[3];
    int ifips, izone, istno;
    int ifld, jfld;
    int len, ier;
    static long seqnum = 0;
/*---------------------------------------------------------------------*/
    *iret = 0;
    seqnum++;

    switch ( maptyp ) {
        case MAP_CNTY:
            /*
             * County.
             */
            for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
		if ( strncmp ( onerec->fields[ifld].name, "FIPS", 
		    strlen("FIPS") ) == 0 ) {
		    ifips = strtol ( onerec->fields[ifld].data, 
		        NULL, 10 );
		    if ( ifips == 0 ) {
		        *iret = 1;
		    }
	            sprintf ( reckey, "%d", ifips );
		    break;
	        }
	    }
	break;

        case MAP_MARI:
            /*
             * Marine.
             */
            for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
		if ( strncmp ( onerec->fields[ifld].name, "ID", 
		    strlen("ID") ) == 0 ) {
		    strncpy ( reckey, onerec->fields[ifld].data, 6 );
		    reckey[6] = '\0';
		    if ( strlen ( reckey ) == (size_t)0 ) {
		        *iret = 1;
		    }
		    break;
	        }
	    }
	break;

        case MAP_MZCN:
            /*
             * Combined marine-county.
             */
	    for ( jfld = 0; jfld < onerec->numfld; jfld++ ) {
                if ( strncmp ( onerec->fields[jfld].name, "COUNTYNAME", 
		    strlen("COUNTYNAME") ) == 0 ) {
 
                    /*
                     * County.
                     */
                    for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
		        if ( strncmp ( onerec->fields[ifld].name, "FIPS", 
			    strlen("FIPS") ) == 0 ) {
		            ifips = strtol ( onerec->fields[ifld].data, 
			        NULL, 10 );
		            if ( ifips == 0 ) {
		                *iret = 1;
		            }
	                    sprintf ( reckey, "%d", ifips );
			    break;
	                }
	            }
		    break;
                } else if ( strncmp ( onerec->fields[jfld].name, "NAME", 
		    strlen("NAME") ) == 0 ) {
 
                    /*
                     * Marine.
                     */
                    for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
		        if ( strncmp ( onerec->fields[ifld].name, "ID", 
			    strlen("ID") ) == 0 ) {
		            strncpy ( reckey, onerec->fields[ifld].data, 6 );
		            reckey[6] = '\0';
		            if ( strlen ( reckey ) == (size_t)0 ) {
		                *iret = 1;
		            }
			    break;
	                }
	            }
		    break;
	        }
	    }
	break;

        case MAP_ZONE:
        case MAP_FIRE:
            /*
             * Public Forecast Zones and Fire Weather Zone.
             */
            for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
                if ( strcmp ( onerec->fields[ifld].name, "STATE" ) == 0 ) {
	            strcpy ( stabbr, onerec->fields[ifld].data );
		} else if ( strncmp ( onerec->fields[ifld].name, "ZONE", 
		    strlen("ZONE") ) == 0 ) {
		    izone = strtol ( onerec->fields[ifld].data, NULL, 10 );
	        }
	    }
	    for ( istno = 0; istno < NUMSTNO; istno++ ) {
	        if ( strncmp ( stateno[istno], stabbr, 2 ) == 0 ) {
	            break;
	        }
	    }
	    if ( istno == NUMSTNO ) {
	        *iret = 1;
		return;
	    }
	    ifips = istno * 10000 + izone * 10;
            sprintf ( reckey, "%d", ifips );
	break;

        case MAP_IEDG:
            /*
             * Ice Edge.
             */
            sprintf ( reckey, "%ld", seqnum );
        break;

        case MAP_RFCR:
            /*
             * River Forecast Center Regions.
             */
            for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
		if ( strncmp ( onerec->fields[ifld].name, "BASIN_ID",
		    strlen("BASIN_ID") ) == 0 ) {
		    strncpy ( reckey, onerec->fields[ifld].data, 5 );
		    reckey[5] = '\0';
		    cst_rmbl ( reckey, reckey, &len, &ier );
		    if ( len == 0 ) {
		        *iret = 1;
		    }
		    break;
	        }
	    }
        break;

        case MAP_CWAB:
            /*
             * County Warning Area.
             */
            for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
		if ( strncmp ( onerec->fields[ifld].name, "WFO",
		    strlen("WFO") ) == 0 ) {
		    strncpy ( reckey, onerec->fields[ifld].data, 3 );
		    reckey[3] = '\0';
		    cst_rmbl ( reckey, reckey, &len, &ier );
		    if ( len == 0 ) {
		        *iret = 1;
		    }
		    break;
	        }
	    }
        break;

        case MAP_RFCB:
            /*
             * River Forecast Center Basins.
             */
            for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
		if ( strncmp ( onerec->fields[ifld].name, "ID",
		    strlen("ID") ) == 0 ) {
		    strncpy ( reckey, onerec->fields[ifld].data, 8 );
		    reckey[8] = '\0';
		    cst_rmbl ( reckey, reckey, &len, &ier );
		    if ( len == 0 ) {
		        sprintf ( reckey, "%ld", seqnum );
		    }
		    break;
	        }
	    }
        break;

        case MAP_TMZN:
            /*
             * Time Zone.
             */
            for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
		if ( strncmp ( onerec->fields[ifld].name, "TIMEZONE",
		    strlen("TIMEZONE") ) == 0 ) {
		    strcpy ( reckey, onerec->fields[ifld].data );
		    cst_rmbl ( reckey, reckey, &len, &ier );
		    if ( len == 0 ) {
		        sprintf ( reckey, "%ld", seqnum );
		    }
		    break;
	        }
	    }
        break;

        case MAP_USST:
            /*
             * US State and Territories.
             */
            for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
		if ( strncmp ( onerec->fields[ifld].name, "FIPS", 
		    strlen("FIPS") ) == 0 ) {
		    ifips = strtol ( onerec->fields[ifld].data, 
		        NULL, 10 );
		    if ( ifips == 0 ) {
		        *iret = 1;
		    }
	            sprintf ( reckey, "%d", ifips );
		    break;
	        }
	    }
        break;

        case MAP_NPSA:
            /*
             * Predictive Service Areas
             */
	    for ( ifld = 0; ifld < onerec->numfld; ifld++ ) {
		if ( strncmp ( onerec->fields[ifld].name, "Shape_Area", 
		    strlen("Shape_Area") ) == 0 ) {
		    strncpy ( reckey, onerec->fields[ifld].data, 19 );
		    reckey[19] = '\0';
		    if ( strlen ( reckey ) == (size_t)0 ) {
			*iret = 1;
		    }
		    break;
		}
	    }
        break;
    }
}
