#include "geminc.h"
#include "gemprm.h"
#include "nmap_data.h"
#include "nmap_dttm.h"


/************************************************************************
 * nmap_dttm.c                                                          *
 *                                                                      *
 * This module contains the 'dttm' object for nmap.     		*
 *                                                                      *
 * CONTENTS:                                                            *
 *      dttm_copy()             copy one dttm struct to another         *
 *      dttm_getCurrentTime()   get the current time in 'dttm' format.  *
 ***********************************************************************/

/*=====================================================================*/

void dttm_copy ( dttmi_t *dttm1, dttmi_t *dttm2 )
/************************************************************************
 * dttm_copy                                              		*
 *                                                                      *
 * This function copies 'dttmi_t' struct dttm2 into dttm1.    		*
 *                                                                      *
 * void dttm_copy(dttm1, dttm2)                   			*
 *                                                                      *
 * Input parameters:                                                    *
 *  *dttm1	 dttmi_t  pointer to the destination date/time structure*
 *  *dttm2	 dttmi_t  pointer to the source date/time structure 	*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      10/96  						*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

        dttm1->year   = dttm2->year;
        dttm1->mon    = dttm2->mon;
        dttm1->day    = dttm2->day;
        dttm1->hour   = dttm2->hour;
        dttm1->min    = dttm2->min;

}

/*=====================================================================*/

void dttm_getCurrentTime ( dttmi_t *cdttm )
/************************************************************************
 * dttm_getCurrentTime                                                  *
 *                                                                      *
 * This function gets the current time in 'dttm' format.                *
 *                                                                      *
 * void dttm_getCurrentTime(cdttm)                                      *
 *                                                                      *
 * Input/Output parameters:                                             *
 *  *cdttm      dttmi_t  pointer to a 'dttmi_t' structure.              *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      10/96                                                *
 ***********************************************************************/
{
time_t    t;
struct tm *ts;
/*---------------------------------------------------------------------*/

        /*
         * get current time
         */
        t = time(NULL);
        ts = gmtime(&t);
        cdttm->year = 1900 + ts->tm_year;
        cdttm->mon  = ts->tm_mon;
        cdttm->day  = ts->tm_mday;
        cdttm->hour = ts->tm_hour;
        cdttm->min  = ts->tm_min;

}

/*=====================================================================*/
