#include "nwx_cmn.h"


#define    DTTM_STR_LEN   49

srchinfo_t srchInfo;


void sstruct_sdttm ( char *user_dattim, int *iret );

/************************************************************************
 * nwxp_sstruct.c                                                       *
 *                                                                      *
 * This module manages the search structure criteria.  			*
 *                                                                      *
 * CONTENTS:                                                            *
 *      sstruct_sdttm()  sets beginning and ending search date/time.    *
 *      sstruct_stxtmk() sets the record breakers.    			*
 *                                                                      *
 ***********************************************************************/

/*=====================================================================*/

void sstruct_sdttm ( char *user_dattim, int *iret )
/************************************************************************
 * sstruct_sdttm							*
 *									*
 * This routine will set the user requested date/time.			*
 * The format of user_dattim is "latest_time - earlieast time"		*
 *									*
 * sstruct_sdttm ( user_dattim, iret )					*
 *									*
 * Input parameters:							*
 *	*user_dattim	char		Requested date/time		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 7/94						*
 * C. Lin/EAI		 8/95   modified from nwx2.1			*
 * D.Plummer/NCEP	12/95	Reorganize for fosd type W data         *
 ***********************************************************************/
{
int		iyear, imonth, iday, ihour, ier;
char		*tpos, dttm1[DTTM_STR_LEN], dttm2[DTTM_STR_LEN];

/*---------------------------------------------------------------------*/
	*iret = G_NORMAL;

/*
 * Set the user date/time information.
 */
	if  ( ( tpos = strchr( user_dattim, '-' ) ) != NULL ) {

/*
 * given starting and ending time
 */
	    strncpy( dttm1, user_dattim, (size_t)(tpos-user_dattim) );
	    dttm1[tpos-user_dattim+1] = CHNULL;
	    dttm_cnvt( dttm1, &iyear, &imonth, &iday, &ihour, &ier );
	    srchInfo.endd.year  = iyear;
	    srchInfo.endd.month = imonth;
	    srchInfo.endd.day   = iday;
	    srchInfo.endd.hour  = ihour;

	    strcpy( dttm2, tpos+1 );
	    dttm_cnvt( dttm2, &iyear, &imonth, &iday, &ihour, &ier );
	    srchInfo.startd.year  = iyear;
	    srchInfo.startd.month = imonth;
	    srchInfo.startd.day   = iday;
	    srchInfo.startd.hour  = ihour;
	}
	else {
/*
 * Only the end date/time is given
 */
	    srchInfo.startd.year  = -1;
	    srchInfo.startd.month = -1;
	    srchInfo.startd.day   = -1;
	    srchInfo.startd.hour  = -1;

	    strcpy( dttm1, user_dattim );
	    dttm_cnvt( dttm1, &iyear, &imonth, &iday, &ihour, &ier );
	    srchInfo.endd.year  = iyear;
	    srchInfo.endd.month = imonth;
	    srchInfo.endd.day   = iday;
	    srchInfo.endd.hour  = ihour;
	}
}

/*=====================================================================*/

void sstruct_stxtmk ( int idtype, struct datatype_list *dtypinfo, int *iret )
/************************************************************************
 * sstruct_stxtmk                                                       *
 *                                                                      *
 * This routine will set the start and end of text markers for 		*
 * searching for the beginning and ending of the report text.           *
 *                                                                      *
 * void sstruct_stxtmk ( idtype, dtypinfo, iret )			*
 *									*
 * Input parameters:                                                    *
 *      idtype           int      Index into data type struct array     *
 *      *dtypinfo        struct  datatype_list   Data type struct array *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int       Return code                     	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Jacobs/NMC         7/94                                           *
 * C. Lin/EAI            9/95 	modified from nwx2.1			*
 * D.W.Plummer          12/95   Add "W"-type data type WTCH/WRN		*
 * D.W.Plummer           2/96   Add "M"-type data type for MOS  	*
 * L. Williams/EAI	 5/96	change search info for "Z" type		*
 * K. Tyle/GSC		 3/97	Add "F"-type data type for FFG		*
 * D.W.Plummer/NCEP	 3/99	Moved type "B" to general case ^A ^C	*
 ***********************************************************************/
{
static  int idtyp_save = -1;

/*---------------------------------------------------------------------*/
        *iret = G_NORMAL;

	if ( idtyp_save == idtype ) 

		return;

	else
		idtyp_save = idtype;

/*
 * Set the start- and end-of-text markers.
 */

		switch ( dtypinfo[idtype].bsflag[0] ) {
		case 'R' :
		case 'F' :

/*
 * For station type data,
 *     s_o_t[0] = Record Separator     (^^)
 *     s_o_t[1] = Control A            (^A)
 *     e_o_t[0] = Record Separator     (^^)
 *     e_o_t[1] = Control C            (^C)
 */
			srchInfo.start_of_text[0][0] = CHRS;
			srchInfo.start_of_text[0][1] = CHNULL;
			srchInfo.start_of_text[1][0] = CHCTLA;
			srchInfo.start_of_text[1][1] = CHNULL;
			srchInfo.end_of_text[0][0]   = CHRS;
			srchInfo.end_of_text[0][1]   = CHNULL;
			srchInfo.end_of_text[1][0]   = CHCTLC;
			srchInfo.end_of_text[1][1]   = CHNULL;

			break;

		case 'M' :

/*
 * For MOS type data,
 *     s_o_t[0] = Null
 *     s_o_t[1] = Control A            (^A)
 *     e_o_t[0] = Record Separator     (^^)
 *     e_o_t[1] = Control C            (^C)
 */
			srchInfo.start_of_text[0][0] = CHNULL;
			srchInfo.start_of_text[0][1] = CHNULL;
			srchInfo.start_of_text[1][0] = CHCTLA;
			srchInfo.start_of_text[1][1] = CHNULL;
			srchInfo.end_of_text[0][0]   = CHRS;
			srchInfo.end_of_text[0][1]   = CHNULL;
			srchInfo.end_of_text[1][0]   = CHCTLC;
			srchInfo.end_of_text[1][1]   = CHNULL;

			break;

		case 'Z' :

/*
 *     s_o_t[0] = Null
 *     s_o_t[1] = Null
 *     e_o_t[0] = Control C   (^C)
 *     e_o_t[1] = Null
 */
			srchInfo.start_of_text[0][0] = CHNULL;
			srchInfo.start_of_text[1][0] = CHNULL;
			srchInfo.end_of_text[0][0]   = CHCTLC;
			srchInfo.end_of_text[0][1]   = CHNULL;
			srchInfo.end_of_text[1][0]   = CHNULL;
	
			break;

		case 'B' :
		default :

/*
 * For bulletin type data or a general search,
 *   s_o_t[0] = Control A   (^A)
 *   s_o_t[1] = Null
 *   e_o_t[0] = Control C   (^C)
 *   e_o_t[1] = Null
 */
			srchInfo.start_of_text[0][0] = CHCTLA;
			srchInfo.start_of_text[0][1] = CHNULL;
			srchInfo.start_of_text[1][0] = CHNULL;
			srchInfo.end_of_text[0][0]   = CHCTLC;
			srchInfo.end_of_text[0][1]   = CHNULL;
			srchInfo.end_of_text[1][0]   = CHNULL;
	
			break;
        }
}
