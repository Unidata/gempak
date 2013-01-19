#include "nwx_cmn.h"

char		dateTime[ 30 ];

int	 	_idtyp_save = -1;
int		_timeCoveredType = EVENT;

usrslct_t	usrSelect;
plotdata_t	plotData;

char		printText[ PRNMAX ]; 
char		reportText[ REPMAX ];
char		wtchText[ MAX_WATCHES ][ REPMAX ];

/************************************************************************
 * nwxp_pdata.c								*
 * 									*
 * This module manages the major processing data elements, including    *
 * the text report, usrSelect structure, and plotData structures.	*
 *									*
 *  CONTENTS:								*
 *	pdata_init()		initialize the data structures		*
 *	pdata_setReportText()	store the input report text		*
 *	pdata_getReportText()	return the stored report text		*
 *	pdata_setDateTime()	store the report date and time		*
 *	pdata_getDateTime()	get the current report date and time	*
 *	pdata_getTimeCovered()	get the time covered type		*
 *	pdata_setTimeCovered()	set the time covered type		*
 ***********************************************************************/

/*=====================================================================*/

void pdata_init ( void  )
/************************************************************************
 * pdata_init								*
 *									*
 * Initialize the data structures					*
 *									*
 * pdata_init ( void )    	 					*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *			None						*
 **									*
 * Log:                                                                 *
 * E. Safford/SAIC	12/07	initial coding				*
 ***********************************************************************/
{
    /*int		ii; */
/*---------------------------------------------------------------------*/
    plotData.mode 	= EMPTY;
    usrSelect.selct_by 	= STATION;
    usrSelect.ndttm	= 1;
    usrSelect.ddttm	= usrSelect.ndttm;
    usrSelect.mapindx	= 0;
    usrSelect.prvnxt	= 0;
    usrSelect.zoomflg	= NO_ZOOM;
    srchInfo.smethod	= STANDARD;	

    _timeCoveredType    = EVENT;

/*
    printText[0]	= '\0';
    reportText[0]	= '\0';
   
    for( ii=0; ii<MAX_WATCHES; ii++ ) {
        wtchText[ ii ][ 0 ] = '\0';
    }
*/
}
/*=====================================================================*/

void pdata_setReportText ( char *text, int *iret  )
/************************************************************************
 * pdata_setReportText							*
 *									*
 * Set the report text.							*
 *									*
 * pdata_setReportText ( text, iret )					*
 *									*
 * Input parameters:                                                    *
 *	*text		char		text report contents		*	
 *									*
 * Output parameters:                                                   *
 *	*iret		int		error code			*
 **									*
 * Log:                                                                 *
 * E. Safford/SAIC	12/07	initial coding				*
 ***********************************************************************/
{
    *iret = 0;

    if ( text != NULL ) {
        strcpy( reportText, text );
    }
    else {
        *iret = -1;
    }
}

/*=====================================================================*/

void pdata_getReportText ( char *report, int *iret  )
/************************************************************************
 * pdata_setReportText							*
 *									*
 * Get a copy of the current report text.  Note that the report text    *
 * is of size REPMAX (defined in gemprm.h), so the calling routine      *
 * should send a string able to contain a string of that size.		*
 *									*
 * void pdata_getReportText ( report, iret )				*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *	*iret		int		error code			*
 *      *report		*char		text report contents		*	
 **									*
 * Log:                                                                 *
 * E. Safford/SAIC	12/07	initial coding				*
 ***********************************************************************/
{
    *iret = 0;
    strcpy( report, reportText );
}

/*=====================================================================*/

void pdata_setDateTime ( char *text, int *iret  )
/************************************************************************
 * pdata_setDateTime  							*
 *									*
 * Set the date/time.  							*
 *									*
 * pdata_setDateTime ( text, iret )					*
 *									*
 * Input parameters:                                                    *
 *	*text		char		date/time string    		*	
 *									*
 * Output parameters:                                                   *
 *	*iret		int		error code			*
 **									*
 * Log:                                                                 *
 * E. Safford/SAIC	12/07	initial coding				*
 ***********************************************************************/
{
    *iret = 0;

    if ( text != NULL ) {
        strcpy( dateTime, text );
    }
    else {
        *iret = -1;
    }
}

/*=====================================================================*/

void pdata_getDateTime ( char *date, int *iret  )
/************************************************************************
 * pdata_getDateTime							*
 *									*
 * Get a copy of the current date/time.  				* 
 *									*
 * void pdata_getDateTime ( date, iret )				*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *	*iret		int		error code			*
 *      *report		*char		text report contents		*	
 **									*
 * Log:                                                                 *
 * E. Safford/SAIC	12/07	initial coding				*
 ***********************************************************************/
{
    *iret = 0;
    strcpy( date, dateTime );
}

/*=====================================================================*/

int pdata_getTimeCovered ( void  )
/************************************************************************
 * pdata_getTimeCovered							*
 *									*
 * Return the value of the _timeCoveredType flag (EVENT or SCHEDULED).	*
 *									*
 * int pdata_getTimeCovered ( void )					*
 *									*
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *			None						*
 * Return:								*
 *	     		int		current _timeCoveredType	*
 **									*
 * Log:                                                                 *
 * E. Safford/SAIC	12/07	initial coding				*
 ***********************************************************************/
{
    return( _timeCoveredType ); 
}

/*=====================================================================*/

void pdata_setTimeCovered ( int newTimeCovered, int *iret  )
/************************************************************************
 * pdata_setTimeCovered							*
 *									*
 * Set the value of the _timeCoveredType flag (EVENT or SCHEDULED).	*
 *									*
 * void pdata_getTimeCovered ( newTimeCovered, iret )			*
 *									*
 * Input parameters:                                                    *
 *	newTimeCovered	int	either EVENT or SCHEDULED		*
 *									*
 * Output parameters:                                                   *
 *	*iret		int	return code				*
 *				   0 = normal				*
 *				  -1 = newTimeCovered out of range	*
 **									*
 * Log:                                                                 *
 * E. Safford/SAIC	12/07	initial coding				*
 ***********************************************************************/
{
    *iret = G_NORMAL;

    if( newTimeCovered != EVENT && newTimeCovered != SCHEDULED ) {
        *iret = -1;
    }

    if( *iret == G_NORMAL ) {
        _timeCoveredType = newTimeCovered;
    }
}

/*=====================================================================*/
