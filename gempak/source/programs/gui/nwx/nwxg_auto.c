
#include "nwx_cmn.h"
#include "nwx_gui.h" 


static XtIntervalId     _timeOutId = (XtIntervalId)NULL;
extern XtAppContext     _appContext;

/*
 *  private callback functions
 */
void auto_checkNewFileCb ( XtPointer, XtIntervalId );

/************************************************************************
 * nwxg_auto.c                                                          *
 *                                                                      *
 * This module implements the auto update feature for NWX.	        *
 *                                                                      *
 * CONTENTS:                                                            *
 *                                                                      *
 *  auto_startAutoUpdt()        start the timed callbacks for auto updt *
 *  auto_stopAutoUpdt()         stop the timed callbacks for auto updt  *
 *  auto_checkNewFileCb()       callback routine for update             *
 ***********************************************************************/

/*=====================================================================*/

void auto_startAutoUpdt ( void )
/************************************************************************
 * auto_startAutoUpdt                                                   *
 *                                                                      *
 * This function checks the auto update flag and adds a timed callback  *
 * if the flag is ON(0).						*
 *                                                                      *
 * void auto_startAutoUpdt ()                                           *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		11/03	initial coding				*
 * E. Safford/SAIC	11/07	use idata_getAutoUpdt()			*
 ***********************************************************************/
{
    /*
     *  Clear out all existing timed callbacks.
     */
    auto_stopAutoUpdt();


    if( idata_getAutoUpdt() == G_FALSE ) {
        /*
 	 *  Initiate callback in 300 seconds
         */
        _timeOutId = XtAppAddTimeOut (_appContext, 300000L,
                        (XtTimerCallbackProc)auto_checkNewFileCb,
                                                (XtPointer)NULL );
    }
}

/*=====================================================================*/

void auto_stopAutoUpdt ( void )
/************************************************************************
 * auto_stopAutoUpdt                                                    *
 *                                                                      *
 * This function removes the _timeOutId callback from the application   *
 * and sets the _timeOutId to NULL.                                     *
 *                                                                      *
 * void auto_stopAutoUpdt ( )                                           *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		11/03	initial coding				*
 ***********************************************************************/
{
    if (_timeOutId !=(XtIntervalId)NULL) {
        XtRemoveTimeOut(_timeOutId);
        _timeOutId = (XtIntervalId)NULL;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void auto_checkNewFileCb ( XtPointer clnt, XtIntervalId id )
/************************************************************************
 * auto_checkNewFileCb                                                  *
 *                                                                      *
 * Returns the automated check for an updated file.                     *
 *                                                                      *
 * void auto_checkNewFileCb ( clnt, id )                         	*
 *                                                                      *
 * Input parameters:                                                    *
 *  clnt		XtPointer	which loop                      *
 *  id			XtIntervalId	not used                        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return value:                                                        *
 *              NONE                                                    *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		11/03   initial coding                          *
 * T. Piper/SAIC	01/04	added nwxerr				*
 * E. Safford/SAIC	11/07	use wnxm_NxmCursor_setCursor		* 
 * E. Safford/SAIC	12/07	rename nwxerr to err_showError()	*
 ***********************************************************************/
{
    int ier;
/*---------------------------------------------------------------------*/
/*
 * Call the dslw routine to do the actual update work.
 */
    wnxm_NxmCursor_setCursor ( dataSelectW, CURS_BUSY );
    dslw_load ( 2, &ier );
    wnxm_NxmCursor_setCursor ( dataSelectW, CURS_DEFAULT ); 
    if ( ier < -7 ) {
	err_showError ( ier );
    }
}
