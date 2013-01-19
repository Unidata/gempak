#include "geminc.h"
#include "gemprm.h"
#include "nmapprm.h"
#include "nmap_data.h"


static XtIntervalId	_timeOutId = (XtIntervalId)NULL;
extern XtAppContext	_appContext;

static Boolean		_actvUpdt[MAX_LOOP];


/*
 *  private callback functions
 */
void auto_checkNewFileCb ( XtPointer, XtIntervalId );

/*
 *  private functions
 */
void auto_checkTimes ( int lp, int *nfiles );
void auto_loadFrames ( int *ld_files );

/************************************************************************
 * nmap_auto.c								*
 *									*
 * This module implements the auto update feature for nmap2.            *
 *									*
 * CONTENTS:								*
 *									*
 *  auto_startAutoUpdt()	start the timed callbacks for auto updt *
 *  auto_stopAutoUpdt()		stop the timed callbacks for auto updt  *
 *									*
 *  auto_getAutoUpdt()		get the status of auto update for a loop*
 *									*
 *  auto_checkNewFileCb()	callback routine for update         	*
 *									*
 *  auto_checkTimes()		check times to see if update required	*
 *									*
 *  auto_loadFrames()		load all frames that need updating	*
 ***********************************************************************/

/*=====================================================================*/

void auto_startAutoUpdt ( void )
/************************************************************************
 * auto_startAutoUpdt	                                                *
 *                                                                      *
 * This function checks each loop for image data and the status of the  *
 * auto update flag.  If a loop contains image data and the auto update *
 * flag is true, then a timed callback is engaged to check for new 	*
 * files.								*
 *                                                                      *
 * void auto_startAutoUpdt ()	                                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       10/99   adapt dataw_startAutoUpdate for nmap2   *
 * E. Safford/GSC	11/99	change auto_update to a single timer	*
 * E. Safford/GSC	06/01	add check to loopw_isHideActv()		*
 ***********************************************************************/
{
int		ii;
Boolean		start_timer;
/*---------------------------------------------------------------------*/

/*
 *  Clear out all existing timed callbacks.
 */
    auto_stopAutoUpdt();

    start_timer = FALSE;

/*
 *  Now restart each loop's timer.
 */
    for (ii=0; ii < MAX_LOOP; ii++) {
    
	if ( auto_getAutoUpdt(ii) && !loopw_isHideActv() ) { 
	    _actvUpdt[ii] = TRUE;
	    start_timer = TRUE;
	}
	else {
	    _actvUpdt[ii] = FALSE;
	    _timeOutId = (XtIntervalId)NULL;
	}
    }

    if (start_timer) {
/*
 *  Initiate callback in 30 seconds
 */
	_timeOutId = XtAppAddTimeOut (_appContext, 30000L,
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
 * E. Safford/GSC       01/99   initial coding                          *
 * E. Safford/GSC	10/99	moved to nmap_auto.c, updated for nmap2 *
 * E. Safford/GSC	11/99	change to a single timer for all loops  *
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/

    if (_timeOutId !=(XtIntervalId)NULL) {

        XtRemoveTimeOut(_timeOutId);
        _timeOutId = (XtIntervalId)NULL;

    }

    for (ii=0; ii < MAX_LOOP; ii++) {
	_actvUpdt[ii] = FALSE;
    }
}


/*=====================================================================*/

Boolean auto_getAutoUpdt ( int lp )
/************************************************************************
 * auto_getAutoUpdt	                                                *
 *                                                                      *
 * This function checks the specified loop for image data and the status*
 * of the auto update flag.  If image data is loaded and the auto update*
 * flag is true, then TRUE is returned.                            	*
 *                                                                      *
 * Boolean auto_getAutoUpdt ( lp )	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp		int	loop number					*
 *                                                                      *
 * Output parameters:                                                   *
 * auto_getAutoUpdt	Boolean		True if auto update is on and   *
 *						img data is loaded	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       10/99   initial coding                          *
 * E. Safford/GSC	05/00	use dataw_isImgDom			*
 ***********************************************************************/
{
    return ( (Boolean)(dataw_isImgDom(lp) && loop_getAutoUpdt(lp)) );
} 

/*=====================================================================*/
/* ARGSUSED */
void auto_checkNewFileCb ( XtPointer clnt, XtIntervalId id )
/************************************************************************
 * auto_checkNewFileCb                                                  *
 *                                                                      *
 * Returns the automated check for an updated file.                     *
 *                                                                      *
 * void auto_checkNewFileCb ( clnt, id )	                        *
 *                                                                      *
 * Input parameters:                                                    *
 *  clnt	XtPointer     which loop                              *
 *  id            XtIntervalId  not used                                *
 *									*
 * Output parameters:                                                   *
 * Return value:							*
 *		NONE							*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       01/99   initial coding                          *
 * E. Safford/GSC       01/99   add check on frame time to trap no data *
 * E. Safford/GSC       02/99   update time line after load             *
 * E. Safford/GSC       02/99   fix max frame auto-update bug           *
 * E. Safford/GSC       04/99   update using a skip value               *
 * E. Safford/GSC       04/99   fix error in time line update           *
 * E. Safford/GSC       04/99   fix memory leak - free dnamelist        *
 * E. Safford/GSC       06/99   drop check of file time & clean up      *
 * E. Safford/GSC	10/99	move to nmap_auto.c, adapt for nmap2	*
 * E. Safford/GSC	11/99	fixed bug - was not updating last frame	*
 * T. Piper/SAIC	05/03	split loading data from checking times	*
 ***********************************************************************/
{
int	ii, ld_files[MAX_LOOP], nfiles;
/*---------------------------------------------------------------------*/

    for (ii=0; ii<MAX_LOOP; ii++) {
	if (_actvUpdt[ii]) {
	    auto_checkTimes(ii, &nfiles);
	    ld_files[ii] = nfiles;
	}
	else {
	    ld_files[ii] = 0;
	}
    }

    auto_loadFrames(ld_files);
/*
 *  restart the timed callback
 */
    auto_startAutoUpdt();
}

/*=====================================================================*/

void auto_checkTimes ( int lp, int *ld_files )
/************************************************************************
 * auto_checkTimes                                                      *
 *                                                                      *
 * Checks whether there are new files to update.			*
 *                                                                      *
 * void auto_checkTimes ( lp, ld_files )                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp		int	which loop 	                                *
 *									*
 * Output parameters:                                                   *
 *  *ld_files	int	number of new files to load			*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       11/99   moved from auto_checkNewFileCb          *
 * E. Safford/GSC       12/99   add xqcpyxm and remove loop_getFramePxm *
 * S. Law/GSC		01/00	reset map after loading frames		*
 * E. Safford/GSC	03/00	add call to loop_restoreLut             *
 * E. Safford/GSC	03/00	get new file list via nim_gtim()        *
 * E. Safford/GSC	03/00	initialize new_files & ld_files         *
 * E. Safford/GSC	03/00	add call to xmloop, clean up		*
 * E. Safford/GSC	04/00	remove call to xmloop (my bad)		*
 * E. Safford/GSC	05/00	shift all frame tags with updates	*
 * E. Safford/GSC	05/00	fix update w/ only 1 frame in loop    	*
 * E. Safford/GSC	06/00	advance to last frame on auto-update  	*
 * E. Safford/GSC	07/00	add mapw_setPGForLoop                 	*
 * M. Li/GSC            03/01   removed mapw_setPGForLoop               *
 * H. Zeng/EAI          04/01   moved blank map before the 1st pixmap   *
 * E. Safford/GSC	05/01	fix error in deselect loop		*
 * E. Safford/GSC	05/01	add nmp_simf update and nmp_rstrproj	*
 * H. Zeng/XTRIA        01/03   changed the para. of roamw_setup()      *
 * T. Piper/SAIC	04/03	Undid previous change, reset to TRUE	*
 *				 and moved to dsp_reloadFrame		*
 * T. Piper/SAIC	05/03	renamed auto_checkTimes and moved the	*
 *				 loading of frames to auto_loadFrames	*
 * T. Piper/SAIC	07/03	removed unused xqcpxm call		*
 * T. Lee/SAIC		08/03	added time range/interval to nim_gtim	*
 * T. Lee/SAIC		01/04	added reference time & fixed auto-update*
 * B. Yin/SAIC          03/04   changed css_gtim calling sequences      *
 * T. Lee/SAIC		04/04	called dataw_useRefTm			*
 * T. Piper/SAIC	04/07	Modified for nim_gtim CSC		*
 ***********************************************************************/
{
    int		ii, jj, kk, new_files, times, itype = 1;
    int		skip, time_comp;
    int		nfrms, from, to, total, target, cnt, ier;
    Boolean	bflag, bauto;
    dattm_t	last_tm, c_tm, ftime;
    char **tmarry=NULL;
    dttms_t	lastim;
    dsrc_t	*dom;
    Boolean	frm_tag;	
/*---------------------------------------------------------------------*/
    skip   = dataw_getSkip(lp);
    
    nfrms = loop_getNumFrames(lp);
    total = nfrms - 1;
    new_files = 0;
    *ld_files = 0;


/*
 *  If _autoUpdate has been toggled off or if the load was aborted
 *  and there are no frames in this loop then just return
 */

/* TSP - auto_getAutoUpdt already checked in auto_startAutoUpdt */
    	
    if ( !auto_getAutoUpdt(lp) || nfrms == 0 ) {
        return;
    }

/*
 *  Get the dom source; exit if it's NULL.
 */
    dom = (dsrc_t *)dataw_getDomSrc(lp);
    if (dom == (dsrc_t *) NULL) {
	return;
    }

/*
 *  get the time of the last frame 
 */
    loop_getFrameTm(lp, nfrms-1, last_tm);

    css_gtim(&itype, c_tm, &ier);

    bauto = loop_getAutoUpdt(lp);
    bflag = dataw_useRefTm(lp);
    tmln_getLastTm ( lp, lastim );
    nim_gtim (dom->attridx, c_tm, dom->range, dom->interval, 
		bflag, bauto, lastim, &times, &tmarry, &ier);

/*
 *  scan the array of times for times greater than the last frame.
 */
    for (ii=0; ii < times; ii++) {

        if ( (strcmp(tmarry[ii], last_tm) > 0 ) ) {

	    new_files++;

/*
 *  find the last time in the dom->frm list and check it
 *  against the new_time.
 */
	    target = -1;
	    for (jj=MAX_FRAME-1; jj>=0 && target < 0; jj--) {

/*
 *  skip over all empty times, looking for the first
 *  (newest) time in the frm array.
 */ 
		if (strlen (dom->frm[jj].ftime) == (size_t)0) {
		    continue;
		}

		time_comp = strcmp(tmarry[ii], dom->frm[jj].ftime);

/*
 *  If time_comp is < 0, the new time is less than the newest 
 *  frm time.  This means the user left one or more unselected
 *  frames at the end of the timeline.  We're now updating
 *  those.  We need to continue scanning until a match is found.
 */
		if ( time_comp < 0 ) {
		    continue;
		}
		else {
		    target = jj;

		    if ( time_comp > 0 ) {

/*
 *  The new_time is greater than the last ftime.  We need to move all the
 *  existing ftimes down one place, dropping the oldest, and replace the
 *  last ftime.  Transfer the selected flag values too.
 */
			for (kk=0; kk < target; kk++) {
			    strcpy (dom->frm[kk].ftime, dom->frm[kk+1].ftime);
			    dom->frm[kk].selected = dom->frm[kk+1].selected;
			}
			strcpy (dom->frm[target].ftime, tmarry[ii]);

/*
 *  initially set the selection flag of the new time to false -- this frame
 *  might not be selected once skip is taken into account.
 */
			dom->frm[target].selected = FALSE;

		    }
		}

	    }

/*
 *  Use this file to build a new frame if it matches the skip interval
 */
    	    if (skip == 0 || new_files % (skip+1) == 0) {

	        (*ld_files)++;
		dom->frm[target].selected = TRUE;

/*
 *  Move all frames times and pixmaps back one frame, dropping the
 *  oldest in the process
 */
    		for (jj=1; jj<nfrms-1; jj++) {

		    from = jj+1;
		    to   = jj;

		    xcpypxm2 (lp, from, to, &ier);

/*
 *  shift the frame times
 */
		    loop_getFrameTm (lp, from, ftime);
		    loop_setFrameTm (lp, to, ftime);

/*
 *  shift the bad frame tags
 */
		    frm_tag = xmfrmtg_getFrmTag(lp, from);
		    xmfrmtg_setFrmTag (lp, to, frm_tag);
		}

/*
 * convert the file name to a timestring format and add it
 * to the nfrms-1 position.
 */
		loop_setFrameTm (lp, nfrms-1, tmarry[ii]);	
 
/*
 *  One final check -- if more than the total number
 *  of frames in dom is set to selected then deselect extras.
 */ 
		cnt = 0;
	 	for (jj=MAX_FRAME-1; jj >= 0; jj--) {

		    if (cnt < total) {
		        if (dom->frm[jj].selected) {
			    cnt++;
			}
		    }
		    else { 
		  	dom->frm[jj].selected = FALSE;
		    }
		}
	    }
	}
	G_FREE(tmarry[ii], char);
    }
    if ( tmarry != NULL ) G_FREE(tmarry, char*);
}

/*=====================================================================*/

void auto_loadFrames(int *ld_files)
/************************************************************************
 * auto_loadFrames                                                      *
 *                                                                      *
 * Checks whether there are new files to update.                        *
 *                                                                      *
 * void auto_loadFrames ( ld_files )                                	*
 *                                                                      *
 * Input parameters:                                                    *
 *  *ld_files   int     number of new files to load                     *
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC	05/03	split from auto_checkTimes		*
 * T. Piper/SAIC	05/03	fixed auto_update bug			*
 * T. Piper/SAIC        07/03   renamed xmloop -> xmloop_loop           *
 * E. Safford/SAIC	02/04	add loop_restoreLut & mbotw_restoreFade *
 * E. Safford/SAIC	05/04	add restoreLut to currlp at end         *
 ***********************************************************************/
{
    int		currlp, ier, ii, imgtyp, lp, nfrms, nindex, total;
    int		depth;
    char	imgfile[256];
    Boolean	first, dataWasLoaded = False;
/*---------------------------------------------------------------------*/

    first = TRUE;
    depth = mcanvw_getDpth();

    for (lp=0; lp<MAX_LOOP; lp++) {
        if (ld_files[lp] > 0) {
	    if ( first ) {
		dsp_setBusy(TRUE);
		first = FALSE;
	    }
	    currlp = loop_getCurLoop ();
	    nfrms = loop_getNumFrames(lp);
	    total = nfrms - 1;
    
/*
 *  Update the image file in the nmp driver.  If this isn't done
 *  eventually the file used for navigation will get deleted, which
 *  will cause errors.
 */
	    imgtyp = NO_IMG;
	    if ( dataw_isRadSelect (lp, &nindex) ) {
	  	imgtyp = RAD_IMG;
	   	dataw_getImgInfo (lp, imgfile);
            }
	    else if ( dataw_isSatSelect(lp, &nindex) ) {
	    	imgtyp = SAT_IMG;
	    	dataw_getImgInfo (lp, imgfile);
	    }

	    if (imgtyp != NO_IMG) {
	    	nmp_simf (lp, imgfile, imgtyp, &ier);	
            }

/*
 *  If this loop is in view, advance to the last frame
 */
	    if ( lp == currlp ) { 
	        xmloop_loop (ANIM_LAST_FRAM, 0); 
	    }
	    else {
	        nmp_rstrproj(lp, &ier);
	    }

/*
 *  Restore LUT & fade for non 8-bit displays
 */
	    if ( depth > _8_BIT ) {
  	        loop_restoreLut( lp ); 
    	        mbotw_restoreFade( lp, False ); 
	    }

	    if (ld_files[lp] > nfrms-1) {
	        ld_files[lp] = nfrms-1;
	    }

	    for (ii=0; ii < ld_files[lp]; ii++) {
		dsp_reloadFrame (lp, total - ii);
		dataWasLoaded = True;
	    }

/*
 *  If no active loop animation, reset the display
 *  to show the new data.
 */
	    if ( lp == currlp ) {
		if ( !loopw_isLoopActv() ) {
		    xmloop_loop (ANIM_LAST_FRAM, 0); 
		}
	    }
	}
    }

/*
 *  If data was loaded, reset the lut and fade
 */
    if( dataWasLoaded && (depth > _8_BIT) ) {
       loop_restoreLut( currlp ); 
       mbotw_restoreFade( currlp, False ); 
    }

    if ( !first ) dsp_setBusy(FALSE);
}
