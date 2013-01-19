#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "nmapprm.h"
#include "nmap_data.h"
#include "hints.h"
#include "Nxm.h"


static loopInfo_t	_lpInfo[MAX_LOOP];
static int		_curLp;

Boolean loop_cmpAttr ( int nlp, int plp );
    
/************************************************************************
 * nmap_loop.c								*
 *									*
 * This module maintains the loop state information for nmap2.          *
 *									*
 * CONTENTS:								*
 *									*
 *   loop_cmpAttr()	  compare attributes from two loops		*
 *   loop_initLoops()     initialize the loop structure                 *
 *   loop_setCurLoop()    set the current loop                  	*
 *   loop_setAutoUpdt()   get the auto update status for a loop  	*
 *   loop_saveAutoUpdt()  save the auto update status for all loops	*
 *   loop_setFrameTm()    set the time of a frame of displayed data	*
 *   loop_setFramePxm()   set the pixmap index for a displayed frame	*
 *   loop_setRoamVal()    set the roam value for a loop                 *
 *   loop_saveRoamVal()   save the roam value for all loops		*
 *   loop_setTmMode()	  set the single time flag for a loop		*
 *   loop_saveTmMode()	  save the single time flag for all loops	*
 *   loop_setNumFrames()  set the number of frames in a loop         	*
 *   loop_setDataChngd()  set the data changed flag for a loop       	*
 *   loop_resetoreLut()   reset the LUT file for a loop                 *
 *   loop_changeLoop      changes the current loop			*
 *									*
 *   loop_getCurLoop()    get the current loop (the one in view)	*
 *   loop_getCurFrame()   get the current frame (the one in view)	*
 *   loop_getAutoUpdt()	  get the status of the loop's auto update flag *
 *   loop_restoreAutoUpdt() restore the auto update status for all loops*
 *   loop_getFrameTm()	  get the time of a specific frame              *
 *   loop_getFramePxm()	  get the pixmap of a specific frame            *
 *   loop_getRoamVal()    get the roam value for a loop                 *
 *   loop_restoreRoamVal()restore the roam value for a loop		*
 *   loop_getTmMode()	  get the single time flag for a loop           *
 *   loop_restoreTmMode() restore the single time flag for all loops	*
 *   loop_getNumFrames()  get the number of frames in a given loop      *
 *   loop_getTotalFrames()get the number of frames in all loops         *
 *   loop_getDataChngd()  get the data changed flag for a loop       	*
 ***********************************************************************/

/*=====================================================================*/

void loop_initLoops ( void )
/************************************************************************
 * loop_initLoops    	                                                *
 *                                                                      *
 * This function initializes the loop information structure.            *
 *                                                                      *
 * void loop_initLoops ()	                                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       10/99   initial coding                          *
 * H. Zeng/EAI          11/99   added save_roam_val                     *
 * T. Lee/GSC		02/01	added single time flag			*
 * M. Li/GSC		06/01	added fade_ratio			*
 * T. Piper/SAIC	06/03	added data_chngd			*
 ***********************************************************************/
{
int	ii, jj;
/*---------------------------------------------------------------------*/

    for (ii=0; ii<MAX_LOOP; ii++) {
        _lpInfo[ii].lp.npxm      = 0;
	_lpInfo[ii].lp.fst_pxm   = 0;
	_lpInfo[ii].lp.lst_pxm   = 0;
	_lpInfo[ii].lp.roam_val  = 0;
        _lpInfo[ii].lp.save_roam_val = 0;
	_lpInfo[ii].lp.fade_ratio    = 1.0F;
	_lpInfo[ii].lp.tm_mode       =  FALSE;
	_lpInfo[ii].lp.save_tm_mode  = FALSE;
	_lpInfo[ii].lp.auto_updt = TRUE;
	_lpInfo[ii].lp.data_chngd = FALSE;

	for (jj=0; jj<MAX_PIXMAP+1; jj++) {
            _lpInfo[ii].frames[jj].ipxm = -1;
            _lpInfo[ii].frames[jj].frmdttm[0] = '\0';
        }
    }
}

/*=====================================================================*/

void loop_setCurLoop ( int lp )
/************************************************************************
 * loop_setCurLoop                                                      *
 *                                                                      *
 * Sets the current loop variable.                                      *
 *                                                                      *
 * void loop_setCurLoop( lp )	                                        *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp		int	loop number					*
 * Output parameters:                                                   *
 *		NONE							*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	09/99	initial coding                          *
 * E. Safford/GSC	10/99	moved to nmap_loop.c                    *
 * M. Li/GSC		03/01	removed mapw__setPGForLoop		*
 ***********************************************************************/
{
    if (lp > -1 && lp < MAX_LOOP) {
	_curLp = lp;
    }
}

/*=====================================================================*/

void loop_setAutoUpdt ( int lp, Boolean flag )
/************************************************************************
 * loop_setAutoUpdt                                                     *
 *                                                                      *
 * Sets the current loop variable.                                      *
 *                                                                      *
 * void loop_setAutoUpdt ( lp, flag )	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp		int	loop number					*
 *  flag	Boolean	auto update status 				*
 *									*
 * Output parameters:                                                   *
 *		NONE							*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	10/99	initial coding                          *
 ***********************************************************************/
{
    if (0 <= lp && lp < MAX_LOOP) {
	_lpInfo[lp].lp.auto_updt = flag;
    }
}

/*=====================================================================*/

void loop_saveAutoUpdt ( void )
/************************************************************************
 * loop_saveAutoUpdt							*
 *                                                                      *
 * Saves the atuo_updt state for all loops.				*
 *                                                                      *
 * void loop_saveAutoUpdt()						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *		NONE							*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC	    10/02	initial coding                          *
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
    for (ii = 0; ii < MAX_LOOP; ii++) {
	_lpInfo[ii].lp.save_auto_updt = _lpInfo[ii].lp.auto_updt;
    }
}

/*=====================================================================*/

void loop_restoreAutoUpdt ( void )
/************************************************************************
 * loop_restoreAutoUpdt							*
 *                                                                      *
 * Resets the atuo_updt state to its saved value for all loops.		*
 *                                                                      *
 * void loop_restoreAutoUpdt()						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *		NONE							*
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC	    10/02	initial coding                          *
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
    for (ii = 0; ii < MAX_LOOP; ii++) {
	_lpInfo[ii].lp.auto_updt = _lpInfo[ii].lp.save_auto_updt;
    }
}

/*=====================================================================*/

void loop_setFrameTm ( int lp, int frm, char *ftime )
/************************************************************************
 * loop_setFrameTm                                                      *
 *                                                                      *
 * Sets the dominant time for a specific frame                          *
 *                                                                      *
 * void loop_setFrameTm( lp, frm, ftime )                               *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp		int	loop number					*
 *  frm 	int	frame number       				*
 *  *ftime	char	time for frame					*
 *									*
 * Output parameters:                                                   *
 *		NONE							*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	10/99	initial coding                          *
 ***********************************************************************/
{
    strcpy(_lpInfo[lp].frames[frm].frmdttm, ftime);
}

/*=====================================================================*/

void loop_setFramePxm ( int lp, int frm, int pxm )
/************************************************************************
 * loop_setFramePxm                                                     *
 *                                                                      *
 * Sets the pixmap index for a specific frame                           *
 *                                                                      *
 * void loop_setFramePxm( lp, frm, pxm )                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp		int	loop number					*
 *  frm 	int	frame number       				*
 *  pxm		int	pixmap number 					*
 *									*
 * Output parameters:                                                   *
 *		NONE							*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	10/99	initial coding                          *
 ***********************************************************************/
{
    _lpInfo[lp].frames[frm].ipxm = pxm;
}

/*=====================================================================*/

void loop_setRoamVal ( int lp, int value )
/************************************************************************
 * loop_setRoamVal                                                      *
 *                                                                      *
 * Sets the roam value for a loop.					*
 *                                                                      *
 * void loop_setRoamVal( lp, value )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  lp		int	loop number					*
 *  value 	int	roam value       				*
 *									*
 * Output parameters:                                                   *
 *		NONE							*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI	    11/99	initial coding                          *
 * T. Lee/GSC	    03/01	set roam value only			*
 ***********************************************************************/
{
    _lpInfo[lp].lp.roam_val = value;
}

/*=====================================================================*/

void loop_saveRoamVal ( void )
/************************************************************************
 * loop_saveRoamVal							*
 *                                                                      *
 * Saves the roam value for all loops.					*
 *                                                                      *
 * void loop_saveRoamVal( )						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *			NONE						*
 **                                                                     *
 * Log:                                                                 *
 * T. Lee/GSC	    03/01	initial coding				*
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
    for (ii = 0; ii < MAX_LOOP; ii++) { 
	_lpInfo[ii].lp.save_roam_val = loop_getRoamVal(ii);
    }
}

/*=====================================================================*/

void loop_setTmMode ( int lp, Boolean flag )
/************************************************************************
 * loop_setTmMode							*
 *                                                                      *
 * Sets the single time flag for a loop                                 *
 *                                                                      *
 * void loop_setTmMode ( lp, flag )					*
 *                                                                      *
 * Input parameters:                                                    *
 *  lp		int	loop number					*
 *  flag 	Boolean	single time flag				*
 *									*
 * Output parameters:                                                   *
 *		NONE							*
 **                                                                     *
 * Log:                                                                 *
 * T. Lee/GSC	    02/01	initial coding                          *
 * T. Lee/GSC	    03/01	set single time mode only		*
 ***********************************************************************/
{
    _lpInfo[lp].lp.tm_mode = flag;
}

/*=====================================================================*/

void loop_saveTmMode ( void )
/************************************************************************
 * loop_saveTmMode							*
 *                                                                      *
 * Saves the single time flag for all loops.				*
 *                                                                      *
 * void loop_saveTmMode ()						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *		NONE							*
 **                                                                     *
 * Log:                                                                 *
 * T. Lee/GSC	    03/01	initial coding                          *
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
    for (ii = 0; ii < MAX_LOOP; ii++) {
	_lpInfo[ii].lp.save_tm_mode = loop_getTmMode (ii);
    }
}

/*=====================================================================*/

void loop_setNumFrames ( int lp, int nfrms )
/************************************************************************
 * loop_setNumFrames                                                    *
 *                                                                      *
 * Sets the number of frames in the loop                                *
 *                                                                      *
 * void loop_setNumFrames( lp, nfrms )	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp		int	loop number					*
 *  nfrms 	int	number of frames       				*
 *									*
 * Output parameters:                                                   *
 *		NONE							*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	10/99	initial coding                          *
 ***********************************************************************/
{
    _lpInfo[lp].lp.npxm      = nfrms;
}

/*=====================================================================*/

void loop_setDataChngd ( int lp, Boolean flag )
/************************************************************************
 * loop_setDataChngd                                                    *
 *                                                                      *
 * Sets the data changed flag for the loop.                             *
 *                                                                      *
 * void loop_setDataChngd( lp, flag )	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp		int	loop number					*
 *  flag 	Boolean	data changed flag value				*
 *									*
 * Output parameters:                                                   *
 *		NONE							*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	1/00	initial coding                          *
 ***********************************************************************/
{
    _lpInfo[lp].lp.data_chngd = flag;
}

/*=====================================================================*/

void loop_restoreLut ( int lp )
/************************************************************************
 * loop_restoreLut                                                      *
 *                                                                      *
 * This function resets the active LUT file to that used by the         *
 * specified loop.							*
 *                                                                      *
 * void loop_restoreLut ( lp )                                     	*
 *                                                                      *
 * Input parameters:                                                    *
 *	lp  		int	loop number       			*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	03/00	initial coding                      	*
 * S. Jacobs/NCEP	 5/01	Removed calls to mbotw_getFadeColor	*
 * E. Safford/GSC	06/01	added calls to mbotw_reloadFade     	*
 * E. Safford/SAIC	02/04	Add param to mbotw_reloadFade       	*
 ***********************************************************************/
{
int	nindex, ier;
char	imtype[81], iminfo[81], imlutf[81], *ptr;
/*---------------------------------------------------------------------*/

/*
 *  Update the color enhancement popup window.
 *  Set the color look-up table for this loop.
 */
    if  ( dataw_isSatSelect ( lp, &nindex ) ) {
        nim_qatt ( nindex, imtype, iminfo, imlutf, &ier );
	im_lutf ( imlutf, &ier, strlen(imlutf) );

	ptr = strchr ( imlutf, '.' );
	    if  ( ptr != NULL )  {
	        *ptr = '\0';
	    }

        NxmEnhw_setLutfile ( 1, imlutf );
        NxmEnhw_update ( 1 );
  	mbotw_reloadFade( lp );  
    }
    else if  ( dataw_isRadSelect ( lp, &nindex ) ) {
        nim_qatt ( nindex, imtype, iminfo, imlutf, &ier );
        im_lutf ( imlutf, &ier, strlen(imlutf) );

        ptr = strchr ( imlutf, '.' );
        if  ( ptr != NULL ) {
	    *ptr = '\0';
	}

	NxmEnhw_setLutfile ( 2, imlutf );
	NxmEnhw_update ( 2 );
    	mbotw_reloadFade( lp ); 
    }
}

/*=====================================================================*/

void loop_changeLoop ( int new_loop )
/************************************************************************
 * loop_changeLoop							*
 *									*
 * This function does all the necessary details to change the current	*
 * loop.								*
 *									*
 * void loop_changeLoop (new_loop)					*
 *									*
 * Input parameters:							*
 *	new_loop	int	new loop				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		06/00	moved from mbtnw_loopSelBtnCb		*
 * E. Safford/GSC	06/00	fixed order of operation error in pgen	*
 * E. Safford/GSC	06/00	remove loop check                     	*
 * E. Safford/GSC	10/00	add loopw_resetHide call               	*
 * E. Safford/GSC	02/01	avoid palette reset                   	*
 * E. Safford/GSC	04/01	add nmp_sproj() call			*
 * E. Safford/GSC	05/01	use nmp_rstrproj instead of nmp_sproj   *
 * M. Li/GSC		06/01	added mbotw_restoreFade			*
 * H. Zeng/EAI          06/01   added call to loopw_resetHide()         *
 * J. Wu/SAIC		11/01	add param in cvg_load()	calling		*
 * J. Wu/SAIC		11/01	remove redundant crg_init/geplot calls	*
 * J. Wu/SAIC		12/01	add layer in cvg_load()	call		*
 * J. Wu/SAIC		12/01	replace cvg_load() with cvg_redraw()	*
 * H. Zeng/EAI          03/02   set condition for roamw_setup()         *
 * T. Piper/SAIC	06/03	added loop_cmpAttr			*
 * E. Safford/SAIC	02/04	add params to mbotw_restoreFade()       *
 * J. Wu/SAIC		08/04	restart GFA win. if it was in ADD mode	*
 * J. Wu/SAIC		10/04	free GFA block memory			*
 * T. Piper/SAIC        12/04   added aodtw_refresh & cldhgtw_refresh   *
 * H. Zeng/SAIC		01/07   removed mcanvw_setLatlon()		*
 * H. Zeng/SAIC		03/07	added call to seekw_destroyWidget	*
 * B. Yin/SAIC		07/07	remove the part to restart GFA GUI	*
 ***********************************************************************/
{
    int		ier, prev_loop, xx, yy;
    Boolean	attr_flag, palw_flag;
/*---------------------------------------------------------------------*/

/*
 * Before switching loop, reset Hide/Show button.
 */
    loopw_resetHide();

/*
 * Proceed to do switching loop job.
 */
    if ( mmenuw_roamShareGet() ) {
        prev_loop = loop_getCurLoop();
        attr_flag = loop_cmpAttr(new_loop, prev_loop);
    }
    else {
	attr_flag = FALSE;
    }

    seekw_saveGhost (TRUE);

    if ( !attr_flag ) {
        nmp_rstrproj( new_loop, &ier );
    }

    palw_flag = pgpalw_isUp();
    
    if (palw_flag) {

/*
 *  Reset the drawing palete to terminate any pending action(s).
 */	
	pgpalw_classPopdown();
	pgpalw_setupOper();	
	
/*
 *  Restore the frames from the master copies.
 */
  	xpgrestlp (); 
    }

    loop_setCurLoop(new_loop);
    mbtnw_setMbtns();

    loop_restoreLut(new_loop);
    mbotw_restoreFade( new_loop, True );

/*
 *  Tell the xw driver of the new loop, but don't update
 *  the display yet.  Must set the roam factor and signal
 *  to reload the vg elements (if in PGEN) first.
 */ 
/*
 * Check if data selection window is up.
 */
    if ( attr_flag ) {
        if ( !dataw_isUp () ) {
	    xmroam_getPos (&xx,&yy);
	    xmloop_switchLoop (new_loop, FALSE);
	    xmroam_setPos (xx,yy);
        }
	else {
	    xmloop_switchLoop (new_loop, FALSE);
	}
    }
    else {
        xmloop_switchLoop (new_loop, FALSE);
        if ( !dataw_isUp () ) {
            roamw_setup (new_loop, TRUE);
        }
    }

    if (palw_flag) {
        xpgsvlp (&ier);
        xpgrfrsh ();
        crg_rebuild ();

/* 
 * load and plot the current frame.
 */
	cvg_redraw (NULL, &ier);

    }

/*
 *  Now update the display.
 */
    xmloop_switchLoop (new_loop, TRUE); 
    loopw_resetHide();
    aodtw_refresh(TRUE);
    cldhgtw_refresh(TRUE);
    seekw_saveGhost (FALSE);
    seekw_destroyWidget();

}

/*=====================================================================*/

int loop_getCurLoop ( void )
/************************************************************************
 * loop_getCurLoop                                                      *
 *                                                                      *
 * Returns a value in range of 0-(MAX_LOOP-1).                          *
 *                                                                      *
 * int loop_getCurLoop( )	                                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * loop_getCurLoop	int		0 - MAX_LOOP-1   		*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	09/99	initial coding                          *
 * E. Safford/GSC	10/99	moved to nmap_loop.c                    *
 ***********************************************************************/
{
    return (_curLp);
}

/*=====================================================================*/

int loop_getCurFrame ( void )
/************************************************************************
 * loop_getCurFrame                                                     *
 *                                                                      *
 * Returns the number of the current frame.                             *
 *                                                                      *
 * int loop_getCurFrame( )	                                        *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * loop_getCurFrame	int	the number of the currently displayed   *
 *				frame or -1 in the event of an error	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	11/99	initial coding                          *
 ***********************************************************************/
{
int	ignore, cpxm, frm;
Boolean found;
/*---------------------------------------------------------------------*/

    found = FALSE;
    xqcpxm (&ignore, &cpxm);
    
    for (frm=0; frm < MAX_PIXMAP+1; frm++) {
        if  (_lpInfo[_curLp].frames[frm].ipxm == cpxm) {
	    found = TRUE;
	    break;
        }
    }

    if (!found) {
 	frm = -1;
    }
    return (frm);  
}

/*=====================================================================*/

Boolean	loop_getAutoUpdt ( int lp )
/************************************************************************
 * loop_getAutoUpdt                                                     *
 *                                                                      *
 * Returns the auto update flag for the given loop                      *
 *                                                                      *
 * Boolean loop_getAutoUpdt( lp )                                       *
 *                                                                      *
 * Input parameters:                                                    *
 *	lp	int	loop number					*
 *									*
 * Output parameters:                                                   *
 * loop_getAutoUpdt	Boolean		value of the auto update flag 	*
 *							for the loop	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	10/99	initial coding                          *
 ***********************************************************************/
{

    if (lp >= 0 && lp < MAX_LOOP) {
        return (_lpInfo[lp].lp.auto_updt);
    }
    else {
	return (FALSE);
    }
}

/*=====================================================================*/

void loop_getFrameTm ( int lp, int frm, char *ftime )
/************************************************************************
 * loop_getFrameTm                                                      *
 *                                                                      *
 * Returns the time of a given frame.                                   *
 *                                                                      *
 * void loop_getFrameTm ( lp, frm, ftime )                              *
 *                                                                      *
 * Input parameters:                                                    *
 *	lp	int	loop number					*
 *	frm	int	frame number					*
 *									*
 * Output parameters:                                                   *
 *	*ftime	char	time of the frame                         	*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	10/99	initial coding                          *
 ***********************************************************************/
{
    if (lp >= 0 && lp < MAX_LOOP) {
	strcpy ( ftime, _lpInfo[lp].frames[frm].frmdttm );
    }
    else {
 	ftime[0] = '\0';	
    }
}

/*=====================================================================*/

int loop_getFramePxm ( int lp, int frm )
/************************************************************************
 * loop_getFramePxm                                                     *
 *                                                                      *
 * Returns the pixmap index for the given loop and frame.               *
 *                                                                      *
 * int	 loop_getFramePxm( lp, frm )	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *	lp	int	loop number					*
 *	frm	int	frame number					*
 *									*
 * Output parameters:                                                   *
 * loop_getFramePxm	int	pixmap index for the frame, 		*
 *						or -1 if not found 	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	10/99	initial coding                          *
 ***********************************************************************/
{
    if (lp >= 0 && lp < MAX_LOOP) {
	return ( _lpInfo[lp].frames[frm].ipxm );
    }
    else {
	return (-1);
    }
}

/*=====================================================================*/

int loop_getRoamVal ( int lp )
/************************************************************************
 * loop_getRoamVal                                                      *
 *                                                                      *
 * Gets the roam value for a loop.					*
 *                                                                      *
 * int loop_getRoamVal( lp )                               		*
 *                                                                      *
 * Input parameters:                                                    *
 *  lp			int	loop number				*
 *									*
 * Output parameters:                                                   *
 *		NONE							*
 * Return parameters:							*
 *  loop_getRoamVal	int	roam value				*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI  	11/99	initial coding                          *
 * T. Lee/GSC		03/01	returned roam value only		*
 ***********************************************************************/
{
    if (lp >= 0 && lp < MAX_LOOP) {
	return (_lpInfo[lp].lp.roam_val);
    }
    else {
	return (-1);
    }
}

/*=====================================================================*/

void loop_restoreRoamVal ( void )
/************************************************************************
 * loop_restoreRoamVal							*
 *                                                                      *
 * Restores the roam value for all loops.				*
 *                                                                      *
 * void loop_restoreRoamVal( )                               		*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *		NONE							*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI  	11/99	initial coding                          *
 * T. Lee/GSC		03/01	restored saved roam value		*
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
    for (ii = 0; ii < MAX_LOOP; ii++) {
	loop_setRoamVal(ii, _lpInfo[ii].lp.save_roam_val);
    }
}

/*=====================================================================*/

Boolean	loop_getTmMode ( int lp )
/************************************************************************
 * loop_getTmMode							*
 *									*
 * Gets the single time flag for a loop.				*
 *									*
 * Boolean loop_getTmMode( lp )						*
 *                                                                      *
 * Input parameters:							*
 *  lp			int	loop number				*
 *									*
 * Output parameters:							*
 * Return value:							*
 *  loop_getTmMode	Boolean	single time flag			*
 **									*
 * Log:									*
 * T. Lee/GSC		02/01	initial coding				*
 * T. Lee/GSc		03/01	return single time flag			*
 ***********************************************************************/
{
    if (lp >= 0 && lp < MAX_LOOP) {
	return (_lpInfo[lp].lp.tm_mode);
    }
    else {
	return (FALSE);
    }
}

/*=====================================================================*/

void loop_restoreTmMode ( void )
/************************************************************************
 * loop_restoreTmMode							*
 *									*
 * Restores the single time flag for all loops.				*
 *									*
 * void loop_restoreTmMode( )                				*
 *                                                                      *
 * Input parameters:							*
 * Output parameters:							*
 *  			NONE						*
 **									*
 * Log:									*
 * T. Lee/GSC		03/01	initial coding				*
 ***********************************************************************/
{
int	ii;
/*---------------------------------------------------------------------*/
    for (ii = 0; ii < MAX_LOOP; ii++) {
	loop_setTmMode(ii, _lpInfo[ii].lp.save_tm_mode);
    }
}

/*=====================================================================*/

int loop_getNumFrames ( int lp )
/************************************************************************
 * loop_getNumFrames                                                    *
 *                                                                      *
 * Returns the number of pixmaps in a given loop.  The total includes   *
 * the blank pixmap.              					*
 *                                                                      *
 * int loop_getNumFrames ( lp )    	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *	lp	int	loop number					*
 *									*
 * Output parameters:                                                   *
 * Return value:							*
 * loop_getNumFrames	int	# of pixmaps in the frame, or -1 error 	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	10/99	initial coding                          *
 ***********************************************************************/
{
    if (lp >= 0 && lp < MAX_LOOP) {
	return ( _lpInfo[lp].lp.npxm );
    }
    else {
	return (-1);
    }
}

/*=====================================================================*/

int loop_getTotalFrames ( int lp )
/************************************************************************
 * loop_getTotalFrames                                                  *
 *                                                                      *
 * Returns the number if pixmaps in all loops, or all loops except lp.  *
 * If the param lp contains a valid loop number (0-3) then that loop is *
 * NOT included in the total.  If lp is not a valid loop number, then   *
 * the return value is the number of loaded pixmaps in all loops.	*
 *                                                                      *
 * int loop_getTotalFrames ( lp )    	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *	lp	int	loop number to exclude in count             	*
 *			  if in range 0 to MAX_LOOP			*
 *									*
 * Output parameters:                                                   *
 * loop_getTotalFrames      int		# of pixmaps in the loops	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	04/00	initial coding                          *
 ***********************************************************************/
{
int	total, ii;
/*---------------------------------------------------------------------*/

    total = 0;

    for (ii=0; ii < MAX_LOOP; ii++) {
	if (lp == ii) {
	    continue;
	}

	total += _lpInfo[ii].lp.npxm;
    }

    return (total);
}

/*=====================================================================*/

Boolean	loop_getDataChngd ( int lp )
/************************************************************************
 * loop_getDataChngd                                                    *
 *                                                                      *
 * Returns the data changed flag for the loop.                          *
 *                                                                      *
 * Boolean loop_getDataChngd ( lp )    	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp			int	loop number				*
 *									*
 * Output parameters:                                                   *
 * loop_getDataChngd	Boolean	value of data_chngd flag for the loop	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	01/00	initial coding                          *
 ***********************************************************************/
{
    return (_lpInfo[lp].lp.data_chngd);
}

/*=====================================================================*/

void loop_setFadeRatio ( int lp, float ratio )
/************************************************************************
 * loop_setFadeRatio                                                    *
 *                                                                      *
 * This function sets the fade ratio in the structure.	                *
 *                                                                      *
 * void loop_setFadeRatio ( lp, ratio )                                 *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp			int	loop number				*
 *  ratio               float   fade ratio                              *
 *                                                                      *
 * Output parameters:                                                   *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC		06/01						* 
 ***********************************************************************/
{
    _lpInfo[lp].lp.fade_ratio    = ratio;
}

/*=====================================================================*/

void loop_getFadeRatio ( int lp, float *ratio )
/************************************************************************
 * loop_getFadeRatio                                                    *
 *                                                                      *
 * This function retrieves the fade ratio from the structure.           *
 *                                                                      *
 * void loop_getFadeRatio ( lp, ratio )                                 *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp                  int     loop number                             *
 *                                                                      *
 * Output parameters:                                                   *
 *  *ratio              float   fade ratio                              *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/GSC            06/01                                           *
 ***********************************************************************/
{
    *ratio = _lpInfo[lp].lp.fade_ratio;
}

/*=====================================================================*/

Boolean loop_cmpAttr ( int nlp, int plp )
/************************************************************************
 * loop_cmpAttr								*
 * 									*
 * This function compares the attributes of two loops.			*
 * 									*
 * Boolean loop_cmpAttr ( nlp, plp )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	nlp		int		new loop number			*
 *	plp		int		previous loop number		*
 **                                                                     *
 * Log:                                                                 *
 * T. Piper/SAIC	06/03	Created					*
 ***********************************************************************/
{
    size_t	which;
    int		ier, nrvl, prvl;
    nmpstr_t	proj[2], ngarea[2], pgarea[2];
/*---------------------------------------------------------------------*/

    if ( nlp != plp ) {
	nmp_gtruattr(nlp, proj[0], ngarea, &ier);
	nmp_gtruattr(plp, proj[1], pgarea, &ier);
	if ( strcmp(proj[0], proj[1]) == 0 ) {
	    if (strlen(ngarea[1]) > (size_t)0 && 
	        strlen(pgarea[1]) > (size_t)0 ) {
		which = 1;
	    }
	    else if (strlen(ngarea[1]) == (size_t)0 &&
		     strlen(pgarea[1]) == (size_t)0 ) {
		which = 0;
	    }
	    else {
	        return(FALSE);
            }

	    if ( strcmp(ngarea[which],pgarea[which]) == 0 ) {
                nrvl = loop_getRoamVal(nlp);
                prvl = loop_getRoamVal(plp);
                if ( nrvl == prvl ) {
		    return(TRUE);
	        }
	    }
        }
    }
    return(FALSE);
}    	
