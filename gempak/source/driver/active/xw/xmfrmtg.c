#include "xwgui.h"

static Boolean bad_frm_save[MAX_PIXMAP];

/************************************************************************
 * xmfrmtg.c                                                            *
 *                                                                      *
 * This module takes care of the bad frame tags (Boolean flags) in      *
 * xwcmn.h.								*
 *                                                                      *
 * CONTENTS:                                                            *
 *									*
 * xmfrmtg_resetLp()		reset the bad_frm tags for a loop	*
 * xmfrmtg_setFrmTag()		set the value of a specific bad_frm tag	*
 *									*
 * xmfrmtg_getFrmTag()		get the value of a specific bad_frm tag	*
 ***********************************************************************/

/*=====================================================================*/

void xmfrmtg_resetLp ( int lp, int *iret )
/************************************************************************
 * xmfrmtg_resetLp                                                      *
 *                                                                      *
 * This subroutine resets the bad_frame tags in the current window.     *
 * The bad_frame tags are used to optionally prevent display of a       *
 * corresponding pixmap in the xmloop function.				*
 *                                                                      *
 * void xmfrmtg_resetLp ( lp, iret )	       				*
 *                                                                      *
 * Input parameters:                                                    *
 *	lp		int	loop number to be reset			*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*iret		int	return code:  0 = NORMAL, -1 = ERROR	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	05/00	initial coding				*
 ***********************************************************************/
{
int		ii;
Window_str	*cwin;
/*---------------------------------------------------------------------*/
  
    if (lp < 0 || lp > MAX_LOOP -1) {
	*iret = -1;
	return;
    }

    *iret = 0;

    cwin = &(gemwindow[current_window]);

    for (ii=0; ii < MAX_PIXMAP; ii++) {
    	cwin->bad_frm[lp][ii] = FALSE;
    }

    _allFrmsBad[lp] = FALSE;   

}

/*=====================================================================*/

void xmfrmtg_setFrmTag ( int lp, int frm, Boolean value )
/************************************************************************
 * xmfrmtg_setFrmTag                                                    *
 *                                                                      *
 * This subroutine sets the value of a specific bad_frm tag in the      *
 * current window.          						*
 *                                                                      *
 * void xmfrmtg_setFrmTag ( lp, frm, value )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	lp		int		loop number             	*
 *	frm		int		frame number			*
 *	value		Boolean		new value for bad_frm flag	*
 *                                                                      *
 * Output parameters:                                                   *
 *			NONE						*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	05/00	initial coding				*
 ***********************************************************************/
{
int		ii;
Window_str	*cwin;
/*---------------------------------------------------------------------*/

    cwin = &(gemwindow[current_window]);
    cwin->bad_frm[lp][frm] = value;

    /*
     *  Poll all the frames and determine if all have been tagged bad
     */
    _allFrmsBad[lp] = TRUE;
    for (ii=0; ii < _numPxm[lp]; ii++) {
	if (!cwin->bad_frm[lp][ii]) {
	    _allFrmsBad[lp] = FALSE;
	    break;	
	}
    }
}

/*=====================================================================*/

Boolean xmfrmtg_getFrmTag ( int lp, int frm )
/************************************************************************
 * xmfrmtg_getFrmTag                                                    *
 *                                                                      *
 * This subroutine returns the value of a specific bad_frm tag in the   *
 * current window.          						*
 *                                                                      *
 * Boolean xmfrmtg_getFrmTag ( lp, frm )       				*
 *                                                                      *
 * Input parameters:                                                    *
 *	lp			int		loop number    		*
 *	frm			int		frame number		*
 *                                                                      *
 * Output parameters:                                                   *
 *	xmfrmtg_getFrmTag	Boolean		value of bad_frm flag	*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC	05/00	initial coding				*
 ***********************************************************************/
{
Window_str	*cwin;
/*---------------------------------------------------------------------*/

    cwin = &(gemwindow[current_window]);

    return ( cwin->bad_frm[lp][frm] );

}

/*=====================================================================*/

void xmfrmtg_saveFrmTag ( int lp, int *iret )
/************************************************************************
 * xmfrmtg_saveFrmTag                                                   *
 *                                                                      *
 * This subroutine saves the values of bad_frm array for a certain loop.*
 *                                                                      *
 * void xmfrmtg_saveFrmTag ( lp, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	lp		int		loop number             	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*iret		int		return value			*
 *					 0 -- normal			*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		04/04	initial coding				*
 ***********************************************************************/
{
int		ii;
Window_str	*cwin;
/*---------------------------------------------------------------------*/

    *iret = 0;

    cwin = &(gemwindow[current_window]);

    for ( ii = 0; ii < _numPxm[lp]; ii++ ) {

         bad_frm_save[ii] = cwin->bad_frm[lp][ii];
    }

    for ( ii = _numPxm[lp]; ii < MAX_PIXMAP; ii++ ) {

         bad_frm_save[ii] = FALSE;
    }

}

/*=====================================================================*/

void xmfrmtg_restoreFrmTag ( int lp, int *iret )
/************************************************************************
 * xmfrmtg_restoreFrmTag						*
 *                                                                      *
 * This subroutine restores the values of bad_frm array for a certain	*
 * loop.								*
 *                                                                      *
 * void xmfrmtg_restoreFrmTag ( lp, iret )				*
 *                                                                      *
 * Input parameters:                                                    *
 *	lp		int		loop number             	*
 *                                                                      *
 * Output parameters:                                                   *
 *	*iret		int		return value			*
 *					 0 -- normal			*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		04/04	initial coding				*
 ***********************************************************************/
{
int		ii;
Window_str	*cwin;
/*---------------------------------------------------------------------*/

    *iret = 0;

    cwin = &(gemwindow[current_window]);

    for ( ii = 0; ii < _numPxm[lp]; ii++ ) {

         cwin->bad_frm[lp][ii] = bad_frm_save[ii];
    }

}

/*=====================================================================*/
