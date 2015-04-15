#include "geminc.h"
#include "gemprm.h"
#include "nmapprm.h"
#include "nmap_data.h"
#include "nmap_mainw.h"
#include "Nxm.h"

#define LOGO_SMALL_OFFSET  ( 0.10F )
#define LOGO_LARGE_OFFSET  ( 0.15F )

static  char   _transferInterrupt = 0;
static  int    _ifnt  = 1;
static  int    _ihwsw = 2;
static  int    _ibrdr = 111;
static  float  _tsize = 1.0F;
static  int    _iwid  = 1;
static  int    _irot  = 1;
static  int    _ijust = 1;


/*
 *  Private functions
 */
void dsp_getPlotOrdr ( int lp, int nsrcs, int plot_ordr[], 
			Boolean *img_data, int *npltsrc);
void dsp_getSingleTmOrdr ( int lp, int nsrcs, int frm, int plot_ordr[], 
			int *ndata, Boolean *img_data);
static void dsp_getTime ( int lp, dattm_t dttm, int *iret );
void dsp_loadFrame ( int lp, int frm, int pxm, dattm_t ctime,
			dttms_t ftime, Boolean view_frm, int nsrcs,
			Boolean fst_frm, Boolean reload );
void dsp_loadLoop ( int lp, Boolean view_lp, int *loaded );
void dsp_loadSingleTime ( int lp, Boolean view_lp, int *loaded );
void dsp_showLoadErr ( void );
void dsp_updatePage ( int current, int total );


/************************************************************************
 * nmap_dsp.c                                                           *
 *                                                                      *
 * This module takes care of data display for nmap.  			*
 *                                                                      *
 * CONTENTS:                                                            *
 *   dsp_loadAllLoops()   load all selected data                        *
 *   dsp_reloadLoop()	  reloads all data for a specific loop		*
 *   dsp_reloadFrame()	  reload the data for one specific frame	*
 *   dsp_updtDisplay()    updates the displayed loop			*
 *   dsp_addLogo()	  adds logo to all frames in one loop		*
 *   dsp_drawLogo()	  draws logo to specific frame			*
 *   dsp_setBusy()        set the busy animation (upper right corner)   *
 *                                                                      *
 *   dsp_getPlotOrdr()    determine the plot order within a pixmap      *
 *   dsp_getSingleTmOrdr()determine the plot order for single time loops*
 *   dsp_updatePage()     update the page (x of y) lower left corner)   *
 *   dsp_setProj()	  set up the correct map projection/garea	*
 *                                                                      *
 *   dsp_loadLoop()       load the specified loop                       *
 *   dsp_loadSingleTime() load the single time loop            		*
 *   dsp_loadFrame()      load the specified frame                      *
 *   dsp_print()          print the specified loop or frame		*
 *   dsp_showLoadErr()	  display a load error message                  *
 *   dsp_getTime()	  get the current or set time as reference 	*
 ***********************************************************************/

/*=====================================================================*/

void dsp_loadAllLoops ( void )       
/************************************************************************
 * dsp_loadAllLoops 							*
 *                                                                      *
 * This function loads each frame into a pixmap.    			*
 *                                                                      *
 * void dsp_loadAllLoops ( )        					*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * E. Safford/GSC	07/99	initial coding				*
 * S. Law/GSC		10/99	_WWN -> _MSC				*
 * S. Jacobs/NCEP       10/99   Added setting LUT file for images       *
 * S. Jacobs/NCEP       10/99   Changed ss_gtim to css_gtim             *
 * S. Jacobs/NCEP       10/99   Removed check for mdl_loaded            *
 * E. Safford/GSC	10/99	use dsp_loadLoop			*
 * E. Safford/GSC	11/99	trim param list, clean up		*
 * E. Safford/GSC	04/00	remove params, rename(was dsp_loadData) *
 * T. Lee/GSC		02/01	added single time loops			*
 * T. Lee/GSC		03/01	changed call seq. of loop_getTmMode	*
 * H. Zeng/EAI          04/01   modified to use dsp_reloadLoop()        *
 ***********************************************************************/
{
int	cur_lp, lp, ier;
/*---------------------------------------------------------------------*/

    dsp_setBusy(TRUE);

    cur_lp = loop_getCurLoop();

/*
 *  Load the current loop first
 */ 
    dsp_reloadLoop (cur_lp, &ier);

/*
 *  Load all remaining loops.
 */
    for (lp=0; lp < MAX_LOOP; lp++) {
        if (lp == cur_lp) {
	    continue;
	}
	dsp_reloadLoop(lp, &ier);
    }
    dsp_setBusy(FALSE);
}

/*=====================================================================*/

void dsp_reloadLoop ( int lp, int *iret )
/************************************************************************
 * dsp_reloadLoop    	                                                *
 *                                                                      *
 * This function reloads the data for one specific loop.                *
 *                                                                      *
 * void dsp_reloadLoop ( lp, iret )	                                *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp		int	loop number for reload				*
 *									*
 * Output parameters:                                                   *
 *  *iret	int	return code 					*
 *			  0 = normal					*
 *			 -1 = no available pixmaps for load		*
 *			 -2 = invalid loop number			*
 *                        1 = quit due to user interruption             *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       10/99   initial coding                          *
 * E. Safford/GSC	11/99	fix crash when roaming the base map	*
 * E. Safford/GSC	12/99	add iret param				*
 * S. Law/GSC		01/00	changed final xmloop_switchLoop to TRUE	*
 * E. Safford/GSC	04/00	param chg dsp_loadLoop & clean up   	*
 * E. Safford/GSC	07/00	remove dsp_setBusy()                	*
 * T. Lee/GSC		 2/01	added single time loops			*
 * T. Lee/GSC		03/01	changed call seq. of loop_getTmMode	*
 * H. Zeng/EAI          04/01   added the check of _transferInterrupt   *
 * H. Zeng/EAI          07/01   updated time line for unloaded loops    *
 * E. Safford/SAIC	08/01	made usr interrupt a warning not error  *
 * E. Safford/SAIC	10/01	move nmp calls here from dataw_clearLoop*
 * E. Safford/SAIC	01/04	add call to loop_restoreLut             *
 ***********************************************************************/
{
int	loaded, cur_lp, ier;
Boolean	in_view;
Widget	drawingW;

static int		xdpth = -1;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if ( xdpth == -1 ) {
	drawingW = mcanvw_getDrawingW();
	xdpth    = DefaultDepth( XtDisplay( drawingW ), 
		   DefaultScreen( XtDisplay( drawingW ) ));
    }	

    if (lp < 0 || lp > MAX_LOOP-1) {
	*iret = -2;
	return;
    }

/*
 * If user has interrupted the loading, clear the previously selected
 * data, update the time line and return with flag -3.
 */
    if (_transferInterrupt) {

        dataw_clearLoop(lp);

        nmp_sdefmap(lp, &ier);
        nmp_sproj(lp, &ier);

        zoomw_clearZoom(lp);
        dataw_updateTmln();

        *iret = 1;
        return;
    }

/*
 *  in_view is true if we're loading into the current loop
 */
    cur_lp  = loop_getCurLoop(); 
    in_view = (Boolean) (cur_lp == lp); 

/*
 *  Make sure the lut is set when using 16 or 24-bit screen mode
 */
    if ( !in_view && xdpth != 8 ) {
        loop_restoreLut( lp );
    }
  
    if (loop_getTmMode(lp)) {
        dsp_loadSingleTime(lp, in_view, &loaded);
    }
    else {
        dsp_loadLoop(lp, in_view, &loaded);
    }

    if (!in_view) { 

/*
 *  reset the xw driver to the cur_lp
 */
        xmloop_switchLoop (cur_lp, FALSE);
    }   

/*
 *  Store some loop information
 */
    loop_setNumFrames (lp, loaded);
}

/*=====================================================================*/

void dsp_reloadFrame ( int lp, int frm )
/************************************************************************
 * dsp_reloadFrame    	                                                *
 *                                                                      *
 * This function reloads the data for one specific frame.               *
 *                                                                      *
 * void dsp_reloadFrame ( lp, frm )                                     *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp		int	loop number					*
 *  frm		int	frame number					*
 *									*
 * Output parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       10/99   initial coding                          *
 * E. Safford/GSC	11/99	add roamw_setup				*
 * S. Jacobs/NCEP	11/99	Added print functionality		*
 * E. Safford/GSC	05/00	add call to mbtnw_loopSelBtnCb		*
 * E. Safford/GSC	05/00	add call to xmfrmtg_setFrmTag 		*
 * E. Safford/GSC	05/00	param change to dsp_loadFrame 		*
 * S. Law/GSC		06/00	removed loop parameter from xpgsvfrm2	*
 * E. Safford/GSC	06/00	rm pageSet call and clean up		*
 * E. Safford/GSC	07/00	remove dsp_setBusy()                	*
 * E. Safford/GSC	12/00	replace css_gtime with dsp_getTime     	*
 * H. Zeng/EAI          04/01   moved blank map before the 1st pixmap   *
 * E. Safford/GSC	06/01	use lp, not cur_lp in dsp_getTime()	*
 * J. Wu/SAIC	        11/01   add param in cvg_load() calling		*
 * J. Wu/SAIC	        12/01   add layer in cvg_load() call		*
 * J. Wu/SAIC	        12/01   rebuild range record after xpgrfrsh()	*
 * J. Wu/SAIC	        12/01   replace cvg_load with cvg_redraw()	*
 * T. Piper/SAIC	 4/03	added the resetting of projection and	*
 *				roam factor from auto_performUpdt	*
 ***********************************************************************/
{
    int		pxm, cur_lp, nfrms, nsrcs, ier;
    Boolean	view_lp, frst_frm, prtflg;
    dattm_t	ctime;
    dattm_t	ftime; 
/*---------------------------------------------------------------------*/

    prtflg = (Boolean) NxmPrt_isPrtFlgSet ();

    cur_lp = loop_getCurLoop(); 
    dsp_getTime ( lp, ctime, &ier );


    pxm = loop_getFramePxm (lp, frm);
    loop_getFrameTm(lp, frm, ftime);

    nfrms    = loop_getNumFrames (lp);
    nsrcs    = dataw_getNumSrcs(lp);

/*
 * frst_frm is true if it is the first (most recent) frame in a loop
 */
    frst_frm   = (Boolean) (nfrms-1 == frm);

/*
 *  view_lp is true if we're loading into the current loop
 */
    view_lp = (Boolean) (cur_lp == lp); 


    if (!view_lp) {
	xmloop_switchLoop (lp, FALSE);
	if  ( ! prtflg )  {
	    roamw_setup (lp, FALSE);
	}
    }

    dsp_loadFrame(lp, frm, pxm, ctime, ftime, view_lp, nsrcs, frst_frm, TRUE); 

    if  (! prtflg) {

/*
 *   Set the frame tag to TRUE.
 */
	xmfrmtg_setFrmTag (lp, frm, FALSE);

        if  (pgpalw_isUp() && view_lp)  {
	    xpgsvfrm2 (frm);
	    xpgrfrsh();
            crg_rebuild();
	}
    }
    else if (pgpalw_isUp() && view_lp) {
	cvg_redraw ( NULL, &ier );
    }

/*
 *  return settings to the current loop
 */
    if (!view_lp) { 

/*
 *  reset the xw driver to the cur_lp and
 *  reset the projection and roam factor
 */ 
        xmloop_switchLoop (cur_lp, FALSE);
	nmp_rstrproj (cur_lp, &ier);
        roamw_setup (cur_lp, TRUE);
    }
}

/*=====================================================================*/

void dsp_updtDisplay ( void )
/************************************************************************
 * dsp_updtDisplay							*
 *                                                                      *
 * This function updates the display of the current loop.  It is      	*
 * normally called at the conclusion of a data load operation (ie after	*
 * all loops have been loaded). 					*
 *                                                                      *
 * void dsp_updtDisplay ( )						*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *			NONE						*
 **                                                                     *
 * E. Safford/GSC	04/00	initial coding				*
 ***********************************************************************/
{
    xmloop_switchLoop (loop_getCurLoop(), TRUE);
}

/*=====================================================================*/

void dsp_setBusy ( Boolean state )
/************************************************************************
 * dsp_setBusy	 							*
 *                                                                      *
 * This function disables or enables the zoom, roam, and loop controls	*
 * and displays or ends the busy animation.				*
 *                                                                      *
 * void dsp_setBusy (state)                       			*
 *                                                                      *
 * Input parameters:                                                    *
 *  state  		Boolean	 true  = disable zoom/roam/loop		*
 *				 false = enable zoom/roam/loop		*
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * E. Safford/GSC	08/99	initial coding				*
 * E. Safford/GSC	11/99	add auto-update stop/start		*
 * S. Law/GSC		01/00	loopw_resetHide -> loopw_setHide	*
 * S. Jacobs/NCEP	 5/00	Reodered function calls			*
 * E. Safford/GSC	07/00	add prev_state checks     		*
 * E. Safford/GSC	10/00	loopw_setHide -> loopw_resetHide	*
 * H. Zeng/EAI          04/01   reset _transferInterrupt to 0           *
 * T. Piper/SAIC	07/03	Removed NxmBusy_setBusyPid		*
 ***********************************************************************/
{
Widget	draw_wid;
static	Boolean prev_state = FALSE;
/*---------------------------------------------------------------------*/

    if (prev_state == state) {
	return;
    }
    prev_state = state;

    if (state) {

        draw_wid = mcanvw_getDrawingW();
        _transferInterrupt = 0;
        NxmBusy_invoke(draw_wid, &_transferInterrupt);

	auto_stopAutoUpdt();
        mbtnw_zoomSensitive(False);
        loopw_sensitive(False);
        roamw_sensitive(False);

    }
    else {

        mbtnw_zoomSensitive(TRUE);
        loopw_sensitive(TRUE);
        loopw_resetHide ();
        roamw_sensitive(TRUE);
	auto_startAutoUpdt();

        if (!_transferInterrupt) {
            NxmBusy_animateFinish();
        }

        _transferInterrupt = 0;

    }
}

/*=====================================================================*/

void dsp_getPlotOrdr ( int lp, int nsrcs, int plot_ordr[], 
					Boolean *img_data, int *npltsrc )
/************************************************************************
 * dsp_getPlotOrdr							*
 *                                                                      *
 * This function determines the plot order for the data in the given  	*
 * panel.                                   				*
 *                                                                      *
 * void dsp_getPlotOrdr (lp, nsrcs, plot_ordr, img_data, npltsrc )	*
 *                                                                      *
 * Input parameters:                                                    *
 *  lp  		int	loop number                  		*
 *  nsrcs		int	number of sources in panel		*
 *                                                                      *
 * Output parameters:                                                   *
 *  plot_ordr[] 	int	array of indicies to data sources	*
 *  *img_data		Boolean True if image data in panel		*
 *  *npltsrc		int	number of source to plot		*
 *									*
 **                                                                     *
 * E. Safford/GSC	09/99	initial coding				*
 * S. Law/GSC		10/99	_WWN -> _MSC				*
 * E. Safford/GSC	10/99	remove mdl_data pararm			*
 * S. Law/GSC		11/99	changed to use new defines		*
 * E. Safford/GSC	02/00	add cnt < nsrcs condition to kk for loop*
 * M. Li/GSC		06/01	add a check for off-data source		*
 * S. Jacobs/NCEP	11/02	Changed order of VGF and MSC data	*
 * M. Li/SAIC		03/08	added CAT_ENS				*
 ***********************************************************************/
{
dsrc_t	*dsrc;
int	ii, cnt;
Cardinal	kk;
int	catg_ordr[] = {CAT_IMG, CAT_GRD, CAT_ENS, CAT_SFC, CAT_SND, CAT_SFF, 
		       CAT_SNF, CAT_VGF, CAT_MSC, CAT_NIL};
/*---------------------------------------------------------------------*/

    *img_data = FALSE;
    *npltsrc = 0;

    for (ii=0; ii<nsrcs; ii++) {
	plot_ordr[ii] = -1;
    }

    if (nsrcs == 1) {
	plot_ordr[0] = 0;
        dsrc = dataw_getDataSrc(lp, 0);

	if (dsrc->catg == CAT_IMG && dsrc->src_on ) {
	    *img_data = TRUE;
	}
	*npltsrc = nsrcs;

    }
    else {
        cnt = 0;
        for (kk=0; kk < XtNumber(catg_ordr) && cnt < nsrcs; kk++) {

            for (ii=0; ii < nsrcs; ii++) {
                dsrc = dataw_getDataSrc(lp, ii);
		if (dsrc->src_on) {
		    if (dsrc->catg == CAT_IMG) {
		        *img_data = TRUE;
		    }
	            if (dsrc->catg == catg_ordr[kk]) {
	                plot_ordr[cnt] = ii;
	                cnt++;
	            }
                }
	    }
        }
	*npltsrc = cnt;
    }
}

/*=====================================================================*/

void dsp_updatePage ( int current, int total )
/************************************************************************
 * dsp_updatePage                                                       *
 *                                                                      *
 * This function calls mbotw_pageSet to set the current frame (page)    *
 * number and total number of frame (pages) information and             *
 * pghdlb_displayAllSel to update any selected vgf elements             *
 *                                                                      *
 * void dsp_updatePage (current, total)                                 *
 *                                                                      *
 * Input parameters:                                                    *
 *  current       int   current page (frame) number                     *
 *  total         int   total number of frames                          *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC       06/98  initial coding                           *
 * E. Safford/GSC       09/98  change pgpalw_isPgen to pgpalw_isUp      *
 * E. Safford/GSC       03/99   use pghdlb_showAllSel                   *
 ***********************************************************************/
{
    mbotw_pageSet (current, total);

    if (pgpalw_isUp()) {
        pghdlb_showAllSel();
    }
}

/*=====================================================================*/

void dsp_loadLoop ( int lp, Boolean view_lp, int *loaded )
/************************************************************************
 * dsp_loadLoop 							*
 *                                                                      *
 * This function loads each loop into a series of pixmaps. 		*
 *                                                                      *
 * void dsp_loadLoop( lp, view_lp, loaded )	   			*
 *                                                                      *
 * Input parameters:                                                    *
 *  lp			int	loop number				*
 *  view_lp		Boolean	True if loop is to be in view after load*
 *                                                                      *
 * Output parameters:                                                   *
 *  *loaded		int	number of pixmaps loaded		*
 *									*
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * E. Safford/GSC	10/99	initial coding				*
 * S. Jacobs/NCEP	10/99	ss_gtim -> css_gtim         		*
 * S. Jacobs/NCEP	10/99	removed check for mdl_loaded		*
 * E. Safford/GSC	11/99	rename dsrc_srcs to nsrcs        	*
 * E. Safford/GSC	11/99	fix crash when roaming the base map	*
 * E. Safford/GSC	11/99	add switchloop call to avoid extra pxms	*
 * E. Safford/GSC	11/99	save pxm info on blank frame           	*
 * S. Jacobs/NCEP	11/99	Added print functionality		*
 * S. Law/GSC		01/00	removed time check when showing last frm*
 * E. Safford/GSC	01/00	use xpgsvfrm2 & display vgf immediately	*
 * S. Law/GSC		01/00	changed to use view_lp for roamw_setup	*
 * S. Law/GSC		01/00	changed so that model loads first 2 last*
 * E. Safford/GSC	02/00	add call to mbotw_loadingLoopSet	*
 * E. Safford/GSC	04/00	simplify params, clean up              	*
 * E. Safford/GSC	04/00	move _switchLoop ahead of roam_setup   	*
 * E. Safford/GSC	04/00	fix bug with *loaded assignment       	*
 * E. Safford/GSC	05/00	add call to xfrmtg_resetLp            	*
 * S. Jacobs/NCEP	 5/00	Added call to function to draw NOAA logo*
 * S. Law/GSC		05/00	added call to cvg_load/geplot		*
 * S. Law/GSC		05/00	reworked max frames checking		*
 * E. Safford/GSC	05/00	add blank map idx to pxmarry		*
 * E. Safford/GSC	05/00	fix miscount on times         		*
 * S. Law/GSC		06/00	removed loop parameter from xpgsvfrm2	*
 * E. Safford/GSC	06/00   removed cvg_load/geplot on map frame    *
 * E. Safford/GSC	07/00   initialize array times                  *
 * S. Jacobs/NCEP	 7/00	Check sub-cat number instead of category*
 * S. Jacobs/NCEP	 9/00	Moved get panel out of times>0 check	*
 * E. Safford/GSC	12/00	replace css_gtime with dsp_getTime 	*
 * H. Zeng/EAI          04/01   moved blank map before the 1st pixmap   *
 * H. Zeng/EAI          04/01   added stop function                     *
 * J. Wu/GSC	  	 4/01	change logo drawing to be loop-specific	*
 * H. Zeng/EAI          04/01   fixed a bug for stop function           *
 * E. Safford/GSC	05/01	fixed a bug in pgen mstr pxmap creation *
 * A. Hardy/GSC          7/01   added queries and saves for text attr.  *
 * E. Safford/GSC	08/01	rmv pxmarry[] param from xmloop_loopSet *
 * H. Zeng/EAI          11/01   modified total_frm value                *
 * J. Wu/SAIC	        11/01   add param in cvg_load() calling		*
 * J. Wu/SAIC	        12/01   add layer in cvg_load() call		*
 * J. Wu/SAIC	        12/01   rebuild range record after xpgrfrsh()	*
 * J. Wu/SAIC	        12/01   replace cvg_load() with cvg_redraw()	*
 * J. Wu/SAIC	        05/03   fix MAX_FRAME bug - out of boundary	*
 * T. Lee/SAIC		09/04	added bin hours to call sequences	*
 * m.gamazaychikov/SAIC 12/04   add dionoff flag to ctb_dtget CS        *
 * m.gamazaychikov/SAIC 04/06   add dtmch flag to ctb_dtget CS          *
 * M. Li/SAIC		03/08	call dslw_getFrstMod in case of CAT_ENS	*
 * F. J. Yen/NCEP	 4/08	add bin mins and mstrct to ctb_dtget CSC*
 ***********************************************************************/
{
    int         ii, jj, kk, frm, count, ier, times; 
    int         curr=0, incr, nsrcs, total_frm, maxfrm, cur_lp, start;
    int         isbcat, d3, d4, d5, d6, d7, d7m, d8, d8m, mstrct,
		dionoff, dtmch;
    int		from, to, offset, new_load; 
    int         jtxfn, jtxhw, jtxwid, jbrdr, jrrotn, jjust;
    float       ssztxt;

    char        asrc[256], *alias, d1[256], d2[256], tmpstr[256];
    char	plot_area[5];
    Boolean	exceeded_pxms, prtflg, view_frm;

    dttms_t	tarry[MAX_FRAME+1];
    dattm_t	c_tm, ftime;
    Widget	wid;
    dsrc_t	*dom;
/*---------------------------------------------------------------------*/

    prtflg  = (Boolean) NxmPrt_isPrtFlgSet ();
    cur_lp = loop_getCurLoop();
/*
 *  initialize times
 */
    for (ii=0; ii < MAX_FRAME+1; ii++) {
	tarry[ii][0] = '\0';
    }
/*
 *  Tell the xw driver which loop is getting loaded.
 */
    xmloop_switchLoop (lp, FALSE);
/*
 *  Set the roam environment.
 */
    if  ( ! prtflg )  {
	roamw_setup (lp, view_lp);
    }

    *loaded = 0;
    exceeded_pxms = FALSE;

    dataw_getDataW(&wid);
/*
 *  Get the times for this loop, based on the dominant source
 */
    times = 0;
    maxfrm = MAX_FRAME;
    exceeded_pxms = FALSE;
    total_frm = loop_getTotalFrames(lp) + 1 - MAX_LOOP;

    dom   = (dsrc_t *)dataw_getDomSrc(lp);

    if (dom != NULL) {
/*
 * Get the data alias name to find the sub-cat number. 
 * This is used to determine the direction of the time line.
 */
	if  ( dom->catg == CAT_VGF )  {
	    strcpy ( tmpstr, "VGF" );
	}
 	else if ( dom->catg == CAT_ENS ) {
	    dslw_getFrstMod ( dom->path, tmpstr );
	}
	else {
	    strcpy (asrc, dom->path);
	    alias  = strtok(asrc, "/");
	    alias  = strtok(NULL, "/");
	    strcpy ( tmpstr, alias );
	}
	ctb_dtget ( tmpstr, d1, d2, &d3, &isbcat, &d4, &d5, &d6,
	        &dionoff, &d7, &d7m, &d8, &d8m, &mstrct, &dtmch, &ier );
    
/*
 *  Forecast data loads first to last, all others last to first
 */
	if  ( isbcat == SCAT_FCT ||
	      isbcat == SCAT_SFF ||
	      isbcat == SCAT_SNF )  {
	    curr = 0;
	    incr = 1;
	}
	else {
	    curr = maxfrm-1;
	    incr = -1;
	}

        for (count = 0, ii = curr; count < MAX_FRAME; count++, ii += incr) {
	    if (total_frm == maxfrm) {
		dom->frm[ii].selected = FALSE;
		exceeded_pxms = TRUE;
	    }
            else if (dom->frm[ii].selected) {
		strcpy(tarry[times+1], dom->frm[ii].ftime);
	        times++;
		total_frm++;
            }
	}
    }

    dataw_getPanelLoc (lp, plot_area);

    if (times > 0) {

/*
 *  Grid data loads first to last, all others last to first
 */
	if  ( isbcat == SCAT_FCT ||
	      isbcat == SCAT_SFF ||
	      isbcat == SCAT_SNF )  {
	    curr = 1;
	    incr = 1;
	}
	else {
	    curr = times;
	    incr = -1;
	}

	mbotw_loadingLoopSet(TRUE, lp);

    	dsp_getTime ( lp, c_tm, &ier );

/*  
 *  FOR EACH FRAME 
 */
        view_frm = TRUE;
        nsrcs = dataw_getNumSrcs(lp);

        for (count = 1, frm = curr; count<=times && !_transferInterrupt; 
             count++, frm+=incr) {
	
	    mbotw_pageSet(frm, times); 

	    dsp_loadFrame(lp, frm, frm, c_tm, tarry[count], 
			   view_lp, nsrcs, view_frm, FALSE);

            if  ( ! prtflg && pgpalw_isUp() && lp == cur_lp)  {
	        xpgsvfrm2 (frm);
            }

/*
 *  Display the most recent frame of the current loop
 *  (view_lp) while everything else loads
 */
  	    if (view_lp && (view_frm || prtflg)) {
	        if (pgpalw_isUp()) { 
  		    cvg_redraw ( NULL, &ier ); 
		}
	        geplot(&ier); 
	    }

	    view_frm = FALSE;

/*
 * check the stop button before going to next frame
 * this should be the LAST ACTION before advancing
 * to next frame
 */
	    NxmBusy_checkStopBtn();

        }   
    }
    else {

/*
 *  No sources -- just load the default map and tell the xw driver
 *  we have just the one pixmap.
 */
        xmloop_loopSet (wid, lp, view_lp, 0, 0, dsp_updatePage, &ier);
    }

/*
 *  Finish the loop up
 */
    if  (!prtflg)  {

/*
 * Check if the total # of frames changed because the user clicked on
 * the STOP button.
 */
        new_load = count - 1;
        if ( _transferInterrupt && (new_load < times) ) {

/*
 * For non-forecast data, shift loaded pixmaps from high end 
 * to low end.
 */
	   if  ( isbcat != SCAT_FCT &&
	         isbcat != SCAT_SFF &&
	         isbcat != SCAT_SNF )  {

               offset   = times - new_load;
    	       for (jj=offset+1; jj<=times; jj++) {

		   from = jj;
		   to   = jj-offset;

                   xscpxm( to, &ier );
                   gclear(&ier); 
		   xcpypxm2 (lp, from, to, &ier);

/*
 *  shift the frame times
 */
		   loop_getFrameTm (lp, from, ftime);
		   loop_setFrameTm (lp, to, ftime);

/*
 * set pixmap index for low end frames
 */
                   loop_setFramePxm(lp, to, to);
	   
               }
	    
	   } /* the end of if( isbcat !=...) */

/*
 * Update the time line to show correct frame info.
 */
	   if  ( isbcat == SCAT_FCT ||
	         isbcat == SCAT_SFF ||
	         isbcat == SCAT_SNF )  {
	       start = 0;
	       incr  = 1;
	   }
	   else {
	       start = maxfrm-1;
	       incr  = -1;
	   }

           ii = start;
           for(kk = new_load+1; kk <= times; kk++) {

	      while ( strcmp(tarry[kk], dom->frm[ii].ftime) != 0 ) {
                      ii += incr;
              }
	      dom->frm[ii].selected = FALSE;

           } /* the end of for(kk =...) */
                  
           dataw_updateTmln();

/*
 * Set new total # of pixmap and current pixmap index.
 */
	   if( curr == times ) {
               times = new_load;
               curr  = times;
           }
           else {
               times = new_load;
           }
 
        } /* the end of if (_transferInterrupt...) */    

/*
 *  add blank map as the first frame of each loop
 */
	xscpxm (0, &ier);
	loop_setFramePxm (lp, 0, 0);
	gclear(&ier);

/* 
 *  Set up animation loop.
 */
        xmloop_loopSet (wid, lp, view_lp, curr, times, 
			dsp_updatePage, &ier);

        gqtext (&jtxfn, &jtxhw, &ssztxt, &jtxwid, &jbrdr, &jrrotn, 
                &jjust, &ier );
        gstext (&_ifnt, &_ihwsw, &_tsize, &_iwid, &_ibrdr, &_irot, 
                &_ijust, &ier );
	nmp_plot(lp, 0, plot_area, &ier);
        gstext (&jtxfn, &jtxhw, &ssztxt, &jtxwid, &jbrdr, &jrrotn, 
                &jjust, &ier );

/*
 *  if we've only loaded a base map (times == 0) and the loop
 *  is currently in view, then plot the map.
 */
  	if (times == 0 && view_lp) { 
	    geplot(&ier);
	}   

	dsp_drawLogo ( lp );

        if  (pgpalw_isUp() && view_lp)  {
	    xpgsvfrm2 (0);
	    xpgrfrsh();
            crg_rebuild();
	}

   
    } /* the end of if(!prtflg...) */

/*
 *  Write out an error message if all requested frames are not 
 *  displayed
 */
    if (exceeded_pxms) {
	dsp_showLoadErr();
    }

/*
 *  Tell the xw driver to reset the bad frame tags for this loop.
 */
    xmfrmtg_resetLp(lp, &ier);

    *loaded = times+1;
    mbotw_loadingLoopSet(FALSE, lp);
}

/*=====================================================================*/

void dsp_loadFrame ( int lp, int frm, int pxm, dattm_t ctime, 
			dttms_t ftime, Boolean view_frm, int nsrcs,
			Boolean fst_frm, Boolean reload )
/************************************************************************
 * dsp_loadFrame 							*
 *                                                                      *
 * This function loads each frame into a pixmap.    			*
 *                                                                      *
 * void dsp_loadFrame( lp, frm, pxm, ctime, ftime, view_frm, nsrcs, 	*
 *					 	     fst_frm, reload )  *
 *                                                                      *
 * Input parameters:                                                    *
 *  lp			int	loop number				*
 *  frm			int	frame number				*
 *  pxm			int	pixmap number				*
 *  ctime		dattm_t current time				*
 *  ftime		dttms_t frame time				*
 *  view_frm		Boolean	True if this frame(lp)is in view now    *
 *  nsrcs		int	number of data sources in loop		*
 *  fst_frm		Boolean	True if this is the first frame in a lp *
 *  reload		Boolean True if this is a frame reload		*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * E. Safford/GSC	10/99	initial coding				*
 * S. Jacobs/NCEP	10/99	ss_gtim -> css_gtim         		*
 * S. Jacobs/NCEP	10/99	removed check for mdl_loaded		*
 * E. Safford/GSC	11/99	remove references to _lpInfo		*
 * S. Jacobs/NCEP	11/99	Added print functionality		*
 * S. Law/GSC		11/99	changed to use new defines		*
 * E. Safford/GSC	01/00	use subcat code for plotting UAIR data	*
 * E. Safford/GSC	03/00	mod param list for dataw_setProj      	*
 * E. Safford/GSC	03/00	add call to im_lutf & fix init load bug *
 * E. Safford/GSC	04/00	remove call to dsp_getMapAttr          	*
 * E. Safford/GSC	05/00	param change to dsp_setProj		*
 * S. Jacobs/NCEP	 5/00	Added call to function to draw NOAA logo*
 * E. Safford/GSC	06/00	remove panel references    		*
 * E. Safford/GSC	06/00	remove lut setting			*
 * S. Law/GSC		07/00	replaced lut setting if printing	*
 * S. Jacobs/NCEP	 2/01	Fixed time match for observed data	*
 * T. Lee/GSC		02/00	added call to dsp_getSingleTmOrdr	*
 * T. Lee/GSC		03/01	changed call seq. of loop_getTmMode	*
 * J. Wu/GSC	  	 4/01	change logo drawing to be loop-specific	*
 * E. Safford/GSC	05/01	add NxmErr_update() call at end		*
 * M. Li/GSC		06/01	add parameter to dsp_getPlotOrdr	*
 * A. Hardy/GSC          7/01   added queries and saves for text attr.  *
 * S. Jacobs/NCEP	 2/02	Added error message after ngd_plot	*
 * T. Piper/SAIC	 4/02	Added st_null to fix UMR		*
 * S. Jacobs/NCEP	10/02	Added check for print flag to set proj	*
 * S. Jacobs/NCEP	11/02	Changed plot order to plot map before	*
 *				misc data types, but after VG files	*
 * T. Lee/SAIC		 8/03	added range/intv to n**_plot		*
 * E. Safford/SAIC	 1/04   always apply lut for 16 & 24-bit mode   *
 * E. Safford/SAIC	 1/04   add mbotw_restoreFade call              *
 * E. Safford/SAIC	 1/04   mv mbotw_restoreFade call (my bad)      *
 * T. Lee/SAIC		 7/04	get TIME_MATCH from pref. table		*
 * T. Lee/SAIC		09/04	added bin hours to call sequences	*
 * m.gamazaychikov/SAIC 12/04   add dionoff flag to ctb_dtget CS        *
 * T. Piper/SAIC	01/05	Added check on dsrc->ionoff == 1	*
 * m.gamazaychikov/SAIC 04/06   change way time matching scheme is set  *
 * M. Li/SAIC		03/08	Added case CAT_ENS			*
 * F. J. Yen/NCEP	04/08	Get bin mins & most recent flag to pass	*
 * M. James/Unidata	04/15	Added check to not plot map for VAD  	*
 ***********************************************************************/
{
    int		kk, ier, which, ignore; 
    int		lens, title, idx;
    int		plot_ordr[MAX_FRMSRC], frmsrc, nplot;
    int         isbcat, icat, nframe, irang, jrang, intrvl;
    int		hrsbfr, hraftr, dionoff, ibf, iaf;
    int		mnsbfr, mnaftr, dmstrct, mbf, maf;
    int		nmin, itarr[5], ndata, dtmch;

    int         jtxfn, jtxhw, jtxwid, jbrdr, jrrotn, jjust;
    float       ssztxt;

    char	img_garea[256], plot_area[5];
    char	imtype[81], iminfo[81], imlutf[81];
    char        asrc[256], *alias, path[256], tmplt[256], tmpstr[256];
    nmpstr_t 	mapInp, projDrp, gareaDrp[2];

    dattm_t	endtim;

    dsrc_t	*dsrc, *dom;

    Boolean	img_loaded, sat_src, rad_src, prtflg, mapdone;

    static Boolean	sat_lutset = FALSE;
    static Boolean	rad_lutset = FALSE;
    static Boolean	nvw = FALSE;
/*---------------------------------------------------------------------*/

    if (nsrcs <= 0) {
        return;
    }

/*
 *  set the current pixmap and order it cleared
 */
    xscpxm( pxm, &ier );
    gclear(&ier); 

/*
 *  store some frame information
 */
    loop_setFrameTm  (lp, frm, ftime);
    loop_setFramePxm (lp, frm, pxm);


    title  = -2;
    dataw_getPanelLoc (lp, plot_area);

    dom    = (dsrc_t *)dataw_getDomSrc(lp);

/*
 *  Determine order of plot
 */

    if (loop_getTmMode(lp)) {
	dsp_getSingleTmOrdr (lp, nsrcs, frm, plot_ordr, &ndata,
			     &img_loaded);
    }
    else {
        dsp_getPlotOrdr(lp, nsrcs, plot_ordr, &img_loaded, &nplot);
	ndata = nplot;
    }

/*
 * If printing, or displaying the "first frame", then set the
 * projection.
 *
 * "First frame" has two meanings:
 * (1) When loading a loop, the first frame is the frame that
 *     is in view while loading the remaining frames.
 * (2) When reloading a frame, "first frame" is True for the
 *     highest frame number. A frame reload is executed when
 *     the user selects the "Reload Frame" button or the
 *     "Print" button on the interface.
 */
    prtflg = (Boolean) NxmPrt_isPrtFlgSet ();

    if  ( fst_frm || prtflg )  {

        if (img_loaded) {
	    dsrc = dataw_getDataSrc (lp, plot_ordr[0]);
	}
	else {
	    dsrc = (dsrc_t *)NULL;
	}

	nmp_sproj(lp, &ier);
    }

/*      
 *  FOR EACH SOURCE
 */
    mapdone = False;
    for (kk=0; kk < ndata; kk++) { 
        frmsrc = plot_ordr[kk];

	dsrc = dataw_getDataSrc(lp, frmsrc);
/*
 * Check for NVW product and set flag to NOT draw map 
 * and latlon for VAD display (kludgy but works)
 */
        if ( strstr(dsrc->path,"NVW") != NULL ) {
            nvw = TRUE;
        } else {
            nvw = FALSE;
	}
/*
 * Get the table entry info for this data source.
 */
	if  ( dsrc->catg == CAT_VGF )  {
	    strcpy ( tmpstr, "VGF" );
	}
	else if ( dsrc->catg == CAT_ENS ) {
            dslw_getFrstMod ( dsrc->path, tmpstr );
        }
	else {
	    strcpy (asrc, dsrc->path);
	    alias  = strtok(asrc, "/");
	    alias  = strtok(NULL, "/");
	    strcpy ( tmpstr, alias );
	}
	ctb_dtget ( tmpstr, path, tmplt, &icat, &isbcat,
		    &nframe, &irang, &intrvl, &dionoff, &hrsbfr, 
		    &mnsbfr, &hraftr, &mnaftr, &dmstrct, &dtmch, &ier ); 

/*
 * Only reset the end time for observed data.
 */
	if  ( ( isbcat == SCAT_SFF ) ||
	      ( isbcat == SCAT_SNF ) ||
	      ( isbcat == SCAT_FCT ) )  {
	    strcpy ( endtim, ctime );
	}
	else {

/*
 * Set the end time, for the time match, to the frame
 * time plus half the range.
 */
	    jrang = irang / 2;
	    ti_ctoi ( ftime, itarr, &ier, strlen(ftime) );
	    ti_addm ( itarr, &jrang, itarr, &ier );
	    ti_itoc ( itarr, endtim, &ier, sizeof(endtim) );
	    st_null ( endtim, endtim, &lens, &ier, sizeof(endtim), sizeof(endtim) );

/*
 * If the end time is after the current time,
 * use the current time.
 */
	    ti_diff ( ctime, endtim, &nmin, &ier,
		      strlen(ctime), strlen(endtim) );

	    if  ( nmin <= 0 )  {
		strcpy ( endtim, ctime );
	    }
	}

	switch (dsrc->catg) {
	    case CAT_IMG:

		nmp_gmapattr(lp, mapInp, projDrp, gareaDrp, &ier);

		which = ( strlen(gareaDrp[1]) > (size_t)0 ) ? 1 : 0;
           	strcpy(img_garea, gareaDrp[which]);

  		if ((Boolean) NxmPrt_isPrtFlgSet ()) { 
		    sat_src = dataw_isSatSelect(lp, &idx);
		    rad_src = dataw_isRadSelect(lp, &idx);

  		    if ( view_frm || (sat_src && !sat_lutset) || 
			 (rad_src && !rad_lutset)) { 
  			nim_qatt (dsrc->attridx, 
				  imtype, iminfo, imlutf, &ier);   
			im_lutf (imlutf, &ier, strlen(imlutf));
  
			if (sat_src) {
			    sat_lutset = TRUE;
			}
			else {
			    rad_lutset = TRUE;
			}
		    }
		}

/* 
 * Initialize text attributes before loading image. 
 */
		gqtext (&jtxfn, &jtxhw, &ssztxt, &jtxwid, &jbrdr, 
		        &jrrotn, &jjust, &ier );
		gstext (&_ifnt, &_ihwsw, &_tsize, &_iwid, &_ibrdr, 
		        &_irot, &_ijust, &ier );

		nim_plot ((Pixmap)pxm, dsrc->attridx, img_garea, 
			   plot_area, ftime, endtim, dsrc->range, 
			   -1, dom->domtmmtch, 0, title, &ier );
		gstext (&jtxfn, &jtxhw, &ssztxt, &jtxwid, &jbrdr, 
		        &jrrotn, &jjust, &ier );

		break;

	    case CAT_SFC:
	    case CAT_SFF:
	    case CAT_SND:
	    case CAT_SNF:

		if ( dsrc->ionoff == 1 ) {
		    ibf = dsrc->bfhr;
		    mbf = dsrc->bfmn;
		    iaf = dsrc->afhr;
		    maf = dsrc->afmn;
		    dmstrct = dsrc->mstrctf;
		}
		else {
		    ibf = 0;
		    mbf = 0;
		    iaf = 0;
		    maf = 0;
		    dmstrct = 0;
		}

		if (isbcat == SCAT_SND || isbcat == SCAT_SNF) {
		    nsn_plot ( (Pixmap)pxm, dsrc->attridx,
			        plot_area, ftime, endtim, 
				dsrc->range, -1, dom->domtmmtch,
				0, title, ibf, mbf, iaf, maf,
				dmstrct, &ier);
		}
		else {   
		    nsf_plot ( (Pixmap)pxm, dsrc->attridx,
			        plot_area, ftime, endtim, 
				dsrc->range, -1, dom->domtmmtch,
				0, title, ibf, mbf, iaf, maf,
				dmstrct, &ier);
  		}   
		break;

	    case CAT_GRD:
	    case CAT_ENS:
		ngd_plot ((Pixmap)pxm, dsrc->attridx,
			   plot_area, ftime, endtim, 
			   dsrc->range, -1, dom->domtmmtch, 
                           0, title, &ier);
		if ( ier != 0 ) 
			er_wmsg ( "NGD", &ier, NULL, &ignore,
			  strlen("NGD"), 0 );
		break;

	    case CAT_VGF:
		nms_plot ((Pixmap)pxm, dsrc->attridx,
			   plot_area, ftime, endtim, irang,
			   dom->domtmmtch, 0, title, &ier);
		break;

	    case CAT_MSC: 
		if  ( !mapdone )  {
		    gqtext (&jtxfn, &jtxhw, &ssztxt, &jtxwid, &jbrdr,
		    	    &jrrotn, &jjust, &ier );
		    gstext (&_ifnt, &_ihwsw, &_tsize, &_iwid, &_ibrdr,
		    	    &_irot, &_ijust, &ier );
		    nmp_plot(lp, (Pixmap)pxm, plot_area, &ier);  
		    gstext (&jtxfn, &jtxhw, &ssztxt, &jtxwid, &jbrdr,
		     	    &jrrotn, &jjust, &ier );
		    mapdone = True;
		}
		nms_plot ((Pixmap)pxm, dsrc->attridx,
			   plot_area, ftime, endtim, irang,
			   dom->domtmmtch, 0, title, &ier);
		break;

	    default:
		break;


 	}	/* switch */

  	title--;

    }  /* kk or SOURCE */		    	  

/*
 *  Draw map
 */
    if  ( !mapdone )  {
	gqtext (&jtxfn, &jtxhw, &ssztxt, &jtxwid, &jbrdr, &jrrotn, 
		&jjust, &ier );
	gstext (&_ifnt, &_ihwsw, &_tsize, &_iwid, &_ibrdr, &_irot, 
		&_ijust, &ier );
	
	if (!nvw) {
	    nmp_plot(lp, (Pixmap)pxm, plot_area, &ier);  
	}
	gstext (&jtxfn, &jtxhw, &ssztxt, &jtxwid, &jbrdr, &jrrotn, 
		&jjust, &ier );
    }

/*
 * Draw the logo, if it is active.
 */
    dsp_drawLogo ( lp );
    NxmErr_update();
}

/*=====================================================================*/

void dsp_print ( void )
/************************************************************************
 * dsp_print	 							*
 *                                                                      *
 * This function prints an entire loop or an individual frame.		*
 *                                                                      *
 * void dsp_print ( )                                                	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * S. Jacobs/NCEP	11/99	Created					*
 * E. Safford/GSC	12/99	Fixed palette reset bug			*
 * S. Jacobs/NCEP	 5/00	Added call to function to draw NOAA logo*
 * E. Safford/GSC	07/00	add setBusy calls      			*
 * S. Jacobs/NCEP	 2/01	Added load prod gen file if no data	*
 * J. Wu/GSC	  	 4/01	change logo drawing to be loop-specific	*
 * A. Hardy/GSC          7/01   added queries and saves for text attr.  *
 * J. Wu/SAIC	        11/01   add param in cvg_load() calling		*
 * J. Wu/SAIC	        12/01   add layer in cvg_load() call		*
 * J. Wu/SAIC	        12/01   rebuild range record after xpgrfrsh()	*
 * J. Wu/SAIC	        12/01   replace cvg_load() with cvg_redraw()	*
 ***********************************************************************/
{
    int		loop, ifrm, ier;
    int         jtxfn, jtxhw, jtxwid, jbrdr, jrrotn, jjust;
    float       ssztxt;

    char	plot_area[5];

    Boolean	lpflg;

/*---------------------------------------------------------------------*/

    dsp_setBusy (TRUE);

    loop = loop_getCurLoop ();
    ifrm = loop_getCurFrame ();

    lpflg = dataw_isLoopActv ( loop );

    if  ( lpflg )  {

	if  ( NxmPrt_isPgFlgSet () )  {
	    dsp_reloadLoop ( loop, &ier );
	}
	else {
	    dsp_reloadFrame ( loop, ifrm );
	}
    }
    else {
	dataw_getPanelLoc ( loop, plot_area );
        gqtext (&jtxfn, &jtxhw, &ssztxt, &jtxwid, &jbrdr, &jrrotn, 
                &jjust, &ier );
        gstext (&_ifnt, &_ihwsw, &_tsize, &_iwid, &_ibrdr, &_irot, 
                &_ijust, &ier );
	nmp_plot ( loop, 0, plot_area, &ier );  
        gstext (&jtxfn, &jtxhw, &ssztxt, &jtxwid, &jbrdr, &jrrotn, 
                &jjust, &ier );
	dsp_drawLogo ( loop );

	if  ( pgpalw_isUp() )  {	    
	    cvg_redraw ( NULL, &ier ); 
        }
    }

    if ( pgpalw_isUp() ) {
	xpgrfrsh();
	pgpalw_setupOper();
    } 
    dsp_setBusy (FALSE);
}

/*=====================================================================*/

void dsp_showLoadErr ( void ) 
/************************************************************************
 * dsp_showLoadErr							*
 *                                                                      *
 * This function displays an error message to the user.  At present	*
 * only the only condition that is flagged as an error is attempting to *
 * load more than MAP_PIX pixmaps.					*
 *                                                                      *
 * void dsp_showLoadErr ( )                                           	*
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * E. Safford/GSC	04/00	inital coding          			*
 ***********************************************************************/
{
int	error, ignore;
/*---------------------------------------------------------------------*/

    error = G_NMAXFR;
    er_wmsg("gemplt", &error, NULL, &ignore, strlen("gemplt"), 0 );
    NxmErr_update();
}

/*=====================================================================*/

void dsp_drawLogo ( int lp )
/************************************************************************
 * dsp_drawLogo 							*
 *                                                                      *
 * This function draws the logo to the current pixmap.			*
 *                                                                      *
 * void dsp_drawLogo ( lp )        					*
 *                                                                      *
 * Input parameters:                                                    *
 *   	lp		int	loop number 			        *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * S. Jacobs/NCEP	 5/00	Created					*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * J. Wu/GSC		 3/01   Added logo emblem ID 			*
 * J. Wu/GSC		 4/01   Enabled drawing multiple logos 		*
 * J. Wu/GSC		 5/01   Added drawing NWS logo 			*
 ***********************************************************************/
{
int	logo_mode, icm, bw, curclr, ilog, ier, ii;
float	size, xn, yn, xloc, x_ll_offset, x_ur_offset;
char	name[15];
/*---------------------------------------------------------------------*/
    
    x_ll_offset = 0.0F;
    x_ur_offset = 0.0F; 
          
    if ( logo_lpLogoActv( lp ) ) {
    
        for ( ii = 0; ii < MAX_LOGO; ii++ ) {
        	
            if  ( logo_isLogoActv ( lp, ii ) )  {

                logo_mode = logo_getCurLogo ( lp, ii );
    	        logo_getInfo ( logo_mode, name, &size, &xn, &yn, &icm );
	    	    
	        if  ( icm == 1 )  {
	            gqcolr ( &curclr, &ier );
	            bw = 1;
	            gscolr ( &bw, &ier );
	        }

	        ilog = ii + 1;	/* Logo ID defined from 1 as NOAA_LOGO */

/*
 *  Count the offset and draw logo.
 */	        
		if ( xn < 0.5F )   
	            xloc = xn + x_ll_offset; 	/* Lower left corner */	
		else
		    xloc = xn + x_ur_offset;    /* upper right corner */
		    
		glogo ( sys_N, &xloc, &yn, &size, &icm, &ilog, &ier, strlen(sys_N) );

	        if  ( icm == 1 )  gscolr ( &curclr, &ier );

/*
 *  Update the offset for drawing next logo.
 */
		switch( logo_mode ) {
		    case 1:
		    case 3:
                        x_ll_offset += LOGO_SMALL_OFFSET; 
		        break;
		    
		    case 2:
		        x_ll_offset += LOGO_LARGE_OFFSET;
		        break;
		    
		    case 4:
		    case 6:
		        x_ur_offset -= LOGO_SMALL_OFFSET;
		        break;

		    case 5:
		        x_ur_offset -= LOGO_LARGE_OFFSET;
		        break;		    
		
		}  /* end of switch */	    
	    }  /* end of drawing one logo */	    
	}  /* end of drawing all logos */ 
    }  
} 

/*=====================================================================*/

static void dsp_getTime ( int lp, dattm_t dttm, int *iret )
/************************************************************************
 * dsp_getTime	 							*
 *                                                                      *
 * This function returns the reference time for the loop.  This will 	*
 * either be the current time, or the time set in the data window.      *
 * This will always be in GMT.						*
 *                                                                      *
 * static void dsp_getTime ( lp, dttm, iret ) 				*
 *                                                                      *
 * Input parameters:                                                    *
 *	lp		int	loop number				*
 *									*
 * Output parameters:                                                   *
 *	dttm		dattm_t	date time string			*
 *	*iret		int	return code 0 = normal -1 = error	*
 *									*
 * Return parameters:                                                   *
 *                      NONE                                            *
 **                                                                     *
 * E. Safford/GSC	12/00	initial coding          		*
 * B. Yin/SAIC          03/04   changed css_gtim calling sequences      *
 ***********************************************************************/
{
int	ier, itype = 1;
dttmi_t *date;

/*---------------------------------------------------------------------*/

    *iret = 0;

    if ( dataw_useRefTm(lp) ) {

	date = (dttmi_t *)dataw_getRefTm(lp);

	sprintf ( dttm, "%02d%02d%02d/%02d%02d",
            date->year%100, date->mon+1, date->day, date->hour, date->min );
    }
    else {

        css_gtim (&itype, dttm, &ier); 

	if ( ier != 0 ) {
	    *iret = -1;
	}
    }
}

/*=====================================================================*/

void dsp_loadSingleTime ( int lp, Boolean view_lp, int *loaded )
/************************************************************************
 * dsp_loadSingleTime 							*
 *                                                                      *
 * This function loads a loop with one grid field and multiple products *
 * which all valid at the same time.					*
 *                                                                      *
 * void dsp_loadSingleTime( lp, view_lp, loaded )   			*
 *                                                                      *
 * Input parameters:                                                    *
 *  lp			int	loop number				*
 *  view_lp		Boolean	True if loop is to be in view after load*
 *                                                                      *
 * Output parameters:                                                   *
 *  *loaded		int	number of pixmaps loaded		*
 *									*
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * T. Lee/GSC		 2/01	initial coding				*
 * H. Zeng/EAI          04/01   moved blank map before the 1st pixmap   *
 * H. Zeng/EAI          04/01   add stop function for NMAP2             *
 * J. Wu/GSC	  	 4/01	change logo drawing to be loop-specific	*
 * H. Zeng/EAI          05/01   modified to fix clearing VG from data   *
 * M. Li/GSC		07/01	added a check for the off source	*
 * A. Hardy/GSC          7/01   added queries and saves for text attr.  *
 * E. Safford/GSC	08/01	rmv pxmarry[] param from xmloop_loopSet *
 * J. Wu/SAIC	        11/01   add param in cvg_load() calling		*
 * J. Wu/SAIC	        12/01   add layer in cvg_load() call		*
 * J. Wu/SAIC	        12/01   rebuild range record after xpgrfrsh()	*
 * J. Wu/SAIC	        12/01   replace cvg_load() with cvg_redraw()	*
 * J. Wu/SAIC	        05/03   fix MAX_FRAME bug - out of boundary	*
 * M. Li/SAIC		03/08	Added case CAT_ENS			*
 ***********************************************************************/
{
    int		ii, jj, frm, ier; 
    int		nsrcs, total_frm, maxfrm, cur_lp;
    int		nframs;
    int         jtxfn, jtxhw, jtxwid, jbrdr, jrrotn, jjust;
    float       ssztxt;

    char	plot_area[5];
    Boolean	exceeded_pxms, prtflg, view_frm;

    dttms_t	tarry[MAX_FRAME+1];
    dattm_t	c_tm;
    Widget	wid;
    dsrc_t	*dom, *dsrc;
/*---------------------------------------------------------------------*/

    prtflg  = (Boolean) NxmPrt_isPrtFlgSet ();
    cur_lp = loop_getCurLoop();

/*
 *  initialize times
 */
    for (ii=0; ii < MAX_FRAME+1; ii++) {
	tarry[ii][0] = '\0';
    }

/*
 *  Tell the xw driver which loop is getting loaded.
 */
    xmloop_switchLoop (lp, FALSE);

/*
 *  Set the roam environment.
 */
    if  ( ! prtflg )  {
        roamw_setup (lp, view_lp);
    }

    *loaded = 0;
    exceeded_pxms = FALSE;

    dataw_getDataW(&wid);

/*
 *  Get the number of frames for this loop, based on the number
 *  of the grid fields selected.
 */
    maxfrm = MAX_FRAME;
    exceeded_pxms = FALSE;
    total_frm = loop_getTotalFrames(lp);

    nframs = 0;
    dom    = (dsrc_t *)dataw_getDomSrc(lp);
    nsrcs  = dataw_getNumSrcs(lp);

    if (dom != NULL && (dom->catg == CAT_GRD || dom->catg == CAT_ENS) ) {

	for ( ii = 0; ii < nsrcs; ii++ ) {
	    if ( total_frm == maxfrm ) {
		exceeded_pxms = TRUE;
		dom->frm[ii].selected = FALSE;
		break;
	    }
	    else {
		dsrc = (dsrc_t *)dataw_getDataSrc (lp, ii);
		if ( (dsrc->catg == CAT_GRD || dom->catg == CAT_ENS) && dsrc->src_on ) {
		    total_frm++;
		    for ( jj = 0; jj < maxfrm; jj++) {
			if (dom->frm[jj].selected) {
		    	    strcpy (tarry[nframs+1], dom->frm[jj].ftime);
			}
		    }
		    nframs++;
		}
	    }
	}
    }

    dataw_getPanelLoc (lp, plot_area);


    if ( nframs > 0 ) {

	mbotw_loadingLoopSet(TRUE, lp);

	dsp_getTime ( lp, c_tm, &ier );

/*  
 *  For each frame, load one grid field and the rest of the non-grid
 *  field. 
 */
	view_frm = TRUE;

	for (frm = 1; frm<=nframs && !_transferInterrupt; frm++) {
	
	    mbotw_pageSet(frm, nframs); 

	    dsp_loadFrame(lp, frm, frm, c_tm, tarry[frm], 
			   view_lp, nsrcs, view_frm, FALSE);

            if  ( ! prtflg && pgpalw_isUp() && lp == cur_lp)  {
	        xpgsvfrm2 (frm);
            }

/*
 *  Display the most recent frame of the current loop
 *  (view_lp) while everything else loads
 */
  	    if (view_lp && (view_frm || prtflg)) {
	        if (pgpalw_isUp()) { 
		    cvg_redraw ( NULL, &ier ); 
		}
	        geplot(&ier); 
	    }

	    view_frm = FALSE;

/*
 * check the stop button before going to next frame
 * this should be the LAST ACTION before advancing
 * to next frame
 */
	    NxmBusy_checkStopBtn();

        }   
    }
    else {

/*
 *  No sources -- just load the default map and tell the xw driver
 *  we have just the one pixmap.
 */
        xmloop_loopSet (wid, lp, view_lp, 0, 0, 
			dsp_updatePage, &ier);
    }

/*
 *  Finish the loop up
 */
    if  (!prtflg)  {

/*
 *  add blank map to each loop
 */
	xscpxm (0, &ier);
	loop_setFramePxm (lp, 0, 0);
	gclear(&ier);

/* 
 * Check if the total # of frames changed because the user clicked on
 * the STOP button.
 */
        if (_transferInterrupt) {

           nframs = frm - 1; 
        }  

/* 
 *  Set up animation loop.
 */
        xmloop_loopSet (wid, lp, view_lp, 1, nframs, 
			dsp_updatePage, &ier);

        gqtext (&jtxfn, &jtxhw, &ssztxt, &jtxwid, &jbrdr, &jrrotn, 
                &jjust, &ier );
        gstext (&_ifnt, &_ihwsw, &_tsize, &_iwid, &_ibrdr, &_irot, 
                &_ijust, &ier );
	nmp_plot(lp, 0, plot_area, &ier);
        gstext (&jtxfn, &jtxhw, &ssztxt, &jtxwid, &jbrdr, &jrrotn, 
                &jjust, &ier );

	dsp_drawLogo ( lp );

        if  (pgpalw_isUp() && view_lp)  {
	    xpgsvfrm2 (0);
	    xpgrfrsh();
            crg_rebuild();
	}

/*
 *  if we've only loaded a base map (nframs == 0) and the loop
 *  is currently in view, then plot the map.
 */
  	if (nframs == 0 && view_lp) { 
	    geplot(&ier);
	}   
    }

/*
 *  Write out an error message if all requested frames are not 
 *  displayed
 */
    if (exceeded_pxms ) {
	dsp_showLoadErr();
    }

/*
 *  Tell the xw driver to reset the bad frame tags for this loop.
 */
    xmfrmtg_resetLp(lp, &ier);
    *loaded = nframs+1;
    mbotw_loadingLoopSet(FALSE, lp);
}

/*=====================================================================*/

void dsp_getSingleTmOrdr ( int lp, int nsrcs, int frm, int plot_ordr[], 
					int *ndata, Boolean *img_data )
/************************************************************************
 * dsp_getSingleTmOrdr							*
 *                                                                      *
 * This function determines the plot order for the single time loop	*
 * data in the given panel.						*
 *                                                                      *
 * void dsp_getSingleTmOrdr(lp, nsrcs, frm, plot_ordr, ndata, img_data)	*
 *                                                                      *
 * Input parameters:                                                    *
 *  lp  		int	loop number                  		*
 *  nsrcs		int	number of data sources			*
 *  frm			int	panel number				*
 *                                                                      *
 * Output parameters:                                                   *
 *  plot_ordr[] 	int	array of indicies to data sources	*
 *  *ndata		int	number of data per panel 		*
 *  *img_data		Boolean True if image data in panel		*
 *									*
 **                                                                     *
 * T. Lee/GSC		 2/01	initial coding				*
 * H. Zeng/EAI          04/01   moved blank map before the 1st pixmap   *
 * M. Li/GSC		06/01	add a check for off-data source		*
 * M. Li/GSC		07/01	subtract ndata by the "off" grid source	*
 * E. Safford/GSC	07/01	revise noff calculation    		*
 * M. Li/SAIC		03/08	Added case CAT_ENS			*
 ***********************************************************************/
{
dsrc_t	*dsrc, *dom;
Cardinal	kk;
int	ii, cnt, idoms, ngds, noff;
int	catg_ordr[] = {CAT_IMG, CAT_GRD, CAT_ENS, CAT_SFC, CAT_SND, CAT_SFF, 
		       CAT_SNF, CAT_MSC, CAT_VGF, CAT_NIL};
/*---------------------------------------------------------------------*/

    *img_data = FALSE;

/*
 *  initial output 
 */
    for (ii=0; ii<nsrcs; ii++) {
	plot_ordr[ii] = -1;
    }

    cnt  = 0;
    ngds = 0;
    noff = 0;

    if (nsrcs == 1) {
	plot_ordr[0] = 0;
	ngds++;
    }
    else {
	idoms = 1;
	dom   = (dsrc_t *)dataw_getDomSrc (lp);

/*
 *  Count the number of sources switched off.  These won't be plotted.
 */
        for (ii = 0; ii < nsrcs; ii++) {
            dsrc = dataw_getDataSrc(lp, ii);
	    if (!dsrc->src_on) {
	        noff++;
	    }
        }

/*
 *  Step through each data category and put data in plotting order.
 */
	for ( kk = 0; kk < XtNumber(catg_ordr) && cnt < nsrcs; kk++ ) {

            for (ii = 0; ii < nsrcs; ii++) {
                dsrc = dataw_getDataSrc(lp, ii);
                if (dsrc->catg == CAT_IMG && dsrc->src_on ) {
                    *img_data = TRUE;
                }

                if ( dsrc->src_on ) {

/*
 *  One grid data at a time.
 */
                    if ( dsrc->catg == catg_ordr[kk] ) {
                        if (dsrc->catg == CAT_GRD || dsrc->catg == CAT_ENS ) {
                            ngds++;
                            if ( frm == 1 ) {
                                if ( dom->attridx == dsrc->attridx ) {
                                    plot_ordr [cnt] = ii;
                                    cnt++;
                                }
                            }
                            else {
                                if ( dom->attridx != dsrc->attridx ) {
                                    idoms++;
                                    if ( frm == idoms ) {
                                        plot_ordr [cnt] = ii;
                                        cnt++;
                                    }
                                }
                            }
                        }
                        else {
                            plot_ordr [cnt] = ii;
                            cnt++;
                        }
                    }
                }
            }
        }
    }

    /*
     *
     * The new nsrcs is recomputed.
     *
     */
    *ndata = nsrcs - ngds - noff + 1;

}

/*=====================================================================*/

void dsp_addLogo ( int lp )
/************************************************************************
 * dsp_addLogo 								*
 *                                                                      *
 * This function adds a new logo to each frame in a given loop.		*
 *                                                                      *
 * void dsp_addLogo ( lp )        					*
 *                                                                      *
 * Input parameters:                                                    *
 *   	lp		int	loop number 			        *
 *                                                                      *
 * Output parameters:                                                   *
 *                      none                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      none                                            *
 *                                                                      *
 **                                                                     *
 * J. Wu/GSC	  	 4/01	Moved from dsp_addAllLogo()		*
 * T. Piper/SAIC	09/06	Removed xmloop_switchLoop		*
 ***********************************************************************/
{
int	frm, pxm, ier, cur_lp, nfrms;

Boolean	prtflg, view_lp;
/*---------------------------------------------------------------------*/

    prtflg = (Boolean) NxmPrt_isPrtFlgSet ();
    cur_lp = loop_getCurLoop ( );
/*
 *  Add the logo to each frame in the loop.
 */

    if ( lp == cur_lp ) {
	view_lp = TRUE;
    }
    else {
	view_lp = FALSE;
    }
/*
 *  Set the roam environment.
 */
    if  ( ! prtflg )  {
        roamw_setup (lp, view_lp);
    }

    nfrms = loop_getNumFrames ( lp );

    for ( frm = 1; frm <= nfrms; frm++ )  {

/*
 * For each frame, get the pixmap and draw the logo to it.
 */
	pxm = loop_getFramePxm ( lp, frm );
	xscpxm ( pxm, &ier );

	dsp_drawLogo ( lp );

    }
}

/*=====================================================================*/
