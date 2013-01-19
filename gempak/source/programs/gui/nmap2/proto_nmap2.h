/************************************************************************
 * proto_nmap2.h							*
 *									*
 * This include file contains prototypes for all the NMAP2.		*
 **									*
 * A. Hardy/GSC		 1/01   Created					*
 * E. Safford/GSC	03/01	added roamw_enableMenu & _disableMenu	*
 * T. Lee/GSC		03/01	added loop_save & _restore roam/TmMode	*
 * E. Safford/GSC	04/01	remove roamw_imgBtnSensitive()       	*
 * H. Zeng/EAI          04/01   added dataw_upateTmln()                 *
 * J. Wu/GSC            04/01   Added loopw_hide()                      *
 * J. Wu/GSC		04/01	add logo_selectCb() & dsp_addLogo()	*
 * H. Zeng/EAI          04/01   added some roamw_xxxxx functions        *
 * J. Wu/GSC		04/01   Added pgpalw_undo() & pgpalw_redo()	*
 * J. Wu/GSC		05/01   Added mbotw_fadeToggle()		*
 * J. Wu/GSC		05/01   add logo_lpLogoActv & mmenuw_getLogoName*
 * S. Jacobs/NCEP        5/01   Changed dataw_getImgInfo                *
 * H. Zeng/EAI          05/01   Changed roamw_arrow*() paras            *
 * M. Li/GSC		06/01	Added mbotw_*Fade* functions          	*
 * J. Wu/GSC		06/01	add SPF functions          		*
 * J. Wu/GSC		07/01	add spfw_getFileName(), dataw_setLoop() *
 *                              dataw_loadsp(), dataw_setLpBtns(),  	*
 *                              dataw_noActSrc()		  	*
 * J. Wu/GSC		07/01	add dataw_clearDataSel()		*
 * J. Wu/GSC		07/01	add spfw_getSource()			*
 * J. Wu/GSC		07/01	add dslw_validSrc()			*
 * J. Wu/GSC		08/01	add dataw_getRoamVal()			*
 * E. Safford/SAIC	09/01	add pgpalw_refresh()  			*
 * S. Jacobs/NCEP	10/01	Added dataw_setImageNav			*
 * E. Safford/SAIC	11/01	add spfw_isUp()       			*
 * M. Li/SAIC		12/01	add mapset functions			*
 * M. Li/SAIC		01/02	modified mapset_popup			*
 * E. Safford/SAIC	01/02	add mcanvw_catchCtrl			*
 * H. Zeng/EAI          05/02   changed para list for zoomw_setZoom     *
 * R. Tian/SAIC		11/02	add auto update lock related functions	*
 * H. Zeng/XTRIA        12/02   added loopw_stopLooping                 *
 * T. Piper/SAIC	05/03	changed gmpk_init			*
 * T. Piper/SAIC	06/03	added mmenuw_roamShareGet		*
 * M. Li/SAIC		06/03	add dslw_getGrdCycle & dslw_getMosCycle	*
 * J. Wu/SAIC		07/03	add spfw_getRangeIntv()			*
 * T. Piper/SAIC        07/03   added mmenuw_loopStopGet                *
 * T. Piper/SAIC	07/03	removed gmpk_rgstr			*
 * M. Li/SAIC		12/03	add dataw_cmdLineLoadSPF and cmdln_*	*
 * H. Zeng/XTRIA	01/04	added aodtw_xxxx functions		*
 * T. Lee/SAIC		02/04	added dataw_getBaseTm, tmln_getLastTm	*
 *				dataw_getRefTimeSetFlag			*
 * E. Safford/SAIC	02/04	add params to mbotw_restoreFade		*
 * H. Zeng/XTRIA	03/04	added more loops			*
 * T. Lee/SAIC		04/04	removed dataw_setNewRefTm		*
 * T. Lee/SAIC		04/04	removed dataw_getRefTimeSetFlag		*
 * H. Zeng/SAIC		04/04	added a para for aodtw_popup()		*
 * T. Lee/SAIC		05/04	added dataw_setNewRefTm, dttmw_popdown	*
 * H. Zeng/SAIC		09/04	added mainw_setIconName()		*
 * T. Piper/SAIC	12/04	Moved pgpalw_refresh to proto_nmaplib.h	*
 * T. Piper/SAIC        12/04   added aodtw_refresh & cldhgtw_refresh   *
 * T. Lee/SAIC          10/04   added spfw_getTmBin()                   *
 * J. Wu/SAIC           03/05   add mainw_setTitleName()		*
 * M. Li/SAIC		06/05	added aodtw*				*
 * M. Li/SAIC		01/07	added aodtw72*, removed aodtw63*	*
 * D.W.Plummer/NCEP	02/07	added mbtnw_CtlBtnsSetSensitive		*
 * H. Zeng/SAIC		03/07	added seekw_destroyWidget		*
 * H. Zeng/SAIC		03/07   added dataw_getTimeStr			*
 * H. Zeng/SAIC		03/07	added mmenuw_lllValGet			*
 * E. Safford/SAIC	07/07	modify mainw_setTitleName		*
 * H. Zeng/SAIC		07/07	modify dslw_popup			*
 * M. Li/SAIC		12/07	added mmenuw_zoomModeGet, zoomw_czoom	*
 *				  and zoomw_zzoom			*
 * M. Li/SAIC		02/08	add gtbl_getEnsProd, dslw_getEns...	*
 * F. J. Yen/NCEP	04/08	added parameters to nmap_spfw_getTmBin	*
 * M. Li/SAIC		05/08	Added dslw_ dslw_ensSel*		*
 * S. Jacobs/NCEP	 3/10	Added cmdln_getLoadVGF, cmdln_getVGF	*
 ***********************************************************************/

#ifndef PROTO_NMAP2
#define	PROTO_NMAP2

/*
 *	NMAP2 prototypes
 *
 *  nidstbl function  -------------------------------
 */
void 	nids_loadTable ( 	int *iret 	);
char   *nids_getLabel  ( 	char *dirnam    ); 
/*
 *  aodtw functions  --------------------------------
 */
Widget  aodtw72_create 	(	Widget   parent);
Widget  aodtw64_create 	(	Widget   parent);
void	aodtw_create   	(	Widget   parent);
void	aodtw_getnumvers( 	int *numv, int *maxlen);
void 	aodtw_getversnm ( 	char ***vname );

Boolean aodtw72_isUp   	(	void 		); 
Boolean aodtw64_isUp   	(	void 		); 
Boolean aodtw_isUp     	(	void 		); 

void    aodtw72_popup  	(	char *ver	);   
void    aodtw64_popup  	(	char *ver	);   
void    aodtw_popup    	(	void 		);   

void 	aodtw_readTable ( 	int *iret 	);

void	aodtw72_refresh (	Boolean	make_new );
void	aodtw64_refresh (	Boolean	make_new );
void	aodtw_refresh  	(	Boolean	make_new );

void 	aodtw_setvers 	( 	char *vers, int *iret );
/*
 *  auto functions  -----------------------------------
 */
Boolean auto_getAutoUpdt (	int 	lp);
void	auto_startAutoUpdt (	void );
void	auto_stopAutoUpdt (	void );

/*
 *  cldhgtw functions  --------------------------------
 */
Widget	cldhgtw_create (	Widget   parent);
Boolean cldhgtw_isUp (		void ); 
void	cldhgtw_popdown (	void ); 
void	cldhgtw_popup (		void );
void    cldhgtw_refresh (	Boolean	make_new );

/*
 * cmdln functions  ----------------------------------
 */
void 	cmdln_cleanUp 		( void );
Boolean cmdln_getAutoLoadData   ( void );
Boolean cmdln_getLoadSPF	( void );
Boolean cmdln_getLoadVGF	( void );
Boolean cmdln_getShowHelp       ( void );
void 	cmdln_getSPF 		( char 	  *filename );
void 	cmdln_getVGF 		( char 	  *filename );
void 	cmdln_parse     	(  int     argc,   
                           	   char    **argv,
                           	   int     *iret );

/*
 *  cursorw functions  --------------------------------
 */
void	cursorw_create (	Widget  parent );
Boolean cursorw_isUp (		void ); 
void	cursorw_popdown (	void ); 
void	cursorw_popup (		void );

/*
 *  dataw functions  ---------------------------------
 */
void	dataw_clearLoop (	int	lp );
void	dataw_cmdLineLoadSPF (	char	*spFile,
				Boolean	load );
void	dataw_create ( 		Widget 	parent );
void	dataw_getDataW (	Widget	*wdgt );
dsrc_t *dataw_getDomSrc (	int	lp );
void	dataw_getImgInfo (	int	lp,
				char 	*fname );
void	dataw_getIRInfo (	char	*dttm,
				char	*path,
				int	*iret );
void	dataw_getStnmName (	char	path[],
				Boolean	extra_flag,
				char	*typstr,
				char	*alias );
void	dataw_getTimeStr  (	int	lp, 
				Boolean set_flag, 
				char	*time  );
void	dataw_getBaseTm (	int	lp, 
				dttms_t btime );
int	dataw_getSkip (		int	lp );
Boolean	dataw_useRefTm (	int	lp );
dttmi_t *dataw_getRefTm (	int	lp );
int	dataw_getNumSrcs (	int	loop );
dsrc_t *dataw_getDataSrc (	int	loop,
				int	frmsrc );
int	dataw_getNumLoops (	void );
void	dataw_getPanelLoc (	int	panel,
				char	location[] );
Boolean dataw_isUp (		void );
Boolean	dataw_isLoopActv (	int	loop );
Boolean	dataw_isSatSelect (	int	loop,
				int	*iindex );
Boolean	dataw_isRadSelect (	int	loop,
				int	*iindex );
Boolean	dataw_isImgInLoop (	int	loop );
Boolean	dataw_isImgDom (	int	loop );
void	dataw_popup (		void );
void	dataw_popdown (		void );
void	dataw_toggle (		void );
void	dataw_setDataSrc (	int	src_type,
				int     dcatg,
				char    *path );
void	dataw_loadTimes (	dsrc_t	*source,
				char	*ctim,
				int	nsel,
				int	skip );
void	dataw_updSelTimes (	void );
void	dataw_updateTmln (	void );
void	dataw_loadData (	void );
void	dataw_setNewRefTm (	dttmi_t *dttm );
void	dataw_disableLoop (	void );
void	dataw_enableLoop (	void );
void	dataw_setLoop (		int	lp );
void	dataw_loadsp  (		void );
void	dataw_setLpBtns (	void );
Boolean	dataw_noActSrc (	void );
void	dataw_clearDataSel (	int	lp );
int	dataw_getRoamVal (	int	lp );
void	dataw_setImageNav (	void );

/*
 *  dslw functions  -----------------------------
 */
void 	dslw_readTable (	char *dsrcnam,
			 	char *tblname  );
Widget 	dslw_create (		Widget  parent );
void 	dslw_popup (		int       dcatg,
				char      *path);
void	dslw_popdown ( 		void );
Boolean dslw_isUp ( 		void );
void	dslw_getGrdResfile ( 	char *dsname,
				char *resfile );
void    dslw_getEnsResfile (    char *dsname,
                                char *resfile );
void 	dslw_getFrstMod ( 	char *dsname, 
				char *falias );
void 	dslw_getModList ( 	char *dsname, 
				char *mlist,
				char *cycIdx );
void    dslw_getMosCycle   (    char *cycle   );
void    dslw_getGrdCycle   (    char *cycle   );
void	dslw_getVGFpathByTitle (char *usrtitle,
				char *path );
Boolean dslw_validSrc (		int	dcatg, 
				char	*dspath );
void dslw_ensSelPopup ( void );
void dslw_ensSelPopdown ( void );
Boolean dslw_ensSelIsUp ( void );
void dslw_getEnsCycle (int modIdx, int cycIdx,  char *cycle );

/*
 *  dsp functions  ------------------------------
 */
void	dsp_addLogo (		int	lp );
void	dsp_drawLogo (		int	lp );
void	dsp_loadAllLoops (	void );
void	dsp_print (		void );
void	dsp_reloadLoop (	int	lp,
				int	*iret );
void	dsp_reloadFrame (	int	lp,
				int	frm );
void	dsp_setBusy (		Boolean	state );
void	dsp_setProj (		int  lp, 
				int  img_inloop, 
				int  reload, 
				dsrc_t  *img_src );
void	dsp_updtDisplay (	void );


/*
 *  dtmbw functions  ------------------------------
 */
Widget	dtmbw_create (		Widget  parent,
				char    *name,
				dttmi_t  *dttm,
				dtmbw_t *dtmbw );
void	dtmbw_dttmSet ( 	dtmbw_t  *dtmbw,
				dttmi_t  *dttm,
				Boolean	 updt );

/*
 *  dttm functions  -----------------------------------
 */
void	dttm_copy (		dttmi_t  *dttm1,
				dttmi_t  *dttm2 );
void	dttm_getCurrentTime ( 	dttmi_t  *cdttm );

/*
 *  dttmw functions  -----------------------------------
 */
Widget	dttmw_create (		Widget  parent );
void	dttmw_popup (		Widget	parent,
				dttmi_t	*dttm );
void	dttmw_popdown (		void );

/*
 *  gmpk functions  ------------------------------------
 */
void	gmpk_init (		int	*iret );
void	gmpk_initTxt (		void );

/*
 *  grd functions  -------------------------------------
 */
void	grd_setAttr ( 		dsrc_t	*dsrc );
void	grd_setEnsAttr ( 	dsrc_t	*dsrc );

/*
 *  gtbl functions  ------------------------------------
 */
int	gtbl_getProd ( 		int      indx,
				char     **prdname,
				char	 **prdpath );
int     gtbl_getEnsProd (       int      indx,
                                char     **prdname,
                                char     **prdpath );
void	gtbl_sortProd ( 	int	nprods,
				int	iprod[] );
void    gtbl_sortEnsProd (      int     nprods,
                                int     iprod[] );

/*
 *  image functions  -----------------------------------
 */
void	image_getFileTm ( 	char	*fname,
			  	char	*ctim );
void	image_resetLut (  	char	*lutfile );
void	image_setAttr ( 	dsrc_t	*dsrc );

/*
 *  * Unidata Addition S. Chiswell
 *   */
void	image_props_popup (	Widget  parent,
				int     index );
void	image_props_popdown ( );



/*
 *  locfmtw functions  ----------------------------
 */
void	locfmtw_create ( 	Widget parent );
int	locfmtw_getPosNum (	void );
Boolean	locfmtw_getPosOnoff (	int    index ); 
void	locfmtw_getPosInfo  (	int    index,
				int*   loc_idx,
			        int*   loc_fmt   );
void	locfmtw_setCurrPos(	int    new_pos   );
int	locfmtw_getLocNum (	void );
char*	locfmtw_getLocStr (	int    index );
char*	locfmtw_getLocId  (	int    index );
Boolean locfmtw_isUp (		void );
void	locfmtw_popdown (	void );
void	locfmtw_popup ( 	void );
void	locfmtw_readLocTbl( 	int	   *iret );
void    locfmtw_locOptCb (      Widget, XtPointer, XtPointer );

/*
 *  logo functions  -----------------------------
 */
void	logo_getInfo (  	int	mode,
				char	name[],
				float	*size,
				float	*xnn,
				float	*ynn,
				int	*icm );
int	logo_getCurLogo ( 	int	lp,
				int   logo );
Boolean logo_isLogoActv ( 	int	lp,
				int   logo ); 
void	logo_selectCb ( 	Widget, long, XtPointer );
Boolean logo_lpLogoActv ( 	int	lp);

/*
 *  loop functions  --------------------------------
 */
void	loop_changeLoop (	int	new_loop );
Boolean	loop_getAutoUpdt (	int	lp );
int	loop_getCurFrame (	void );
int	loop_getCurLoop (	void );
Boolean	loop_getDataChngd (	int   	lp);
void	loop_getFadeRatio (	int	lp,
				float	*ratio);
int	loop_getFramePxm (	int	lp,
				int	frm );
void	loop_getFrameTm (	int	lp,
				int	frm,
				char	*ftime );
int	loop_getNumFrames (	int	lp );
int	loop_getRoamVal (	int	lp );
Boolean	loop_getTmMode (	int	lp );
int	loop_getTotalFrames (	int	lp );
void	loop_initLoops (	void );
void	loop_restoreLut (	int	lp );
void	loop_restoreRoamVal (	void );
void	loop_restoreTmMode (	void );
void	loop_saveRoamVal (	void );
void	loop_saveTmMode	(	void );
void	loop_setCurLoop (	int  	lp );
void	loop_setAutoUpdt (	int	lp,
				Boolean	flag );
void	loop_saveAutoUpdt ( void );
void	loop_restoreAutoUpdt ( void );
void	loop_setDataChngd (	int	lp,
				Boolean	flag );
void	loop_setFrameTm (	int	lp,
				int	frm,
				char	*ftime );
void	loop_setFramePxm (	int	lp,
				int	frm,
				int	pxm );
void	loop_setNumFrames (	int	lp,
				int	nfrms );
void	loop_setRoamVal (	int	lp,
				int	value );
void	loop_setTmMode (	int	lp, 
				Boolean	flag );
void	loop_setFadeRatio (	int	lp,
				float	ratio);

/*
 *  loopw functions  ------------------------------
 */
void	loopw_back (		void );
void	loopw_create (		Widget parent );
void	loopw_frwd (		void );
void	loopw_hide (		void );
Boolean loopw_isLoopActv (	void );
Boolean loopw_isHideActv (	void );
void    loopw_stopLooping(      void );
void	loopw_last (		void );
void	loopw_lpfd (		void );
void	loopw_resetHide (	void );
void	loopw_rock (		void );
void	loopw_sensitive (	Boolean state );
void	loopw_stop (		void );

/*
 *  mainw functions  ---------------------------------------
 */
Widget	mainw_create ( 		Widget	parent  );
void	mainw_setIconName (     char* iconName  );
void	mainw_setTitleName (    const char *spfName, const char *cycle  );
void	mainw_removeCycle(	void );


/*
 * map_set functions --------------------------------------
 */
void	mapset_apply	(	void	);
Widget  mapset_create	(	Widget 	parent );
Boolean mapset_isUp	(	void	);
void	mapset_popdown  (	void	);
void	mapset_popup	(	int 	func	);
Boolean	mapset_settingChngd (	int	loop	);

/*
 *  mapw functions  -----------------------------------------
 */
Widget	mapw_create	(	Widget	parent);
void	mapw_ctlBtnCb	(	Widget, long, XtPointer );
void	mapw_ctlBtnUpd	(	void	);
void	mapw_drawMap	(	int lp, char *panel);
void	mapw_getDEFmap	(	char *proj, char *garea );
void	mapw_getDEFproj	(	char *garea, char *proj );
int	mapw_isUp	(	void	);
void	mapw_popup	(	void	);
void	mapw_popdown	(	void	);
void	mapw_readAreaTbl(	void	);
void	mapw_readOvlTbl	(	void	);
void	mapw_redrawMap	(	void	);
void	mapw_setMap	(	int lp, int type );
void	mapw_setMapstrs	(	int lp	);
void	mapw_setPGForLoop (	int lp	);
void	mapw_updtOvlBtns  (	void	);
void	mapw_updtMapBtns  (	int lp	);

/*
 *  mbotw functions  -----------------------------------
 */
Widget	mbotw_create ( 		Widget parent );
void	mbotw_fadeToggle ( 	void	);
void	mbotw_setFade ( 	int lp, float ratio, Boolean updtDsply ); 
void	mbotw_restoreFade ( 	int lp, Boolean updtDsply );
void	mbotw_reloadFade ( 	int lp );

/*
 *  mbtnw functions  --------------------------------
 */
Widget	mbtnw_create (		Widget parent );
void	mbtnw_loopSetSensitive (Boolean state);
void	mbtnw_setLoopOne (	void );
void	mbtnw_setLoopTwo (	void );
void	mbtnw_setLoopThree (	void );
void	mbtnw_setLoopFour (	void );
void	mbtnw_setLoopFive (	void );
void	mbtnw_setLoopSix (	void );
void	mbtnw_setLoopSeven (	void );
void	mbtnw_setLoopEight (	void );
void	mbtnw_setLoopNine (	void );
void	mbtnw_setLoopTen (	void );
void	mbtnw_setLoopEleven (	void );
void	mbtnw_setLoopTwelve (	void );
void	mbtnw_setLoopThirteen (	void );
void	mbtnw_setLoopFourteen (	void );
void	mbtnw_setLoopFifteen (	void );
void	mbtnw_setLoopSixteen (	void );
void	mbtnw_setMbtns (	void );
void	mbtnw_zoomSensitive (	Boolean state);
void    mbtnw_autoUpdtToggle ( void );
void    mbtnw_autoLockToggle ( void );
void    mbtnw_CtlBtnsSetSensitive ( Boolean state );


/*
 *  mcanvw functions  -----------------------------
 */
void	mcanvw_catchCtrl (	void );
Widget	mcanvw_create    (	Widget 	parent );
void	mcanvw_rgstr     ( 	void );
void	mcanvw_getDims   ( 	int *width, int *height );
int     mcanvw_getDpth	 (	void );

/*
 *  mmenuw functions  ------------------------------
 */
void	mmenuw_create ( 	Widget   parent );
void	mmenuw_extendedZoomSet (Boolean  *zoomtyp );
int 	mmenuw_getLogoName (	void );
Boolean mmenuw_loopStopGet (	void );
Boolean	mmenuw_roamShareGet(	void );
Boolean mmenuw_lllValGet   (	void );
Boolean mmenuw_zoomModeGet (	void );

/*
 *  mpcstw functions  ---------------------------------
 */
Widget	mpcstw_create (		Widget parent );
int	mpcstw_isUp (		void );
void	mpcstw_popup (		char   *proj,
				char   *garea );
void	mpcstw_popdown (	void );

/*
 *  msc functions  ----------------------------------
 */
void	msc_setAttr ( 		dsrc_t	*dsrc );
void	msc_popup ( 		Widget	parent,
				int	indx );
void	msc_popdown (		void );

/*
 *  obs functions  ------------------------------------
 */
void	obs_setAttr (		dsrc_t	*dsrc );

/*
 *  pgpalw functions  ---------------------------------
 */
void	pgpalw_redo (		void );
void	pgpalw_undo (		void );

/*
 *  roamw functions  ---------------------------------
 */
void	roamw_arrowDown (	Widget   wdgt,
				XEvent	*event,
				String	*params,
				Cardinal *num_params );
void	roamw_arrowLeft (	Widget   wdgt,
				XEvent	*event,
				String	*params,
				Cardinal *num_params );
void	roamw_arrowRight (	Widget   wdgt,
				XEvent	*event,
				String	*params,
				Cardinal *num_params );
void	roamw_arrowUp ( 	Widget   wdgt,
				XEvent	*event,
				String	*params,
				Cardinal *num_params );
Widget	roamw_create (		Widget	parent );
void	roamw_createMenu (	Widget	parent );
void	roamw_ctlBtnSnstiv (	Boolean	state );
void	roamw_disableMenu (	void );
void	roamw_enableMenu (	void );
Boolean roamw_isUp (		void );
void	roamw_popdown (		void );
void	roamw_popup (		void );
void	roamw_sensitive (	Boolean	state );
void	roamw_setScreenXY ( 	int  	screen_x,
				int	screen_y );
void	roamw_setup (		int	loop,
				Boolean	view );
void	roamw_setWinSize (	int	pxmw_w,
				int	pxmw_h,
				int	screen_w,
				int	screen_h );

/*
 *  rsrc functions  ----------------------------------
 */
int	rsrc_getParm (		char	*pname,
				char	*parm );
int	rsrc_readFile (		char	*fname );

/*
 *  seekw functions  ----------------------------------
 */
Widget	seekw_create ( 		Widget parent );
Boolean seekw_isUp ( 		void ); 
void	seekw_popdown ( 	void );
void	seekw_popup ( 		void );
void	seekw_refresh ( 	void );
void	seekw_saveGhost ( 	Boolean hide_flag );
void	seekw_update ( 		void );
void	seekw_destroyWidget ( 	void );

/*
 *  spf functions  ----------------------------------
 */
void	spfw_create (		Widget	parent );
void	spfw_getFileName ( 	char	*file_name );
void	spfw_getSource ( 	int	lp,
                         	int	src,
                         	int	*catn,
				char	*srcstr,
				int	*iret);
void	spfw_getRangeIntv( 	int	lp,
                         	int	src,
                         	int	*rng,
				int	*intv,
				int	*drt,
				int	*iret);
void	spfw_getTmBin ( 	int	lp,
                         	int	src,
                         	int	*ionoff,
                         	int	*ibfr,
                         	int	*mbfr,
				int	*iaftr,
				int	*maftr,
				int	*mstrct,
				int	*iret);
Boolean	spfw_isUp (		void );
void	spfw_popdown (		void );
void	spfw_popup (		int	func );

/*
 *  stnmw functions  ----------------------------------
 */
Widget	stnmw_create ( 		Widget  parent);
Boolean stnmw_isUp ( 		void );
void	stnmw_popup (   	char	*ctype,
				char	*stnm_alias,
				int	indx);
void	stnmw_popdown ( 	void );
void	stnmw_popdownModel ( 	void );

/*
 *  tmln functions
 */
void	tmln_clearTimeInfo (	void );
Widget	tmln_createCanvas (	Widget	parent );
void	tmln_drawClear (	void );
int	tmln_getNewTimes (	dsrc_t	*source,
				char	*ctim,
				int	*ntimes,
				dttms_t	new_times[] );
void    tmln_getLastTm (	int     lp,
                                dttms_t ltime );
void	tmln_getSelected (	int	*ntimes,
				dttms_t	*tarry );
void	tmln_getSkipFactor (	int	*skip );
void	tmln_getTimeInfo (	int	*ntimes,
				dttms_t	*tarry );
void	tmln_redraw (		int  	box_motion );
void	tmln_setTimeInfo (	Boolean	direction,
				int	ntimes,
				dttms_t	*tarry,
				Boolean	*bselect );
void	tmln_setTotalFrame (	int	nframes );
void	tmln_setSkipFactor (	int	skip,
				Boolean	adjust );
/*
 *  vgf functions  ----------------------------------------
 */
void	vgf_getFname (		char	*instr,
				char	*vgfname );
void	vgf_getLatestFile (	char	*dir,
				char	*vgfname );
int	vgf_setDomtTime	(	dsrc_t	*data,
				frame_t	frames[] );
int	vgf_validateDsname (	char	*instr);

/*
 *  vtbl functions  -------------------------------------
 */
void	vtbl_checkCurDir (	void );
int	vtbl_getDefUser (	void );
void	vtbl_getPath (		char	*name,
				char	*path,
				int	*iret );

/*
 *  zoomw functions  -------------------------------------
 */
void	zoomw_clearZoom (	int	loop  );
Widget	zoomw_create (		Widget parent );
void    zoomw_czoom (  		Widget   wdgt,
                                XEvent  *event,
                                String  *params,
                                Cardinal *num_params );
Boolean	zoomw_isZoomActv (	int	loop  );
void	zoomw_loadData (	void );
void	zoomw_setZoom (		int	lp    );
void	zoomw_zoomFlagRetriv (	void );
void	zoomw_zoomFlagSave (	void );
void    zoomw_zzoom ( 	 	Widget   wdgt,
                                XEvent  *event,
                                String  *params,
                                Cardinal *num_params );	

#endif	/* PROTO_NMAP2 */
