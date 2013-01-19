#include "geminc.h"
#include "gemprm.h"
#include "gpc.h"
#include "proto_gpc.h"
#include "hints.h"
#include "Nxm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"

#define	MAXVERT		  9
#define INC_COL		  7	/* Number of button columns in includes	*/
#define	CWA_COL		  5	/* Number of cwa btn columns in includes */
#define NUMMZN  	 15	/* Number of marine zones to check */
#define NMZNCH  	  8	/* Number of mz for including 'CW' */
#define MAXFIPS   	100	/* Maximum number of clustered FIPS codes */
#define WBCMZ_TBL	"MZ_CNTY"
#define BNDS_FILE	"WBCMZ_BNDS"
/*
 *  Static Local Global variables describing watch box
 */
static 	float    	_latWbx[9], _lonWbx[9];

static  VG_DBStruct	_elW;
static  int             _shapeWbx;
static	char		_vertInfo[MAXVERT][60];
static	char		_cntyInfo[MAXCNTY][80];
static	char		_stIncInfo[MXSTATES*3+1];
static	int		_areaWbx, _hwsmWbx, _hwnmWbx, _nCnty, _nCntyInfo,
			_nStates, _nCwas;
static char		*_mzones[] = { "AM", "AN", "GM", "PH", "PK",
                                       "PM", "PS", "PZ", "LH", "LO",
				       "LM", "LE", "LS", "LC", "SL"};

struct  _list_info
{
        float   lat;
        float   lon;
        int     cy_fips;
        char    desc[33];
        char    state[3];
        char    st_fips[8];
        char    wfo[10];
};

static	struct  _list_info      _lInfo[MAX_CNTY];

static	char	*_fmtCntyList = { "%-8s %-2s   %-14s %5.2f %7.2f    %5d  %-4s" };
static	char	_cstl_list[80], _lakes[20];

/*
 * Local Global Variables used for storing FIPS codes.
 */
static  int	_ncfips;
static  int     _cfips[MAXFIPS];

/*
 *  private functions
 */
void pgwatch_addCntyClst ( int cy_fips,  int  original );
int  pgwatch_cmpCnty ( struct _list_info *list1, struct _list_info *list2 );
void pgwatch_rmvCntyClst ( int cy_fips );
void pgwatch_rmvOneCnty ( int cy_index );
void pgwatch_addOneCnty ( int cty_fips );
void pgwatch_addPermClst ( int cy_fips,  int  original );
void pgwatch_rmvPermClst ( int cy_fips );
void pgwatch_setCwaBtnState ( void     );


/************************************************************************
 * nmap_pgwatch.c							*
 *									*
 * This module saves watch element information and computes/gathers	*
 * attribute values for later public queries.				*
 *									*
 * CONTENTS:								*
 *	pgwatch_init()		initializes a watch element w/ def vals	*
 *	pgwatch_clrcnty()	clears the county list			*
 *	pgwatch_clrNumCwas()	set num of CWA (_nCwas) to zero		*
 *	pgwatch_redocnty()	re-computes counties touching watch	*
 *	pgwatch_editcnty()	edits county list in element and local	*
 *      pgwatch_addGrpCnty()    retrieves counties within a state       *
 *      pgwatch_rmvGrpCnty()    removes counties within a state         *
 *	pgwatch_save()		saves a watch element			*
 *      pgwatch_restore()       restores an updated watch element back  *
 *                              to WORK_FILE                            *
 *      pgwatch_getCurElm()     gets the addr. of local copy of watch   *
 *                              element                                 *
 *									*
 *	pgwatch_gvert()		gets vertex information			*
 *	pgwatch_gattr()		gets attribute information		*
 *	pgwatch_gcnty()		gets county information			*
 *	pgwatch_gstate()	gets state information			*
 *									*
 *	pgwatch_cmpCnty()	compare county sort function		*
 *      pgwatch_addCntyClst()   adds counties in the same cluster       *
 *      pgwatch_rmvCntyClst()   removes counties in the same cluster    *
 *      pgwatch_rmvOneCnty()    removes a county from the county list	*
 *      pgwatch_addOneCnty()    adds a county into the county list	*
 *	pgwatch_loadStCwa()	loads state&CWA buttons			*
 *	pgwatch_setCwaBtnState()set btn state based on watch county list*
 ***********************************************************************/

/*=====================================================================*/

void pgwatch_init ( VG_DBStruct *el, int style, int shape, 
		    int color, int mtype, float msize, 
                    int mwidth, char cnty_fill, int cnty_colr, 
		    int np, float *lat, float *lon, int *iret )
/************************************************************************
 * pgwatch_init								*
 *									*
 * This function initializes a watch element.				*
 * Anchor points are calculated and saved with the watch element.	*
 * Assume that style and shape have already been assigned.		*
 *									*
 * void pgwatch_init( el, style, shape, color, mtype, msize, mwidth,    *
 *		      cnty_fill, cnty_colr, np, lat, lon, iret )	*
 *									*
 * Input parameters:							*
 *	*el		VG_DBStruct	Watch element			*
 *	style		int		Watch style			*
 *	shape		int		Watch shape			*
 *	color		int		Watch color			*
 *	mtype		int		Watch marker type		*
 *	msize		float		Watch marker size		*
 *      mwidth		int             Watch marker width		*
 *	cnty_fill	char		County fill flag		*
 *	cnty_colr	int		County fill color		*
 *	np		int		Number of points		*
 *	*lat		float		Latitudes			*
 *	*lon		float		Longitudes			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 1/00						*
 * D.W.Plummer/NCEP	10/00	Calculate anchor points and save w/ el	*
 * J. Wu/SAIC		06/02	use CUR_WBX_VER for ver. control	*
 * H. Zeng/XTRIA        01/03   added marker info. for watch            *
 * A. Hardy/NCEP	 7/04	Set _lakes and _cst_list to null	*
 * H. Zeng/SAIC		10/04	added two para.				*
 ***********************************************************************/
{
    int		ii, ier;
    WatchBoxType *pwbx;
/*---------------------------------------------------------------------*/

    *iret = 0;

    pwbx = &(el->elem.wbx);

    /*
     *  Set watch box element version number.
     */
    el->hdr.version  = CUR_WBX_VER;
    el->hdr.filled   = cnty_fill;

    el->hdr.maj_col = color;
    el->hdr.min_col = cnty_colr;

    pwbx->info.w_style = style;
    pwbx->info.w_shape = shape;
    pwbx->info.w_type  = UNDWTCH;

    /*
     * Set marker attributes
     */
    pwbx->info.w_mrktyp= mtype;
    pwbx->info.w_mrksiz= msize;
    pwbx->info.w_mrkwid= mwidth;

    /*
     *  Adjust ("snap") the points before saving them in the element.
     */
    pgwpts_setSnap ( 1 ) ;
    pgwpts_get ( 0, shape, lat, lon, lat, lon, &ier );
    pgwpts_setSnap ( 0 ) ;

    pwbx->info.numpts  = np;
    for ( ii = 0; ii < np; ii++ )  {
	pwbx->latlon[ii] = lat[ii];
	pwbx->latlon[np+ii] = lon[ii];
    }

    /*
     *  Retrieve anchor information and save.
     */
    pgwbxw_getAnchor ( 0, el->elem.wbx.info.w_a0id,
        &(el->elem.wbx.info.w_a0lt), &(el->elem.wbx.info.w_a0ln),
        &(el->elem.wbx.info.w_a0dis), el->elem.wbx.info.w_a0dir,
        &ier );

    pgwbxw_getAnchor ( 1, el->elem.wbx.info.w_a1id,
        &(el->elem.wbx.info.w_a1lt), &(el->elem.wbx.info.w_a1ln),
        &(el->elem.wbx.info.w_a1dis), el->elem.wbx.info.w_a1dir,
        &ier );

    /*
     *  Initialize the rest to missing.
     */
    pwbx->info.w_number = -9999;
    pwbx->info.w_file[0] = '\0';

    pwbx->info.w_istat = 0;
    pwbx->info.w_iss_t[0] = '\0';
    pwbx->info.w_exp_t[0] = '\0';
    pwbx->info.w_severity = 0;
    pwbx->info.w_timezone[0] = '\0';
    pwbx->info.w_hailsz[0] = '\0';
    pwbx->info.w_windg[0] = '\0';
    pwbx->info.w_tops[0] = '\0';
    pwbx->info.w_msmv_d[0] = '\0';
    pwbx->info.w_msmv_s[0] = '\0';
    pwbx->info.w_states[0] = '\0';
    pwbx->info.w_adjarea[0] = '\0';
    pwbx->info.w_replw[0] = '\0';
    pwbx->info.w_fcstr[0] = '\0';
    pwbx->info.w_issued = 0;

    pwbx->info.wsm_iss_t[0] = '\0';
    pwbx->info.wsm_exp_t[0] = '\0';
    pwbx->info.wsm_ref[0] = '\0';
    pwbx->info.wsm_from[0] = '\0';
    pwbx->info.wsm_meso[0] = '\0';
    pwbx->info.wsm_fcstr[0] = '\0';

    pwbx->info.numcnty = 0;
    pwbx->info.cn_flag = 1;
    _lakes[0] = '\0';
    _cstl_list[0] = '\0';

    pgwatch_save ( el );

}

/*=====================================================================*/

void pgwatch_clrcnty ( void )
/************************************************************************
 * pgwatch_clrcnty							*
 *									*
 * This function clears the county list.				*
 *									*
 * void pgwatch_clrcnty( )						*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 1/00						*
 * D.W.Plummer/NCEP	 2/00	Changed calling sequence		*
 * D.W.Plummer/NCEP	 3/00	Set _nCnty and _nCntyInfo to 0		*
 * A. Hardy/NCEP	 7/04	Set _lakes and _cst_list to null	*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _elW.elem.wbx.info.numcnty = 0;

    _nCnty = 0;
    _nCntyInfo = 0;

    _lakes[0] = '\0';
    _cstl_list[0] = '\0';
    _elW.elem.wbx.info.w_adjarea[0] = '\0';

}

/*=====================================================================*/

void pgwatch_redocnty ( void )
/************************************************************************
 * pgwatch_redocnty							*
 *									*
 * This function re-computes the county list touching the watch.	*
 *									*
 * void pgwatch_redocnty( )						*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 1/00						*
 * S. Law/EAI		02/00	removed parameter from pgwatch_editcnty	*
 * D.W.Plummer/NCEP	 2/00	Changed calling sequence		*
 * D.W.Plummer/NCEP	 8/00	Changed call to clo_ routine		*
 * D.W.Plummer/NCEP	 5/01	Added Grt Lakes & Cstl Waters adj areas	*
 * D.W.Plummer/NCEP	 8/01	Repl clo_bqinfo w/ cst_gtag		*
 * D.W.Plummer/NCEP	12/01	Chgs for pgwatch_editcnty call seq chg	*
 * A. Hardy/NCEP	 3/04   chgd CNTY_BNDS to BNDS_FILE		*
 * A. Hardy/NCEP	 7/04   Added check for coast and lake zones 	*
 * H. Zeng/SAIC		01/05	added a strcpy() for w_adjarea		*
 ***********************************************************************/
{
    int		ii, np, ncnty, ier, ipos;
    float	flat[MAXCNTY], flon[MAXCNTY];
    int		fips[MAXCNTY];
    WatchBoxType *pwbx;
    char	info[128], stpo[4], data[12], prefs_tag[11];
    Boolean usezn;
/*---------------------------------------------------------------------*/


    pwbx = &(_elW.elem.wbx);

    np = pwbx->info.numpts;

    clo_binpoly( BNDS_FILE, np, pwbx->latlon, 
		&(pwbx->latlon[np]), &ier );
    clo_tgltln( BNDS_FILE, MAXCNTY, &ncnty, flat, flon, &ier );

    for ( ii = 0; ii < ncnty; ii++ )  {
            clo_bginfo ( BNDS_FILE, ii, info, &ier );
            cst_gtag ( "FIPS", info, "99999", data, &ier );
            cst_numb ( data, &(fips[ii]), &ier );
    }

    pgwatch_editcnty (0, &_elW, ncnty, fips);
    
    /*
     * Determine if the watch is touching a Great Lake.
     */
    clo_binpoly( "GREAT_LAKES", np,
                pwbx->latlon, &(pwbx->latlon[np]), &ier );

    _elW.elem.wbx.info.w_adjarea[0] = '\0';
    _lakes[0] = '\0';
    for ( ii = 0; ii < clo_qnhot(); ii++ )  {

        clo_bginfo( "GREAT_LAKES", ii, info, &ier );
        cst_gtag( "ID", info, "?", stpo, &ier );
        strcat(_lakes, stpo);
        strcat(_lakes, " ");

    }

    /*
     * Determine if the watch is touching a coastal state.
     */
    clo_binpoly( "ADJ_CSTL", np, 
		pwbx->latlon, &(pwbx->latlon[np]), &ier);

    _cstl_list[0] = '\0';
    for ( ii = 0; ii < clo_qnhot(); ii++ )  {

        clo_bginfo( "ADJ_CSTL", ii, info, &ier );
        cst_gtag( "ID", info, "?", stpo, &ier );
        strcat(_cstl_list, stpo);
        strcat(_cstl_list, " ");

    }


    /* If the watch touches a marine zone, check if marine zones are to
     * be included. If not, add 'CW' if touching a coastal state or add
     * the Great Lake id if necessary.
     */
    strcpy (prefs_tag, "ADD_MARZON");
    ctb_pfbool (prefs_tag, &usezn, &ier );
    ipos = -1;
    for ( ii = 0; ii < NMZNCH; ii++){
	if( strstr(_elW.elem.wbx.info.w_states, _mzones[ii] ) != NULL ) {
	    ipos = ii;
	    break;
	}
    }
    if ( usezn == TRUE ) {
        if ( ipos >= 0 ) {
	    strcpy ( pwbx->info.w_adjarea, "CW" );
        }
        else {
	    strcpy ( pwbx->info.w_adjarea, " " );
        }
    }
    else {
	if ( strlen ( _lakes ) > (size_t)0 ) {
            strcpy ( pwbx->info.w_adjarea, _lakes );
            strcpy ( _elW.elem.wbx.info.w_adjarea, _lakes );
        }
	else if ( strlen ( _cstl_list) > (size_t)0 ) {
            strcpy ( pwbx->info.w_adjarea, "CW" );
            strcpy ( _elW.elem.wbx.info.w_adjarea, "CW" );
        }
        else  {
	    strcpy ( pwbx->info.w_adjarea, " " );
	    strcpy ( _elW.elem.wbx.info.w_adjarea, " " );
        }
    }

}

/*=====================================================================*/

void pgwatch_editcnty ( int eflag, VG_DBStruct *el, int nlocs, int fips[] )
/************************************************************************
 * pgwatch_editcnty							*
 *									*
 * This function edits the county list based on FIPS codes.		*
 *									*
 * Valid values for eflag:						*
 * 	0 - initialize county list with the given FIPS code(s)		*
 * 	1 - add the given FIPS code(s) to the county list		*
 * 	2/3 - delete the given FIPS code(s) to the county list		*
 *	4 - use given FIPS code(s)					*
 *									*
 * void pgwatch_editcnty (eflag, el, nlocs, fips )			*
 *									*
 * Input parameters:							*
 *	eflag	int		Edit flag (legal values above)		*
 *	*el	VG_DBStruct	Watch element				*
 *	nlocs	int		Number of points to edit		*
 *	fips[]	int		FIPS code(s)				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 1/00						*
 * S. Law/GSC		02/00	cleanup, removed ier parameter		*
 * D.W.Plummer/NCEP	 2/00	set _nCnty=0 for eflag == 4		*
 * D.W.Plummer/NCEP	 8/00	changed call to clo_ routine		*
 * H. Zeng/EAI          01/01   Updated state btns on county list window*
 * H. Zeng/EAI          02/01   modified for county cluster             *
 * D.W.Plummer/NCEP      8/01   Add FIPS code processing		*
 * D.W.Plummer/NCEP     12/01   Chg calling sequence to accept FIPS  	*
 * J. Wu/SAIC		06/02	Watch box ver. 4->5, "cn_stat" removed 	*
 * A. Hardy/NCEP	 3/04   changed COUNTY to WBCMZ_TBL		*
 * A. Hardy/NCEP	 7/04   Added check for coastal marine zones 	*
 * A. Hardy/NCEP	10/04   Added permanent clustering 		*
 * A. Hardy/NCEP	02/05   Add global vars&reorg clust tbl calls   *
 * H. Zeng/SAIC		07/05	added call to pgwlst_getStatesForm()	*
 * H. Zeng/SAIC		01/06	removed st&cwa btn update		*
 ***********************************************************************/
{
    int		ii, jj, cy_fips, ier;
    int		nret, err_code, thrshld, ipos;
    float	flat, flon;
    char	desc[33], state[3], info[256], data[12], max_st[8];
    char	st_fips[9], wfo[12], max_cnty[8], prefs_tag[11];
    Boolean	usezn, useperm, msg_done;
    WatchBoxType    *pwbx;
/*---------------------------------------------------------------------*/
   /*
    * Check if marine zones are to be used.
    */

    strcpy (prefs_tag, "ADD_MARZON");
    ctb_pfbool (prefs_tag, &usezn, &ier );

    if ( usezn == FALSE ) {
        thrshld = 51;
    }
    else {
        thrshld = 100;
    }
    if ( ier != 0 ) {
        thrshld = 100;
    }
   /*
    * Initialize global variables.
    */
    _ncfips = 0;
    for ( ii=0;ii < MAXFIPS; ii++) {
        _cfips[ii] = 0;
    }

    if ( eflag == 0 || eflag == 4 ) _nCnty = 0;

    for ( ii = 0; ii < nlocs; ii++ )  {

        clo_findnum ( WBCMZ_TBL, fips[ii], sizeof(info), &nret, info, &ier );

	cst_gtag (   "LAT", info, "-9999.0",    data, &ier );
	cst_crnm ( data, &flat, &ier );
	cst_gtag (   "LON", info, "-9999.0",    data, &ier );
	cst_crnm ( data, &flon, &ier );
	cst_gtag (  "NAME", info,       "?",    desc, &ier );
	cst_gtag (    "ST", info,       "?",   state, &ier );
	cst_gtag (  "STID", info,       "?", st_fips, &ier );
	cst_gtag (  "STNM", info,   "-9999",    data, &ier );
	cst_numb ( data, &cy_fips, &ier );
	cst_gtag ( "COL10", info,       "?",     wfo, &ier );

	jj = 0;
	while ( jj < _nCnty && _lInfo[jj].cy_fips != cy_fips ) jj++;

	if ( (jj == _nCnty) && ( (cy_fips/10000) <= thrshld) ) {
	    if (_nCnty < MAX_CNTY) {

		/*
		 *  County not found on list, add it.
		 */
		_lInfo[_nCnty].lat = flat;
		_lInfo[_nCnty].lon = flon;
		_lInfo[_nCnty].cy_fips = cy_fips;
		strcpy( _lInfo[_nCnty].desc, desc );
		cst_lcuc( _lInfo[_nCnty].desc, _lInfo[_nCnty].desc, &ier );
		strcpy( _lInfo[_nCnty].state, state );
		strcpy( _lInfo[_nCnty].st_fips, st_fips );
		strcpy( _lInfo[_nCnty].wfo, wfo );
                _nCnty++;

                if ( eflag != 4 ) {
	           /* 
		    * Check for permanent clusters, if turned on.
		    */
                    strcpy (prefs_tag, "PERM_CLUST");
                    ctb_pfbool (prefs_tag, &useperm, &ier );

		    if( pgwlst_getClstStatus() == 1 ) {

		       /*
                        * Add counties within the same cluster if the clustering
                        * status is ON.
                        */
                        pgwatch_addCntyClst( cy_fips, (int)(eflag == 0) );
                    }

                    if ( useperm == TRUE ) {
		       /*
                        * Add permanent counties/marine zones within the
                        * same cluster. 
                        */
                        pgwatch_addPermClst( cy_fips, (int)(eflag == 0) ); 
                    }
                }

	    } /* the end of if(_nCnty < MAX_CNTY) */

	    if ( _nCnty == MAX_CNTY )  {
		err_code = 5;
                sprintf(max_cnty, "%d", MAX_CNTY );
		er_wmsg ( "pgen", &err_code, max_cnty, &ier, 
			strlen("pgen"), strlen(max_cnty) );
		NxmErr_update();
	    }
	}
	else {
	    /*
	     * toggle county stat as appropriate
	     */
	    switch (eflag) {
	      case 0:	/*  Re-do list from given points	*/
	      case 1:	/*  Add one or more counties to the cluster list*/
	       /*
		* Permanent clustering.
		*/
                strcpy (prefs_tag, "PERM_CLUST");
                ctb_pfbool (prefs_tag, &useperm, &ier );

		if( pgwlst_getClstStatus() == 1 ) {

		   /*
                    * Add counties within the same cluster if the clustering
                    * status is ON.
                    */
                   pgwatch_addCntyClst( cy_fips, (int)(eflag == 0) );

                }
                if ( useperm == TRUE ) {
		   /*
                    * Add permanent counties/marine zones within the
                    * same cluster. 
                    */
                    pgwatch_addPermClst( cy_fips, (int)(eflag == 0) );
		}


		break;

	      case 2:	/*  Delete a county from the list	*/
	      case 3:	
				
		pgwatch_rmvOneCnty( jj );		
		
		if( pgwlst_getClstStatus() == 1 ) {

		   /*
                    * Rmv. counties within the same cluster if the clustering
                    * status is ON.
                    */
                   pgwatch_rmvCntyClst( cy_fips );

                }
		/* 
		 * Check for permanent clusters, if turned on.
		 */
                 strcpy (prefs_tag, "PERM_CLUST");
                 ctb_pfbool (prefs_tag, &useperm, &ier );

                 if ( useperm == TRUE ) {
                    /*
                     * Remove permanent counties/marine zones within the
                     * same cluster.
                     */
                     pgwatch_rmvPermClst( cy_fips );
		 }


		break;


	      case 4:	/* Use given locations and stats */

		break;

	    } /* the end of switch */
	} /* the end of else */
    } /* the end of for */


    /*
     *  Sort _list_info structure
     *  Added (int(*)(const void*, const void*)) cast to satisfy qsort
     */
    qsort( _lInfo, _nCnty, sizeof( struct _list_info), 
	   (int(*)(const void*, const void*))pgwatch_cmpCnty );


    /*
     *  Assign values back to element and create formatted list;
     */
    pwbx = &(el->elem.wbx);
    pwbx->info.numcnty = _nCnty;
    _stIncInfo[0] = '\0';
    _nStates = 0;
    _nCntyInfo = 0;
    msg_done = FALSE;
    for ( ii = 0; ii < _nCnty; ii++ )  {

	    /*
	     *  Format county information and add state to "included"
	     */
	    sprintf( _cntyInfo[_nCntyInfo], _fmtCntyList,
                     _lInfo[ii].st_fips, _lInfo[ii].state, _lInfo[ii].desc,
                     _lInfo[ii].lat, _lInfo[ii].lon, 
		     _lInfo[ii].cy_fips, _lInfo[ii].wfo) ;
	    _nCntyInfo++;

	    if (_nStates < MXSTATES && 
                strstr(_stIncInfo, _lInfo[ii].state) == (char *)NULL )  {

		  strcat( _stIncInfo, _lInfo[ii].state );
		  strcat( _stIncInfo, " " );
	          _nStates++;
	    }
	    else if ( _nStates >= MXSTATES && !msg_done ) {
	        err_code = 10;
		sprintf(max_st, "%d", MXSTATES );
	        er_wmsg ( "pgen", &err_code, max_st, &ier, 
			  strlen("pgen"), strlen(max_st) );
	        NxmErr_update();
                msg_done = TRUE;
	    }

	    pwbx->info.cn_fips[ii] = _lInfo[ii].cy_fips;
	    pwbx->info.cn_ltln[ii] = _lInfo[ii].lat;
	    pwbx->info.cn_ltln[_nCnty+ii] = _lInfo[ii].lon;

    } /* the end of for ( ii = 0;... */

     
    /*
     * Update state info. for watch element.
     */
    pwbx->info.w_states[0] = '\0';
    strcat ( pwbx->info.w_states, _stIncInfo );

    ipos = -1;
    for ( ii = 0; ii < NMZNCH; ii++){
	if( strstr(pwbx->info.w_states, _mzones[ii] ) != NULL ) {
	    ipos = ii;
	    break;
	}
    }

    if ( usezn == TRUE ) {
        if ( ipos >= 0 ) {
	    strcpy ( _elW.elem.wbx.info.w_adjarea, "CW" );
        }
        else {
	    strcpy ( _elW.elem.wbx.info.w_adjarea, " " );
        }
    }
    else {

        strcpy ( _elW.elem.wbx.info.w_adjarea,  pwbx->info.w_adjarea );
    }

}

/*=====================================================================*/

void pgwatch_rmvGrpCnty ( int grp_typ, char *name, VG_DBStruct *el )
/************************************************************************
 * pgwatch_rmvGrpCnty							*
 *									*
 * This function removes a group of counties within a certain state,    *
 * CWA, for other well defined areas from current watch county list.	*
 *									*
 * void pgwatch_rmvGrpCnty (grp_typ, name, el)			        *
 *									*
 * Input parameters:							*
 *	grp_typ int		group type				*
 *				  0 -- State				*
 *				  1 -- CWA				*
 *	name	char*		group name		                *
 *	*el	VG_DBStruct	Watch element				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          01/01   initial coding                          *
 * J. Wu/SAIC		06/02	Watch box ver. 4->5, "cn_stat" removed 	*
 * H. Zeng/SAIC		01/06	added group type parameter		*
 ***********************************************************************/
{
    int		    ii, err_code, ier;
    char	    max_st[8];
    Boolean	    msg_done;
    WatchBoxType    *pwbx;
/*---------------------------------------------------------------------*/

    /*
     * Remove counties according to the group type.
     */
    switch ( grp_typ ) {

      case 0:  /* State */

        for ( ii = 0; ii < _nCnty; ii++ )  {

          if( strcasecmp(name, _lInfo[ii].state) == 0 ) {
	      pgwatch_rmvOneCnty( ii );
	      ii--;
	  }
        }

        break;

      case 1:  /* CWA */

        for ( ii = 0; ii < _nCnty; ii++ )  {

          if( strcasecmp(name, _lInfo[ii].wfo) == 0 ) {
	      pgwatch_rmvOneCnty( ii );
	      ii--;
	  }
        }

        break;

      default:
 
        break;

    }
   
    /*
     *  Sort _list_info structure
     *  Added (int(*)(const void*, const void*)) cast to satisfy qsort
     */
    qsort( _lInfo, _nCnty, sizeof( struct _list_info), 
	   (int(*)(const void*, const void*))pgwatch_cmpCnty );


    /*
     *  Assign values back to element and create formatted list;
     */
    pwbx = &(el->elem.wbx);
    pwbx->info.numcnty = _nCnty;
    _stIncInfo[0] = '\0';
    _nStates = 0;
    _nCntyInfo = 0;
    msg_done = FALSE;
    for ( ii = 0; ii < _nCnty; ii++ )  {

	    /*
	     *  Format county information and add state to "included"
	     */
	    sprintf( _cntyInfo[_nCntyInfo], _fmtCntyList,
                     _lInfo[ii].st_fips, _lInfo[ii].state, _lInfo[ii].desc,
                     _lInfo[ii].lat, _lInfo[ii].lon, 
		     _lInfo[ii].cy_fips, _lInfo[ii].wfo) ;
	    _nCntyInfo++;

	    if (_nStates < MXSTATES && 
                strstr(_stIncInfo, _lInfo[ii].state) == (char *)NULL )  {

		  strcat( _stIncInfo, _lInfo[ii].state );
		  strcat( _stIncInfo, " " );
	          _nStates++;

	    }
	    else if ( _nStates >= MXSTATES && !msg_done ) {
	        err_code = 10;
		sprintf(max_st, "%d", MXSTATES );
	        er_wmsg ( "pgen", &err_code, max_st, &ier, 
			  strlen("pgen"), strlen(max_st) );
	        NxmErr_update();
                msg_done = TRUE;
	    }

	pwbx->info.cn_fips[ii] = _lInfo[ii].cy_fips;
	pwbx->info.cn_ltln[ii] = _lInfo[ii].lat;
	pwbx->info.cn_ltln[_nCnty+ii] = _lInfo[ii].lon;

    }

    /*
     * Update state info. for watch element.
     */
    pwbx->info.w_states[0] = '\0';
    strcat ( pwbx->info.w_states, _stIncInfo );

}

/*=====================================================================*/

void pgwatch_addGrpCnty ( int grp_typ, char *name, VG_DBStruct *el )
/************************************************************************
 * pgwatch_addGrpCnty							*
 *									*
 * This function adds a group of counties within a certain state, CWA   *
 * or other well defined areas into current watch county list.		*
 *									*
 * void pgwatch_addGrpCnty (grp_typ, name, el)			        *
 *									*
 * Input parameters:							*
 *	grp_typ int		group type				*
 *				  1 -- State				*
 *				  2 -- CWA				*
 *	name	char*		group name		                *
 *	*el	VG_DBStruct	Watch element				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          01/01   initial coding                          *
 * J. Wu/SAIC		06/02	Watch box ver. 4->5, "cn_stat" removed 	*
 * H. Zeng/SAIC		01/06	added group type parameter		*
 ***********************************************************************/
{
    int		    ii, jj, nfips, fips[MAXFIPS], err_code, ier;
    char	    max_st[8];
    Boolean	    new_cty, msg_done;
    WatchBoxType    *pwbx;
/*---------------------------------------------------------------------*/

    /*
     * Add counties according to the group type.
     */
    switch ( grp_typ ) {

      case 0:  /* State */

        /*
         * No codes in here yet since state buttons never have chance
         * to be turned back on from off.
         */

        break;

      case 1:  /* CWA */

        clo_findcwa ("MZ_CNTY", name, MAXFIPS, &nfips, fips, &ier );

        /*
         * check each county fips, if it is new, add it into current
         * watch county list.
         */
        for ( ii = 0; ii < nfips; ii++ ) {

	  new_cty = TRUE;
          for ( jj = 0; jj < _nCnty; jj++ )  {
            if ( _lInfo[jj].cy_fips == fips[ii]) {
	      
              new_cty = FALSE;
              break;
            }
          }

          if ( new_cty == TRUE ) {

	      pgwatch_addOneCnty( fips[ii] );
          }

        }

        break;

      default:
 
        break;

    } /* the end of switch ( grp_typ ... */


    /*
     *  Sort _list_info structure
     *  Added (int(*)(const void*, const void*)) cast to satisfy qsort
     */
    qsort( _lInfo, _nCnty, sizeof( struct _list_info), 
	   (int(*)(const void*, const void*))pgwatch_cmpCnty );


    /*
     *  Assign values back to element and create formatted list;
     */
    pwbx = &(el->elem.wbx);
    pwbx->info.numcnty = _nCnty;
    _stIncInfo[0] = '\0';
    _nStates = 0;
    _nCntyInfo = 0;
    msg_done = FALSE;
    for ( ii = 0; ii < _nCnty; ii++ )  {

	    /*
	     *  Format county information and add state to "included"
	     */
	    sprintf( _cntyInfo[_nCntyInfo], _fmtCntyList,
                     _lInfo[ii].st_fips, _lInfo[ii].state, _lInfo[ii].desc,
                     _lInfo[ii].lat, _lInfo[ii].lon, 
		     _lInfo[ii].cy_fips, _lInfo[ii].wfo) ;
	    _nCntyInfo++;

	    if (_nStates < MXSTATES && 
                strstr(_stIncInfo, _lInfo[ii].state) == (char *)NULL )  {

		  strcat( _stIncInfo, _lInfo[ii].state );
		  strcat( _stIncInfo, " " );
	          _nStates++;
	    }
	    else if ( _nStates >= MXSTATES && !msg_done ) {
	        err_code = 10;
		sprintf(max_st, "%d", MXSTATES );
	        er_wmsg ( "pgen", &err_code, max_st, &ier, 
			  strlen("pgen"), strlen(max_st) );
	        NxmErr_update();
                msg_done = TRUE;
	    }

	pwbx->info.cn_fips[ii] = _lInfo[ii].cy_fips;
	pwbx->info.cn_ltln[ii] = _lInfo[ii].lat;
	pwbx->info.cn_ltln[_nCnty+ii] = _lInfo[ii].lon;

    }


    /*
     * Update state info. for watch element.
     */
    pwbx->info.w_states[0] = '\0';
    strcat ( pwbx->info.w_states, _stIncInfo );


}

/*=====================================================================*/

void pgwatch_rmvCntyClst ( int cy_fips )
/************************************************************************
 * pgwatch_rmvCntyClst							*
 *									*
 * This function removess counties within the same cluster in on the    *
 * county list.                                                         *
 *									*
 * void pgwatch_rmvCntyClst ( cy_fips )			                *
 *									*
 * Input parameters:							*
 *	cy_fips	        int	   core county fips code		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          02/01   initial coding                          *
 * J. Wu/SAIC		06/02	Watch box ver. 4->5, "cn_stat" removed 	*
 * A. Hardy/NCEP	01/05	Made ncfips,cfips global variables      *
 ***********************************************************************/
{
    int		ii, jj, ier;
    char        ccwfo[10], ccname[32];
/*---------------------------------------------------------------------*/

    _ncfips = 0;
    for (ii = 0; ii < MAXFIPS; ii++ ) {
        _cfips[ii] = 0;
    }
    ctb_ccfind(cy_fips, ccwfo, ccname, &_ncfips, _cfips, &ier);
    for( ii = 0; ii < _ncfips; ii++ ) {
       jj = 0;
       while(jj < _nCnty && _lInfo[jj].cy_fips != _cfips[ii])  jj++;

       if (jj != _nCnty)  {
           pgwatch_rmvOneCnty( jj );
       }


    } /* the end of for */

}

/*=====================================================================*/

void pgwatch_addCntyClst ( int cy_fips, int original )
/************************************************************************
 * pgwatch_addCntyClst							*
 *									*
 * This function adds counties within the same cluster in on the county *
 * list.                                                                *
 *									*
 * void pgwatch_addCntyClst ( cy_fips, original )			*
 *									*
 * Input parameters:							*
 *	cy_fips	        int	   core county fips code		*
 *      original        int        treat counties as original ones?     *
 *                                 0 -- No, other value -- Yes          *
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          02/01    initial coding                         *
 * D.W.Plummer/NCEP	 8/01	 Changes for call to clo_findnum	*
 * J. Wu/SAIC		06/02	Watch box ver. 4->5, "cn_stat" removed 	*
 * A. Hardy/NCEP	 3/04	changed COUNTY to WBCMZ_TBL		*
 * H. Zeng/SAIC		01/05	added marine zone threshold check	*
 * A. Hardy/NCEP	01/05	Made ncfips,cfips global variables      *
 ***********************************************************************/
{
    int		ii, jj, kk, err_code, thrshld, ier;
    float	flat, flon;
    char	desc[33], state[3], st_fips[9];
    char        ccwfo[12], ccname[32], info[256], data[12];
    int         len_cfips, nret;
    char	max_cnty[8], prefs_tag[11];
    Boolean	usezn;
/*---------------------------------------------------------------------*/

    sprintf(max_cnty, "%d", MAX_CNTY );

    len_cfips = sizeof(_cfips) / sizeof(_cfips[0]);
    ctb_ccfind(cy_fips, ccwfo, ccname, &_ncfips, _cfips, &ier);
    for( ii = 0; ii < _ncfips; ii++ ) {
       jj = 0;
       while(jj < _nCnty && _lInfo[jj].cy_fips != _cfips[ii])  jj++;

       if (jj == _nCnty)  {
 
	  if ( _nCnty < MAX_CNTY )  {

	    /*
             * Cleanup info string.
             */
	    for( kk = 0; kk < 256; kk++)  info[kk] = ' ';

            /*
             * Get county full info from fips code.
             */
            clo_findnum(WBCMZ_TBL, _cfips[ii], len_cfips, &nret, info, &ier);

	    cst_gtag (   "LAT", info, "-9999.0",    data, &ier );
	    cst_crnm ( data, &flat, &ier );
	    cst_gtag (   "LON", info, "-9999.0",    data, &ier );
	    cst_crnm ( data, &flon, &ier );
	    cst_gtag (  "NAME", info,       "?",    desc, &ier );
	    cst_gtag (    "ST", info,       "?",   state, &ier );
	    cst_gtag (  "STID", info,       "?", st_fips, &ier );
	    cst_gtag (  "STNM", info,   "-9999",    data, &ier );
	    cst_numb ( data, &cy_fips, &ier );
	    cst_gtag ( "COL10", info,       "?",   ccwfo, &ier );

            /*
             * Check if marine zones are to be used.
             */

            strcpy (prefs_tag, "ADD_MARZON");
            ctb_pfbool (prefs_tag, &usezn, &ier );

            if ( usezn == FALSE ) {
                 thrshld = 51;
            }
            else {
                 thrshld = 100;
            }
            if ( ier != 0 ) {
                 thrshld = 100;
            }

     
            if ( cy_fips / 10000 <= thrshld ) {

	       _lInfo[_nCnty].lat = flat;
	       _lInfo[_nCnty].lon = flon;
	       _lInfo[_nCnty].cy_fips = _cfips[ii];
	       strcpy( _lInfo[_nCnty].desc, desc );
	       cst_lcuc( _lInfo[_nCnty].desc, _lInfo[_nCnty].desc, &ier );
	       strcpy( _lInfo[_nCnty].state, state );
	       strcpy( _lInfo[_nCnty].st_fips, st_fips );
	       strcpy( _lInfo[_nCnty].wfo, ccwfo );
               _nCnty++;
            }


	  }
	  else  {
	    err_code = 5;
	    er_wmsg ( "pgen", &err_code, max_cnty, &ier, 
			strlen("pgen"), strlen(max_cnty) );
	    NxmErr_update();
	  }
       }

    } /* the end of for */

}

/*=====================================================================*/

void pgwatch_save ( VG_DBStruct *el )
/************************************************************************
 * pgwatch_save								*
 *									*
 * This function saves a watch element (el) to the local global watch	*
 * element structure in this module (_elW). It does not save the	*
 * element to a file. It does, however, compute some information from	*
 * the watch element (such as watch area) and save the values in	*
 * local global variables (such as _areaWbx).				*
 *									*
 * void pgwatch_save (el)						*
 *									*
 * Input parameters:							*
 *	*el	VG_DBStruct	Watch element				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * D.W.Plummer/NCEP	 4/99						*
 * D.W.Plummer/NCEP	 4/99	Force county name string to upper case	*
 * D.W.Plummer/NCEP	 6/99	Calculate half-width in nm to nearest 5	*
 * M. LI/GSC		10/99	Modified clo_dist code			*
 * M. Li/GSC		10/99	Added multi-point cal. to clo_dist	*
 * H. Zeng/EAI          12/99   modified to include more watch info.    *
 * S. Law/GSC		01/00	removed ier param, added _editcnty call	*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * H. Zeng/EAI          01/01   removed variable first                  *
 * J. Wu/GSC		02/01	Modified 'unused1' & 'unused2' in VG 	*
 *				to 'smooth' & 'version'			*
 * J. Wu/SAIC		06/02	Watch box ver. 4->5, "cn_stat" removed 	*
 * D.W.Plummer/NCEP	10/02	add doc + direct assign of hdr		*
 ***********************************************************************/
{
    int		ii, np, npx, ier;
    char	ddanc[8], ddvor[8], stnanc[8], stnvor[8];
    float	latx, lonx, base, height, hafwid;

    WatchBoxType    *pwbx_new, *pwbx_orig;
/*---------------------------------------------------------------------*/

    pwbx_new  = &(el->elem.wbx);
    pwbx_orig = &(_elW.elem.wbx);

    np = pwbx_orig->info.numpts;
    npx = 1;

    _elW.hdr = el->hdr;

    pwbx_orig->info.numpts   = pwbx_new->info.numpts;
    pwbx_orig->info.w_style  = pwbx_new->info.w_style;
    pwbx_orig->info.w_type   = pwbx_new->info.w_type;
    pwbx_orig->info.w_number = pwbx_new->info.w_number;
    pwbx_orig->info.w_shape  = pwbx_new->info.w_shape;
    pwbx_orig->info.w_mrktyp = pwbx_new->info.w_mrktyp;
    pwbx_orig->info.w_mrksiz = pwbx_new->info.w_mrksiz;
    pwbx_orig->info.w_mrkwid = pwbx_new->info.w_mrkwid;  

    strcpy ( pwbx_orig->info.w_a0id, pwbx_new->info.w_a0id );
    pwbx_orig->info.w_a0lt  = pwbx_new->info.w_a0lt;
    pwbx_orig->info.w_a0ln  = pwbx_new->info.w_a0ln;
    pwbx_orig->info.w_a0dis  = pwbx_new->info.w_a0dis;
    strcpy ( pwbx_orig->info.w_a0dir, pwbx_new->info.w_a0dir );

    strcpy ( pwbx_orig->info.w_a1id, pwbx_new->info.w_a1id );
    pwbx_orig->info.w_a1lt  = pwbx_new->info.w_a1lt;
    pwbx_orig->info.w_a1ln  = pwbx_new->info.w_a1ln;
    pwbx_orig->info.w_a1dis  = pwbx_new->info.w_a1dis;
    strcpy ( pwbx_orig->info.w_a1dir, pwbx_new->info.w_a1dir );

    strcpy( pwbx_orig->info.w_file, pwbx_new->info.w_file );
    pwbx_orig->info.w_issued = pwbx_new->info.w_issued;

    if(pwbx_new->info.w_number != IMISSD) {    
	pwbx_orig->info.w_istat = pwbx_new->info.w_istat;
	strcpy( pwbx_orig->info.w_iss_t, pwbx_new->info.w_iss_t );
	strcpy( pwbx_orig->info.w_exp_t, pwbx_new->info.w_exp_t );
	pwbx_orig->info.w_severity = pwbx_new->info.w_severity;
	strcpy (pwbx_orig->info.w_timezone, pwbx_new->info.w_timezone);
	strcpy( pwbx_orig->info.w_hailsz, pwbx_new->info.w_hailsz );
	strcpy( pwbx_orig->info.w_windg, pwbx_new->info.w_windg );
	strcpy( pwbx_orig->info.w_tops, pwbx_new->info.w_tops );
	strcpy( pwbx_orig->info.w_msmv_d, pwbx_new->info.w_msmv_d );
	strcpy( pwbx_orig->info.w_msmv_s, pwbx_new->info.w_msmv_s );
	strcpy( pwbx_orig->info.w_states, pwbx_new->info.w_states );
	strcpy( pwbx_orig->info.w_adjarea, pwbx_new->info.w_adjarea );
	strcpy( pwbx_orig->info.w_replw, pwbx_new->info.w_replw );
	strcpy( pwbx_orig->info.w_fcstr, pwbx_new->info.w_fcstr );

    }

    pwbx_orig->info.cn_flag = pwbx_new->info.cn_flag;
    pwbx_orig->info.numcnty = pwbx_new->info.numcnty;
    _nCnty = pwbx_orig->info.numcnty;
    for ( ii = 0; ii < _nCnty; ii++ )  {
	pwbx_orig->info.cn_fips[ii] = pwbx_new->info.cn_fips[ii];
	pwbx_orig->info.cn_ltln[ii] = pwbx_new->info.cn_ltln[ii];
	pwbx_orig->info.cn_ltln[_nCnty+ii] = 
	    pwbx_new->info.cn_ltln[_nCnty+ii];
    }

    np = pwbx_orig->info.numpts;
    for ( ii = 0; ii < np; ii++ )  {
	pwbx_orig->latlon[ii] = pwbx_new->latlon[ii];
	pwbx_orig->latlon[ii+np] = pwbx_new->latlon[ii+np];
	_latWbx[ii] = pwbx_orig->latlon[ii];
	_lonWbx[ii] = pwbx_orig->latlon[ii+np];
    }

    _latWbx[np] = _latWbx[0];
    _lonWbx[np] = _lonWbx[0];

    _shapeWbx = pwbx_new->info.w_shape;

    /*
     *  Compute and save vertex information.
     */
    for ( ii = 0; ii < np; ii++ )  {

	if ( ii < 4 )  {
	  if ( pwbx_orig->info.w_a0dis != IMISSD )  {
	    sprintf ( ddanc, "%-3d %-3s", 
		pwbx_orig->info.w_a0dis, pwbx_orig->info.w_a0dir );
	    strcpy ( stnanc, pwbx_orig->info.w_a0id );
	  }
	  else  {
	    sprintf ( ddanc, "--- ---" );
	    strcpy ( stnanc, "---" );
	  }
	}
	else  {
	  if ( pwbx_orig->info.w_a1dis != IMISSD )  {
	    sprintf ( ddanc, "%-3d %-3s", 
		pwbx_orig->info.w_a1dis, pwbx_orig->info.w_a1dir );
	    strcpy ( stnanc, pwbx_orig->info.w_a1id );
	  }
	  else  {
	    sprintf ( ddanc, "--- ---" );
	    strcpy ( stnanc, "---" );
	  }
	}
	pgwlst_updVorLst( sys_M, np, pwbx_orig->latlon, 
			  &(pwbx_orig->latlon[np]), ii,
			  ddvor, stnvor, &ier );
	sprintf(_vertInfo[ii], "%-8.2f %-8.2f   %-8s %-4s   %-8s %-4s",
		pwbx_orig->latlon[ii], 
		pwbx_orig->latlon[ii+np], 
		ddanc, stnanc, ddvor, stnvor );
    }

    /*
     *  Compute and save area and half-width information.
     */
    if ( pwbx_orig->info.w_style == PGRAM )  {

	clo_dist ( &(pwbx_orig->latlon[0]), 
		   &(pwbx_orig->latlon[np+0]), &npx, 
		   &(pwbx_orig->latlon[4]), &(pwbx_orig->latlon[np+4]), 
		   &base, &ier );

	cgr_lindist( pwbx_orig->latlon[np+0], pwbx_orig->latlon[0],
		     pwbx_orig->latlon[np+4], pwbx_orig->latlon[4],
		     pwbx_orig->latlon[np+1], pwbx_orig->latlon[1], 
		     &lonx, &latx, &height, &ier );

	clo_dist( &latx, &lonx, &npx, &(pwbx_orig->latlon[1]), 
		  &(pwbx_orig->latlon[np+1]), &height, &ier );

	_areaWbx = G_NINT ( base * height * 2.0F * M2NM * M2NM );

	clo_dist( &(pwbx_orig->latlon[0]), 
		  &(pwbx_orig->latlon[np+0]), &npx,  
		  &(pwbx_orig->latlon[1]), &(pwbx_orig->latlon[np+1]),
		  &hafwid, &ier );

	_hwsmWbx = G_NINT( ( hafwid * M2SM ) / 5.0F ) * 5;
	_hwnmWbx = G_NINT( ( hafwid * M2NM ) / 5.0F ) * 5;

    }
    else  {
	_areaWbx = IMISSD;
	_hwsmWbx = IMISSD;
	_hwnmWbx = IMISSD;
    }

    np = pwbx_new->info.numcnty;
    pgwatch_editcnty (4, el, np, pwbx_new->info.cn_fips );

}

/*=====================================================================*/

int pgwatch_restore ( void )
/************************************************************************
 * pgwatch_restore							*
 *									*
 * This function restores the local global watch element (_elW) back	*
 * to WORK_FILE.							*
 *									*
 * int  pgwatch_restore()				                *
 *									*
 * Input parameters:							*
 *		   NONE				                        *
 *									*
 * Return value:							*
 *	pgwatch_restore		int        Return code			*
 *									*
 **									*
 * Log:									*
 * H. Zeng/EAI          12/99   initial coding                          *
 * S. Law/GSC		03/00	added parameter to pgutls_prepNew	*
 * E. Safford/GSC	11/00	add undo to changes & fix color		*
 * H. Zeng/EAI          11/00   changed for the new undo design         *
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * H. Zeng/EAI          12/00   modified for multiple undo steps        *
 * J. Wu/SAIC		06/02	Watch box ver. 4->5, "cn_stat" removed 	*
 * H. Zeng/SAIC		10/04	made min_col change with watch type	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    int		ii, nc, location, np, wtype, level, ier, ier1; 
    float	llx, lly, urx, ury, *x_coords, *y_coords;
    char        grp[4], logstr[10];
    VG_DBStruct	el, temp_el;
/*---------------------------------------------------------------------*/
    location = pgactv_getElmLoc();
    if (location == -1) return (-1);

    pgactv_getDevPts (&np, &x_coords, &y_coords);
    pgutls_prepNew (location, &el, &llx, &lly, &urx, &ury, &ier);

    pgundo_newStep();
    pgundo_storeThisLoc(location, UNDO_DEL, &ier);

    el.elem.wbx.info.w_istat  = _elW.elem.wbx.info.w_istat;
    el.elem.wbx.info.w_number = _elW.elem.wbx.info.w_number;
    strcpy(el.elem.wbx.info.w_iss_t, _elW.elem.wbx.info.w_iss_t); 
    strcpy(el.elem.wbx.info.w_exp_t, _elW.elem.wbx.info.w_exp_t); 
    
    el.elem.wbx.info.w_type = _elW.elem.wbx.info.w_type;

    if (_elW.elem.wbx.info.w_type != UNDWTCH ) {

       /*
        * Retrieve county fill color info. by calling ces_get().
        */
       temp_el.hdr.vg_type = WBOX_ELM;
       temp_el.hdr.vg_class = CLASS_WATCHES;
       wtype = _elW.elem.wbx.info.w_type;

       ces_get( wtype, &temp_el, &ier);

       if (ier  != 0) {

          sprintf(logstr, "%i for WATCHES", wtype);
          strcpy(grp, "CES");
          level = 2;
          er_lmsg ( &level, grp, &ier, logstr, &ier1,
                            strlen(grp), strlen(logstr) );
   	  NxmErr_update();
       }
       else {

          el.hdr.maj_col = _elW.hdr.maj_col = temp_el.hdr.maj_col;
          el.hdr.min_col = _elW.hdr.min_col = temp_el.hdr.min_col;
       }

    }

    strcpy ( el.elem.wbx.info.w_a0id, _elW.elem.wbx.info.w_a0id );
    el.elem.wbx.info.w_a0lt = _elW.elem.wbx.info.w_a0lt;
    el.elem.wbx.info.w_a0ln = _elW.elem.wbx.info.w_a0ln;
    el.elem.wbx.info.w_a0dis = _elW.elem.wbx.info.w_a0dis;
    strcpy ( el.elem.wbx.info.w_a0dir, _elW.elem.wbx.info.w_a0dir );

    strcpy ( el.elem.wbx.info.w_a1id, _elW.elem.wbx.info.w_a1id );
    el.elem.wbx.info.w_a1lt = _elW.elem.wbx.info.w_a1lt;
    el.elem.wbx.info.w_a1ln = _elW.elem.wbx.info.w_a1ln;
    el.elem.wbx.info.w_a1dis = _elW.elem.wbx.info.w_a1dis;
    strcpy ( el.elem.wbx.info.w_a1dir, _elW.elem.wbx.info.w_a1dir );

    el.elem.wbx.info.w_severity=_elW.elem.wbx.info.w_severity;
    strcpy(el.elem.wbx.info.w_timezone, _elW.elem.wbx.info.w_timezone);
    strcpy(el.elem.wbx.info.w_hailsz, _elW.elem.wbx.info.w_hailsz);	
    strcpy(el.elem.wbx.info.w_windg, _elW.elem.wbx.info.w_windg);
    strcpy(el.elem.wbx.info.w_tops, _elW.elem.wbx.info.w_tops);
    strcpy(el.elem.wbx.info.w_msmv_d, _elW.elem.wbx.info.w_msmv_d);
    strcpy(el.elem.wbx.info.w_msmv_s, _elW.elem.wbx.info.w_msmv_s);
    strcpy(el.elem.wbx.info.w_states, _elW.elem.wbx.info.w_states);
    strcpy(el.elem.wbx.info.w_adjarea,_elW.elem.wbx.info.w_adjarea);
    strcpy(el.elem.wbx.info.w_replw,  _elW.elem.wbx.info.w_replw);
    strcpy(el.elem.wbx.info.w_fcstr,  _elW.elem.wbx.info.w_fcstr);
    strcpy(el.elem.wbx.info.w_file,   _elW.elem.wbx.info.w_file );
    el.elem.wbx.info.w_issued = _elW.elem.wbx.info.w_issued;    

    el.elem.wbx.info.numcnty = _elW.elem.wbx.info.numcnty;    
    el.elem.wbx.info.cn_flag = _elW.elem.wbx.info.cn_flag;    
    nc = el.elem.wbx.info.numcnty;
    for ( ii = 0; ii < nc; ii++ )  {
	el.elem.wbx.info.cn_fips[ii] = _elW.elem.wbx.info.cn_fips[ii];
	el.elem.wbx.info.cn_ltln[ii] = _elW.elem.wbx.info.cn_ltln[ii];
	el.elem.wbx.info.cn_ltln[nc+ii] = _elW.elem.wbx.info.cn_ltln[nc+ii];
    }

    /* 
     * store and redraw 
     */
    pgvgf_saveNewElm (NULL, sys_D, &el, np, 
				  x_coords, y_coords, TRUE, &location, &ier); 
    pgundo_storeThisLoc(location, UNDO_ADD, &ier);
    pgundo_endStep();
    pgutls_redraw (location, &el, &ier);

    return (0);

}

/*=====================================================================*/

VG_DBStruct *pgwatch_getCurElm ( void )
/************************************************************************
 * pgwatch_getCurElm                                                    *
 *                                                                      *
 * Returns the addr. of the local copy of the watch element             *
 *                                                                      *
 * VG_DBStruct* pgwatch_getCurElm ()	                                *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 *		NONE							*
 * Return value:							*
 * pgwatch_getCurElm	VG_DBStruct*	   the addr. of watch element 	*
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/EAI          12/99   initial coding                          *
 ***********************************************************************/
{
    return ( &_elW );
}

/*=====================================================================*/

void pgwatch_gvert ( int nv, char *str )
/************************************************************************
 * pgwatch_gvert	                                                *
 *                                                                      *
 * This function returns vertex information in a string as:		*
 * lat lon dist dir anchorstn dist dir vorstn				*
 *                                                                      *
 * void pgwatch_gvert( nv, str )					*
 *                                                                      *
 * Input parameters:                                                    *
 * 	nv	int			Vertex number			*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*str	char         		Vertex information		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      4/99                                           *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    strcpy( str, _vertInfo[nv] );

    return;

}

/*=====================================================================*/

void pgwatch_gattr ( int *hwsm, int *hwnm, int *shape, int *area )
/************************************************************************
 * pgwatch_gattr                                                        *
 *                                                                      *
 * This function returns watch attributes.				*
 *                                                                      *
 * void pgwatch_gattr( hwsm, hwnm, shape, area )			*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 * *hwsm	int		Watch half-width (statue miles)		*
 * *hwnm	int		Watch half-width (nautical miles)	*
 * *shape	int		Watch shape (0=NS, 1=EW, or 2=ESOL)	*
 * *area	int		Watch area (square nautical miles)	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      4/99                                           *
 * T. Lee/GSC           10/00   Updated prolog                          *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    *hwsm = _hwsmWbx;
    *hwnm = _hwnmWbx;

    *shape = _shapeWbx;

    *area = _areaWbx;

    return;

}

/*=====================================================================*/

void pgwatch_gcnty ( int n, int *nc, char *str )
/************************************************************************
 * pgwatch_gcnty                                                        *
 *                                                                      *
 * This function returns county information in a string as:		*
 * lat lon dist dir anchorstn dist dir vorstn				*
 * st_fips cy_fips desc state lat lon wfo				*
 *                                                                      *
 * void pgwatch_gcnty( n, nc, str )					*
 *                                                                      *
 * Input parameters:                                                    *
 * 	n	int		County number				*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*nc	int          	Total number of counties		*
 * 	*str	char         	County information			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      4/99                                           *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    *nc = _nCntyInfo;

    if ( _nCntyInfo == 0 )  {
	strcpy( str, "-" );
    }
    else  {
        strcpy( str, _cntyInfo[n] );
    }

    return;

}

/*=====================================================================*/

void pgwatch_gQCcnty ( int *n_inactv, char ***inactv_cnty_out, 
		       int *n_actv,   char ***actv_cnty_out     )
/************************************************************************
 * pgwatch_gQCcnty                                                      *
 *                                                                      *
 * This function returns quality control counties. Both inactive	*
 * counties inside the watch area and active counties outside the watch *
 * area.								*
 *                                                                      *
 * void pgwatch_gQCcnty( n_inactv, inactv_cnty_out,			*
 *			 n_actv,     actv_cnty      )			*
 *                                                                      *
 * Input parameters:                                                    *
 *				NONE					*
 *                                                                      *
 * Output parameters:                                                   *
 * 	*n_inactv	int     Total number of inactive counties	*
 *	***inactv_cnty_out						*
 *			char	List of inactive counties		*
 *	*n_actv		int	Total number of active counties		*
 * 	***actv_cnty_out						*
 *			char    List of active counties			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * H. Zeng/SAIC		05/05	initial coding				*
 ***********************************************************************/
{
    int		ncnty, ii, total_num, max_ver, max_idx, ier;
    int		jj, npts, npls, *inout, n_st, cnty_fips;
    float	*xpt, *ypt, *xpl, *ypl;
    float	flat[MAXCNTY], flon[MAXCNTY];
    char	fips[8*MAX_CNTY], **inactv_cnty, **actv_cnty;
    char	info[256], data[12], desc[33], state[3];
    char	st_fips[9], wfo[12];
    gpc_polygon union_poly;	
/*---------------------------------------------------------------------*/

    /*
     * Initialize the output parameters.
     */
    *n_inactv   = 0;
    inactv_cnty = NULL;
    *n_actv     = 0;
    actv_cnty   = NULL;

    /*
     * Set the total number of current counties.
     */
    ncnty = _nCnty;
    if ( ncnty == 0 )  return;

    /*
     * Construct fips char array.
     */
    fips[0] = '\0';
    for ( ii = 0; ii < _nCnty; ii++ ) {

      sprintf ( fips+strlen(fips), "%-d;", _lInfo[ii].cy_fips );
    }

    clo_blasso ( "WBCMZ_BNDS", "FIPS", &ncnty, fips, &union_poly, &ier );

    if ( ier != 0 )  return;


    /*
     * From here on, we start to find the active counties
     * OUTSIDE the watch area.
     */

    /*
     * Find the largest polygon in the polygon array.
     */
    max_ver = 0;
    total_num = union_poly.num_contours;

    for ( ii = 0; ii < total_num; ii++ ) {

      if ( union_poly.hole[ii] != 0 )  continue;

      if ( union_poly.contour[ii].num_vertices > max_ver ) {

        max_ver = union_poly.contour[ii].num_vertices;
        max_idx = ii;
      }
    }

    /*
     * Construct the lat/lon array for the largest polygon.
     */
    npls = max_ver;

    xpl = (float*) malloc ( npls * sizeof(float) );
    ypl = (float*) malloc ( npls * sizeof(float) );

    for ( ii = 0; ii < npls; ii++ ) {

      xpl[ii] = union_poly.contour[max_idx].vertex[ii].x;
      ypl[ii] = union_poly.contour[max_idx].vertex[ii].y;
    }

    /*
     * Construct the lat/lon array for the current county list.
     */
    npts = _nCnty;

    xpt = (float*) malloc ( npts * sizeof(float) );
    ypt = (float*) malloc ( npts * sizeof(float) );

    for ( ii = 0; ii < npts; ii++ ) {

      xpt[ii] = _lInfo[ii].lat;
      ypt[ii] = _lInfo[ii].lon;
    }

    /*
     * Call cgr_inpoly to find the active counties OUTSIDE
     * the watch area.
     */
    inout = (int*) malloc ( npts * sizeof(int) );

    cgr_inpoly ( sys_M, &npts, xpt, ypt, sys_M, &npls, xpl, ypl, 
		 inout, &ier );

    /*
     * Construct active counties array for output.
     */
    actv_cnty = (char**) malloc ( npts * sizeof(char*) );

    for ( ii = 0; ii < npts; ii++ ) {

      if ( inout[ii] != 0 )  continue;

      actv_cnty[*n_actv] = (char*) malloc ( 80 * sizeof(char) );
      strcpy ( actv_cnty[*n_actv], _cntyInfo[ii]);
      (*n_actv)++;
    }  

    /*
     * Free up memory at the end of current step.
     */
    free (xpt);
    free (ypt);
    free (xpl);
    free (ypl);
    free (inout);  


    /*
     * From here on, we start to find the inactive counties
     * INSIDE the watch area.
     */

    /*
     * Construct inactive counties array for output.
     */
    inactv_cnty = (char**) malloc ( MAXCNTY * sizeof(char*) );

    /*
     * Loop through all the polygons that are holes in the polygon array.
     */
    for ( ii = 0; ii < total_num; ii++ ) {

      if ( union_poly.hole[ii] == 0 )  continue;

      /*
       * For each polygon hole, find all the counties within it.
       * This will be done in two steps. first call clo_binpoly(),
       * then call crg_inpoly().
       */
      npls = union_poly.contour[ii].num_vertices;

      xpl = (float*) malloc ( npls * sizeof(float) );
      ypl = (float*) malloc ( npls * sizeof(float) );

      for ( jj = 0; jj < npls; jj++ ) {

        xpl[jj] = union_poly.contour[ii].vertex[jj].x;
        ypl[jj] = union_poly.contour[ii].vertex[jj].y;
      }
 
      clo_binpoly( "WBCMZ_BNDS", npls, xpl, ypl, &ier );
      clo_tgltln ( "WBCMZ_BNDS", MAXCNTY, &ncnty, flat, flon, &ier );

      /*
       * Call cgr_inpoly to find the inactive counties INSIDE
       * the watch area.
       */
      inout = (int*) malloc ( ncnty * sizeof(int) );

      cgr_inpoly ( sys_M, &ncnty, flat, flon, sys_M, &npls, xpl, ypl, 
		   inout, &ier );
 
      for ( jj = 0; jj < ncnty; jj++ ) {

        if ( inout[jj] == 0 )  continue;

        /*
         * For the counties inside the polygon, we record the info
         * and construct inactv_cnty array.
         */
        inactv_cnty[*n_inactv] = (char*) malloc ( 80 * sizeof(char) );

        clo_bginfo ( "WBCMZ_BNDS", jj, info, &ier );
        cst_gtag   ( "FIPS", info, "99999", data, &ier );
        cst_numb   ( data,   &cnty_fips, &ier );

        clo_findnum( "MZ_CNTY", cnty_fips, sizeof(info), 
		     &n_st, info, &ier );

	cst_gtag (   "NAME",  info,     "?",    desc, &ier );
	cst_gtag (   "ST",    info,     "?",   state, &ier );
	cst_gtag (   "STID",  info,     "?", st_fips, &ier );
	cst_gtag (   "COL10", info,     "?",     wfo, &ier );

	sprintf  (   inactv_cnty[*n_inactv], _fmtCntyList, st_fips, 
		     state, desc, flat[jj], flon[jj], cnty_fips, wfo );

        (*n_inactv)++;

      } /* the end of for ( jj = 0; jj < ncnty... */

      /*
       * Free up memory at the end of current step.
       */
      free (xpl);
      free (ypl);
      free (inout);  

    } /* the end of for ( ii = 0; ii < total_num... */


    /*
     * Assign the addresses to inactv_cnty_out and actv_cnty_out.
     */
    (*inactv_cnty_out) = inactv_cnty;
    (*actv_cnty_out)   = actv_cnty;

    /*
     * Free up memory for polygon array.
     */
    gpc_free_polygon( &union_poly );

}

/*=====================================================================*/

void pgwatch_gstate ( char *str )
/************************************************************************
 * pgwatch_gstate                                                       *
 *                                                                      *
 * This function returns state information in a string.			*
 *                                                                      *
 * void pgwatch_gstate( str )						*
 *                                                                      *
 * Input parameters:                                                    *
 *                                                                      *
 * Output parameters:                                                   *
 * *str		char     	State list (ie., "AR KS MO OK")		*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      4/99                                           *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    strcpy( str, _stIncInfo );

    return;

}

/*=====================================================================*/

int pgwatch_cmpCnty ( struct _list_info *list1, struct _list_info *list2 )
/************************************************************************
 * pgwatch_cmpCnty                                                    	*
 *                                                                      *
 * This function compares county descriptor names.                      *
 *                                                                      *
 * int pgwatch_cmpCnty ( list1, list2 )                               	*
 *                                                                      *
 * Input parameters:                                                    *
 * *list1     struct _list_info      struct _list_info element          *
 * *list2     struct _list_info      struct _list_info element          *
 *                                                                      *
 * Output parameters:                                                   *
 *                                                                      *
 * Return parameters:                                                   *
 * pgwatch_cmpCnty	int         Return code (ala strcmp)            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * D.W.Plummer/NCEP      4/99                                           *
 * A. Hardy/NCEP	 3/04	Modified to put marine zones after 	*
 *				counties. Sort marine zones alpha too.	*
 ***********************************************************************/
{
    int 	ival, ii;
    Boolean	found1, found2;
/*---------------------------------------------------------------------*/
    ival = 0;
    found1 = False;
    found2 = False;

   /*
    * Check if either list elems. are a marine zone.
    */
    for ( ii = 0; ii < NUMMZN; ii++ ) {
	if ( strcmp (list1->state, _mzones[ii] ) == 0 ) {
	    found1 = True;
	}
	if ( strcmp (list2->state, _mzones[ii] ) == 0 ) {
	    found2 = True;
	}
    }
   /*
    * Check to see if the state ids. are the same.
    */

    if ( !found1 && !found2 ) {
        ival = strcmp( list1->state, list2->state );

        if ( ival != 0 )  return ( ival );

       /*
        * If they are the same, Check to see if the descriptors are 
	* the same.
        */

        ival = strcmp( list1->desc, list2->desc);
        return (ival);

    }
    if ( found1 && found2 ) {
        ival = strcmp( list1->state, list2->state );
        if ( ival != 0 )  return ( ival );
        ival = strcmp( list1->desc, list2->desc);
        return (ival);
    }
    if ( !found1 && found2 ) {
        ival = -1;
        return ( ival );
    }
    if ( found1 && !found2 ) {
        ival = 1;
        return ( ival );
    }
        return ( ival );
}


/*=====================================================================*/

void pgwatch_rmvOneCnty ( int cy_index )
/************************************************************************
 * pgwatch_rmvOneCnty							*
 *									*
 * This function removes a given county from the county list. 		*
 *									*
 * void pgwatch_rmvOneCnty ( cy_index )			                *
 *									*
 * Input parameters:							*
 *	cy_index	    int	   The county index in county list array*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * J. Wu/SAIC          06/02   initial coding                           *
 ***********************************************************************/
{
    int		ii, ier;
/*---------------------------------------------------------------------*/

    /*
     *  Remove the given county by copying the remaining items down to
     *  fill in the space starting from the given index.
     */
    if ( cy_index >= _nCnty  ) return;
    
    if ( _nCnty == 1 ) {
	pgwatch_clrcnty( );
    }
    else {
        _nCnty--;
        for ( ii = cy_index; ii < _nCnty; ii++ ) {
	    _lInfo[ii].lat = _lInfo[ii+1].lat;
	    _lInfo[ii].lon = _lInfo[ii+1].lon;
	    _lInfo[ii].cy_fips =  _lInfo[ii+1].cy_fips;
	    strcpy( _lInfo[ii].desc, _lInfo[ii+1].desc );
	    cst_lcuc( _lInfo[ii].desc, _lInfo[ii].desc, &ier );
	    strcpy( _lInfo[ii].state, _lInfo[ii+1].state );
	    strcpy( _lInfo[ii].st_fips, _lInfo[ii+1].st_fips );
	    strcpy( _lInfo[ii].wfo, _lInfo[ii+1].wfo );
        }    
    }
}

/*=====================================================================*/

void pgwatch_addOneCnty ( int cty_fips )
/************************************************************************
 * pgwatch_addOneCnty							*
 *									*
 * This function adds a given county into the county list. 		*
 *									*
 * void pgwatch_addOneCnty ( cty_fips )			                *
 *									*
 * Input parameters:							*
 *	cty_fips        int    fips code of county to be added		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC	       01/06	initial coding				*
 ***********************************************************************/
{
    int		ii, n_cty, err_code, thrshld, ier;
    float	flat, flon;
    char	desc[33], state[3], st_fips[9], prefs_tag[11];
    char        wfo[12], info[256], data[12], max_cnty[8];
    Boolean	usezn;
/*---------------------------------------------------------------------*/

    /*
     * Check total number of counties in the beginning.
     */
    if ( _nCnty >= MAX_CNTY ) {

      err_code = 5;
      sprintf(max_cnty, "%d", MAX_CNTY );
      er_wmsg ( "pgen", &err_code, max_cnty, &ier, 
			strlen("pgen"), strlen(max_cnty) );
      NxmErr_update();

      return;
    }


    /*
     * find all info for this county.
     */    
    for( ii = 0; ii < 256; ii++)  info[ii] = ' ';

    clo_findnum( "MZ_CNTY", cty_fips, sizeof(info), 
		 &n_cty, info, &ier );

    cst_gtag (   "LAT", info, "-9999.0",    data, &ier );
    cst_crnm (    data, &flat, &ier );
    cst_gtag (   "LON", info, "-9999.0",    data, &ier );
    cst_crnm (    data, &flon, &ier );
    cst_gtag (  "NAME", info,       "?",    desc, &ier );
    cst_gtag (    "ST", info,       "?",   state, &ier );
    cst_gtag (  "STID", info,       "?", st_fips, &ier );
    cst_gtag ( "COL10", info,       "?",     wfo, &ier );

    /*
     * Check if marine zones are to be used.
     */
    strcpy (prefs_tag, "ADD_MARZON");
    ctb_pfbool (prefs_tag, &usezn, &ier );

    if ( ier != 0 ) {
         thrshld = 100;
    }
    else if ( usezn == FALSE ) {
         thrshld = 51;
    }
    else {
         thrshld = 100;
    }
     
    if ( cty_fips / 10000 <= thrshld ) {

      /*
       * Add this county into current watch county list.
       */
      _lInfo[_nCnty].lat     =  flat;
      _lInfo[_nCnty].lon     =  flon;
      _lInfo[_nCnty].cy_fips =  cty_fips;
      strcpy( _lInfo[_nCnty].desc, desc );
      cst_lcuc( _lInfo[_nCnty].desc, _lInfo[_nCnty].desc, &ier );
      strcpy( _lInfo[_nCnty].state, state );
      strcpy( _lInfo[_nCnty].st_fips, st_fips );
      strcpy( _lInfo[_nCnty].wfo, wfo );

      _nCnty++;
    }

}

/*=====================================================================*/

void pgwatch_rmvPermClst ( int cy_fips )
/************************************************************************
 * pgwatch_rmvPermClst							*
 *									*
 * This function removes permanent counties within the same cluster in  *
 * the county/marine zone list.                                         *
 *									*
 * void pgwatch_rmvPermClst ( cy_fips )		                	*
 *									*
 * Input parameters:							*
 *	cy_fips		int		core county fips code		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * A. Hardy/NCEP	10/04		copied from pgwatch_rmvCntyClst * 
 * A. Hardy/NCEP	01/05	changed search method through clst. tbls*
 ***********************************************************************/
{
    int		ii, jj, ij, nn, ier;
    int		nperm, pfips[MAXFIPS], tmpfips[100], icnt, iunq;
    char        pcwfo[10], pcname[32];
/*---------------------------------------------------------------------*/
    icnt = 0;
    iunq = 0;

   /*
    * If there are not any optional clusters, reset the global variables.
    */

    if ( _ncfips == 0 ) {
        _ncfips = 1;
        _cfips[0] = cy_fips; 
    }

   /*
    * Loop over all of the counties found by the original cluster and find
    * the first permanent grouping for each county. The search in the permanent
    * table is only on the first FIPS code, not the entire string.
    */

    for (ij=0; ij < _ncfips; ij++ ) {
        ctb_permccfind(_cfips[ij], pcwfo, pcname, &nperm, pfips, &ier);
        if ( ier == 0 ) {
            for ( nn=0; nn < nperm; nn++ ) {
                tmpfips[icnt] = pfips[nn];
                icnt++;
            }
        }
    }

   /*
    * Uniquely sort the temporary fips code array.
    */

    if ( icnt > 0 ) { 
        pgwatch_numsort ( &icnt, tmpfips, &iunq, tmpfips, &ier);
    }
    else {
      iunq = _ncfips;
      tmpfips[0] = cy_fips;
    }

    for( ii = 0; ii < iunq; ii++ ) {
       jj = 0;
       while(jj < _nCnty && _lInfo[jj].cy_fips != tmpfips[ii])  jj++;

       if (jj != _nCnty)  {
           pgwatch_rmvOneCnty( jj );
       }


    } /* the end of for */

}

/*=====================================================================*/

void pgwatch_addPermClst ( int cy_fips, int original )
/************************************************************************
 * pgwatch_addPermClst							*
 *									*
 * This function adds permenant counties within the same cluster in the *
 * county list. The search through the permanent table uses all of the  *
 * FIPS codes (if found) in the optional table. These codes are sorted  *
 * uniquely and then added to the county list.				*
 *									*
 * void pgwatch_addPermClst ( cy_fips, original )			*
 *									*
 * Input parameters:							*
 *	cy_fips	        int	   core county fips code		*
 *      original        int        treat counties as original ones?     *
 *                                 0 -- No, other value -- Yes          *
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * A. Hardy/NCEP	10/04	copied from pgwatch_addCntyClst		*
 * H. Zeng/SAIC		01/05	added marine zone threshold check	*
 * A. Hardy/NCEP	01/05	change search through tables		*
 * A. Hardy/NCEP	02/05	Fixed an IF check for _ncfips		*
 ***********************************************************************/
{
    int		ii, jj, ij, kk, nn, icnt, err_code, thrshld, ier;
    float	flat, flon;
    char	desc[33], state[3], st_fips[9];
    char        pcwfo[12], pcname[32], info[256], data[12];
    int         len_cfips, nret, iunq;
    int		tmpfips[100], pfips[MAXFIPS], nperm;
    char	max_cnty[8], prefs_tag[11];
    Boolean	usezn;
/*---------------------------------------------------------------------*/
    icnt = 0;
    iunq = 0;
    sprintf(max_cnty, "%d", MAX_CNTY );

   /*
    * If there are not any optional clusters, reset the global variables.
    */
    if ( ( _ncfips == 0 ) || ( _ncfips == 1 ) ) {
        _ncfips = 1;
        _cfips[0] = cy_fips; 
    }

   /*
    * Loop over all of the counties found by the original cluster and find
    * the first permanent grouping for each county. The search in the permanent
    * table is only on the first FIPS code, not the entire string.
    */

    len_cfips = sizeof(tmpfips) / sizeof(tmpfips[0]);
    for (ij=0; ij < _ncfips; ij++ ) {
        ctb_permccfind(_cfips[ij], pcwfo, pcname, &nperm, pfips, &ier);
        if ( ier == 0 ) {
            for ( nn=0; nn < nperm; nn++ ) {
                tmpfips[icnt] = pfips[nn];
                icnt++;
            }
        }
    }

   /*
    * Uniquely sort the temporary fips code array.
    */

    if ( icnt > 0 ) { 
        pgwatch_numsort ( &icnt, tmpfips, &iunq, tmpfips, &ier);
    }
    else {
      iunq = 0;
    }
   /*
    * Loop over the counties and add those from the perm. cluster group.
    */
    for( ii = 0; ii < iunq; ii++ ) {
       jj = 0;
       while(jj < _nCnty && _lInfo[jj].cy_fips != tmpfips[ii]) jj++;

       if (jj == _nCnty)  {
 
	  if ( _nCnty < MAX_CNTY )  {

	    /*
             * Cleanup info string.
             */
	    for( kk = 0; kk < 256; kk++)  info[kk] = ' ';

            /*
             * Get county full info from fips code.
             */
            clo_findnum(WBCMZ_TBL, tmpfips[ii], len_cfips, &nret, info, &ier);

	    cst_gtag (   "LAT", info, "-9999.0",    data, &ier );
	    cst_crnm ( data, &flat, &ier );
	    cst_gtag (   "LON", info, "-9999.0",    data, &ier );
	    cst_crnm ( data, &flon, &ier );
	    cst_gtag (  "NAME", info,       "?",    desc, &ier );
	    cst_gtag (    "ST", info,       "?",   state, &ier );
	    cst_gtag (  "STID", info,       "?", st_fips, &ier );
	    cst_gtag (  "STNM", info,   "-9999",    data, &ier );
	    cst_numb ( data, &cy_fips, &ier );
	    cst_gtag ( "COL10", info,       "?",   pcwfo, &ier );

            /*
             * Check if marine zones are to be used.
             */

            strcpy (prefs_tag, "ADD_MARZON");
            ctb_pfbool (prefs_tag, &usezn, &ier );

            if ( usezn == FALSE ) {
                 thrshld = 51;
            }
            else {
                 thrshld = 100;
            }
            if ( ier != 0 ) {
                 thrshld = 100;
            }

            if ( cy_fips / 10000 <= thrshld ) {

	         _lInfo[_nCnty].lat = flat;
	         _lInfo[_nCnty].lon = flon;
	         _lInfo[_nCnty].cy_fips = tmpfips[ii];
	         strcpy( _lInfo[_nCnty].desc, desc );
	         cst_lcuc( _lInfo[_nCnty].desc, _lInfo[_nCnty].desc, &ier );
	         strcpy( _lInfo[_nCnty].state, state );
	         strcpy( _lInfo[_nCnty].st_fips, st_fips );
	         strcpy( _lInfo[_nCnty].wfo, pcwfo );
                 _nCnty++;
	    }

	  }
	  else  {
	    err_code = 5;
	    er_wmsg ( "pgen", &err_code, max_cnty, &ier, 
			strlen("pgen"), strlen(max_cnty) );
	    NxmErr_update();
	  }
       }

      } /* the end of for */

}

/*=====================================================================*/

 void pgwatch_numsort ( int *nin, int *inpint, int *nout, int *outnum, 
			int *iret)
/************************************************************************
 * pgwatch_numsort                                                      *
 *                                                                      *
 * This subroutine sorts a list of numbers. The output list is sorted   *
 * forwards and contains only the unique entries. 			*
 * The input and output arrays may be the same.                         *
 *                                                                      *
 * pgwatch_numsort ( nin, *inpint, *nout, *outnum, *iret) 	        *
 *                                                                      *
 * Input parameters:                                                    *
 *      *nin           	int             Number of input FIPS codes	*
 *      **inpint (nin) 	char*           Input FIPS codes                *
 *                                                                      *
 * Output parameters:                                                   *
 *      *nout           int             Number of output FIPS codes     *
 *      **outnum (nout) char            Sorted FIPS codes		*
 *      *iret           int             Return code                     *
 *                                        0 = normal return             *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/NCEP         01/05		Modifed from cst_sort           *
 ***********************************************************************/
 {
     int     ii, jj, istop, swpbuf;
/*---------------------------------------------------------------------*/
     *iret  = 0;
     istop = *nin;

    /*
     * Load output array.
     */

     for ( ii = 0; ii < *nin; ii++ ) {
         outnum [ii] = inpint[ii] ;
     }

    /*
     * Perform bubble sort.
     */

     for ( ii = 0; ii < istop; ii++ ) {
         for (jj = ii+1; jj < istop; ++jj ) {
             if ( outnum[ii] > outnum[jj] )  {
                  swpbuf = outnum[ii];
                  outnum[ii] = outnum[jj];
                  outnum[jj] = swpbuf;
             }
         }
     }

    /*
     * If the user has requested, return only unique entries.
     */

     jj = 0;
     for ( ii = 1; ii < *nin; ii++) {
          if  ( outnum [ii] != outnum [jj] ) {
              jj++;
              outnum [jj] = outnum [ii];
          }
     }
     *nout = jj+1;
     for ( ii = *nout+1; ii < *nin; ii++ ) {
          outnum [ii] = 0;
     }
 }

/*=====================================================================*/

void pgwatch_clrNumCwas ( void )
/************************************************************************
 * pgwatch_clrNumCwas							*
 *									*
 * This function set variable _nCwas to zero.				*
 *									*
 * void pgwatch_clrNumCwas ( )						*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		01/06	initial coding				*
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

    _nCwas = 0;
}

/*=====================================================================*/

void pgwatch_loadStCwa ( void )
/************************************************************************
 * pgwatch_loadStCwa							*
 *									*
 * This function loads the state and CWA buttons in their corresponding	*
 * areas. the function should be called whenever there is a change to   *
 * the current watch county list.					*
 *									*
 * void pgwatch_loadStCwa( )						*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		01/06	initial coding				*
 ***********************************************************************/
{
    IncInfo     *incstruct;
    CwaIncInfo  *cwa_incstruct;
    Boolean	new_st, new_wfo, msg_done, msg2_done;
    XmString	xmstr;
    Widget      state_form;
    int         ii, jj, err_code, ier, nrow;
    char	max_st[8], max_cwa[8], lbl_nm[12];
/*---------------------------------------------------------------------*/

    /*
     * state_form points to the form widget that has state and CWA 
     * info. on county list window.
     */ 
    state_form = pgwlst_getStatesForm();
    if( XtIsManaged(state_form) )  XtUnmanageChild( state_form );

    /*
     * Unmanage all state buttons on county list window. Prepare for new
     * state buttons.
     */
    incstruct = (IncInfo*)pgwlst_getStateBtns();
    for (ii = 0; ii < MXSTATES; ii++) {
	XtVaSetValues (incstruct[ii].wid, XmNset, FALSE, NULL);
        if( XtIsManaged(incstruct[ii].wid) ) {
	    XtUnmanageChild (incstruct[ii].wid);
        }
	incstruct[ii].include = FALSE;
    }

    /*
     * Unmanage all CWA buttons on county list window. Prepare for new
     * CWA buttons.
     */
    cwa_incstruct = (CwaIncInfo*)pgwlst_getCwaBtns();
    for (ii = 0; ii < MXCWAS; ii++) {
        if( XtIsManaged(cwa_incstruct[ii].form_wid) ) {
	    XtUnmanageChild (cwa_incstruct[ii].form_wid);
        }
    }
    XtUnmanageChild ( pgwlst_getInoutRC() );

    /*
     *  set up new state buttons on county list window.
     */
    _nStates  = 0;
    msg_done  = FALSE;
    msg2_done = FALSE;
    for ( ii = 0; ii < _nCnty; ii++ )  {

      /*
       * set up new state buttons.
       */
      new_st = TRUE;
      for ( jj = 0; jj < _nStates; jj++ ) {
	if ( strcasecmp (incstruct[jj].name, _lInfo[ii].state) == 0 ) {

	  new_st = FALSE;
          break;
        }
      }

      if ( new_st && _nStates < MXSTATES) {

        strcpy (incstruct[_nStates].name, _lInfo[ii].state);
	xmstr = XmStringCreateLocalized (incstruct[_nStates].name);
	XtVaSetValues (incstruct[_nStates].wid, 
		       XmNlabelString,	xmstr,
		       XmNset,		TRUE,
		       NULL);
	XmStringFree (xmstr);
	incstruct[_nStates].include = TRUE;
	_nStates++;
      }
      else if ( _nStates >= MXSTATES && !msg_done ) {
	err_code = 10;
	sprintf(max_st, "%d", MXSTATES );
	er_wmsg ( "pgen", &err_code, max_st, &ier, 
		  strlen("pgen"), strlen(max_st) );
	NxmErr_update();
        msg_done = TRUE;
      }

      /*
       * set up new CWA buttons on county list window.
       */
      new_wfo = TRUE;
      for ( jj = 0; jj < _nCwas; jj++ ) {
	if ( strcasecmp (cwa_incstruct[jj].name, _lInfo[ii].wfo) == 0 ) {

	  new_wfo = FALSE;
          break;
        }
      }

      if ( new_wfo && _nCwas < MXCWAS) {

	strcpy (cwa_incstruct[_nCwas].name, _lInfo[ii].wfo);
        sprintf(lbl_nm, "%s  ", cwa_incstruct[_nCwas].name);
	xmstr = XmStringCreateLocalized (lbl_nm);
	XtVaSetValues (cwa_incstruct[_nCwas].lbl_wid, 
		       XmNlabelString,	xmstr,
		       NULL);
	XmStringFree (xmstr);
	_nCwas++;
      }
      else if ( _nCwas >= MXCWAS && !msg2_done ) {
	        err_code = 11;
		sprintf(max_cwa, "%d", MXCWAS );
	        er_wmsg ( "pgen", &err_code, max_cwa, &ier, 
			  strlen("pgen"), strlen(max_cwa) );
	        NxmErr_update();
                msg2_done = TRUE;
	    }

    } /* the end of for ( ii = 0;... */

    /*
     * Set # of row for state buttons.
     */
    if (_nStates > 0) {
	nrow = (_nStates % INC_COL) ? (_nStates / INC_COL) + 1 : 
	    _nStates / INC_COL;

	XtVaSetValues (XtParent (incstruct[0].wid), 
		       XmNnumColumns, nrow, 
		       NULL);
        for (jj=0;jj<_nStates;jj++)  XtManageChild(incstruct[jj].wid);
    }

    /*
     * Set # of row for CWA buttons.
     * And manage IN-OUT label.
     * And call to set the states of CWA buttons.
     */
    if (_nCwas > 0) {

        XtManageChild ( pgwlst_getInoutRC() );

	nrow = (_nCwas % CWA_COL) ? (_nCwas / CWA_COL) + 1 : 
	    _nCwas / CWA_COL;
	XtVaSetValues (XtParent (cwa_incstruct[0].form_wid), 
		       XmNnumColumns, nrow, 
		       NULL);

        for (jj=0;jj<_nCwas;jj++)  XtManageChild(cwa_incstruct[jj].form_wid);
        pgwatch_setCwaBtnState ();
    }

    /*
     * Manage the outer form widget
     */
    if ( ! XtIsManaged(state_form) )  XtManageChild( state_form );

}

/*=====================================================================*/

void pgwatch_setCwaBtnState ( void )
/************************************************************************
 * pgwatch_setCwaBtnState						*
 *									*
 * This function sets the states for current CWA buttons.		*
 *									*
 * void pgwatch_setCwaBtnState( )					*
 *									*
 * Input parameters:							*
 *									*
 * Output parameters:							*
 *									*
 **									*
 * Log:									*
 * H. Zeng/SAIC		01/06	initial coding				*
 ***********************************************************************/
{
    CwaIncInfo  *cwa_incstruct;
    int         nfips, fips[MAXFIPS], ii, jj, kk, num_in, ier;
/*---------------------------------------------------------------------*/

    /*
     * get CWA list widget ids.
     */
    cwa_incstruct = (CwaIncInfo*)pgwlst_getCwaBtns();

    /*
     * loop through all current CWA widgets and set the 
     * button states.
     */
    for ( ii = 0; ii < _nCwas; ii++ ) {

      /*
       * find all the county fips codes that belong to a WFO office.
       */
      clo_findcwa ("MZ_CNTY", cwa_incstruct[ii].name, MAXFIPS, &nfips,
		   fips, &ier );

      /*
       * check each fips code against curent watch county list.
       * calculate the total number of counties that are in the
       * current watch county list.
       */
      num_in = 0;

      for ( jj = 0; jj < nfips; jj++ ) {
        for ( kk = 0; kk < _nCnty; kk++ )  {

          if ( fips[jj] == _lInfo[kk].cy_fips ) {
	    num_in++;
            break;
          }
        }

      }

      /*
       * set the state of CWA button according to num_in.
       */
      if ( num_in == 0 ) {
        cwa_incstruct[ii].state = -1;  /* all counties out */
      }
      else if ( num_in < nfips ) {
        cwa_incstruct[ii].state =  0;  /* partial counties in */
      }
      else {
        cwa_incstruct[ii].state =  1;  /* all counties in */
      }

      /*
       * set ON/OFf of CWA button according to state.
       */
      switch ( cwa_incstruct[ii].state ) {

        case  1:

  	  XtVaSetValues ( cwa_incstruct[ii].in_btn,
	                  XmNset,		TRUE,
			  NULL );        
  	  XtVaSetValues ( cwa_incstruct[ii].out_btn,
	                  XmNset,		FALSE,
			  NULL );      
          break;

        case  0:

  	  XtVaSetValues ( cwa_incstruct[ii].in_btn,
	                  XmNset,		FALSE,
			  NULL );        
  	  XtVaSetValues ( cwa_incstruct[ii].out_btn,
	                  XmNset,		FALSE,
			  NULL );      
          break;

        case -1:

  	  XtVaSetValues ( cwa_incstruct[ii].in_btn,
	                  XmNset,		FALSE,
			  NULL );        
  	  XtVaSetValues ( cwa_incstruct[ii].out_btn,
	                  XmNset,		TRUE,
			  NULL );      
          break;

        default:

          break;

      }

    } /* the end of for ( ii = 0; ii < _nCwas... */

}

/*=====================================================================*/

