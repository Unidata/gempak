#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "NxmTxt.h"
#include "pgprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "hints.h"
#include "proto_xw.h"

/************************************************************************
 * nmap_pgvgf.c                                                         *
 *                                                                      *
 * This module contains the VGF related functions for product		*
 * generation.               						*
 *                                                                      *
 * CONTENTS:                                                            *
 * pgvgf_save()       save VG element to a file and display the element *
 * pgvgf_saveNewElm() save a new VG element defined by a series points 	*
 * pgvgf_dsp()        display the newly stored element 			*
 ***********************************************************************/

/*=====================================================================*/

void pgvgf_save ( char grptyp, int grpnum, int np, float lat[], 
				float lon[], int *location, int *iret )
/************************************************************************
 * pgvgf_save								*
 *									*
 * This function saves VG elements to a file and then displays them to	*
 * the device.								*
 *									*
 * pgvgf_save ( grptyp, grpnum, np, lat, lon, location, iret )		*
 *									*
 * Input parameters:							*
 *	grptyp		char		Group type			*
 *	grpnum		int		Group number			*
 *	np		int		Number of points		*
 *	lat[]		float		latitudes			*
 *	lon[]		float		longitudes			*
 *									*
 * Output parameters:							*
 *	*location	int		location of new element		*
 *	*iret		int		Return code			*
 *					 -1 = Drawing function undeterm	*
 *					 -2 = No Text String entered    *
 *									*
 **									*
 * Log:									*
 * D. Keiser/GSC	 5/97	Moved from NxmMouReset			*
 * D. Keiser/GSC	 6/97	Added symbol storing and drawing	*
 * E. Wehner/EAi	 7/97	Added fill and close flags		*
 * D.W. Plummer/NCEP	 7/97	Added dashed lines & filled arrow	*
 * D. Keiser/GSC	 7/97	Change cpg_getset calling seq,		*
 *				get winds info from query funcs		*
 * E. Wehner/EAi	 7/97	Get front and line settings from DLG	*
 * E. Wehner/EAi	 8/97	Reorg code.  remove dr.set, cpggetset	*
 * E. Wehner/Eai	 8/97	use crg lib for element ranging		*
 * E. Safford/GSC	 9/97	replaced grP->cmd with cmd_ routines    *
 * C. Lin/EAI	 	10/97	add line color    			*
 * C. Lin/EAI	 	10/97	rename from NxmStoDspVG, cleanup    	*
 * E. Safford/GSC       10/97   added text handling functionality       *
 * C. Lin/EAI	 	10/97	add CLASS_WATCHES    			*
 * C. Lin/EAI	 	10/97	cleanup, pull code into pgvgf_saveNewElm*
 * C. Lin/EAI	 	11/97	bug fix, add turbsym			*
 * C. Lin/EAI           12/97   modified for watch box color            *
 * E. Safford/GSC       12/97   modified for new align values in GTEXT  *
 * C. Lin/EAI           12/97   bug fixes: initialize parameters        *
 * F. Yen/NCEP           1/98   Updated calls for crg library cleanup   *
 * W. Li/EAI		02/98	added call to pgobj_getId.		*
 * E. Safford/GSC	03/98	added location & -2 return code         *
 * S. Law/GSC		03/98	nmap_pgwndw_getAttr -> .._getData	*
 * W. Li/EAI		04/98	add OBJ_SPTEXTUD			*
 * C. Lin/EAI		04/98	add combo symbols			*
 * F. J. Yen/NCEP       04/98   updated with new ces function names     *
 * T. Lee/GSC           04/98   Added offset scaling for combo symbols  *
 * C. Lin/EAI		04/98	add group number and group type		*
 * E. Safford/GSC	05/98	update for saveNewElm                   *
 * I. Durham/GSC	05/98	changed underscore decl. to an include	*
 * S. Law/GSC		05/98	Added changes from drwids.h		*
 * D.W.Plummer/NCEP	 6/98	changes for watch boxes			*
 * W. Li/EAI		07/98	added color for front			*
 * T. Lee/GSC		08/98	Changed combo slash to a special symbol	*
 * C. Lin/EAI		09/98	use GRPTYP_COMSYM for combo-symbol	*
 * W. Li/EAI		10/98	Added call to pgxxx_xxxColor		*
 * E. Safford/GSC	11/98	adapt to use new gempak level comsyms	*
 * A. Hardy/GSC         12/98	Added circle element                    *
 * W. Li/EAI		01/99	NxmTxtA_XXX --> pgtxt_XXX		*
 * W. Li/EAI		03/99	added call to pgsymb_setLatLon()	*
 * S. Law/GSC		03/99	Updated parameters for pgline_getAttr	*
 * S. Law/GSC		05/99	added CLASS_TRACKS			*
 * G. Krueger/EAI	05/99	Corrected circle class			*
 * S. Law/GSC		07/99	added CLASS_SIGMETS			*
 * D.W.Plummer/NCEP	12/99	added all the watch format options init	*
 * D.W.Plummer/NCEP	 1/00	added call to pgwatch_init		*
 * S. Law/GSC		02/00	Added CCF				*
 * H. Zeng/EAI		08/00	added call to pgtrkw_resultsPopup()	*
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * J. Wu/GSC		02/01	Modified 'unused1' in VG to 'smooth'	*
 * E. Safford/GSC	03/01	changed params for pgline_getAttr()	*
 * E. Safford/GSC	04/01	use grptyp and grpnum, not ces values   *
 * M. Li/SAIC		10/01	Added MARKER				*
 * J. Wu/SAIC		10/01	change params for pgline_getAttr()	*
 * E. Safford/SAIC	02/02	pgline_setColor -> pgline_saveAttr  	*
 * T. Piper/SAIC	 4/02	added call to pgutls_initHdr to fix UMRs*
 * J. Wu/SAIC		08/02	allow symbols not listed on the object	*
 *				pallette to be saved as lines' label	*
 * J. Wu/SAIC		11/02	add CLASS_LIST				*
 * J. Wu/SAIC		11/02	set list items to 0 for a new list	*
 * J. Wu/SAIC		11/02	add color attr for list			*
 * H. Zeng/XTRIA	01/03   modified arguments to pgwbxw_getAttr	*
 * H. Zeng/XTRIA	07/03	added volcano element			*
 * J. Wu/SAIC		10/03	add CLASS_MET				*
 * H. Zeng/XTRIA	10/03   added ash cloud element			*
 * J. Wu/SAIC		02/04	add CLASS_MET->GFA_ELM			*
 * J. Wu/SAIC		10/04	Access GFA attr with cvg_setFld()	*
 * H. Zeng/SAIC		10/04	added two para. for pgwbxw_getAttr	*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 * M. Li/SAIC		09/06	added increment to pgwndw_getData	*
 * M. Li/SAIC		10/06	check for wind speed/directon limit	*
 ***********************************************************************/
{
    int		iclass, iobj, elmid, ifcod, ier, subtype, gemtyp, one=1, incr;
    int		wcolr, wstyle, wshape, mtype, mwidth, dummy1, cnty_colr, maxspd;
    float	fcode, msize, dummy;
    float 	wndspd, wnddir, tempdir;
    char	text[MAX_TEXT], dummyc;
    signed char	cnty_fill;
    VG_DBStruct	*el;
    textattrib_t attribs;
/*---------------------------------------------------------------------*/

    *iret = 0;
    if (np < 1) return; 

    /* 
     * Process each class of objects (FRONTS, LINES, SYMBOLS) 
     * For each class, set the gempak type (if applicable), and the 
     * element class and header in the VG structure.  After the info
     * is "set", use a call to cvg_writef to store the element
     */
    G_CALLOC(el, VG_DBStruct, one, "pgvgf_save:  VG_DBStruct");
    
    pgutls_initHdr( &(el->hdr) );

    iclass = pgpalw_getCurClassId();
    iobj   = pgpalw_getCurObjId();

    /*
     *  If a symbol is not listed on the object pallette but listed
     *  in the line's label pull-down menu, retrieve its object id
     *  for proper save & display.
     */
    if ( ( iobj == 0 ) && ( pglabel_getLabType() == 2 ) ) {
	pgline_getSymbInfo( &iclass, &iobj );
    }
    
    switch (iclass) {

      /*
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       *     FRONT CLASS ELEMENTS 
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       */
      case  CLASS_FRONTS:
	pgobj_getId (iclass, iobj, &elmid, &gemtyp, &ifcod);
	el->hdr.vg_type  = elmid;
	el->hdr.vg_class = CLASS_FRONTS;

	ces_get(gemtyp, el, &ier);

	ifcod += ( el->elem.frt.info.fwidth * 10);
	el->elem.frt.info.fcode = ifcod;

	pgfrtw_getAttr(&(el->hdr.smooth),&(el->hdr.maj_col), 
		       &(el->hdr.min_col));
	pgfrtw_setmajColor();
	pgfrtw_setminColor();
	break;

	/*
	 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 *     LINES CLASS ELEMENTS 
	 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 */
      case CLASS_LINES:
	pgobj_getId (iclass, iobj, &elmid, &gemtyp, &ifcod);
	el->hdr.vg_type = elmid;
	el->hdr.vg_class = CLASS_LINES;

  	ces_get(gemtyp, el, &ier); 

	/*
	 * get line attributes
	 */
	if (elmid == LINE_ELM) {
	    pgline_getAttr(&(el->hdr.filled), 
			   &(el->hdr.closed),
			   &(el->hdr.maj_col),
			   &(el->elem.lin.info.width),
			   &dummy, &dummy1,
			   &(el->hdr.smooth), 
			   &(dummyc));
	}
	else {
	    if ( gemtyp == 24 || gemtyp == 25 ) {
	        pgline_getAttr(&(el->hdr.filled), 
			   &(el->hdr.closed),
			   &(el->hdr.maj_col),
			   &(el->elem.spl.info.splwid),
			   &(el->elem.spl.info.splsiz),
			   &(el->elem.spl.info.splstr),
			   &(el->hdr.smooth),
			   &(dummyc));	    
	    }
	    else {	    	    
	        pgline_getAttr(&(el->hdr.filled), 
			   &(el->hdr.closed),
			   &(el->hdr.maj_col),
			   &(el->elem.spl.info.splwid),
			   &(el->elem.spl.info.splsiz),
			   &(dummy1),
			   &(el->hdr.smooth),
			   &(dummyc));	    
	    }
	}

	pgline_saveAttr();
	break;

	/*
	 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 *     WATCHES CLASS ELEMENTS 
	 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 */
      case CLASS_WATCHES:

	/*
	 * get watch color 
	 */
	pgwbxw_getAttr(&wcolr, &wstyle, &wshape, 
		       &mtype, &msize,  &mwidth,
		       &cnty_fill, &cnty_colr );

	/*
	 * get element setting
	 */
	el->hdr.vg_class = CLASS_WATCHES;
	el->hdr.vg_type  = WBOX_ELM;

	pgwatch_init ( el, wstyle, wshape, wcolr, mtype, msize, 
                       mwidth, cnty_fill, cnty_colr, np, lat, lon,
		       &ier );

	break;

	/*
	 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 *     SYMBOLS, COMSYM & MARKER CLASS ELEMENTS 
	 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 */
      case CLASS_SYMBOLS:
      case CLASS_COMSYM:
      case CLASS_MARKER:
	pgobj_getId (iclass, iobj, &elmid, &gemtyp, &subtype);
	el->hdr.vg_type = elmid;
	fcode = (float)subtype;

	el->hdr.vg_class = CLASS_SYMBOLS;
	ces_get(subtype, el, &ier);

	pgsymb_getColor(&(el->hdr.maj_col));
	el->elem.sym.info.ityp = 0;
	el->elem.sym.data.code[0] = fcode;
	el->elem.sym.data.offset_xy[0] = 0;
	el->elem.sym.data.offset_xy[1] = 0;
	pgsymb_saveAttr();
	pgsymb_setLatLon(lat[0], lon[0]);

	break;

	/*
	 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 *     WINDS CLASS ELEMENTS 
	 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 */
      case CLASS_WINDS:
	pgobj_getId (iclass, iobj, &elmid, &gemtyp, &ifcod);
	el->hdr.vg_type = elmid;
	gemtyp = -1;

	el->hdr.vg_class = CLASS_WINDS;
	ces_get( gemtyp, el, &ier);

	np = 1;
	pgwndw_getData (&wnddir, &wndspd, &(el->hdr.maj_col), &incr);
	maxspd = (el->hdr.vg_type == BARB_ELM) ? 400 : 200;
	if ( incr == 1 ) {
	    el->elem.wnd.data.spddir[np] += wnddir;
	    el->elem.wnd.data.spddir[0]  += wndspd;

     	    if ( el->elem.wnd.data.spddir[1] > 360.0F )
                el->elem.wnd.data.spddir[1] = (float)((int)el->elem.wnd.data.spddir[1] % 360);
            if ( el->elem.wnd.data.spddir[1] < 0.0F ) {
                tempdir = (float)((int)G_ABS(el->elem.wnd.data.spddir[1]) % 360);
                el->elem.wnd.data.spddir[1] = 360.0F - tempdir;
            }

	    if ( el->elem.wnd.data.spddir[0] < 0.0F ) el->elem.wnd.data.spddir[0] = 0.0F;
	    if ( el->elem.wnd.data.spddir[0] > maxspd ) el->elem.wnd.data.spddir[0] = (float)maxspd;
	}
	else {
	    el->elem.wnd.data.spddir[np] = wnddir;
            el->elem.wnd.data.spddir[0]  = wndspd;
        }
	pgwndw_saveColor();
	break;

	/*
	 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 *     TEXT CLASS ELEMENTS 
	 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 */
      case CLASS_TEXT:
	el->hdr.vg_class = CLASS_TEXT;
	pgtxt_getAttr(&attribs, text);
	if (strlen(text) <= (size_t)0) {
	    G_FREE(el, VG_DBStruct);
	    *iret = -2;
	    return;
	}


	/*
	 * Note on alignment:
	 *	Calls for TEXTC_ELM and TEXT_ELM types are sent to GTEXT,
	 *	while the special text types are sent to GTXSY.  At the
	 *	moment GTEXT uses a value range of 1 .. 3 for alignment,
	 *	while GTXSY uses the older range of -1 .. 1.  Eventually
	 *	GTXSY will be modified to use the same range.  Until that
	 *	happens the alignment will be modifed as below. 
	 *	pgtxt_getAttr will return a range of -1 to 1 for the 
	 *	alignment value.
	 *	Currently, all new text elements are being saved as special
	 *	text (since all but one case have special features).
	 */

	el->hdr.maj_col			= attribs.colr;
	el->hdr.min_col			= attribs.colr;
	el->hdr.vg_type			= SPTX_ELM;
	el->elem.spt.info.sptxtyp	= attribs.sptxtyp;
	el->elem.spt.info.itxfn		= attribs.gemfont;
	el->elem.spt.info.ithw		= attribs.fontithw;
	el->elem.spt.info.iwidth		= attribs.iwidth;
	el->elem.spt.info.ialign		= attribs.align;
	el->elem.spt.info.turbsym	= attribs.turb;
	el->elem.spt.info.rotn		= attribs.frotn;
	el->elem.spt.info.sztext		= attribs.fsize;
	el->elem.spt.info.offset_x	= 0;
	el->elem.spt.info.offset_y	= 0;
	el->elem.spt.info.txtcol		= attribs.colr;
	el->elem.spt.info.lincol		= attribs.colr;
	el->elem.spt.info.filcol		= attribs.colr;
	strcpy(el->elem.spt.text, text);
	break;

        /*
         * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
         *     CIRCLE CLASS ELEMENTS
         * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
         */
      case CLASS_CIRCLE:
	pgobj_getId (iclass, iobj, &elmid, &gemtyp, &ifcod);
	el->hdr.vg_type = elmid;
	el->hdr.vg_class = CLASS_CIRCLE;

	ces_get(gemtyp, el, &ier);

	/*
	 * get line attributes
	 */
	pgcirc_getAttr( &(el->hdr.maj_col), &(el->elem.cir.info.width) );
	el->hdr.min_col = el->hdr.maj_col;
	pgcirc_saveAttr();
	break;

    	/*
	 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 *     TRACKS CLASS ELEMENTS 
	 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 */
      case CLASS_TRACKS:
	pgobj_getId (iclass, iobj, &elmid, &gemtyp, &ifcod);
	el->hdr.vg_type = TRKSTORM_ELM;
	el->hdr.vg_class = CLASS_TRACKS;
	el->elem.trk.info.subtype = 0;

	ces_get(0, el, &ier);

	el->elem.trk.info.nipts = el->elem.trk.info.npts = np;

	pgtrkw_getAttr (el);
	pgtrkw_extrapolate (lat, lon, el);
	pgtrkw_resultsPopup();
	np = 0;

	break;

    	/*
	 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 *     SIGMETS CLASS ELEMENTS 
	 * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
	 */
      case CLASS_SIGMETS:
	pgobj_getId (iclass, iobj, &elmid, &gemtyp, &ifcod);
	el->hdr.vg_type = elmid;
	el->hdr.vg_class = CLASS_SIGMETS;

	if (elmid == SIGCCF_ELM) {
	    el->elem.ccf.info.subtype = -99;

	    ces_get(-99, el, &ier);

	    el->elem.ccf.info.npts = np;

	    pgccfw_getAttr (el);
	}
	else if (elmid == VOLC_ELM) {

	    ces_get(-99, el, &ier);

	    /*
	     * Set special symbol code for volcano.
	     */
	    el->elem.vol.info.code = (float)201;

	    /*
	     * Set offsets from latlon location for volcano.
	     */
	    el->elem.vol.offset_xy[0] = 0;
	    el->elem.vol.offset_xy[1] = 0;

	    pgvolw_getAttr ( el );
	    
	}
	else if (elmid == ASHCLD_ELM) {
	    el->elem.ash.info.subtype = -99;

	    ces_get(-99, el, &ier);

	    el->elem.ash.info.npts = np;

	    pgvacw_getAttr ( el );
	    
	}
	else {
	    el->elem.sig.info.subtype = -99;

	    ces_get(-99, el, &ier);

	    el->elem.sig.info.npts = np;
	    el->elem.sig.info.seqnum = 0;

	    pgsigw_getAttr (el);
	}

	break;
      
      case CLASS_LIST:
	pgobj_getId (iclass, iobj, &elmid, &gemtyp, &subtype);
	el->hdr.vg_type = elmid;

	el->hdr.vg_class = CLASS_LIST;
	ces_get(subtype, el, &ier);

	el->elem.lst.info.subtyp = subtype;
	
	pglist_getAttr(&el->elem.lst.info.mrktyp,
	               &el->hdr.maj_col,
	               &el->elem.lst.info.mrksiz,
	               &el->elem.lst.info.mrkwid );
		
	/*
	 *  Set the number of items to 0 for a new LIST object.
	 */
	el->elem.lst.data.nitems = 0;

	break;
    
      case CLASS_MET:
	pgobj_getId ( iclass, iobj, &elmid, &gemtyp, &subtype );
	el->hdr.vg_type = elmid;

	el->hdr.vg_class = CLASS_MET;
	ces_get ( subtype, el, &ier );
	
	if ( el->hdr.vg_type == JET_ELM ) { 
            el->elem.jet.line.spl.info.numpts = np;
	}
	else if ( el->hdr.vg_type == GFA_ELM ) {
	    
	    el->elem.gfa.info.npts = np;
	    cvg_setFld ( el, TAG_GFA_LAT, "-999.0", &ier );
	    cvg_setFld ( el, TAG_GFA_LON, "-999.0", &ier );
	    
	    pggfaw_getAttr ( el );
	}
	
	break;
  
    }


    /*
     *  Note:   calls to ces_get() return the default grptyp and grpnum
     *		for the element in question.  The group information does 
     *		_NOT_ reflect the current state of the element attribute 
     *		window though.  Use the supplied grptyp and grpnum instead.
     */
    el->hdr.grptyp = grptyp;
    el->hdr.grpnum = grpnum;

    pgvgf_saveNewElm(NULL, sys_M, el, np, lat, lon, TRUE, location, &ier);

    G_FREE(el, VG_DBStruct);

    pgvgf_dsp(*location, &ier);

    NxmErr_update();

    geplot(&ier);

}

/*=====================================================================*/

void pgvgf_saveNewElm ( char *filnam, char *sys, VG_DBStruct *el, int np, 
			float rx[], float ry[], Boolean updatepl,
			int *location, int *iret )
/************************************************************************
 * pgvgf_saveNewElm							*
 *									*
 * This function saves a new VG element defined by a series of		*
 * points.								*
 *									*
 * pgvgf_saveNewElm ( filnam, sys, el, np, rx, ry, updatepl, location,  *
 * 		      iret )						*
 *									*
 * Input parameters:							*
 *	*filnam		char		VG filename			*
 *	*sys		char		coordinate system of the points	*
 *	*el		VG_DBStruct	pointer to an element structure	*
 *	np		int		Number of points		*
 *	rx[]		float		X coordinates			*
 *	ry[]		float		Y coordinates			*
 *	updatepl	Boolean		Flag to update placement info	*
 *									*
 * Output parameters:							*
 *	*location	int		location of the new elem in file*
 *	*iret		int		Return code			*
 *					0 - normal                      *
 *	 				-19 - error saving record	*
 *					-27 = invalid wtch/warn box type*
 *					-31 = invalid front attribute	*
 **									*
 * Log:									*
 * C. Lin/EAI		11/97						*
 * S. Law/GSC		04/98	Updated calls to cvg_frsto, cvg_linsto,	*
 *				and cvg_splsto				*
 * E. Safford/GSC	05/98	add filnam				*
 * S. Law/GSC		05/98	Added changes from drwids.h		*
 * D.W.Plummer/NCEP	 6/98	changes for watch boxes			*
 * C. Lin/EAI		09/98   remove pgfrtw_setSvdSmth, 		*
 *				modify calling seq. to pgline_setSvdChar*
 * A. Hardy/GSC		12/98	Added calls to cvg_cirsto               *
 * E. Safford/GSC	03/99	add call to xpgrfrsh			*
 * S. Law/GSC		05/99	added CLASS_TRACKS			*
 * G. Krueger/EAI	05/99	Modified circles for latlon array	*
 * G. Krueger/EAI	06/99	Fixed single point circle problem	*
 * S. Law/GSC		07/99	added CLASS_SIGMETS			*
 * S. Law/GSC		08/99	updated element				*
 * S. Law/GSC		09/99	updated element				*
 * D.W.Plummer/NCEP	 1/00	updated call for watch element		*
 * S. Law/GSC		02/00	cleanup					*
 * S. Law/GSC		05/00	removed hard coded CCF colors		*
 * H. Zeng/EAI		08/00	added skip factor for track		*
 * F. J. Yen/NCEP	08/00	updated call for sigmet element		*
 * D.W.Plummer/NCEP	10/00	updated call for watch element		*
 * M. Li/GSC		10/00	updated call for track element		*
 * J. Wu/GSC		11/00	Replaced calls to cvg_???sto() with     *
 *	                        direct calls to gtrans() & cvg_sv???()  *
 * A. Hardy/GSC         11/00   renamed coordinate system declaration   *
 * J. Wu/GSC		12/00	Replaced calls to cvg_sv???() with      *
 *	                        new cvg_writelm() & cvg_initelm()       *
 * J. Wu/GSC		02/01	Replaced cvg_writelm() with cvg_writef()*
 * E. Safford/GSC	02/01	fix circle lat/lon error		*
 * J. Wu/GSC		12/01	rebuild range record after xpgrfrsh()	*
 * J. Wu/SAIC		02/02	set layering flags for current layer	*
 * T. Piper/SAIC	04/02	fixed UMR with circles			*
 * J. Wu/GSC		10/02	replace crg_rebuild() with crg_set()	*
 * J. Wu/GSC		11/02	add CLASS_LIST				*
 * J. Wu/SAIC		11/02	transfer list items to current list 	*
 * H. Zeng/XTRIA	07/03	added volcano and ash cloud		*
 * J. Wu/GSC		10/03	add CLASS_MET				*
 * H. Zeng/XTRIA	10/03   added more more ash cloud		*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC		11/03	call cvg_snapjet for CLASS_MET->JET_ELM	*
 * J. Wu/SAIC		02/04	add CLASS_MET->GFA_ELM			*
 * B. Yin/SAIC		04/04	add CLASS_MET->TCA_ELM			*
 * B. Yin/SAIC		05/04	Modified to get record size dynamically	*
 * J. Wu/SAIC		08/04	Correct LIST range record calculation	*
 * J. Wu/SAIC		10/04	recalculate GFA size			*
 * B. Yin/SAIC		12/05	check if GFA is closed			*
 * B. Yin/SAIC		02/06	remove the call to pggfa_isClosed	*
 * S. Danz/AWC		07/06	Update to new cvg_writef() parameter    *
 * S. Danz/AWC		08/06	Add parameter to update placement info	*
 * S. Danz/AWC          08/06   Updated to use cvg_checkplace to find   *
 * 				area impacted and call crg_rebuild()    *
 * E. Safford/SAIC	06/07	set saved flag false for error on write *
 * B. Yin/SAIC		03/08	check if GFA from lines are more than 3	*
 * B. Yin/SAIC		03/08	check if FBBA states list changes	*
 * B. Yin/SAIC		06/08	remove the check for FBBA states list 	*
 ***********************************************************************/
{
    int		nn, ii, jj, ier, start, recsize, lens, cur_layer, found;
    int		iblk, nblk, npblk, ipt1, lclspt, npx, iquad, lquad[4];
    float	*platx, *plony, *tmplon, *tmplat;
    float	latx[2], lony[2], lon, lat, outx[2], outy[2];
    float       rlat, rlon, dlon, dist, dir, inf_bbox[4];
    char	system[2], *ptca; 
    Boolean	samesys, saved;
    VG_DBStruct	*cur_list;
/*---------------------------------------------------------------------*/

    *iret = G_NORMAL;   
    saved = FALSE;
    nn = np;
    start = -1;      /* Indicating write to the end of file */
    platx = rx;
    plony = ry;
   
    if ( np < 0 ) {
        *iret = -28;
	return;
    }

    /*
     *  Check if FBBA text are more than 3 lines.
     */
    pggfawp_checkGfa3Lines( el );

    /*
     *  Check if FBBA states list changes.
     *  The state list warning function is taken out via AWC request.
     *  The code is kept in case needed later. (8/6/2008)
     */
    /*
    pggfawp_statesListWarning( el );
    */ 
    
    cur_layer = pglayer_getCurLayer ();
    
    /*
     * Ininitialize specific characteristics for VG elements.
     */
    cvg_initelm( el );
    
    /* 
     *  Determine the coordinate system.
     */
    samesys = TRUE;
    strcpy (system, sys);
    if ( np == 0 ) strcpy (system, sys_M);
    if ( strcmp( system, sys_M ) != 0) samesys = FALSE;
        
    /* 
     *  Allocate space for converting if not in map ("M") coordinate.
     */
    if ( !samesys ) {
        tmplon = (float *)malloc(sizeof(float) * np);
        tmplat = (float *)malloc(sizeof(float) * np);
    }
          
    /*
     *  Handle each class accordingly
     */
     
    switch ( el->hdr.vg_class ) {
      /*
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       *     FRONT CLASS ELEMENTS 
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       */
      case  CLASS_FRONTS:	
	if ( np == 0 ) {
	    nn = el->elem.frt.info.numpts;
	    platx = el->elem.frt.latlon;
	    plony = &(el->elem.frt.latlon[nn]);
	}
        
	if ( !samesys ) {
           gtrans (system, sys_M, &nn, platx, plony, tmplat, tmplon, 
		   &ier, strlen(system), strlen(sys_M));
        }
	else {
	   tmplon = plony;
	   tmplat = platx;	   
	}

        if ( nn >= MAXPTS ) {
	    *iret = -28;
	    return;
	}
	
	/*
	 *  Split the line into as many blocks as necessary.
	 */
        nblk = (nn - 2) / (MAXPTS - 1) + 1;
        for ( iblk = 0; iblk < nblk; iblk++ ) {
	   if ( iblk < (nblk - 1) ) {
	       npblk = MAXPTS - 1;
	   }
	   else {
	       npblk = np - (MAXPTS - 1) * iblk;
	   }
	   
	   recsize = ( (sizeof(float) * 2 * npblk) + 
	      	        sizeof(VG_HdrStruct) + sizeof(FrontInfo) );
	   ipt1 = iblk * (MAXPTS - 1);

	   el->hdr.recsz = recsize;	   
	   el->hdr.range_min_lon = tmplon[ipt1];
	   el->hdr.range_min_lat = tmplat[ipt1];
	   el->hdr.range_max_lon = tmplon[ipt1];
	   el->hdr.range_max_lat = tmplat[ipt1];

	   for ( ii = 0; ii < npblk; ii++ ) {
	       el->elem.frt.latlon[ii      ] = tmplat[ipt1 + ii];
	       el->elem.frt.latlon[ii+npblk] = tmplon[ipt1 + ii];

	       if ( el->hdr.range_min_lon > el->elem.frt.latlon[ii+npblk] )
	   	    el->hdr.range_min_lon = el->elem.frt.latlon[ii+npblk];
	       if ( el->hdr.range_min_lat > el->elem.frt.latlon[ii] )
		    el->hdr.range_min_lat = el->elem.frt.latlon[ii];
	       if ( el->hdr.range_max_lon < el->elem.frt.latlon[ii+npblk] )
		    el->hdr.range_max_lon = el->elem.frt.latlon[ii+npblk];
	       if ( el->hdr.range_max_lat < el->elem.frt.latlon[ii] )
		    el->hdr.range_max_lat = el->elem.frt.latlon[ii];
	   }
     
	   if ( el->elem.frt.info.fcode < 0 || el->elem.frt.info.fcode > 999 
	        || el->elem.frt.info.fpipsz <= 0  
	        || el->elem.frt.info.fpipst <= 0 )  {
	       *iret = -31;
	       return;
	   }

	   el->elem.frt.info.numpts = npblk;
	   strcpy ( el->elem.frt.info.frtlbl, "STJ" );
	   
	   cvg_writef( el, start, el->hdr.recsz, filnam, TRUE, location, &ier );   
	}
        
	saved = TRUE;
	break;

      /*
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       *     LINES CLASS ELEMENTS 
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       */
      case CLASS_LINES:
	if ( np == 0 ) {
	    if (el->hdr.vg_type == SPLN_ELM) {
		nn = el->elem.spl.info.numpts;
		platx = el->elem.spl.latlon;
		plony = &(el->elem.spl.latlon[nn]);
	    }
	    else {
		nn = el->elem.lin.info.numpts;
		platx = el->elem.lin.latlon;
		plony = &(el->elem.lin.latlon[nn]);
	    }
	}

	if ( !samesys ) {
           gtrans (system, sys_M, &nn, platx, plony, tmplat, tmplon, 
		   &ier, strlen(system), strlen(sys_M));
        }
	else {
	   tmplon = plony;
	   tmplat = platx;	   
	}

	/*
	 *  Make sure that closed lines will remain visually closed.
	 */
	lclspt = 0;
        if ( nn >= MAXPTS && ( el->hdr.closed != 0 || el->hdr.filled != 0 ) )
	     lclspt = 1;
        
	/*
	 *  Split the line into as many blocks as necessary.
	 */
	nblk = (np - 2) / (MAXPTS - 1) + 1 + lclspt;
        for ( iblk = 0; iblk < nblk; iblk++ ) {
            if ( iblk < (nblk - 1 - lclspt) ) {
        	npblk = MAXPTS - 1;
            }
            else if ( lclspt == 0 || iblk < (nblk - 1) ) {
        	npblk = np - (MAXPTS - 1) * iblk;
            }
            else {
	        npblk = 2;
	    }
	     
	    ipt1 = iblk * (MAXPTS - 1);
	    el->hdr.range_min_lon = tmplon[ipt1];
	    el->hdr.range_min_lat = tmplat[ipt1];
	    el->hdr.range_max_lon = tmplon[ipt1];
	    el->hdr.range_max_lat = tmplat[ipt1];

	    /*
	     *  Use reasonable characteristics for lines that are too long.
	     */	    
	    if ( nn >= MAXPTS ) {
	        el->hdr.filled = 0;
	        el->hdr.closed = 0;
	    }
	
	    if ( el->hdr.vg_type == SPLN_ELM ) {    /* Special line */
	        recsize = ( (sizeof(float) * 2 * npblk) + 
	           	     sizeof(VG_HdrStruct) + sizeof(SpLineInfo) );
	        for ( ii = 0; ii < npblk; ii++ ) {
	            if ( iblk <= nblk - 1 - lclspt ) {
	                /*
	                 *  Not closing off a line, add current point.
	                 */
	        	el->elem.spl.latlon[ii      ] = tmplat[ipt1 + ii];
	        	el->elem.spl.latlon[ii+npblk] = tmplon[ipt1 + ii];
	            }	            
	            else {
		        /*
	                 *  Closing off the line.
	                 */
	        	el->elem.spl.latlon[1      ] = tmplat[0];
	        	el->elem.spl.latlon[1+npblk] = tmplon[0];
	        	el->elem.spl.latlon[0      ] = tmplat[nn - 1];
	        	el->elem.spl.latlon[0+npblk] = tmplon[nn - 1];
	            }

	            /* 
	             *	    Establish max and mins if they apply. 
	             */
	            if ( el->hdr.range_min_lon > el->elem.spl.latlon[ii+npblk] )
	        	 el->hdr.range_min_lon = el->elem.spl.latlon[ii+npblk];
	            if ( el->hdr.range_min_lat > el->elem.spl.latlon[ii] )
		         el->hdr.range_min_lat = el->elem.spl.latlon[ii];
	            if ( el->hdr.range_max_lon < el->elem.spl.latlon[ii+npblk] )
	          	 el->hdr.range_max_lon = el->elem.spl.latlon[ii+npblk];
	            if ( el->hdr.range_max_lat < el->elem.spl.latlon[ii] )
		         el->hdr.range_max_lat = el->elem.spl.latlon[ii];
	        }
     
	        el->elem.spl.info.numpts = npblk;	    
	    }
	    
	    else {    /* Regular line: LINE_ELM */
	        recsize = ( (sizeof(float) * 2 * npblk) + 
	        	     sizeof(VG_HdrStruct) + sizeof(LineInfo) );
	        for ( ii = 0; ii < npblk; ii++ ) {
	            if ( iblk <= nblk - 1 - lclspt ) {
	        	el->elem.lin.latlon[ii      ] = tmplat[ipt1 + ii];
	        	el->elem.lin.latlon[ii+npblk] = tmplon[ipt1 + ii];
	            }
	            else {
	        	el->elem.lin.latlon[1      ] = tmplat[0];
	        	el->elem.lin.latlon[1+npblk] = tmplon[0];
	        	el->elem.lin.latlon[0      ] = tmplat[nn - 1];
	        	el->elem.lin.latlon[0+npblk] = tmplon[nn - 1];
	            }

	            if ( el->hdr.range_min_lon > el->elem.lin.latlon[ii+npblk] )
	        	 el->hdr.range_min_lon = el->elem.lin.latlon[ii+npblk];
	            if ( el->hdr.range_min_lat > el->elem.lin.latlon[ii] )
		         el->hdr.range_min_lat = el->elem.lin.latlon[ii];
	            if ( el->hdr.range_max_lon < el->elem.lin.latlon[ii+npblk] )
	          	 el->hdr.range_max_lon = el->elem.lin.latlon[ii+npblk];
	            if ( el->hdr.range_max_lat < el->elem.lin.latlon[ii] )
		         el->hdr.range_max_lat = el->elem.lin.latlon[ii];
	        }
     
	        el->elem.lin.info.numpts = npblk;	    
	    }
	    
	    el->hdr.recsz = recsize;	   		   
	    cvg_writef( el, start, el->hdr.recsz, filnam, TRUE, location, &ier ); 	
        }
	
	saved = TRUE;  	
	break;

      /*
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       *     SYMBOLS CLASS ELEMENTS 
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       */
       case CLASS_SYMBOLS:
	if ( np == 0 ) {
	    nn = el->elem.sym.info.numsym;
	    platx = el->elem.sym.data.latlon;
	    plony = &(el->elem.sym.data.latlon[nn]);
	}
	
	if ( !samesys ) {
           gtrans (system, sys_M, &nn, platx, plony, tmplat, tmplon, 
		   &ier, strlen(system), strlen(sys_M));
        }
	else {
	   tmplon = plony;
	   tmplat = platx;	   
	}
        
	if ( nn >= MAXPTS ) {
	    *iret = -28;
	    return;
	}
	
	recsize = ( nn * ( sizeof( float ) * 3 + sizeof( int ) * 2 )
	                 + sizeof( VG_HdrStruct) + sizeof( SymInfo ) );	

	el->hdr.range_min_lat = tmplat[0];
	el->hdr.range_min_lon = tmplon[0];
	el->hdr.range_max_lat = tmplat[0];
	el->hdr.range_max_lon = tmplon[0];
        el->elem.sym.info.numsym = nn;
	el->elem.sym.data.latlon[0] = tmplat[0];
	el->elem.sym.data.latlon[nn] = tmplon[0];
			
 	break;

      /*
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       *     WINDS CLASS ELEMENTS 
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       */
       case CLASS_WINDS:		
	if ( np == 0 ) {
	    nn = el->elem.wnd.info.numwnd;
	    platx = el->elem.wnd.data.latlon;
	    plony = &(el->elem.wnd.data.latlon[nn]);
	}

	if ( !samesys ) {
           gtrans (system, sys_M, &nn, platx, plony, tmplat, tmplon, 
		   &ier, strlen(system), strlen(sys_M));
        }
	else {
	   tmplon = plony;
	   tmplat = platx;	   
	}

	recsize = ( sizeof( float ) * nn * 4 + sizeof( VG_HdrStruct)
	          + sizeof( WindInfo ) );	

	el->hdr.range_min_lat = tmplat[0];
	el->hdr.range_min_lon = tmplon[0];
	el->hdr.range_max_lat = tmplat[0];
	el->hdr.range_max_lon = tmplon[0];
        el->elem.wnd.info.numwnd = nn;
	el->elem.wnd.data.latlon[0] = tmplat[0];
	el->elem.wnd.data.latlon[1] = tmplon[0];
		        
	break;

      /*
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       *     TEXT CLASS ELEMENTS 
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       */
      case CLASS_TEXT:
	if ( np == 0 ) {
	    if (el->hdr.vg_type == SPTX_ELM) {
		latx[0] = el->elem.spt.info.lat;
		lony[0] = el->elem.spt.info.lon;
	    }
	    else {
		latx[0] = el->elem.txt.info.lat;
		lony[0] = el->elem.txt.info.lon;
	    }
	}
	else {
	    latx[0] = rx[0];
	    lony[0] = ry[0];
	}
	
	if ( !samesys ) {
           gtrans (system, sys_M, &nn, &latx[0], &lony[0], &lat, &lon, 
		   &ier, strlen(system), strlen(sys_M));
        }
	else {
	   lon = lony[0];
	   lat = latx[0];	   
	}
        
	if (el->hdr.vg_type == SPTX_ELM)            
	    lens = strlen(el->elem.spt.text);
        else
	    lens = strlen(el->elem.txt.text);

	/*
	 *  Check if MAXPTS exceeded or MAX_TEXT or no text.
	 */
	if ( nn >= MAXPTS || lens == 0 ) {
	    *iret = -28;
	    return;
	}
		
	el->hdr.range_min_lat = lat;
	el->hdr.range_min_lon = lon;
	el->hdr.range_max_lat = lat;
	el->hdr.range_max_lon = lon;
                
	if (el->hdr.vg_type == SPTX_ELM) {            
	    el->elem.spt.info.lat = lat;
	    el->elem.spt.info.lon = lon;
	    recsize = ( sizeof( VG_HdrStruct) + 
	                sizeof( SpTextInfo) + lens + 1 );	
        } 
	else {	  
	    el->elem.txt.info.lat = lat;
	    el->elem.txt.info.lon = lon;	    
	    el->elem.txt.info.ialign = el->elem.txt.info.ialign%10 - 2;
	    recsize = ( sizeof( VG_HdrStruct) + 
	                sizeof( TextInfo) + lens + 1 );	
	}       
		
	break;

      /*
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       *     CIRCLES CLASS ELEMENTS 
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       */
      case CLASS_CIRCLE:

	if ( np == 0 ) {
	    nn = el->elem.cir.info.numpts;
	    latx[0] = el->elem.cir.data.latlon[0];
	    lony[0] = el->elem.cir.data.latlon[2];
	    latx[1] = el->elem.cir.data.latlon[1];
	    lony[1] = el->elem.cir.data.latlon[3];
	}
	else if ( np == 1 ) {
	    latx[0] = rx[0];
	    lony[0] = ry[0];
	}
	else {
	    latx[0] = rx[0];
	    lony[0] = ry[0];
	    latx[1] = rx[1];
	    lony[1] = ry[1];
	}

	if ( nn == 1 ) {
	    nn = 2;
	    latx[1] = latx[0];
	    lony[1] = lony[0];
	}
	
        if ( !samesys ) {
	   gtrans (system, sys_M, &nn, latx, lony, outx, outy, &ier, 
		   strlen(system), strlen(sys_M));
        }
	else {
	    outx[0] = latx[0];
	    outy[0] = lony[0];
	    outx[1] = latx[1];
	    outy[1] = lony[1];
	}
        
	/*
         *  Initialize quadrant detection.
         */
        for ( iquad = 0; iquad < 4; iquad++ ) {
	     lquad[iquad] = 0;
        }
    
        recsize = ( (sizeof(float) * 2 * 2) + sizeof(VG_HdrStruct) + 
		 sizeof(LineInfo) );
        npx = 1;
        clo_dist ( &outx[0], &outy[0], &npx, &outx[1], &outy[1], &dist, &ier );
        
	/*
         *  Finding the max and min latitudes and longitudes.
         */
        el->hdr.range_min_lon = 180.F;
        el->hdr.range_min_lat = 90.F;
        el->hdr.range_max_lon = -180.F;
        el->hdr.range_max_lat = -90.F;
        
	for ( ii = 0; ii < 360; ii += 10 ){
	    dir = (float)ii;
	    clo_dltln ( &outx[0], &outy[0], &dist, &dir, &rlat, &rlon, &ier );
	    dlon = rlon;
	    if ( rlon -  outy[0] > 180.F ) dlon = rlon - 360.F;
	    if ( outy[0] - rlon > 180.F ) dlon = rlon + 360.F;
	    if ( dlon < el->hdr.range_min_lon ) el->hdr.range_min_lon = dlon;
	    if ( rlat < el->hdr.range_min_lat ) el->hdr.range_min_lat = rlat;
	    if ( dlon > el->hdr.range_max_lon ) el->hdr.range_max_lon = dlon;
	    if ( rlat > el->hdr.range_max_lat ) el->hdr.range_max_lat = rlat;

	    if ( rlon >=    0.F && rlon <  90.F ) lquad[0] = 1;
	    if ( rlon >=   90.F && rlon < 180.F ) lquad[1] = 1;
	    if ( rlon >= -180.F && rlon < -90.F ) lquad[2] = 1;
	    if ( rlon >=  -90.F && rlon <   0.F ) lquad[3] = 1;
        }

        if ( lquad[0] == 1 && lquad[1] == 1 && 
	     lquad[2] == 1 && lquad[3] == 1 ) {
	    el->hdr.range_min_lon = -180.F;
	    el->hdr.range_max_lon =  180.F;
	    if ( outx[0] >= 0.0F ) {
	        el->hdr.range_min_lat =   0.F;
	        el->hdr.range_max_lat =  90.F;
	    } 
	    else {
	        el->hdr.range_min_lat = -90.F;
	        el->hdr.range_max_lat =   0.F;
	    }
        }
		
	el->elem.cir.info.numpts = nn;
               
	for (ii=0; ii < nn; ii++) {
	    el->elem.cir.data.latlon[ii] = outx[ii];
	    el->elem.cir.data.latlon[ii+nn] = outy[ii];
	}
	break;
	
      /*
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       *     WATCHBOX, TRACKS, SIGMETS CLASS ELEMENTS 
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       */
      case  CLASS_WATCHES:
      case  CLASS_TRACKS:
      case  CLASS_SIGMETS:
      
        if ( el->hdr.vg_class == CLASS_WATCHES ) {
            if ( el->elem.wbx.info.w_type == TRWWTCH || 
	         el->elem.wbx.info.w_type == TORWTCH ||
	         el->elem.wbx.info.w_type == 0 ||
	         el->elem.wbx.info.w_type == UNDWTCH ) { ; }
            else {
	          *iret = -27;
	          return;
	    }
        }
        
	if ( np == 0 ) {
	    if ( el->hdr.vg_class == CLASS_WATCHES ) {
	        nn = el->elem.wbx.info.numpts;
	        platx = el->elem.wbx.latlon;
	        plony = &(el->elem.wbx.latlon[nn]);
            }
	    else if ( el->hdr.vg_class == CLASS_TRACKS ) {
	        nn = el->elem.trk.info.npts;
	        platx = el->elem.trk.latlon;
	        plony = &(el->elem.trk.latlon[nn]);	    
	    }
	    else {
	        if ( el->hdr.vg_type == SIGCCF_ELM ) {
	    	    nn = el->elem.ccf.info.npts;
		    platx = el->elem.ccf.latlon;
		    plony = &(el->elem.ccf.latlon[nn]);		
		}
	        else if ( el->hdr.vg_type == ASHCLD_ELM ) {
	    	    nn = el->elem.ash.info.npts;
		    platx = el->elem.ash.latlon;
		    plony = &(el->elem.ash.latlon[nn]);		
		}
		else {
		    nn = el->elem.sig.info.npts;
		    platx = el->elem.sig.latlon;
		    plony = &(el->elem.sig.latlon[nn]);		
		}		
	    }
	}
	
	if ( !samesys ) {  /* Transfer coordinates */
           gtrans (system, sys_M, &nn, platx, plony, tmplat, tmplon, 
		   &ier, strlen(system), strlen(sys_M));
        }
	else {
	   tmplon = plony;
	   tmplat = platx;	   
	}

	if ( nn >= MAXPTS ) {
	    *iret = -28;
	    return;
	}
       
	if ( el->hdr.vg_class == CLASS_WATCHES ) {
	    el->elem.wbx.info.numpts = nn;	
	    recsize = ( sizeof(float) * 2 * nn + sizeof( VG_HdrStruct ) 
	              + sizeof( WatchBoxInfo ) );	
	    
	    for ( ii = 0; ii < nn; ii++ ) {
	         el->elem.wbx.latlon[ii] = tmplat[ii];
                 el->elem.wbx.latlon[ii + nn] = tmplon[ii];
	    }
	}    
        else if ( el->hdr.vg_class == CLASS_TRACKS ) {
	    el->elem.trk.info.npts= nn;
            recsize = (sizeof (VG_HdrStruct) + sizeof (TrackType) );
            
	    for ( ii = 0, jj = nn; ii < nn; ii++, jj++) {
                el->elem.trk.latlon[ii] = tmplat[ii];
	        el->elem.trk.latlon[jj] = tmplon[ii];
           }	
	}
	else {
	    if ( el->hdr.vg_type == SIGCCF_ELM) {
                el->elem.ccf.info.npts= nn;
                recsize= (sizeof (VG_HdrStruct) + sizeof (CCFType));
                
		for ( ii = 0, jj = nn; ii < nn; ii++, jj++) {
                    el->elem.ccf.latlon[ii] = tmplat[ii];
	            el->elem.ccf.latlon[jj] = tmplon[ii];	    
                }            
	    }
	    else if ( el->hdr.vg_type == VOLC_ELM) {
                recsize= ( sizeof (VG_HdrStruct) + sizeof (VolInfo) +
                           sizeof( float ) * 2 + sizeof( int ) * 2   );
                
		for ( ii = 0, jj = nn; ii < nn; ii++, jj++) {
                    el->elem.vol.latlon[ii] = tmplat[ii];
	            el->elem.vol.latlon[jj] = tmplon[ii];   
                }            
	    }
	    else if ( el->hdr.vg_type == ASHCLD_ELM) {
                el->elem.ash.info.npts= nn;
                recsize= (sizeof (VG_HdrStruct) + 
			  sizeof (float) * nn * 2 + sizeof (SptxType) +
			  sizeof (AshInfo));
                
		for ( ii = 0, jj = nn; ii < nn; ii++, jj++) {
                    el->elem.ash.latlon[ii] = tmplat[ii];
	            el->elem.ash.latlon[jj] = tmplon[ii];	    
                }            
	    }
	    else {
                el->elem.sig.info.npts= nn;
                recsize = ( sizeof (VG_HdrStruct) +
		            sizeof (float) * nn * 2 + sizeof (SigmetInfo));
                
		for ( ii = 0, jj = nn; ii < nn; ii++, jj++) {
                    el->elem.sig.latlon[ii] = tmplat[ii];
	            el->elem.sig.latlon[jj] = tmplon[ii];	    
                }	    
	    }	
	}

        /*
	 *  Find the ranges of latitude and longitude.
	 */	
	el->hdr.range_min_lat = tmplat[0];
	el->hdr.range_min_lon = tmplon[0];
	el->hdr.range_max_lat = tmplat[0];
	el->hdr.range_max_lon = tmplon[0];
	
	for ( ii = 0; ii < nn; ii++ ) {            
	    if ( el->hdr.range_min_lon > tmplon[ii] )
	        el->hdr.range_min_lon = tmplon[ii];
	    if ( el->hdr.range_min_lat > tmplat[ii] )
	        el->hdr.range_min_lat = tmplat[ii];
	    if ( el->hdr.range_max_lon > tmplon[ii] )
	        el->hdr.range_max_lon = tmplon[ii];
	    if ( el->hdr.range_max_lat > tmplat[ii] )
	        el->hdr.range_max_lat = tmplat[ii];	    
	}
	     
	break;

      /*
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       *     LIST CLASS ELEMENTS 
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       */
      case CLASS_LIST:		
	
	/*
	 *  Retrieve the FIPS/WFO codes and centroids.
	 */
        if ( el->elem.lst.data.nitems > 0 ) {
            pglist_setCurList ( el );
        }

        cur_list = pglist_getCurList ();	
	el->elem.lst.data = cur_list->elem.lst.data;
	
	recsize = ( sizeof(VG_HdrStruct) + 
                    sizeof(ListInfo) + sizeof(ListData) );
	
	np =  cur_list->elem.lst.data.nitems;
	
	/*
	 *  Find the max. & min. range.
	 */
        el->hdr.range_min_lon = el->elem.lst.data.lat[0];
        el->hdr.range_min_lat = el->elem.lst.data.lat[0];
        el->hdr.range_max_lon = el->elem.lst.data.lon[0];
        el->hdr.range_max_lat = el->elem.lst.data.lat[0];
	
	for ( ii = 0; ii < np; ii++ ) {
            if ( el->hdr.range_min_lon > el->elem.lst.data.lon[ii] )
                el->hdr.range_min_lon = el->elem.lst.data.lon[ii];
            if ( el->hdr.range_min_lat > el->elem.lst.data.lat[ii] )
                el->hdr.range_min_lat = el->elem.lst.data.lat[ii];
            if ( el->hdr.range_max_lon < el->elem.lst.data.lon[ii] )
                el->hdr.range_max_lon = el->elem.lst.data.lon[ii];
            if ( el->hdr.range_max_lat < el->elem.lst.data.lat[ii] )
                el->hdr.range_max_lat = el->elem.lst.data.lat[ii];
        }    
		        
	break;	

      /*
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       *     MET CLASS ELEMENTS 
       * ++++++++++++++++++++++++++++++++++++++++++++++++++++++
       */
      case  CLASS_MET:	
	if ( np == 0 ) {
	    if ( el->hdr.vg_type == JET_ELM ) {
	        nn = el->elem.jet.line.spl.info.numpts;
	        platx = el->elem.jet.line.spl.latlon;
	        plony = &(el->elem.jet.line.spl.latlon[nn]);
	    }
	    else if ( el->hdr.vg_type == GFA_ELM )  { 
		nn = el->elem.gfa.info.npts;
	        platx = el->elem.gfa.latlon;
	        plony = &(el->elem.gfa.latlon[nn]);
	    }	
	} 
        
	if ( !samesys ) {
            gtrans ( system, sys_M, &nn, platx, plony, tmplat, tmplon, 
		   &ier, strlen(system), strlen(sys_M) );
        }
	else {
	    tmplon = plony;
	    tmplat = platx;	   
	}

        if ( nn >= MAXPTS ) {
	    *iret = -28;
	    return;
	}
	
	if ( el->hdr.vg_type == JET_ELM ) {
	    el->elem.jet.line.spl.info.numpts = nn;
	    recsize = (int) ( sizeof(VG_HdrStruct) + sizeof(JetType) );
	    
	    for ( ii = 0; ii < nn; ii++ ) {
	        el->elem.jet.line.spl.latlon[ii]    = tmplat[ii];
	        el->elem.jet.line.spl.latlon[ii+nn] = tmplon[ii];
	    }
	    
	    /*
	     *  Snap barbs/hashs on jet line before saving it.
	     */
	    if ( el->hdr.vg_type == JET_ELM ) {
	        cvg_snapjet ( el, el, &ier );
	    }	    	    
	}
	else if ( el->hdr.vg_type == GFA_ELM ) {
	    el->elem.gfa.info.npts = nn;
	    recsize = (int) ( sizeof(VG_HdrStruct) + sizeof(int) * 2
	         + (sizeof(char) * el->elem.gfa.info.nblocks * STD_STRLEN)
    		 + (sizeof(float) * (size_t)(2 * nn) ) );
            	    
	    for ( ii = 0; ii < nn; ii++ ) {
	        el->elem.gfa.latlon[ii]    = tmplat[ii];
	        el->elem.gfa.latlon[ii+nn] = tmplon[ii];
	    }
	}
	else if ( el->hdr.vg_type == TCA_ELM ) {
            recsize = (int) ( sizeof(VG_HdrStruct) + 
		      cvg_loadTcaFileInfo(el->elem.tca.info, &ptca,  &ier));
	    free( ptca );
	}
		
	
	/*
	 *  Find the max. & min. range.
	 */
	el->hdr.range_min_lon = tmplon[0];
	el->hdr.range_min_lat = tmplat[0];
	el->hdr.range_max_lon = tmplon[0];
	el->hdr.range_max_lat = tmplat[0];
	
	for ( ii = 0; ii < nn; ii++ ) {
	    	    
	    if ( el->hdr.range_min_lon > tmplon[ii] )
	   	    el->hdr.range_min_lon = tmplon[ii];
	    if ( el->hdr.range_min_lat > tmplat[ii] )
		    el->hdr.range_min_lat = tmplat[ii];
	    if ( el->hdr.range_max_lon <  tmplon[ii] )
		    el->hdr.range_max_lon = tmplon[ii];
	    if ( el->hdr.range_max_lat < tmplat[ii] )
		    el->hdr.range_max_lat = tmplat[ii];
	}
     	   	
	break;
		
    }

    /*
     *  Save VG elements.
     */
    if ( !saved ) {
	el->hdr.recsz = recsize;
	cvg_writef( el, start, el->hdr.recsz, filnam, TRUE, location, &ier );
	if( ier >= 0 ) {
	    saved = TRUE;
	}
    }
    
    if ( ier < 0 ) *iret = ier;
    
    /* 
     *  Set file_saved & changes_made flags on current layer.
     */
    if ( saved ) {        
	pglayer_setChngMade ( cur_layer, TRUE );
        
	if ( ( filnam != (char *)NULL ) &&
	     ( strcmp(filnam, cvg_getworkfile()) != 0 ) ) {
	    pglayer_setFileSaved ( cur_layer, TRUE );	
	}
    }
    
    /* 
     *  Clean up.
     */
    if ( !samesys ) {
    	free ( tmplat );
    	free ( tmplon );
    }

    /*
     * If requested, update placement information.  Its not always a good
     * idea since some callers have 'batches' of updates to save, and they
     * would be better off waiting till all the updates were done before
     * having placement updated
     */
    if (saved && updatepl && cvg_plenabled()) {
        /*
         * Mark elements in placement that are effected by
         * the new element, and get the area of influence back
         */
        cvg_checkplace(el, 0, *location, &found, inf_bbox, &ier);
        if (found > 0) {
            /*
             * Update the area effected
             */
            xpgpaste(inf_bbox[0], inf_bbox[2], inf_bbox[1], inf_bbox[3], &ier);

            cvg_rfrsh(NULL, inf_bbox[0], inf_bbox[2], 
                        inf_bbox[1], inf_bbox[3], &ier
                    );

	    /* 
             * since we and could impact everything in the area,
             * recreate the range records, callers that do a 'group'
             * of items should handle the placement update on their
             * own and reconstruct the crg at the end of the group
             * processing
             */
            crg_rebuild();
        } else {
            /*
             * This isn't apparently going to bother anyone else
             * so just update placement.
             */
            cvg_updateplace(FALSE, &ier);
        }

        /*
         * Re-read the VG element since placement may have 
         * adjusted it
         */

        /*
         * Free GFA/TCA break point memory, we will be putting it back
         */
        if ( el->hdr.vg_type == TCA_ELM ) {
            cvg_freeBkpts ( el );
        }
        else if ( el->hdr.vg_type == GFA_ELM ) {
            cvg_freeElPtr ( el );
        }

        /*
         * Get the updated record so CRG can use it later
         */
        if (!filnam) {
            cvg_rdrec ( cvg_getworkfile(), *location, el, &ier );
        } else {
            cvg_rdrec ( filnam, *location, el, &ier );
        }
    }

    xpgrfrsh();
    
    /*
     *  Build range record for the saved element.
     *  Warning: pgvgf_saveNewElm() is used in a loop for multi-selected
     *           elements. Use crg_rebuild() here instead of crg_set()
     *	         will severely slow down the performance since crg_rebuild()
     *           rebuilds the range record for every element in WORK_FILE. 
     */
    if ( saved )  {
        crg_set ( el, *location, cur_layer, &ier);
    }

}

/*=====================================================================*/

void pgvgf_dsp ( int location, int *iret )
/************************************************************************
 * pgvgf_dsp								*
 *									*
 * This function redisplay the element just created/stored by reading   *
 * the record, setting the range, and displaying.			*
 *									*
 * pgvgf_dsp ( location, iret )						*
 *									*
 * Input parameters:							*
 *	location	int	location of the new elem in file	*
 *									*
 * Output parameters:							*
 *	*iret		int	Return code				*
 *									*
 **									*
 * Log:									*
 * C. Lin/EAI		04/98						*
 * F. J. Yen/NCEP	05/98	Changed cds_dspvg to cds_dspelm		*
 * W. LI/EAI		02/99	Added call to pghdlb_select()		*
 * G. Krueger/EAI	05/99	Removed invalid CVG_RDREC parameter	*
 * J. Wu/SAIC		12/01	add layer in crg_set() call		*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * J. Wu/SAIC		10/04	free GFA block memory			*
 ***********************************************************************/
{
    int		layer, ier;
    VG_DBStruct	newel;
/*---------------------------------------------------------------------*/

    *iret = 0;

    cvg_rdrec ( cvg_getworkfile(), location, &newel, &ier );
    layer = pglayer_getCurLayer( );
    crg_set(&newel, location, layer, &ier);
    cds_dspelm ( &newel, &ier );

    if (pglabel_getLabFlag()){  /* if label being selected */
	pghdlb_deselectAll();
	pghdlb_select(&newel, location);
	mbotw_mouseSet(LMHINT_NEXT, MMHINT_LABEL);
    }
    
    /*
     * Free TCA break point memory
     */
    if ( newel.hdr.vg_type == TCA_ELM ) {
        cvg_freeBkpts ( &newel );
    }
    else if ( newel.hdr.vg_type == GFA_ELM ) {
        cvg_freeElPtr ( &newel );
    }
    
}

/*=====================================================================*/
