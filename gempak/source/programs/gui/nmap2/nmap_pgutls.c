#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "pgcmn.h"
#include "proto_xw.h"

/*
 *  Private functions
 */
static void pgutls_vrfyNumeric ( Widget text_w, XtPointer clnt, 
			         XtPointer call, Boolean ngt_ok, 
			         Boolean flt_ok, Boolean blnk_ok,
				 Boolean slash_ok ); 
/************************************************************************
 * nmap_pgutls.c							*
 *									*
 * This module contains the utility functions for product generation.	*
 *									*
 * CONTENTS:								*
 * pgutls_prepNew()		preparation for the drawing event	*
 * pgutls_redraw()		redraw					*
 * pgutls_nextIndex()		utility to return the next array index	*
 * pgutls_getSmoothPts()	utility to load intermediate points	*
 * pgutls_getSmoothedPt()	locates nearest point on smoothed line	*
 * pgutls_fReverseArray()	reverse elements in a float array	*
 * pgutls_isComsym()            true if a Comsym is selected            *
 * pgutls_isNumber()            true if is a number selected            *
 * pgutls_createOptionMenu()	creates an option menu			*
 * pgutls_optPbCb()		callback for the option buttons		*
 * pgutls_setOptMenu()		sets the current option menu item	*
 * pgutls_refresh()		selective area refresh			*
 * pgutls_checkNumDataCb()	Check for only numeric data in text wid *
 * pgutls_initHdr()		Initialize a vg hdr record		*
 * pgutls_regroup()		Move elems from group to group		*
 * pgutls_vrfyNumeric()		verifies numeric input to a text widget *
 * pgutls_vrfyPosFltCb()	verifies input is a positive float num.	*
 * pgutls_vrfyPosIntCb()	verifies input is a positive integer	*
 * pgutls_vrfyPosIntBlkCb()	verifies input is pos. int. with blanks	*
 * pgutls_vrfyUpperCaseCb()	verifies input is upper case           	*
 * pgutls_vrfyNoPunctCb()	verifies input contains no punctuation 	*
 * pgutls_vrfyNoDigitCb()	verifies input contains no digits 	*
 * pgutls_vrfyLmtPunctCb()	verifies input with non-punct, '.' & '-'*
 ***********************************************************************/

/*=====================================================================*/

void pgutls_prepNew ( int location, VG_DBStruct *el, float *llx, 
			float *lly, float *urx, float *ury, int *iret )
/************************************************************************
 * pgutls_prepNew							*
 *									*
 * This function prepares event handlers and the vgf for the update to	*
 * the VGF file.  Specifically, it determines which element is to be	*
 * updated, refreshes the pixmaps behind the old element after		*
 * getting the range of the element, then reads the old element to be	*
 * used as a template, and deletes the old element                      *
 *									*
 * void pgutls_prepNew (location, el, llx, lly, urx, ury, iret)		*
 *									*
 * Input parameters:							*
 *	location	int		offset (-1 to use current)	*
 *									*
 * Output parameters:							*
 *	*el		VG_DBStruct	the element			*
 *	*llx		float		lower left X of the element	*
 *	*lly		float		lower left Y of the element	*
 *	*urx		float		upper right X of the element	*
 *	*ury		float		upper right Y of the element	*
 *	*iret		int		return code			*
 *					  -1 = invalid location		*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 9/97	Created					*
 * C. Lin/EAi		10/97	rename from drw_prepnew, cleanup	*
 * C. Lin/EAi		10/97	add refresh under old element		*
 * C. Lin/EAi		11/97	add call to crg_clear			*
 * S. Law/GSC		04/98	add call to pghdlb_deselectEl		*
 * S. Law/GSC		05/98	added check for pghot_getElmLoc		*
 * E. Safford/GSC	06/98	updated pghdlb_deselectEl call		*
 * E. Safford/GSC	06/98	added call to crg_grfrsh		*
 * E. Safford/GSC	12/98	limit area of refresh			*
 * S. Law/GSC		03/00	added location parameter and clean up	*
 * H. Zeng/EAI          11/00   changed cvg_rdrec() parameters          *
 * J. Wu/SAIC           01/02   add layer param in crg_get() call       *
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvg_getworkfile			*
 * J. Wu/SAIC		07/04	add filter param. to crg_get()		*
 * J. Wu/SAIC		12/04	move crg_clear after crg_get for refresh*
 * S. Danz/AWC		07/06	Added new cvg_delet placement argument	*
 * J. Wu/SAIC		04/08	refresh GFA completely			*
 ***********************************************************************/
{
    int		num, ier, el_layer, found;
    float	inf_bbox[4], llx1, lly1, urx1, ury1;
    Boolean	secRangeExist;
    filter_t	filter;
/*---------------------------------------------------------------------*/

    *iret = 0;

/*
 * before deleting the old element, look in the range array for
 * the matching file offset as the element to be deleted.  This
 * tells us which range record in the range array has to be updated.
 */
    if (location == -1) {
	location = pgactv_getElmLoc();
	if (location == -1) {
	    *iret = -1;
	    return;
	}
    }

/*
 * See if the item was placed or effects placement as that
 * could increase the size of the area effected
 */
    cvg_rdrec(cvg_getworkfile(), location, el, &ier);
    cvg_checkplace(el, 0, location, &found, inf_bbox, &ier);
    cvg_delet(cvg_getworkfile(), location, TRUE, &ier);

    crg_getinx(location, &num, &ier);
    if (ier == 0) {
	pghdlb_deselectEl (num, TRUE);

        crg_get( num, &el_layer, filter, llx, lly, urx, ury, &ier);
	
	secRangeExist = False;
	if ( ier == 0 ) {
	    
	    crg_getsecrange( num, &llx1, &lly1, &urx1, &ury1, &ier );            
	    if ( ier == 0 ) {
		llx1 = G_MIN( *llx, llx1 );
                lly1 = G_MIN( *lly, lly1 );
                urx1 = G_MAX( *urx, urx1 );
                ury1 = G_MAX( *ury, ury1 );
                
		secRangeExist = True;
	    }
	}
	
	crg_clear(num, &ier);

/*
 * Update the refresh extent if the area impacted by
 * placement was bigger than the area from CRG
 * 
 * Also update the refresh extent impacted by the union of both 
 * both range boxes in some GFA elements.
 */     
        if (found > 0) {
            *llx = G_MIN(*llx, inf_bbox[0]);
            *lly = G_MIN(*lly, inf_bbox[2]);
            *urx = G_MAX(*urx, inf_bbox[1]);
            *ury = G_MAX(*ury, inf_bbox[3]);
        }

	if ( ier == 0 ) {
	    
	    if ( secRangeExist ) {
		llx1 = G_MIN( *llx, llx1 );
                lly1 = G_MIN( *lly, lly1 );
                urx1 = G_MAX( *urx, urx1 );
                ury1 = G_MAX( *ury, ury1 );
	        
		pgutls_refresh( llx1, lly1, urx1, ury1, &ier);
	    }
	    else {
	        pgutls_refresh( *llx, *lly, *urx, *ury, &ier);
	    }
	    
	               

/*
 * If we may have impacted other stuff with placement
 * we will need to rebuild the range records
 */
            if (found) {
                crg_rebuild();
            }
	}
    }
}

/*=====================================================================*/

void pgutls_redraw ( int location, VG_DBStruct *el, int *iret )
/************************************************************************
 * pgutls_redraw                                     			*
 *                                                                      *
 * This function handles the redisplay of the screen after an element   *
 * has been stored.                                                     *
 *                                                                      *
 * void pgutls_redraw( location, el, iret )                             *
 *                                                                      *
 * Input parameters:                                                    *
 *  location    int     Location of element being edited in the file    *
 *  *el          VG_DBStruct     Element being edited                   *
 *                                                                      *
 * Output parameters:                                                   *
 *  *iret                int     Error/return code                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * E. Wehner/Eai        09/97   Created                                 *
 * C. Lin/EAi           10/97   rename from drw_redisp, cleanup         *
 * C. Lin/EAi           01/98   remove extra cvg_dspvg                  *
 * F. Yen/NCEP          01/98   Updated calls for crg library cleanup   *
 * E. Safford/GSC       04/98   add param to hdlb_select calls          *
 * H. Zeng/EAI          11/00   changed cvg_rdrec() parameters          *
 * J. Wu/SAIC		12/01	add layer in crg_set() call		*
 * J. Wu/SAIC		01/02	add layer in crg_get() call		*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * J. Wu/SAIC		07/04	add filter param. to crg_get()		*
 * J. Wu/SAIC		04/08	refresh GFA completely			*
 ***********************************************************************/
{
    int		num, ier, layer, el_layer;
    float	llx, lly, urx, ury, llx1, lly1, urx1, ury1;
    filter_t	filter;
/*---------------------------------------------------------------------*/

    *iret = 0;

/*
 * REDISPLAY -- Redisplay the newly created/stored element
 *              by reading the record, setting the range, and displaying
 */
    cvg_rdrec( cvg_getworkfile(), location, el, &ier );
    layer = pglayer_getCurLayer( );
    crg_set( el, location, layer, &ier);

/*
 * Put handlebars back on the element
 */
    pghdlb_select(el, location);

    pgactv_setActvElm(el, location);

    crg_getinx(location, &num, &ier);
    crg_get(num, &el_layer, filter, &llx, &lly, &urx, &ury, &ier);
        
/*
 * Refresh the whole area covered by the union of both range boxes.
 */
    crg_getsecrange( num, &llx1, &lly1, &urx1, &ury1, &ier );
    if ( ier == 0 ) {
        llx = G_MIN( llx, llx1 );
        lly = G_MIN( lly, lly1 );
        urx = G_MAX( urx, urx1 );
        ury = G_MAX( ury, ury1 );
    }
    
    cvg_rfrsh( NULL, llx, lly, urx, ury, &ier );       

}

/*=====================================================================*/

int pgutls_nextIndex ( int num_index, int cur_index, Boolean direction )
/************************************************************************
 * pgutls_nextIndex							*
 *									*
 * Utility for locating the next/previous array index assuming a	*
 * circular linking of head to tail.					*
 *									*
 * int pgutls_nextIndex (num_index, cur_index, direction)		*
 *									*
 * Input parameters:							*
 *	num_index	int	Total number of elements in the array	*
 *	cur_index	int	Current element selected in the array	*
 *	direction	Boolean	NEXT = forward, PREV = backward		*
 *									*
 * Ouput parameters:							*
 * pgutls_nextIndex	int	next/previous array element		*
 *									*
 **									*
 * Log:									*
 * E. Safford/GSC	11/97	Initial coding				*
 * S. Law/GSC		02/99	changed direction to a Boolean		*
 ***********************************************************************/
{
    if (direction == NEXT) {
        if (cur_index == (num_index - 1))
            return 0;
        else
            return (cur_index + 1);
    }
    else {	/* PREV */
        if (cur_index == 0)
            return (num_index - 1);
        else
            return (cur_index - 1);
    }
}

/*=====================================================================*/

void pgutls_getSmoothPts ( int np, float *x_pts, float *y_pts, 
				int smooth_lvl, int max_pts, int i_trnc, 
				int f_trnc, int *smooth_pts, 
				float *smooth_x, float *smooth_y, int *iret ) 
/************************************************************************
 * pgutls_getSmoothPts                                                  *
 *                                                                      *
 * Utility for retrieving an array of intermeditate or smoothed points  *
 * for a given line.							*
 *                                                                      *
 * void pgutls_getSmoothPts ( np, x_pts, y_pts, smooth_lvl, max_pts,    *
 *				i_trnc, f_trnc, smooth_pts, smooth_x, 	*
 *						      smooth_y, iret )  *
 *                                                                      *
 * Input parameters:                                                    *
 *  np		int	number of points in the given line		*
 *  *x_pts	float	array of x coordinate pts			*
 *  *y_pts	float  array of y coordinate pts			*
 *  smooth_lvl  int	smoothing level of line				*
 *  max_pts	int	the maximum number of intermediate points       *
 *  i_trnc	int	initial truncation point			*
 *  f_trnc	int	final truncation point 				*
 *                                                                      *
 * Ouput parameters:                                                    *
 *  *smooth_pts int	the actual number of intermediate pts returned  *
 *  *smooth_x	float	array of x coordinate intermediate pts		*
 *  *smooth_y	float	array of y coordinate intermediate pts		*
 *  *iret	int	return code					*
 *			  0 = normal					*
 *			  2 = max_pts exceeded, returning original pts  *
 *			  4 = too few points to calculate smoothing     *
 *			 -2 = invalid smooth value			*
 **                                                                     *
 * Log:                                                                 *
 * E. Safford/GSC  07/98        Initial coding                          *
 * E. Safford/GSC  08/98 	add truncation points			*
 ***********************************************************************/
{
int	ii, ier;
float	smooth_val, crvscl;
/*---------------------------------------------------------------------*/

    *iret = 0;

    if (smooth_lvl < 1)
	*iret = -2;
    else if (np < 3)
	*iret = 4;

    if ( *iret != 0 ) {		/* load existing pts into return arrays */
	for (ii=0; ii < np; ii++) {
	    smooth_x[ii] = x_pts[ii];
	    smooth_y[ii] = y_pts[ii];
	}
	*smooth_pts = np;
    }	
    else {

/*
 *  Set smooth_val according to smooth_lvl and 
 *  crvscl (device curve scaling factor) for xw drawing
 */
        if (smooth_lvl == 1 )
	    smooth_val = 1.0F;
        else 
	    smooth_val = 5.0F;

	gqcvsc (&crvscl, &ier);

/*
 * cv_prmt is a fortran routine and the index must be incremented
 */
	if (i_trnc) 
	    i_trnc++;
	if (f_trnc) 
	    f_trnc++;

        cv_prmt (&np, x_pts, y_pts, &smooth_val, &max_pts, &crvscl, 
	            &i_trnc, &f_trnc, smooth_pts, smooth_x, smooth_y, iret);

    }
}

/*=====================================================================*/

void pgutls_getSmoothedPt ( float xx, float yy, int start, int end, 
				Boolean closed, int smooth_lvl,
			   float *smooth_x, float *smooth_y, int *iret )
/************************************************************************
 * pgutls_getSmoothedPt							*
 *									*
 * Routine for locating a point on the loaded smoothed line		*
 *									*
 * void pgutls_getSmoothedPt ( xx, yy, start, end, closed, smooth_lvl,	*
 *					smooth_x, smooth_y, iret )	*
 *									*
 * Input parameters:							*
 *	xx		float	x coordinate of button press		*
 *	yy		float	y coordinate of button press		*
 *	start		int	start of located segment		*
 *	end		int	end of located segment			*
 *	closed		Boolean	whether the line is closed		*
 *	smooth_lvl	int	smoothing level				*
 *									*
 * Output parameters:							*
 *      *smooth_x	float	x coordinate of smoothed pt		*
 *      *smooth_y	float	y coordinate of smoothed pt		*
 *	*iret		int	return code				*
 *				   0 = normal				*
 *				   1 = xx, yy point beyond tie-in dist  *
 *				  -1 = error				*
 **									*
 * Log:									*
 *  E. Safford/GSC	08/98	initial coding				*
 *  E. Safford/GSC	09/98	add truncation points			*
 *  S. Law/GSC		02/99	moved from _pgpdel_getSmoothedPt	*
 *  E. Safford/GSC	04/99   closed lines don't have closed pt added *
 *				  prior to call to this routine		*
 *  E. Safford/GSC	02/01	added iret & return code checks		*
 *  J. Wu/SAIC		09/01	add parentheses around && within ||	*
 *  W.D.Plummer/NCEP	12/02	chg call sequence of cgr_segdist	*
 ***********************************************************************/
{
    float	dx[MAXGHOST], dy[MAXGHOST], x_seg[6], y_seg[6];
    float	dist, *dcx, *dcy;
    int		start_pt, end_pt, ii, idx, ier, smooth_pts, dcn;
    int		dummy1, dummy2, i_trnc, f_trnc;
/*---------------------------------------------------------------------*/


    *iret = ier = 0;
    pgactv_getDevPts (&dcn, &dcx, &dcy);

    if (dcn < 6) {			/* ghost entire line */

/*
 *  add in the closed point if necessary
 */
	if (closed) {
	    dcn++;
	    dcx[dcn-1] = dcx[0];
	    dcy[dcn-1] = dcy[0];
	}
        pgutls_getSmoothPts (dcn, dcx, dcy, smooth_lvl, 
    		MAXGHOST, 0, 0, &smooth_pts, dx, dy, &ier);
    }
    else if (!closed) {			/* non-closed lines */

	start_pt = (start > 2)?  start - 2 : 0;
        end_pt   = (end < dcn - 1) ?  end + 2 : dcn - 1;

        for (ii=0, idx = start_pt; idx <= end_pt; ii++, idx++) {
            x_seg[ii] = *(dcx + idx); 
	    y_seg[ii] = *(dcy + idx); 

	    if (idx == start) 
	        i_trnc = ii;
	    else if (idx == end)
		f_trnc = ii;
	}

  	pgutls_getSmoothPts (ii, x_seg, y_seg, smooth_lvl, 
	 	MAXGHOST, i_trnc, f_trnc, &smooth_pts, dx, dy, &ier);
    }	
    else {				/* closed lines */

	if ((start == 0 && end == dcn-1) || (start == dcn-1 && end == 0)) {

    	    /*  
	     *  start and end are the two end points -- load backwards from 1
	     */
            for (ii=0, idx = 2; ii < 7; ii++, 
    	          idx = pgutls_nextIndex (dcn, idx, PREV)) {
	        x_seg[ii] = *(dcx + idx); 
	        y_seg[ii] = *(dcy + idx); 
	    }
	}
	else {

/*
 *  Load the x/y_seg arrays in two pieces starting at start and
 *  start + 1.  Note traversal direction is backwards for the first
 *  half, then forwards for the second.
 */
            for (ii=2, idx = start; ii >= 0; ii--, 
    	          	idx = pgutls_nextIndex (dcn, idx, PREV)) {
	        x_seg[ii] = *(dcx + idx); 
	        y_seg[ii] = *(dcy + idx); 
	    }

	    idx = pgutls_nextIndex (dcn, start, NEXT);
	    for (ii=3; ii<6; ii++, 
	    	  	idx = pgutls_nextIndex (dcn, idx, NEXT)) {
	        x_seg[ii] = *(dcx + idx); 
	        y_seg[ii] = *(dcy + idx); 
	    }

	}

  	pgutls_getSmoothPts (ii, x_seg, y_seg, smooth_lvl, 
	    	MAXGHOST, 0, 0, &smooth_pts, dx, dy, &ier);
    }

    if ( ier < 0 ) {
        *iret = -1;
	return;
    }

    cgr_segdist (&smooth_pts, dx, dy, &xx, &yy, &dist,
 	               &dummy1, &dummy2, smooth_x, smooth_y, &ier);
    if ( ier < 0 ) {
	*iret = -1;
    }
    else if ( dist > LINE_TIEIN ) {
	*iret = 1;
    }

}

/*=====================================================================*/

void pgutls_fReverseArray ( int np, float *farray_x, float *farray_y )
/************************************************************************
 * pgutls_fReverseArray							*
 *									*
 * This function reverse the order of the information stored in an	*
 * array of floats							*
 *									*
 * void pgutls_fReverseArray (np, farray_x, farray_y)			*
 *									*
 * Input parameters:							*
 *	np		int	number of points in the array		*
 *	*farray_x	float	the array of x points			*
 *	*farray_y	float	the array of y points			*
 *									*
 * Ouput parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		09/98	Initial coding				*
 ***********************************************************************/
{
    int		ii, jj, half;
    float	tmp;
/*---------------------------------------------------------------------*/
    half =  np / 2;
    jj = np - 1;
    for (ii = 0; ii < half; ii++, jj--) {
	tmp              = *(farray_x + ii);
	*(farray_x + ii) = *(farray_x + jj);
	*(farray_x + jj) = tmp;

	tmp              = *(farray_y + ii);
	*(farray_y + ii) = *(farray_y + jj);
	*(farray_y + jj) = tmp;
    }
}

/*=====================================================================*/

Boolean pgutls_isComsym ( void )
/************************************************************************
 * pgutls_isComsym                                                   	*
 *                                                                      *
 * This function checks whether combo-symbol is included in the        	*
 * selected elements.                         				*
 *                                                                      *
 * Boolean pgutls_isComsym()                                            *
 *                                                                      *
 * Input parameters:                                                    *
 * Output parameters:                                                   *
 * pgutls_isComsym	Boolean	is combo-symbol included		*
 *									*
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      	9/98                                            *
 * E. Safford/GSC	09/98	moved from nmap_pggrpw.c		*
 ***********************************************************************/
{
int     i, n, gnum, ier;
char    sel, gtyp;
Boolean found;
/*---------------------------------------------------------------------*/

    found = False;

    i = 0;
    n = pghdlb_elemSelected();

    while (n && (i < MAX_EDITABLE_ELEMS)) {

        crg_gsel(i, &sel, &ier);

	if (sel) {

	    crg_ggrp(i, &gtyp, &gnum, &ier);

	    if (gtyp == GRPTYP_COMSYM) {
	        found = True;
		break;
            }
            n--;
        }
        i++;
    }
    return (found);
}

/*=====================================================================*/

Boolean pgutls_isNumber ( char *string )
/************************************************************************
 * pgutls_isNumber                                                   	*
 *                                                                      *
 * This function checks whether a character is a number	        	*
 *                                                                      *
 * Boolean pgutls_isNumber ( string )                                   *
 *                                                                      *
 * Input parameters:                                                    *
 *	*string		char	string					*
 *									*
 * Output parameters:                                                   *
 * pgutls_isNumber	Boolean is character a number			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * W. Li/EAI		12/98						*
 * E. Safford/GSC	12/98	use cst_numb to check for valid number  *
 * W. Li/EAI		01/99	added string length check		*
 ***********************************************************************/
{
int	conv, ier;

/*---------------------------------------------------------------------*/
    if (strlen(string) < (size_t)12){ /*cst_numb only handle a string < 12 */
	cst_numb (string, &conv, &ier);
	if (ier == 0)
	    return TRUE;
	else
	    return FALSE;
    }
    else
    return FALSE;
}

/*=====================================================================*/

void pgutls_createOptionMenu ( Widget parent, int nbuttons,
				XtPointer pstart_item, String label_str, 
				XtCallbackProc callback, Widget *form, 
				Widget *label, Widget *menu, Widget pbs[],
				char *btnstrs[] )
/************************************************************************
 * pgutls_createOptionMenu						*
 *									*
 * This function creates an option menu.				*
 *									*
 * void pgutls_createOptionMenu (parent, nbuttons, pstart_item,		*
 *				 label_str, callback, form, label,	*
 *				 menu, pbs, btnstrs)			*
 *									*
 * Input parameters:							*
 *	parent		Widget	the parent widget			*
 *	nbuttons	int	number of menu buttons			*
 *	pstart_item	XtPointer  pointer to starting menu item	*
 *	label_str	String	label string for label			*
 *									*
 * Ouput parameters:							*
 *	callback	XtCallbackPro	push button callback function	*
 *	*form		Widget	the underlying form			*
 *	*label		Widget	the side label				*
 *	*menu		Widget	the option menu				*
 *	pbs[]		Widget	the menu push buttons			*
 *	*btnstrs[]	char	the menu push button labels		*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		09/99	Initial coding				*
 * H. Zeng/EAI          11/99   Modified for two-line labels            *
 * E. Safford/GSC	11/00	change pstart_item type to XtPointer	*
 * E. Safford/GSC	12/00	Add XtCallbackProc cast			*
 * T. Piper/SAIC	10/05	declared ii long			*
 * T. Piper/SAIC	06/06	declared label_str String		*
 ***********************************************************************/
{
    int		*start_item;
    long	ii;
    XmString	xmstr;
    Widget	pdmenu;
/*---------------------------------------------------------------------*/

    start_item = (int *) pstart_item;

   *form = 
	(Widget) XtVaCreateManagedWidget ("create_omen_form",
					  xmFormWidgetClass,	parent,
					  NULL);

    xmstr = XmStringCreateLtoR(label_str, XmFONTLIST_DEFAULT_TAG);

    *label  = XtVaCreateManagedWidget (label_str,
				      xmLabelWidgetClass,	*form,
				      XmNleftAttachment,	XmATTACH_FORM,
                                      XmNlabelString,           xmstr,
				      NULL); 

    XmStringFree(xmstr);

    pdmenu = XmCreatePulldownMenu (*form, "omen_pdmenu", NULL, 0);
    *menu  = XmCreateOptionMenu   (*form, "omen_optmenu", NULL, 0);

    if (btnstrs == NULL) {
	for (ii = 0; ii < nbuttons; ii++) {
	    pbs[ii] = 
		XtVaCreateManagedWidget ("omen_pb",
					 xmPushButtonWidgetClass, pdmenu,
					 XmNuserData, pstart_item,
					 NULL);

	    if (callback) {
		XtAddCallback (pbs[ii], XmNactivateCallback, 
			       callback, (XtPointer) ii);
	    }
	    else {
		XtAddCallback (pbs[ii], XmNactivateCallback, 
			       (XtCallbackProc)pgutls_optPbCb, (XtPointer) ii);
	    }
	}
    }
    else {
	for (ii = 0; ii < nbuttons; ii++) {
	    pbs[ii] = 
		XtVaCreateManagedWidget (btnstrs[ii],
					 xmPushButtonWidgetClass, pdmenu,
					 XmNuserData, pstart_item,
					 NULL);

	    if (callback) {
		XtAddCallback (pbs[ii], XmNactivateCallback, 
			       callback, (XtPointer) ii);
	    }
	    else {
		XtAddCallback (pbs[ii], XmNactivateCallback, 
			       (XtCallbackProc)pgutls_optPbCb, (XtPointer) ii);
	    }
	}
    }

    xmstr = XmStringCreateLocalized ("");
    XtVaSetValues (*menu, 
		   XmNlabelString,	xmstr,	
		   XmNsubMenuId,	pdmenu,
		   XmNmenuHistory,	pbs[*start_item], 
		   XmNrightAttachment,	XmATTACH_FORM,
		   XmNleftAttachment,	XmATTACH_WIDGET,
		   XmNleftWidget,	*label,
		   NULL);
    XmStringFree (xmstr);

    XtManageChild (*menu);

}

/*=====================================================================*/
/* ARGSUSED */
void pgutls_optPbCb ( Widget wid, long which, XtPointer call )
/************************************************************************
 * pgutls_optPbCb							*
 *									*
 * Callback function for option menu push buttons.			*
 *									*
 * void pgutls_optPbCb (wid, which, call)				*
 *									*
 * Input parameters:							*
 *	wid	Widget		widget ID				*
 *	which	int		which button				*
 *	call	XtPointer	not used				*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		09/99	copied from pgwfmt_optCb		*
 ***********************************************************************/
{
    int		*optval;
    XtPointer	userdata;
/*---------------------------------------------------------------------*/

    XtVaGetValues (wid, XmNuserData, &userdata, NULL);
    optval = (int *)userdata;
    *optval = (int)which;
}

/*=====================================================================*/

void pgutls_setOptMenu ( char curr_label[], char labels[][MAXTBLNAME], 
				int nlabels, struct optMenuStrc *omstrc )
/************************************************************************
 * pgutls_setOptMenu							*
 *									*
 * Finds the curr_label among the labels; replaces the push button	*
 * labels with labels[] and manages the buttons, if necessary;		*
 * unmanages extra buttons, and set curr_label in the menu history.	*
 *									*
 * void pgutls_setOptMenu (curr_label, labels, nlabels, omstrc)		*
 *									*
 * Input parameters:							*
 *	curr_label[]		char	current label string		*
 *	labels[][MAXTBLNAME]	char	label strings			*
 *	nlabels			int	number of label strings		*
 *	*omstrc			struct optMenuStrc			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		09/99	initial coding				*
 * S. Law/GSC		04/00	pgsigw_setOptMenu -> pgutls_setOptMenu	*
 * H. Zeng/EAI          10/00   checking if omstrc->menu exists         *
 ***********************************************************************/
{
    int		ii;
    XmString	xmstr;
/*---------------------------------------------------------------------*/

    omstrc->current = 0;
    if (strlen (curr_label) > (size_t)0) {
	for (ii = 0; ii < MAXNOPT; ii++) {
	    if (strcmp (curr_label, labels[ii]) == 0) {
		omstrc->current = ii;
		break;
	    }
	}
    }

    for (ii = 0; ii < nlabels; ii++) {
	xmstr = XmStringCreateLocalized (labels[ii]);

	XtVaSetValues (omstrc->pb[ii], XmNlabelString, xmstr, NULL);

	XmStringFree (xmstr);

	if (!(XtIsManaged (omstrc->pb[ii]))) {
	XtManageChild (omstrc->pb[ii]);
	}
    }

    for (; ii < MAXNOPT; ii++) {
	if (XtIsManaged (omstrc->pb[ii])) {
	    XtUnmanageChild (omstrc->pb[ii]);
        }
    }

    if(omstrc->menu != NULL) {
       XtVaSetValues (omstrc->menu, 
		   XmNmenuHistory, omstrc->pb[omstrc->current], 
		   NULL);
    }
}

/*=====================================================================*/

void pgutls_refresh ( float llx, float lly, float urx, float ury, int *iret )
/************************************************************************
 * pgutls_refresh							*
 *									*
 * Copies the background image for the selected area, then refreshes	*
 * the VGF elements.  Input coordinates must be device coordinates.	*
 *									*
 * void pgutls_refresh (llx, lly, urx, ury, iret)			*
 *									*
 * Input parameters:							*
 *	llx	float	lower left x coordinate				*
 *	lly	float	lower left y coordinate				*
 *	urx	float	upper right x coordinate			*
 *	ury	float	upper right y coordinate			*
 *									*
 * Output parameters:							*
 *			None.						*
 *									*
 * Return parameters:							*
 *	*iret	int	return code					*
 *									*
 **									*
 * Log:									*
 * S. Law/GSC		04/98	initial coding				*
 * E. Safford/GSC	06/98	added call to crg_grfrsh		*
 * E. Safford/GSC	12/98	limit area of refresh			*
 * E. Safford/SAIC	11/01	mv _pghdlb_refresh -> pgutls_refresh	*
 ***********************************************************************/
{
    *iret = 0;

    xpgpaste (llx, lly, urx, ury, iret); 

    if (*iret >= 0) {
   	cvg_rfrsh (NULL, llx, lly, urx, ury, iret);
    }
}

/*=====================================================================*/
/* ARGSUSED */
static void pgutls_vrfyNumeric ( Widget text_w, XtPointer clnt, 
			         XtPointer call, Boolean ngt_ok, 
			         Boolean flt_ok, Boolean blnk_ok,
				 Boolean slash_ok ) 
/************************************************************************
 * pgutls_vrfyNumeric							*
 *									*
 * This is an internal function used to verify the contents of a text	*
 * widget as the user is entering data. It examines the string entered	*
 * and verifies that it contains only the numeric data expected based 	*
 * on a combination of flags: positive/negative, float/int, and blanks/	*
 * noblanks. Only digits, one '.' for float numbers, and one '-' in the	*
 * 1st position in the text string for negative numbers, are allowed. 	*
 * Any other characters	will force the "doit" flag of the cbs struct be	*
 * set to "FALSE". This	signals the text widget to NOT display the data *
 * and SOUND the alarm bell (a single beep). Note that the bell may be	*
 * switched off by setting the text widget's XmNaudibleWarning value,	*
 * which is "TRUE" by default.						*
 *									*
 * Note: (1) Use proper wrap-up functions like pgutls_vrfyPosIntCb,	*
 * 	     pgutls_vrfyPosIntBlkCb, and pgutls_vrfyPosFltCb, not the 	*
 *           pgutls_vrfyNumeric directly,  to verify a specific 	*
 *	     numeric data type. Those functions call pgutls_vrfyNumeric	*
 *	     with proper flags combination.				*
 *	 (2) Those wrap-up functions should be linked to a text widget 	*
 *           via the XmNmodifyVerifyCallback event, which fires before 	*
 *	     the XmNvalueChangedCallback event. So use those functions	*
 *	     to verify the input and wire a second event to the 	*
 *	     XmNvalueChangedCallback event to process the input.  	*
 *									*
 * static void pgutls_vrfyNumeric ( text_w, clnt, call, ngt_ok,		*
 *						 flt_ok, blnk_ok )	*
 *									*
 * Example of usage:							*
 *									*
 *    XtAddCallback ( myTextWidget, XmNmodifyVerifyCallback,		*
 *			(XtCallbackProc)pgutls_vrfy******Cb, NULL );	*
 *									*
 *									*
 * Input parameters:							*
 *	text_w		Widget	  text widget				*
 *	clnt		XtPointer Widget's event data (not used here)   *
 *	call		XtPointer callback structure			*
 *	ngt_ok		Boolean	  True/False - negative allowed or not	*
 *	flt_ok		Boolean	  True/False - float number or integer	*
 *	blnk_ok		Boolean	  True/False - blank allowed or not	*
 *	slash_ok	Boolean	  True/False - '/' allowed or not	*
 *									*
 * Output parameters:							*
 *			None.						*
 **									*
 * Log:									*
 * E. Safford/SAIC	05/02	initial coding                      	*
 * J. Wu/SAIC		05/02	generalize/rename as pgutls_vrfyNumeric	*
 * B. Yin/SAIC		12/04	added slash_ok parameter		*
 ***********************************************************************/
{
    int 	ii, type, ier;
    char	*text;
    Boolean	reject, decimalFound = False;

    XmTextVerifyCallbackStruct *cbs = 
				(XmTextVerifyCallbackStruct *) call;
/*---------------------------------------------------------------------*/

    if (cbs->text->ptr == NULL) {
	return;
    }

/*
 *  Get the current contents of the text widget and see if the 
 *  decimal point is already there.
 */
    XtVaGetValues (text_w, XmNvalue, &text, NULL);
    if ( strchr (text, '.') != NULL ) {
	decimalFound = True;
    }

    reject = False;
    for (ii=0; ii < cbs->text->length; ii++) {

/*
 * cst_alnum returns type == 2 if the char is a digit
 */
	cst_alnm ( cbs->text->ptr[ii], &type, &ier );

	if ( type != 2 ) {
	    
	    reject = True;

/*  
 *  Check for blanks/negative sign/decimal point flags.
 *      blnk_ok - blanks acceptable at any positions.
 *      ngt_ok - one '-' acceptable if it's the 1st char.
 *      flt_ok - one '.' acceptable at any positions.
 *  Any other non-digit character will be flagged as bad.
 */
	    if ( blnk_ok && cbs->text->ptr[ii] == ' ' ) {
	        reject = False;
            }
	    else if ( ngt_ok && ( cbs->text->ptr[ii] == '-' ) 
	                     && ( cbs->startPos == 0 ) )  {
		reject = False;	    
	    }
	    else if ( flt_ok && ( cbs->text->ptr[ii] == '.' ) 
	                     && ( !decimalFound )  )  {
		reject = False;
		decimalFound = True;	    
	    }	 	    	    
	    else if ( slash_ok && cbs->text->ptr[ii] == '/' ) {
	        reject = False;
            }

	}
	
/*
 *  The doit flag of the callback structure is used to signal
 *  to X to not allow the typed character to be entered into the
 *  text widget.
 */
	if ( reject ) {
	    cbs->doit = FALSE;
	    break;
	}	
    }

    XtFree(text);
}

/*=====================================================================*/
/* ARGSUSED */
void pgutls_vrfyPosIntCb ( Widget text_w, XtPointer clnt, 
						XtPointer call ) 
/************************************************************************
 * pgutls_vrfyPosIntCb							*
 *									*
 * This callback function is used to verify that a positive integer is  *
 * entered as the contents of a text widget - only digits permitted,	*
 * no '-', no '.', and no blank spaces. Any other characters wil be	*
 * rejected and trigger a sound beep. See pgutls_vrfyNumeric() for the  *
 * example of usage							*
 *									*
 * void pgutls_vrfyPosIntCb( text_w, clnt, call )			*
 *									*
 * Input parameters:							*
 *	text_w		Widget	  text widget				*
 *	clnt		XtPointer Widget's event data (not used here)   *
 *	call		XtPointer callback structure			*
 *									*
 * Output parameters:							*
 *			None.						*
 **									*
 * Log:									*
 * E. Safford/SAIC	05/02	initial coding                      	*
 * J. Wu/SAIC		05/02	call the generalized pgutls_vrfyNumeric	*
 * B. Yin/SAIC		12/04	changed pgutls_vrfyNumeric calling seq. *
 ***********************************************************************/
{
    pgutls_vrfyNumeric ( text_w, clnt, call, 
   				 False, False, False, False );
}

/*=====================================================================*/
/* ARGSUSED */
void pgutls_vrfyPosIntBlkCb ( Widget text_w, XtPointer clnt, 
						XtPointer call ) 
/************************************************************************
 * pgutls_vrfyPosIntBlkCb						*
 *									*
 * This callback function is used to verify that a positive integer is  *
 * entered as the contents of a text widget with digits/spaces allowed.	*
 * Any other characters wil be rejected and trigger a sound beep. 	*
 * See pgutls_vrfyNumeric() for the example of usage.		 	*
 *									*
 * void pgutls_vrfyPosIntBlkCb( text_w, clnt, call )			*
 *									*
 * Input parameters:							*
 *	text_w		Widget	  text widget				*
 *	clnt		XtPointer Widget's event data (not used here)   *
 *	call		XtPointer callback structure			*
 *									*
 * Output parameters:							*
 *			None.						*
 **									*
 * Log:									*
 * E. Safford/SAIC	05/02	initial coding                      	*
 * J. Wu/SAIC		05/02	call the generalized pgutls_vrfyNumeric	*
 * B. Yin/SAIC		12/04	changed pgutls_vrfyNumeric calling seq. *
 ***********************************************************************/
{
    pgutls_vrfyNumeric ( text_w, clnt, call, 
   				 False, False, True, False );
}

/*=====================================================================*/
/* ARGSUSED */
void pgutls_vrfyPosFltCb ( Widget text_w, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgutls_vrfyPosFltCb							*
 *									*
 * This callback function is used to verify that a positive float number*
 * is entered as the contents of a text widget - only digits and one '.'*
 * allowed, no '-' and no spaces. Any other characters wil be rejected	*
 * and trigger a sound beep. See pgutls_vrfyNumeric() for the example of*
 * usage.								*
 *									*
 * void pgutls_vrfyPosFltCb( text_w, clnt, call )			*
 *									*
 * Input parameters:							*
 *	text_w		Widget	  text widget				*
 *	clnt		XtPointer Widget's event data (not used here)   *
 *	call		XtPointer callback structure			*
 *									*
 * Output parameters:							*
 *			None.						*
 **									*
 * Log:									*
 * E. Safford/SAIC	05/02	initial coding                      	*
 * J. Wu/SAIC		05/02	call the generalized pgutls_vrfyNumeric	*
 * B. Yin/SAIC		12/04	changed pgutls_vrfyNumeric calling seq. *
 ***********************************************************************/
{
    pgutls_vrfyNumeric ( text_w, clnt, call, 
   				 False, True, False, False );
}

/*=====================================================================*/

void pgutls_initHdr ( VG_HdrStruct *hdr )
/************************************************************************
 * pgutls_initHdr							*
 *									*
 * Sets the values of hdr to reasonable defaults.  This routine is      *
 * designed as a safety check.  It should be called before loading up   *
 * an element for a ces_set() or similar call which uses a VG_DBStruct  *
 * element to pass element info to ensure no empty (and therefore       *
 * garbage filled) values get passed into ces_set() or similar routine.	* 
 *									*
 * Note that vg_type and vg_class and recsz fields are not completed.   *
 * If these fields should ever end up empty you face larger problems    *
 * than a garbage filled vg hdr.					*
 *									*
 * Note too that this routine does not allocate space for the hdr.  It  *
 * assumes that the pointer passed in already points to sufficient      *
 * space for the structure.						*
 *									*
 * void pgutls_initHdr ( hdr )                    			*
 *									*
 * Input/Output parameters:						*
 *	*hdr	VG_HdrStruct	header structure       			*
 *									*
 **									*
 * Log:									*
 * E. Safford/SAIC	12/01	initial coding				*
 ***********************************************************************/
{

    hdr->delete		= 0;
    hdr->filled		= 0;
    hdr->closed		= 0;
    hdr->smooth		= 0;
    hdr->version	= 0;
    hdr->grptyp		= 0;
    hdr->grpnum		= 0;
    hdr->maj_col	= 1;		/* Vanilla */
    hdr->min_col	= 1;
    hdr->range_min_lat  = 26.0F;		/* roughly the bounds of the */
    hdr->range_min_lon  = -120.0F;	/* continental US	     */
    hdr->range_max_lat  = 48.0F;
    hdr->range_max_lon  = -67.0F;

}

/*=====================================================================*/

int pgutls_regroup ( char frm_grptyp, int frm_grpnum, 
	             char to_grptyp, int to_grpnum, int *iret )
/************************************************************************
 * pgutls_regroup							*
 *									*
 * This routine moves all vg elements from the frm group and places     *
 * them in the to group.  The number of elements moved is returned.     *
 *									*
 * Note that this routine moves elements to the new group by deleting   *
 * the old element and resaving with the new group type and number.     *
 * This routine adds deleted elements and new elements to the current   *
 * undo step.  The calling routine must manage the start and end of     *
 * the undo step.                      					*
 *									*
 * int pgutls_regroup ( frm_grptyp, frm_grpnum, to_grptyp, to_grpnum,	*
 *								*iret )	*
 *									*
 * Input parameters:							*
 *	frm_grptyp	char	moving elements from this group type	*
 *	frm_grpnum	int	moving elements from this group number	*
 *	to_grptyp	char	moving elements to this group type	*
 *	to_grpnum	int	moving elements to this group number	*
 *									*
 * Output parameters:							*
 *	*iret		int	-1 if the frm_grptyp and/or grpnum <= 0	*
 *	     		    	-2 if the toi_grptyp and/or grpnum <= 0	*
 * Return:                   						*
 *	pgutls_regroup	int	total number of elements moved		*
 **									*
 * Log:									*
 * E. Safford/SAIC	03/02	initial coding				*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * J. Wu/SAIC           10/04   free GFA block memory      		*
 * S. Danz/AWC          08/06   New flag to pgvgf_saveNewElm to place el*
 ***********************************************************************/
{
    int			ctr = 0, nelm = 0, offset, ier;
    int			npts, new_offset, ii;
    int			idx[MAX_EDITABLE_ELEMS];
    float		llx, lly, urx, ury, xx[MAXPTS], yy[MAXPTS];

    VG_DBStruct		el;
/*---------------------------------------------------------------------*/


    *iret = 0;

    if ( frm_grptyp <= 0 || frm_grpnum <= 0 ) {
	*iret = -1;
    }
    else if ( to_grptyp <= 0 || to_grpnum <= 0 ) {
	*iret = -2;
    }


    if (*iret >= 0 ) {

/*
 *  Get the range record indices to the elements in this group.
 */
        crg_gginx (frm_grptyp, frm_grpnum, MAX_EDITABLE_ELEMS,
			idx, &nelm, &ier);

/*
 *  Loop over all the elements, deleting each using _prepNew()
 *  changing the group type and number and resaving.
 */
	for (ii=0; ii < nelm; ii++) {
	    crg_goffset ( idx[ii], &offset, &ier );
	
	    if ( offset > 0 ) {

		pgundo_storeThisLoc (offset, UNDO_DEL, &ier);

	        pgutls_prepNew ( offset, &el, &llx, &lly, &urx, &ury, &ier );
		cvg_todev ( &el, &npts, xx, yy, &ier);

		el.hdr.grptyp = to_grptyp;
		el.hdr.grpnum = to_grpnum;

		pgvgf_saveNewElm ( cvg_getworkfile(), sys_D, &el, npts, xx, yy,
					TRUE, &new_offset, &ier );

		pgundo_storeThisLoc ( new_offset, UNDO_ADD, &ier );
		
/*
 * Free TCA/GFA memory
 */
                if ( el.hdr.vg_type == TCA_ELM ) {
                    cvg_freeBkpts ( &el );
                }
                else if ( el.hdr.vg_type == GFA_ELM ) {
                    cvg_freeElPtr ( &el );
                }

		pgutls_redraw ( new_offset, &el, &ier );

/*
 * Free TCA/GFA memory
 */
                if ( el.hdr.vg_type == TCA_ELM ) {
                    cvg_freeBkpts ( &el );
                }
                else if ( el.hdr.vg_type == GFA_ELM ) {
                    cvg_freeElPtr ( &el );
                }
		
		if (ier >= 0 ) ctr ++;
	    }
	}
    }

    return (ctr);
}

/*=====================================================================*/
/* ARGSUSED */
void pgutls_vrfyTimeCb ( Widget text_w, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgutls_vrfyTimeCb							*
 *									*
 * This callback function is used to verify that a time string in GEMPAK*
 * format is entered as the contents of a text widget - only digits and *
 * '/' allowed. Any other characters wil be rejected and trigger a sound*
 * beep. See pgutls_vrfyNumeric() for the example of usage.		*
 *									*
 * void pgutls_vrfyTimeCb( text_w, clnt, call )				*
 *									*
 * Input parameters:							*
 *	text_w		Widget	  text widget				*
 *	clnt		XtPointer Widget's event data (not used here)   *
 *	call		XtPointer callback structure			*
 *									*
 * Output parameters:							*
 *			None.						*
 **									*
 * Log:									*
 * B. Yin/SAIC		12/04	Created					*
 ***********************************************************************/
{
    pgutls_vrfyNumeric ( text_w, clnt, call, 
   				 False, False, False, True );
}

/*=====================================================================*/
/* ARGSUSED */
void pgutls_vrfyUpperCaseCb ( Widget text_w, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgutls_vrfyUpperCaseCb						*
 *									*
 * This callback function is used to verify that the unput text is all  *
 * upper case.  Numeric and special characters are not changed.  Lower  *
 * case text is promoted to upper case text.				*
 *									*
 * void pgutls_vrfyUpperCaseCb( text_w, clnt, call )			*
 *									*
 * Input parameters:							*
 *	text_w	Widget	  text widget					*
 *	clnt	XtPointer Widget's event data (not used here)   	*
 *	call	XtPointer callback structure				*
 *									*
 * Output parameters:							*
 *			None.						*
 **									*
 * Log:									*
 * E. Safford/SAIC	04/07	Initial coding				*
 ***********************************************************************/
{
    char	*newText;
    int		ier;

    XmTextVerifyCallbackStruct *cbs = 
				(XmTextVerifyCallbackStruct *) call;
/*---------------------------------------------------------------------*/

    if ( cbs->text->ptr == NULL || !cbs->doit ) {
	return;
    }

    G_MALLOC( newText, char, cbs->text->length + 1, 
    				"pgutls_vrfyUpperCaseCb: newText" ); 

    cst_lcuc( cbs->text->ptr, newText, &ier );
    strncpy( cbs->text->ptr, newText, cbs->text->length );

    G_FREE( newText, char );
}

/*=====================================================================*/
/* ARGSUSED */
void pgutls_vrfyNoPunctCb ( Widget text_w, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgutls_vrfyNoPunctCb							*
 *									*
 * This callback function is used to verify that the input text         *
 * contains no punctuation characters.					*
 *									*
 * void pgutls_vrfyNoPunctCb( text_w, clnt, call )			*
 *									*
 * Input parameters:							*
 *	text_w	Widget	  text widget					*
 *	clnt	XtPointer Widget's event data (not used here)   	*
 *	call	XtPointer callback structure				*
 *									*
 * Output parameters:							*
 *			None.						*
 **									*
 * Log:									*
 * E. Safford/SAIC	04/07	Initial coding				*
 * J. Wu/SAIC		10/07	allow '.' 				*
 * J. Wu/SAIC		12/07	move '.' to pgutls_vrfLmtPunctCb	*
 ***********************************************************************/
{
    int		ii;
    Boolean	allOk = True;
    XmTextVerifyCallbackStruct *cbs = 
				(XmTextVerifyCallbackStruct *) call;
/*---------------------------------------------------------------------*/

    if ( cbs->text->ptr == NULL || !cbs->doit ) {
	return;
    }

/*
 *  Valid inputs are any non-punctuation characters.  
 */
    for( ii=0; ii < cbs->text->length && allOk; ii++ ) {
        if( ispunct( cbs->text->ptr[ ii ] ) != 0 ) { 
	    
	    allOk = False;
        } 
    }

    if( !allOk ) {
	cbs->doit = False;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgutls_vrfyNoDigitsCb ( Widget text_w, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgutls_vrfyNoDigitsCb						*
 *									*
 * This callback function is used to verify that the input text         *
 * contains no digits.                 					*
 *									*
 * void pgutls_vrfyNoDigitsCb( text_w, clnt, call )			*
 *									*
 * Input parameters:							*
 *	text_w	Widget	  text widget					*
 *	clnt	XtPointer Widget's event data (not used here)   	*
 *	call	XtPointer callback structure				*
 *									*
 * Output parameters:							*
 *			None.						*
 **									*
 * Log:									*
 * E. Safford/SAIC	04/07	Initial coding				*
 ***********************************************************************/
{
    int		ii;
    Boolean	allOk = True;
    XmTextVerifyCallbackStruct *cbs = 
				(XmTextVerifyCallbackStruct *) call;
/*---------------------------------------------------------------------*/

    if ( cbs->text->ptr == NULL || !cbs->doit ) {
	return;
    }

/*
 *  Valid inputs are any non-punctuation characters.  
 */
    for( ii=0; ii < cbs->text->length && allOk; ii++ ) {
        if( isdigit( cbs->text->ptr[ ii ] ) != 0 ) {
	    allOk = False;
        } 
    }

    if( !allOk ) {
	cbs->doit = False;
    }
}

/*=====================================================================*/
/* ARGSUSED */
void pgutls_vrfyLmtPunctCb ( Widget text_w, XtPointer clnt, XtPointer call ) 
/************************************************************************
 * pgutls_vrfyLmtPunctCb						*
 *									*
 * This callback function is used to verify that the input text         *
 * contains no punctuation characters, other than '.' and '-'.		*
 *									*
 * void pgutls_vrfyLmtPunctCb( text_w, clnt, call )			*
 *									*
 * Input parameters:							*
 *	text_w	Widget	  text widget					*
 *	clnt	XtPointer Widget's event data (not used here)   	*
 *	call	XtPointer callback structure				*
 *									*
 * Output parameters:							*
 *			None.						*
 **									*
 * Log:									*
 * J. Wu/SAIC		12/07	Modify from  pgutls_vrfyNoPunctCb	*
 ***********************************************************************/
{
    int		ii;
    Boolean	allOk = True;
    XmTextVerifyCallbackStruct *cbs = 
				(XmTextVerifyCallbackStruct *) call;
/*---------------------------------------------------------------------*/

    if ( cbs->text->ptr == NULL || !cbs->doit ) {
	return;
    }
    
/*
 *  Valid inputs are any non-punctuation characters, except '.' & '-'.  
 */
    for( ii=0; ii < cbs->text->length && allOk; ii++ ) {
        if( ispunct( cbs->text->ptr[ ii ] ) != 0 &&
	    cbs->text->ptr[ ii ] != '.' &&
	    cbs->text->ptr[ ii ] != '-' ) { 
	    
	    allOk = False;
        } 
    }

    if( !allOk ) {
	cbs->doit = False;
    }
}
