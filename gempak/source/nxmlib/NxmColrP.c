#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"
#include "proto_xw.h"


static Pixel _colrPixels[GRAPH_COLORS];


/************************************************************************
 * NxmColrP.c								*
 *                                                             		*
 * This module contains functions for color editing pallete, incluing	*
 * creating the pallete, obtaining the pixel number of a color, 	*
 * select a color by set the button, and de-select all color by unset	*
 * all color buttons.							*
 * 									*
 * CONTENTS:								*
 *									*
 *	NxmColrP_create() 	     creates the color editing pallete	*
 *	NxmColrP_getColorPixel()  get the pixel number of a color	*
 *	NxmColrP_setColor()	     set color button			*
 *	NxmColrP_deselectAll()    unset all color cells			*
 ***********************************************************************/

/*=====================================================================*/

NxmColrP_t *NxmColrP_create ( Widget parent, int num_col, int orient, 
				XtEventHandler func )
/************************************************************************
 * NxmColrP_create	                                                *
 *                                                                      *
 * this function creates the color editing pallete which consists of 	*
 * color buttons listed in a matrix form.  The orientation of the major	*
 * dimension is determined by maximun available colors, orientation	*
 * flag, and cell number in major dimension.				*
 *                                                                      *
 * NxmColrP_t *NxmColrP_create ( parent, num_col, orient, func )        *
 *                                                                      *
 * Input parameters:                                                    *
 *      parent      Widget      parent widget ID                	*
 *	num_col	    int	        cell number in major dimension		*
 *	orient	    int	        orientation of major dimension:		*
 *				   1 = horizontal, 0 = vertical		*
 *	func	    XtEventHandler	event handler function		*
 *									*
 * Output parameters:                                                   *
 *               NONE                                                   *
 *									*
 * Return parameters:                                                   *
 *	*NxmColrP_create	NxmColrP_t     ID of the created widget	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Wang/GSC  	04/96   	                              	*
 * S. Wang/GSC		06/96	add orient and func variables		*
 * S. Wang/GSC		06/96	add col_num variable			*
 * C. Lin/EAI		08/96   modify for multi-instantiation  	*
 * S. Wang/GSC		04/97	clean up & move to nxm library		*
 * I. Durham/GSC 	05/98	changed underscore decl. to an include	*
 * E. Safford/GSC	12/98	check XmVERSION as well as XmREVISION	*
 * E. Safford/GSC	04/99	fix irix6 compiler warning		*
 * J. Wu/GSC		07/00   Removed tabs at lines with <tab>#       *
 * T. Piper/SAIC	01/04	removed XmRevision check, clrs -> Pixel	*
 ***********************************************************************/
{
NxmColrP_t *color;
Widget  rc, button;
Pixel	clrs[GRAPH_COLORS];
int     bank, iret;
long	ii;
char	fr_name[8];

static int nclr = 0;

/*---------------------------------------------------------------------*/

	color = (NxmColrP_t *)malloc(sizeof(NxmColrP_t));

        rc = XtVaCreateManagedWidget(   "rc",
                xmRowColumnWidgetClass, parent,
        	XmNpacking,             XmPACK_COLUMN,
		XmNnumColumns,          num_col,
		NULL);

	if ( orient == 1 )
            XtVaSetValues(rc,
                XmNorientation,         XmHORIZONTAL,
                NULL);

/*
 * check color bank availability
 */
	if ( !nclr  ) {
        	bank = GraphCid;
        	xqclrs( &bank, &nclr, clrs, &iret);
		if (iret != 0) 
			return (color); 

/*
 * set the color for each unit in color palette	
 */
		if ( nclr > GRAPH_COLORS )
		    nclr = GRAPH_COLORS;

		for(ii=0; ii<nclr; ii++) 
		    _colrPixels[ii] = clrs[ii];
	}
	
/*
 * create color blocks
 */
	for(ii=1; ii<nclr; ii++) {

	    sprintf(fr_name, "%ld", ii);

	    color->colrFrame[ii] = XtVaCreateManagedWidget(fr_name,
			    xmFrameWidgetClass,	    rc,
			    XmNwidth,		    25,
			    XmNheight,		    20,
			    XmNshadowType,	    XmSHADOW_OUT,
			    NULL);
	    button = XtVaCreateManagedWidget(NULL,
                            xmLabelWidgetClass,      color->colrFrame[ii],
                            XmNbackground,           _colrPixels[ii],
                            XmNtopShadowColor,       _colrPixels[ii],
                            XmNbottomShadowColor,    _colrPixels[ii],
                            NULL);

	    XtAddEventHandler(button,
                     	ButtonReleaseMask,     FALSE ,
                     	(XtEventHandler)func, 
			(XtPointer)ii);
	}

	color->selectedFrame = -1;
	return(color);

}

/*=====================================================================*/

Pixel NxmColrP_getColorPixel ( int which )
/************************************************************************
 * NxmColrP_getColorPixel	                                        *
 *                                                                      *
 * this function get the pixel number of a color in the pallete         *
 *                                                                      *
 * int	NxmColrP_getColorPixel(which)                                	*
 *                                                                      *
 * Input parameters:                                                    *
 *	which		int	index of color in pallete		*
 *									*
 * Output parameters:                                                   *
 *                      NONE                                            *
 *									*
 * Return parameters:                                                   *
 *		pixel number of the indexed color			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *      S. Wang/GSC             06/96                                   *
 ***********************************************************************/
{
    return( _colrPixels[which]);
}

/*=====================================================================*/

void NxmColrP_setColor ( NxmColrP_t *attr_colr, int which )
/************************************************************************
 * NxmColrP_setColor		 	                                *
 *                                                                      *
 * this function select the indexed color in the pallete                *
 *                                                                      *
 * void NxmColrP_setColor( attr_colr, which )   		        *
 *                                                                      *
 * Input parameters:                                                    *
 *	*attr_colr	NxmColrP_t	pointer to the color pallete	*
 *	which		int	       index of color in pallete	*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *      S. Wang/GSC             06/96                                   *
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

    if (attr_colr->selectedFrame > -1) {
        XtVaSetValues( attr_colr->colrFrame[attr_colr->selectedFrame],
                        XmNshadowType, XmSHADOW_OUT,
                        NULL);
    }

    XtVaSetValues( attr_colr->colrFrame[which],
                        XmNshadowType, XmSHADOW_IN,
                        NULL);

    attr_colr->selectedFrame = which;

}

/*=====================================================================*/

void NxmColrP_deselectAll ( NxmColrP_t *attr_colr )
/************************************************************************
 * NxmColrP_deselectAll	               		                        *
 *                                                                      *
 * this function deselect all the colors of the pallete.                *
 *                                                                      *
 * void NxmColrP_deselectAll(attr_colr)   		               	*
 *                                                                      *
 * Input parameters:                                                    *
 *   *attr_colr		NxmColrP_t	pointer to the color pallete	*
 *									*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *      C. Lin/EAI             06/96                                    *
 ***********************************************************************/
{

    if (attr_colr->selectedFrame > -1)
        XtVaSetValues( attr_colr->colrFrame[attr_colr->selectedFrame],
        	XmNshadowType, XmSHADOW_OUT,
                NULL);

    attr_colr->selectedFrame = -1;

}
