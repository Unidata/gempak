#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h" 
#include "proto_xw.h"

/************************************************************************
 * NuiColorInit.c							*
 *									*
 * This module contains the Nui implementation of the color bar and its *
 * related setting routines.						*
 *									*
 * CONTENTS:								*
 *	NuiColorBarCreate()	creates the color bar			*
 *	NuiColorBarReset()   	resets color bar to the default colors	*
 *	NuiColorEditPopup()	creates the color edit window		*
 *									*
 * log: 								*
 * J. Cowie/COMET	11/95	Changed bank argument in call to xqclrs *
 *				to address-of (&). Changed return code  *
 *				check					*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * S. Jacobs/NCEP	 8/98	Changed colors from long int to int	*
 * E. Safford/GSC	01/99	removed NuiColorBarTableCreate		*
 ***********************************************************************/

void NuiColorBarCreate ( Widget parent, Boolean show_active_color ) 
/************************************************************************
 * NuiColorBarCreate 	            				   	*
 *								   	*
 * Creates the color bar using parent as the container widget.	   	*
 *								   	*
 * void NuiColorBarCreate ( parent,show_active_color )			*
 *									*
 * Input Parameters:							*
 *  parent		Widget	color bar container widget     		*
 *  show_active_color	Boolean toggle for initial color display	*
 *				  TRUE =  show only the active color	*
 *				  FALSE = show all colors		*
 * 									*
 * Output Parameters:							*
 *  NONE								*
 **									*
 * Log:								   	*
 * E. Safford/GSC	01/99	add header and clean up  		*
 * T. Piper/SAIC        01/04   major cleanup				*
 ***********************************************************************/
{
int	bank, ncolors, iret;
Pixel	colr_long[MAXCOLORS];
/*---------------------------------------------------------------------*/

    NxmLoadColorTable( parent, CLR_TBL );

    bank = GraphCid;
    xqclrs( &bank, &ncolors, colr_long, &iret);
    if ( iret < 0 ) {
        return;
    }
    else {
        if ( ncolors > GRAPH_COLORS ) {
            ncolors = GRAPH_COLORS;
        }
    }

    NxmColorBarCreate(parent, ncolors, colr_long, show_active_color);
	
}

/*=====================================================================*/

void NuiColorBarReset ( Widget parent )
/************************************************************************
 * NuiColorBarReset 	            				   	*
 *								   	*
 * Reset a previously displayed color bar     			   	*
 *								   	*
 * NuiColorBarReset ( parent )						*
 *									*
 * Input parameters:							*
 *  parent	Widget	color bar container widget     			*
 * 									*
 * Output parameters:							*
 *  NONE								*
 **									*
 * Log:								   	*
 * E. Safford/GSC	01/99	initial coding				*
 * T. Piper/SAIC	01/04	major cleanup				*
 ***********************************************************************/
{
int	bank, ncolors, iret;
Pixel	colr_long[MAXCOLORS];
/*---------------------------------------------------------------------*/

    NxmLoadColorTable(parent, CLR_TBL ); 

    bank = GraphCid;
    xqclrs( &bank, &ncolors, colr_long, &iret);

    if ( iret < 0 ) {
	return;
    }
    else {
	if ( ncolors > GRAPH_COLORS ) {
	    ncolors = GRAPH_COLORS;
	}
    }

    NxmColorBarReload(ncolors, colr_long);
	
}

/*=====================================================================*/

void NuiColorEditPopup ( Widget parent )
/************************************************************************
 * NuiColorEditPopup 	            				   	*
 *								   	*
 * Create the color popup edit window for the color bar.	   	*
 *								   	*
 * NuiColorEditPopup ( parent )						*
 *									*
 * Input parameters:							*
 *  parent	Widget	top level widget for window with color bar	*
 * 									*
 * Output parameters:							*
 *  NONE								*
 **									*
 * Log:								   	*
 * E. Safford/GSC	01/99	add header, clean up			*
 * T. Piper/SAIC        01/04   removed NAWIPS_TABLES reference		*
 ***********************************************************************/
{
int	bank, ncolors, iret;
char	colr_filename[20];
Pixel	colr_long[MAXCOLORS];
/*---------------------------------------------------------------------*/

    bank = GraphCid;
    xqclrs( &bank, &ncolors, colr_long, &iret);

    if ( iret < 0 ) {
	return;
    }
    else {
        if ( ncolors > GRAPH_COLORS ) {
            ncolors = GRAPH_COLORS;
        }
    }

    strcpy( colr_filename, "nawips_color_names");

    NxmColorEditPopupCreate(parent, "color_edit", colr_long, 
                colr_filename, ncolors);

}
