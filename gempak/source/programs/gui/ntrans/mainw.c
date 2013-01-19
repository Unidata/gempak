#include "geminc.h"
#include "interface.h"
#include "Nxm.h"

#define GEM_COLORS 33

Widget	colrbar_frame;



void create_main ( Widget parent )
/************************************************************************
 *	create_main()							*
 *									*
 *	This module creates the main window area.			*
 *									*
 *	Input parameters:						*
 *	parent	Wiget		parent widget id			*
 *									*
 * S. Wang/GSC	  01/97 extract from old ntrans.c			*
 * S. Wang/GSC	  03/97 change help file table name			*
 * G. Krueger/EAI 11/97 Renamed NxmHelp functions			*
 * E. Safford/SAIC	10/01	make help window 80 columns wide	*
 * T. Piper/SAIC	02/04	Changed name and location of help index	*
 ***********************************************************************/
{

/*---------------------------------------------------------------------*/

	toplevel_form = XtCreateManagedWidget("toplevel_form",
			xmFormWidgetClass, parent,
			NULL,		   0);
	create_top_menubar(toplevel_form);

	create_legend(toplevel_form);
	colrbar_frame = XtVaCreateManagedWidget("colrbar",
				xmFrameWidgetClass, toplevel_form,
				XmNbottomAttachment,XmATTACH_FORM,
				XmNleftAttachment,  XmATTACH_FORM,
				XmNrightAttachment, XmATTACH_FORM,
				NULL);

	NuiColorBarCreate(colrbar_frame,True);
	NuiColorEditPopup(toplevel_form);

	create_drawingW(toplevel_form);
	create_file_selection(toplevel_form);
	create_local_products(toplevel_form);

	NxmPrt_create("ntrans", toplevel_form, ntrans_print);
	create_models(toplevel_form);

	create_selectgroup(toplevel_form);

	NxmHelp_create( toplevel_form, "HelpDialog", "HelpText",
			"$GEMHLP/hlp/ntransIndex.hlp", 20, 80);
}
