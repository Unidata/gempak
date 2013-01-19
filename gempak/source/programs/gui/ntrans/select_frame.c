#include "geminc.h"
#include "gemprm.h"
#include "interface.h"
#include "Nxm.h"

extern WidgetList	file_mb;

Widget	frame_listW;
Widget	frame_select_toplevel;

/************************************************************************
*	SELECT_FRAME.C							*
*									*
*   Module to take care of creating frame selection panel.		*
*									*
*   Log:								*
*   Chien Lin/EAI      08/93						*
*   Chien Lin/EAI      01/95 change select_frame help file from 	*
*			     print.hlp to viewframe.hlp 		*
*   G. Krueger/EAI     11/97 Renamed NxmHelp functions			*
*   I. Durham/GSC	5/98 Changed call for underscore		*
************************************************************************/

void create_selectframe ( Widget parent )
{
Arg		args[10];
Widget		pane, rowcol;

/*---------------------------------------------------------------------*/
/* Create a popup shell & list widget for the file list. */
	frame_select_toplevel = XmCreateBulletinBoardDialog(parent,
			"frameselect",args, 0);

	pane = XtVaCreateManagedWidget("FrameSelectPane",
				xmPanedWindowWidgetClass,
				frame_select_toplevel,
				XmNsashWidth,  1,
				XmNsashHeight, 1,
				NULL);

	frame_listW = create_framelist(pane, 0);

	rowcol = XtVaCreateManagedWidget("FrameSelectRc",
				  xmRowColumnWidgetClass, pane,
				  XmNorientation, XmHORIZONTAL,
				  NULL );

	create_std_buttons( rowcol, (XtCallbackProc)frame_select );
}

/*=======================================================================*/
/* ARGSUSED */
void frame_select ( Widget w, long which, XtPointer call )
{
/*----------------------------------------------------------------------*/
    switch (which) {

	case	0:     /* Select */
		view_frame();
		break;

	case	1:	/* HELP */
		NxmHelp_helpBtnCb( NULL, 10, NULL );
		break;

	case	2:	/* CANCEL */
		XtUnmanageChild(frame_select_toplevel);
		break;
    }
}

/*========================================================================*/

void view_frame ( void )
{
int		framecnt, ii, jj;
XmStringTable	selectedframes;
char		fname[100];

/*-----------------------------------------------------------------------*/
	XtVaGetValues(frame_listW,
			XmNselectedItemCount, &framecnt,
			XmNselectedItems,     &selectedframes,
			NULL);

	if ( framecnt != 0 ) {
		NxmColorbarReset(0);
		GroupLoadFlag = 0;
		ViewFrame =  XmListItemPos(frame_listW,
					   selectedframes[0]);

		NxmLoopbuttonSensitive( False );

		strcpy(fname, "Frame: ");
		strcat( fname, (meta_st.frame[ViewFrame-1]).label );
		display_legend_message(1, fname);
/*
 * clear all flags in prt_multiPanel structure
 */
		for ( ii = 0; ii < MAXPANEL; ii++ )
		    for ( jj = 0; jj < MAXPANEL; jj++ )
			prt_multiPanel_info[ii][jj].flag = 0;

		load_frame(ViewFrame);
	}
}

/*========================================================================*/

void load_frame ( int frameno )
{
int	iret;

/*----------------------------------------------------------------------*/

	gstanm(&iret);

	frameno --;
	PixmapData.old_pixmap_no  = 0;
	PixmapData.pixmap_no	  = 0;
	defaultView();
	Trans_Frame(frameno);

	genanm(&iret);
	PixmapData.pixmap_no++;
	PixmapData.old_pixmap_no++;
	PixmapData.current_pixmap = 0;
	display_pixmap();

	XtSetSensitive(file_mb[1], True);
	XtSetSensitive(file_mb[2], True);
}
