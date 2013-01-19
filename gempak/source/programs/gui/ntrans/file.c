/************************************************************************
*	FILEMENU.C							*
*									*
*   Module to take care of File selection popup  for ntrans.		*
*									*
*   Log:								*
*   Chien Lin/EAI      02/93						*
*   Chien Lin/EAI      09/93	 add clear window			*
*   Chien Lin/EAI      09/93	 allocate the largest frame		*
*				    for frame buffer			*
*   S. Wang/GSC        01/97	 rewrite alloc_framebuffer		*
*   G. Krueger/EAI     11/97	 Renamed NxmHelp functions		*
*   J. Wu/GSCI         05/01	 free XmStrings				*
*   R. Tian/SAIC       04/02	 Added reset of DEVICE in gempak	*
* T. Piper/SAIC		07/03	Replaced xwcmn.h with gemprm.h		*
************************************************************************/
#include "geminc.h"
#include "gemprm.h"
#include "interface.h"
#include "panel.h"
#include "Nxm.h"


#define 	FLABELSIZE	64

short		*FrameBuffer = NULL;


void create_file_selection ( Widget parent )
/***********************************************************************
 *	This subroutine creates the file selection widget.
 *
 *		CREATE_FILE_SELECTION  ( PARENT )
 *
 *	Input parameters:
 *		PARENT	Wiget		parent widget
 **									*
 * T. Piper/SAIC	2/02	Fixed ABR; removed setting XmNtitle	*
 ***********************************************************************/
{
Arg		args[10];
Cardinal	argcnt, i;
XmStringTable	xmstr;

/*--------------------------------------------------------------------*/

/* Create a popup shell & list widget for the file list. */

	xmstr = (XmStringTable)XtMalloc(3*sizeof(XmString *));
	argcnt = 0;
	
	xmstr[argcnt] = XmStringCreateLocalized("Search");
	XtSetArg(args[argcnt], XmNapplyLabelString, xmstr[argcnt]);  
	argcnt++;

	xmstr[argcnt] = XmStringCreateLocalized("Select");
	XtSetArg(args[argcnt], XmNokLabelString, xmstr[argcnt]);  
	argcnt++;

	file_select_toplevel = XmCreateFileSelectionDialog(parent,
		"select_meta_file",args,argcnt);

	for ( i = 0; i < argcnt; i++ ) {
	    XmStringFree( xmstr[ i ] );
	}	
	XtFree ( (XtPointer)xmstr );

	XtAddCallback(file_select_toplevel, XmNokCallback,
		(XtCallbackProc)Select_File_Callback, NULL);

	XtAddCallback(file_select_toplevel, XmNcancelCallback,
		(XtCallbackProc)Cancel_Select_Callback, NULL);

	XtAddCallback(file_select_toplevel, XmNhelpCallback,
		(XtCallbackProc)NxmHelp_helpBtnCb, (XtPointer)5 );

}

/*=======================================================================*/
/* ARGSUSED */
void Cancel_Select_Callback ( Widget wdgt, XtPointer clnt, XtPointer call )
/**************************************************************************
 *	This subroutine is the callback function of cancel button
 *		of the file selection box.
 *
 *		Cancel_Select_Callback	( wdgt, clnt, call )
 *
 ***************************************************************************/
{

	 XtUnmanageChild(file_select_toplevel);

	 if (load_flag) {
		NxmLoopbuttonSensitive( True );
	 };

	XtSetSensitive(menu_b[0], True);
}

/*========================================================================*/
/* ARGSUSED */
void Select_File_Callback ( Widget wdgt, XtPointer clnt, 
			XmFileSelectionBoxCallbackStruct *select_data )
/*************************************************************************
 *	This subroutine is the callback function of OK button
 *		of the file selection box.
 *
 *		Select_File_Callback  ( wdgt, clnt, select_data )
 *									*
 * T. Piper/SAIC	10/06	Replaced XmStringGetLtoR with		*
 *					 XmStringUnparse		*
 ***********************************************************************/
{
char		*filename;
int		flen;

/*------------------------------------------------------------------------*/
    filename = XmStringUnparse (select_data->value, NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
    flen = (int)strlen(filename);
    if ( (filename == NULL) | (filename[flen-1] == '/') ) {
	printf("\n no file selected\n");
    }
    else {
	strcpy(MetaFile,filename);
	XtUnmanageChild(file_select_toplevel);
	if ( !MpanelMode ) clear_window();
	load_meta();
    }
    if (filename) XtFree(filename);
}

/*=====================================================================*/

void load_meta ( void )
{
int		ii, status;
char		groupfile[200] = "\0";
char		**framelabel;
int       	ixtype, iytype, iret;
float     	yx, xll, yll, xul, yul;

/*-----------------------------------------------------------------------*/

	initialize();
	display_legend( 0 );
	display_legend_message( 1, " ");

	status = ScanFile(MetaFile);

	if (status) {

		NxmLoopbuttonSensitive( False );

		load_flag = 1;
		FrameNo = meta_st.num_frames;
		framelabel = (char **)malloc((size_t)FrameNo*sizeof(char *));

		alloc_framebuffer();

		for (ii=0; ii<FrameNo; ii++) {
		    framelabel[ii] = (char *)malloc(FLABELSIZE+1);
		    strcpy(framelabel[ii], (meta_st.frame[ii]).label);
		}

		add_framelist(framelabel);

		for (ii=0; ii<FrameNo; ii++) {
		    free(framelabel[ii]);
		}
		free( framelabel );
		
		add_IncNo();

		strccpy(groupfile, MetaFile,'.');
		strcat(groupfile,".GROUP");
		load_model( menubar_form, groupfile );

		XtSetSensitive(menu_b[1], True);
		XtSetSensitive(menu_b[2], True);
		XtSetSensitive(menu_b[3], True);
		XtSetSensitive(menu_b[4], True);

/*
 *  Reset DEVICE in gempak
 */
        	ixtype  = 1;
        	iytype  = 1;
        	xll     = 0.0F;
        	yll     = 0.0F;
		if(meta_head.version == 2) {
        	    xul     = (float)meta_head.fxsize;
        	    yul     = (float)meta_head.fysize;
        	    yx      = 1.0F * yul / xul;
		} else {
        	    xul     = 32767.0F;
        	    yul     = 32767.0F;
        	    yx      = 1.0F;
		}
        	gsgraf( &ixtype,&iytype,&yx,&xll,&yll,&xul,&yul,&iret);
	}
}

/*=====================================================================*/

void alloc_framebuffer ( void )
{
int		i, buffer_size, start, end;
int		allocate;
static int	old_size;

/*---------------------------------------------------------------------*/

	buffer_size = 0;
	allocate = 0;

	for (i=0; i<FrameNo; i++) {
		start = (meta_st.frame[i]).off_byte;
		end   = (meta_st.frame[i]).end_byte;

		if ( (end - start) > buffer_size )
			buffer_size = end - start;
	}

	if ( FrameBuffer == (short *)NULL )
		allocate = 1;
	else {
		if ( buffer_size/2 > old_size ) {
			free(FrameBuffer);
			FrameBuffer = (short *)NULL;
			allocate = 1;
		}
	}
	if (allocate) {
		FrameBuffer = (short *)malloc((size_t)(buffer_size/2)*sizeof(short));
		old_size = buffer_size/2;
	}


	if ( FrameBuffer == NULL) {
		printf("### Cannot get enough space for FrameBuffer\n");
		exit(1);
	}
}

/*========================================================================*/

void initialize ( void )
{

/*-------------------------------------------------------------------------*/

	delete_framelist();
	delete_grouplist();

	FrameNo = 0;
	GroupNo = 0;
	SelectGroupNo = 0;
	GroupLoadFlag = 0;

	PixmapData.current_pixmap = 0;
	PixmapData.pixmap_no = 1;
	NxmChangePixmapData( PixmapData.current_pixmap, PixmapData.pixmap_no);
}
