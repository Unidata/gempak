#include "geminc.h"
#include "gemprm.h"
#include "interface.h"
#define PANEL
#include "panel.h"
#include "Nxm.h"


extern int	meta_id;
extern WidgetList file_mb;
extern Widget	filenameW, group_listW, save_group_popup;

Widget		model_select_toplevel;

char	WaitFlag;

/*
 *  Private functions
*/
void Cancel_Model_Select	( Widget, XtPointer, XtPointer );
void save_grouplist		( char *filename );
void Select_Model_Callback	( Widget, XtPointer, XmFileSelectionBoxCallbackStruct* );

/************************************************************************
 *	GP_CALLBKS.C							*
 *									*
 *   Module to take care of callbacks for selecting group panel. 	*
 *									*
 *   Log:								*
 *   Chien Lin/EAI      10/92						*
 *   Chien Lin/EAI	6/93  add busy popup				*
 *   Chien Lin/EAI	9/93  add check_memory(), check_pixmaps()	*
 *   S. Wang/GSC 	1/97  use GPLT functions to replace X		*
 *			      functions, add setView() and		*
 *			      defaultView()				*
 *   G. Krueger/EAI	9/97  Changed NxmWarning -> NxmWarn_show	*
 *   G. Krueger/EAI     11/97  Renamed NxmHelp functions		*
 *   I. Durham/GSC	5/98  Changed call for underscore		*
 *   J. Wu/GSC		5/01  free XmStrings				*
 ***********************************************************************/

/*=====================================================================*/
/* ARGSUSED */
void Save_Popup_Callback ( Widget wdgt, long which, XtPointer call )
{
int		status = 0;

/*---------------------------------------------------------------------*/

	switch (which) {

	case	0:     /* OK */
		save_group();
		status = 0;
		break;

	case	1:	/* CANCEL */
		status = 1;
		break;

	default:
		status = 1;
		break;
	}

	if (status)
	XtUnmanageChild(save_group_popup);
}

/*=====================================================================*/

void save_group ( void )
{
int		status = 0;
char		filename[80];

/*---------------------------------------------------------------------*/

      get_text(filenameW, &filename[0]);

      if (filename[0] != (char)NULL){
		strcat(filename,".GROUP");
		save_grouplist(filename);
		status = 1;
      }
      else    {
		NxmWarn_show(save_group_popup,
			       "Please Specify the File Name.");

		status = 0;
      }

      if (status)
      XtUnmanageChild(save_group_popup);
}

/*=====================================================================*/

void save_grouplist ( char *filename )
{
int		i, j;
FILE		*fp;

/*---------------------------------------------------------------------*/

	fp = fopen(filename,"w");

	if ( fp == NULL ) {
		 NxmWarn_show(save_group_popup,
		 "You may not have write permission in this directory/file.");
		return;
	}

	fprintf(fp,"%d\n",FrameNo);
	fprintf(fp,"%d\n",GroupNo);

	for ( i = 0; i < GroupNo; i++) {

		fprintf(fp,"%s\n",GroupList[i].groupname);

		fprintf(fp,"%d\n",GroupList[i].frame_num);

		for( j=0; j< GroupList[i].frame_num;j++)
			fprintf(fp,"%d ", GroupList[i].frames[j]);

		fprintf(fp,"\n");
	}

	fclose(fp);
}

/*=====================================================================*/

void load_model ( Widget wdgt, char filename[] )
{
FILE	*fp;
int	ii, jj, framecnt,frameno;
char	message[]="The number of frames in metafile is not consistent with the Model.";

/*---------------------------------------------------------------------*/

	fp = fopen(filename,"r");

	if (fp != NULL) {

	    fscanf(fp,"%d\n", &frameno);

	    if (frameno != FrameNo)
			NxmWarn_show(wdgt, message);
	    else {

		fscanf(fp,"%d\n", &GroupNo);

		for (ii = 0; ii < GroupNo; ii++) {
			fscanf(fp, "%[^\n]", GroupList[ii].groupname);

			fscanf(fp, "%d\n", &GroupList[ii].frame_num);
			framecnt = GroupList[ii].frame_num;

			for (jj = 0; jj< framecnt; jj++)
				fscanf(fp,"%d", &GroupList[ii].frames[jj]);

			fscanf(fp,"\n");
		}
		add_grouplist();
	     }

	     fclose(fp);

	}
	else
	   if ( OpenModel && (FrameNo > 0) )
		autogroup();
}

/*=====================================================================*/

void create_model_selection ( Widget parent )
/************************************************************************
 **									*
 * T. Piper/SAIC	2/02	Fixed ABR; removed setting XmNtitle	*
 ***********************************************************************/
{
Arg			args[10];
Cardinal		argcnt, i;
XmStringTable		xmstr;

/*-------------------------------------------------------------------*/
/* Create a popup shell & list widget for the file list. */
        
	xmstr = (XmStringTable)XtMalloc(2*sizeof(XmString *));
	argcnt = 0;
	
	xmstr[argcnt] = XmStringCreateLocalized("Search");
	XtSetArg(args[argcnt], XmNapplyLabelString, xmstr[argcnt]);  
	argcnt++;
	
	xmstr[argcnt] = XmStringCreateLocalized("Select");
	XtSetArg(args[argcnt], XmNokLabelString, xmstr[argcnt]);  
	argcnt++;

	model_select_toplevel = XmCreateFileSelectionDialog(parent,
		"User GROUP File Selection", args, argcnt);

	for ( i = 0; i < argcnt; i++ ) {
	    XmStringFree( xmstr[ i ] );
	}	
	XtFree ( (XtPointer)xmstr );
	
	XtAddCallback(model_select_toplevel, XmNokCallback,
			(XtCallbackProc)Select_Model_Callback, NULL);

	XtAddCallback(model_select_toplevel, XmNcancelCallback,
			(XtCallbackProc)Cancel_Model_Select, NULL);

	XtAddCallback(model_select_toplevel, XmNhelpCallback,
			(XtCallbackProc)NxmHelp_helpBtnCb, (XtPointer)4 );

}

/*=====================================================================*/
/* ARGSUSED */
void Select_Model_Callback ( Widget wdgt, XtPointer clnt, 
			XmFileSelectionBoxCallbackStruct *select_data )
/* T. Piper/SAIC	10/06	Replaced XmStringGetLtoR with		*
 *					 XmStringUnparse		*
 ***********************************************************************/
{
char	*filename;

/*-------------------------------------------------------------------*/
    filename = XmStringUnparse (select_data->value, NULL, XmCHARSET_TEXT,
                                XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);
    if (!*filename) {
	printf("\n no file selected\n");
    }
    else {
	XtUnmanageChild(model_select_toplevel);
	load_model( wdgt, filename );
    }
    XtFree(filename);
}

/*=====================================================================*/
/* ARGSUSED */
void Cancel_Model_Select ( Widget wdgt, XtPointer clnt, XtPointer call )
{
    XtUnmanageChild(model_select_toplevel);
}

/*=====================================================================*/

void load_group ( int groupno )
/************************************************************************
 *									*
 * load_group	 							*
 *									*
 * this function load a group of frames and display the valid ones	*
 *									*
 * void load_group(groupno)						*
 *									*
 * Input parameters:							*
 * groupno	int		group id number 			*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * C. Lin/EAI	   12/92	first version				*
 * S. Wang/GSC	   1/97 	rewrite for xwp driver			*
 * S. Wang/GSC	   5/97 	add NxmBusy_* functions 		*
 ***********************************************************************/
{
int	 i, j, kk;
int	 n_frame;
int	 iret;
int	 fm, getframe;
prt_multiPanel_t *ps_info;

/*---------------------------------------------------------------------*/

	 kk = 0;

/* keep frame info for ps file */
	 ps_info = &(prt_multiPanel_info[Mpanel.selectRow][Mpanel.selectCol]);
	 ps_info->flag	    = 1;
	 ps_info->group_no  = groupno;
	 ps_info->meta_id   = meta_id;
	 ps_info->meta_st   = meta_st;
	 display_legend( 1 );

	 if ( !MpanelMode )
	     NxmColorbarReset(0);
	 groupno--;
	 loadBeginNotify();

	 PixmapData.pixmap_no = 0;
	 gstanm(&iret);

	 if( ValidTimeSet>0 ) {
		for(i=0; i<ValidTimeSet; i++) {
		    NxmBusy_checkStopBtn();
		    if (WaitFlag == 1)
			break;
		    for( j=0; j<GroupList[groupno].frame_num; j++) {
			getframe = -1;
			fm = GroupList[groupno].frames[j] - 1;
			if(strstr(meta_st.frame[fm].label,
				ValidTimeString[i]) != NULL ) {
			   getframe = fm;
			   break;
			}
		    }

		    ps_info->frame[kk] = getframe;
		    setView(Mpanel.selectRow, Mpanel.selectCol);
		    if ( getframe > -1)
			Trans_Frame( getframe );
		    kk++;
		    (PixmapData.pixmap_no)++;
		    gsplot(&iret);
		}
	 }
	 else {
		for(i=0; i<GroupList[groupno].frame_num; i ++ ) {
		    NxmBusy_checkStopBtn();
		    if (WaitFlag == 1)
			break;
		    fm = GroupList[groupno].frames[i] - 1;
		    ps_info->frame[kk] = fm;
		    setView(Mpanel.selectRow, Mpanel.selectCol);
		    Trans_Frame( fm );
		    kk++;
		    (PixmapData.pixmap_no)++;
		    gsplot(&iret);
		}
	 }

	 ps_info->frame_num = kk;

	 if ( !MpanelMode ) {
		PixmapData.old_pixmap_no = PixmapData.pixmap_no;
	 }
	 else {
	     n_frame = nFrame();
	     if ( PixmapData.pixmap_no < n_frame )
		for ( i=PixmapData.pixmap_no; i< n_frame; i++ ) {
		    setView(Mpanel.selectRow, Mpanel.selectCol);
		    (PixmapData.pixmap_no)++;
		    gsplot(&iret);
		}
		PixmapData.old_pixmap_no = n_frame;
	 }

	 genanm( &iret );

	 if ( !WaitFlag)
	     NxmBusy_animateFinish();
	 else {
	     WaitFlag = 0;
	 }

	 PixmapData.current_pixmap = 0;
	 NxmChangePixmapData( PixmapData.current_pixmap,
					PixmapData.old_pixmap_no);
	 displayPixmap();
	 loadEndNotify();

}

/*=====================================================================*/

void setView ( int row, int col )
/************************************************************************
 *									*
 * setView								*
 *									*
 * this function set the viewing area of graph				*
 *									*
 * void setView(row, col)						*
 *									*
 * Input parameters:							*
 * row		int		row number in multipanel		*
 * col		int		col number in multipanel		*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC	   2/97 						*
 ***********************************************************************/
{
float	x_start, y_start;
float	x_end, y_end;
int	adj_row, iret;

/*---------------------------------------------------------------------*/


	adj_row = (Mpanel.rows - 1) - row;
	x_start =  (float)col/(float)Mpanel.columns ;
	y_start =  (float)adj_row/(float)Mpanel.rows ;
	x_end	=  (float)(col + 1)/(float)Mpanel.columns ;
	y_end	=  (float)(adj_row + 1)/(float)Mpanel.rows ;

/*
 * select viewing area
 */
	gsview(&x_start, &y_start, &x_end, &y_end, &iret);
	if ( iret != 0 ) {
		printf(" error calling gsview, iret = %d\n", iret);
		return;
	}

/*
 * Clear the panel.
 */
	gclpnl ( &x_start, &y_start, &x_end, &y_end, &iret );
}

/*=====================================================================*/

void defaultView ( void )
/************************************************************************
 *									*
 * defaultView								*
 *									*
 * this function set the viewing area to be whole screen		*
 *									*
 * void defaultView()							*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC	   1/97 						*
 ***********************************************************************/
{
float	x_start, y_start;
float	x_end, y_end;
int	iret;

/*---------------------------------------------------------------------*/

	x_start =  0.0F;
	y_start =  0.0F;
	x_end	=  1.0F;
	y_end	=  1.0F;

/*
 * clears the default viewing area
 */

	gclear ( &iret );

	gsview(&x_start, &y_start, &x_end, &y_end, &iret);
	if ( iret != 0 ) {
		printf(" error calling gsview, iret = %d\n", iret);
		return;
	}
}

/*=====================================================================*/

void loadBeginNotify ( void )
 /***********************************************************************
 * loadBeginNotify							*
 *									*
 * this function begins the loading action.				*
 *									*
 * void loadBeginNotify()						*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC	   2/97 						*
 * E. Safford/GSC	07/01	Remove menu_b[*] settings       	*
 * T. Piper/SAIC	07/03	Removed NxmBusy_setBusyPid		*
 ***********************************************************************/
{
    WaitFlag = 0;
    NxmBusy_invoke(DrawingW,&WaitFlag);
    NxmLoopbuttonSensitive( False );
}

/*=====================================================================*/

void loadEndNotify ( void )
 /***********************************************************************
 * loadEndNotify							*
 *									*
 * this function ends the loading action				*
 *									*
 * void loadEndNotify() 						*
 *									*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC	   	 2/97 						*
 * E. Safford/GSC	07/01	Remove menu_b[*] settings       	*
 ***********************************************************************/
{
    NxmLoopbuttonSensitive( True );
    XtSetSensitive(file_mb[1], True);
    XtSetSensitive(file_mb[2], True);
}

/*=====================================================================*/

void reload_group ( void )
 /***********************************************************************
 * reload_group 							*
 *									*
 * this function reload all the frames to the resized window		*
 *									*
 * void reload_group()							*
 *									*
 * Input  parameters:							*
 * Output parameters:							*
 * Return parameters:							*
 *			NONE						*
 *									*
 **									*
 * Log: 								*
 * S. Wang/GSC	   2/97 						*
 ***********************************************************************/
{
int	iret, frame_num;

/*---------------------------------------------------------------------*/

	 if ( !MpanelMode )
	     NxmColorbarReset(0);

	 loadBeginNotify();

	 PixmapData.pixmap_no = 0;
	 gstanm(&iret);
	 gclear(&iret);

	 frame_num = loadAllPage();
	 genanm(&iret);

	 if ( !WaitFlag)
	     NxmBusy_animateFinish();
	 else {
	     PixmapData.old_pixmap_no = frame_num;
	     WaitFlag = 0;
	 }

	 PixmapData.current_pixmap = 0;

	 NxmChangePixmapData( PixmapData.current_pixmap,
			PixmapData.old_pixmap_no);
	 displayPixmap();

	 loadEndNotify();
}
