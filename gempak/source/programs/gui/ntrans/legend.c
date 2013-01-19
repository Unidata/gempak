#include "geminc.h"
#include "gemprm.h"
#include "interface.h"
extern Widget fileLabelW; /* in select_group.c for group selection panel */

/************************************************************************
* legend.c	                                                   	*
*                                                                  	*
*   Module to take care of creating legend area.  	 	   	*
*                                                                  	*
**									*
* Log:                                                           	*
* Chien Lin/EAI      9/93                                        	*
* J. Wu/GSC          5/01	free XmStrings	                	*
* S. Jacobs/NCEP     3/02	Fixed LOGGING option			*
* A. Hardy/NCEP      6/03	added tmzn to CSS_DATE			*
* B. Yin/SAIC       03/04	changed css_date calling sequences      *
************************************************************************/

#define FILENAME	0
#define GROUPNAME	1


void get_laststr ( char *inpstr, char *outstr );

Widget	legend_labelW[3];
char	filstr[100];

/*=====================================================================*/

void create_legend ( Widget parent )
{
Widget		form;
XmString	xmstr;
/*---------------------------------------------------------------------*/

        legend_frame = XtVaCreateManagedWidget("legend",
                                xmFrameWidgetClass, parent,
                                XmNtopAttachment,   XmATTACH_WIDGET,
                                XmNtopWidget,       menubar_form,
                                XmNleftAttachment,  XmATTACH_FORM,
                                XmNrightAttachment, XmATTACH_FORM,
                                NULL);

	form = XtVaCreateManagedWidget("legendform",
                xmFormWidgetClass, legend_frame,
                NULL);

        xmstr = XmStringCreateLocalized("1");
	legend_labelW[2] = XtVaCreateManagedWidget("framelabel",
                xmLabelWidgetClass, form,
                XmNrightAttachment,  XmATTACH_FORM,
		XmNlabelString, xmstr,
                NULL);
	XmStringFree( xmstr );
	
        legend_labelW[0] = XtVaCreateManagedWidget("filelabel",
                xmLabelWidgetClass, form,
                XmNleftAttachment,  XmATTACH_POSITION,
                NULL);

	legend_labelW[1] = XtVaCreateManagedWidget("grouplabel",
                xmLabelWidgetClass, form,
                XmNleftAttachment,  XmATTACH_POSITION,
                NULL);
}

/*=====================================================================*/

void display_legend ( int type )
{
char fname[80], name[100];
XmString	xmstr;
/*---------------------------------------------------------------------*/

	switch ( type ) {

	    case FILENAME:
			if ( MetaFile[0] == '\0' ) return;

			strcpy(name, "File: ");
			
			get_laststr( MetaFile, fname );
			strcat(name, fname);
			xmstr = XmStringCreateLocalized(name);
			XtVaSetValues(fileLabelW, 
			    XmNlabelString, xmstr,
			    NULL);
			XmStringFree( xmstr );
#ifdef LOGGING
			strcpy ( filstr, name );
#endif
			break;

	    case GROUPNAME:
			if ( (!GroupLoadFlag) | 
				(SelectGroupNo < 1 ) ) return;

			strcpy(name, "Group: ");
			strcat(name, GroupList[SelectGroupNo - 1].groupname);
			break;
	}
	display_legend_message( type, name );
}

/*=====================================================================*/

void display_legend_message ( int type, char *message )
{
XmString	xmstr;

#ifdef LOGGING
int		lenm, itype, iy, im, id, ih, in, is, ij, ier;
char            tmzn[4];
#endif
/*---------------------------------------------------------------------*/

	xmstr = XmStringCreateLocalized(message);
	XtVaSetValues( legend_labelW[type], 
	                  XmNlabelString, xmstr,
			  NULL );
	XmStringFree( xmstr );

#ifdef LOGGING
/*
 * Write the information about this group or frame
 * to the log file.
 */
	cst_lstr ( message, &lenm, &ier );
	if  ( type == 1 && lenm != 0 )  {
	    itype = 0;
	    css_date ( &itype, &iy, &im, &id, &ih, &in, &is, &ij, 
	               tmzn, &ier );
	    fprintf ( logptr, "%04d%02d%02d/%02d%02d\t%s\t%s\n",
		      iy, im, id, ih, in, filstr, message );
	}
#endif
}

/*=====================================================================*/

void get_laststr ( char *inpstr, char *outstr )
{
char *tempstr, tempinstr[256];
/*---------------------------------------------------------------------*/
	strcpy(tempinstr, inpstr);

	if ( strstr(tempinstr, "/") != NULL ) {
		tempstr = strtok(tempinstr, "/");
		strcpy(outstr, tempstr);

		while ( (tempstr = strtok(NULL, "/")) != NULL ) 
			strcpy(outstr, tempstr);
	}
}
