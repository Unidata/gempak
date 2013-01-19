#include "geminc.h"
#include "gemprm.h"
#include "xwcmn.h"
#include "Nxm.h"
#include "NxmInit.h"
#include "NxmColorEdit.h"

/*
 *  Private functions
 */
void _NxmColorListSelect (   Widget, XtPointer,
                             XmListCallbackStruct  *call );
/************************************************************************
 * NxmColorNameLists.c							*
 *									*
 * CONTENTS:								*
 *	NxmColorNamelistsCreate						*
 *	_NxmColorListSelect						*
 ***********************************************************************/

void NxmColorNamelistsCreate ( Widget parent, char *colrname_file )
/************************************************************************
 * NxmColorNamelistsCreate						*
 *									*
 * NxmColorNamelistsCreate ( parent, colrname_file )			*
 *									*
 * Input parameters:							*
 *	parent		Widget						*
 *	*colrname_file	char						*
 **									*
 * Log:									*
 * T. Piper/SAIC	01/04	replaced fopen with cfl_tbop		*
 * T. Piper/SAIC	01/04	replaced fscanf w/cfl_trln, ssscanf	*
 ***********************************************************************/
{
Widget    list;
FILE	  *colrfile;
int	  ii, ier, count;
XmString  *xmstr;
char      buff[128], cname[256][20];
Arg	  args[10];
Cardinal  argcnt;

/*--------------------------------------------------------------------*/

        colrfile = cfl_tbop(colrname_file, CLR_DIR, &ier);
        if ( colrfile == NULL  ||  ier != 0 ) { 
            printf("Warning:  Cannot find color list file %s \n",
                        colrname_file);
            exit(0);
        }

	count = 0;
        while ( !feof(colrfile) ) {
            cfl_trln(colrfile, sizeof(buff), buff, &ier);
            if ( ier == 0 ) {
                sscanf(buff, "%s",  cname[count] );
                count++;
            }
        }

        fclose(colrfile);

  	xmstr = (XmString *) XtMalloc(sizeof(XmString) * (size_t)count);

  	for(ii = 0; ii < count; ii++) {
             xmstr[ii] = XmStringCreateLocalized(cname[ii]);
	}

  	argcnt=0;
  	XtSetArg(args[argcnt], XmNitems, xmstr); argcnt++;
  	XtSetArg(args[argcnt], XmNitemCount, count ); argcnt++;
  	XtSetArg(args[argcnt], XmNselectionPolicy, XmSINGLE_SELECT); argcnt++;
  	XtSetArg(args[argcnt], XmNscrollingPolicy, XmAUTOMATIC); argcnt++;
  	XtSetArg(args[argcnt], XmNvisibleItemCount,5 ); argcnt++;
  	list = XmCreateScrolledList(parent,"color_list",args, argcnt);
	XtManageChild( list );

  	XtAddCallback(list, XmNsingleSelectionCallback, 
		(XtCallbackProc)_NxmColorListSelect,
                (XtPointer)NULL);

  	for (ii = 0; ii < count; ii++ ) {
  	    XmStringFree(xmstr[ii]);
	}
	XtFree((XtPointer)xmstr);
}

/*=====================================================================*/
/* ARGSUSED */
void _NxmColorListSelect ( Widget wdgt, XtPointer clnt, 
				XmListCallbackStruct *call )
/************************************************************************
 * _NxmColorListSelect							*
 *									*
 * _NxmColorListSelect ( wdgt, clnt, call )				*
 *									*
 * Input parameters:							*
 *	wdgt		Widget						*
 *	data		XtPointer					*
 *	*call		XmListCallbackStruct				*
 **									*
 ***********************************************************************/
{
char *text;

/*---------------------------------------------------------------------*/

    XmStringGetLtoR(call->item, XmFONTLIST_DEFAULT_TAG, &text);

    if ( text != NULL ) {
       	XStoreNamedColor(gemdisplay, gemmap, text, 
		NXMcurrentColor.pixel,
		DoRed | DoGreen | DoBlue);

	XtFree(text);

	XQueryColor(gemdisplay, gemmap, &NXMcurrentColor);

	NxmColorSetSliders();

    }
}
