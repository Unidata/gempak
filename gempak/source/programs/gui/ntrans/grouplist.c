#include "geminc.h"
#include "interface.h"
#include "panel.h"


extern Widget   group_listW;
extern Widget   ggroup_listW;


void grouplist_Callback ( Widget, XtPointer, XmListCallbackStruct* );


/************************************************************************
 *									*
 * S. Wang/GSC	 01/97	re_grouping and clean up			*
 *									*
 ***********************************************************************/


/*=====================================================================*/

Widget create_grouplist ( Widget parent )
{
Arg         args[15];
Cardinal    argcnt;
Widget      widget, form;

/*---------------------------------------------------------------------*/

        form = XtVaCreateManagedWidget ("ListForm",
                                   xmFormWidgetClass,
                                   parent,
                                   NULL );

        XtVaCreateManagedWidget ("List of Groups",
                                   xmLabelWidgetClass,
                                   form,
                                   XmNtopAttachment, XmATTACH_POSITION,
                                   XmNtopPosition, 2,
                                   XmNleftAttachment, XmATTACH_POSITION,
                                   XmNleftPosition, 25,
                                   XmNrightAttachment, XmATTACH_POSITION,
                                   XmNrightPosition, 75,
                                   NULL );

        argcnt = 0;
        XtSetArg(args[argcnt], XmNtopAttachment, XmATTACH_POSITION); argcnt++;
        XtSetArg(args[argcnt], XmNtopPosition, 15); argcnt++;
        XtSetArg(args[argcnt], XmNbottomAttachment, XmATTACH_FORM); argcnt++;
        XtSetArg(args[argcnt], XmNleftAttachment, XmATTACH_FORM); argcnt++;
        XtSetArg(args[argcnt], XmNrightAttachment, XmATTACH_FORM); argcnt++;
        XtSetArg(args[argcnt], XmNselectionPolicy, XmSINGLE_SELECT); argcnt++;
        XtSetArg(args[argcnt], XmNscrollBarPlacement, XmBOTTOM_RIGHT); argcnt++;
        widget = XmCreateScrolledList(form,"group_list",args,argcnt);

        XtAddCallback(widget, XmNsingleSelectionCallback,
                             (XtCallbackProc)grouplist_Callback, NULL);

        XtManageChild(widget);
        return(widget);

}

/*=====================================================================*/

void add_grouplist ( void )
{
int             ii;
XmString        *xm_string;
/*---------------------------------------------------------------------*/

        xm_string = (XmString *)XtMalloc(sizeof(XmString) * (size_t)GroupNo);

        for ( ii = 0; ii < GroupNo; ii++)
                xm_string[ii] = XmStringCreateLocalized(GroupList[ii].groupname);

        XtVaSetValues(group_listW,
                        XmNitems, xm_string,
                        XmNitemCount, GroupNo,
                        NULL);

        XtVaSetValues(ggroup_listW,
                        XmNitems, xm_string,
                        XmNitemCount, GroupNo,
                        NULL);

	for ( ii = 0; ii < GroupNo; ii++) {
	    XmStringFree(xm_string[ii]);
	}
	XtFree((XtPointer)xm_string);
}

/*=====================================================================*/
/* ARGSUSED */
void grouplist_Callback ( Widget widget, XtPointer clnt, 
				XmListCallbackStruct *list )
{
    SelectGroupNo = list->item_position;
}
