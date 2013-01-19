#include "geminc.h"
#include "cpgcmn.h"


#define OK 1
#define CANCEL 2

extern XmStringCharSet char_set;
extern Widget exit_dlg;    /* exit box dialog */
extern Widget rastop_dlg;
extern int active_node;
extern char raster_fname[120];
extern int samp_factor;
extern PrdRec prec;    /* standard product generation record */
extern Pixmap pmap;


/* ARGSUSED */
void vexitCB ( Widget w, long clnt,  XtPointer call )
/************************************************************************
 * vexitCB								*
 *									*
 * This is the call back function for the exit dialog box.		*
 *									*
 * vexitCB  ( w, clnt, call )						*
 *									*
 * Input parameters:							*
 *	w		Widget	Widget that activated the callback	*
 * 	clnt		long	Passed in number (cancel/exit)		*
 * 	call		XtPointer	Structure of info		*
 *									*
 **									*
 * Log:									*
 *	E.Wehner/EAI	6/96		Created			 	*
 * 	R. Tian/SAIC    05/02   Renamed pgcmn.h to be cpgcmn.h          *
 ***********************************************************************/
{
    switch(clnt)
    {
      case OK:
        exit(0);
        break;
      case CANCEL:
        XtUnmanageChild(exit_dlg);
        break;
    }
}

/*=====================================================================*/

void vraster_openCB ( Widget w, long clnt, 
			XmSelectionBoxCallbackStruct *call )
/************************************************************************
 * vraster_openCB							*
 *									*
 * This is the call back function for the raster file open dialog box	*
 *									*
 * vraster_openCB  ( w, clnt, call )					*
 *									*
 * Input parameters:							*
 *	w		Widget	Widget that activated the callback	*
 * 	clnt		long	Passed in number (cancel/exit)		*
 * 	*call		XmSelectionBoxCallbackStruct  Structure of info	*
 *									*
 **									*
 * Log:									*
 * E.Wehner/EAI		6/96	Created				 	*
 * T. Piper/SAIC	2/02	Freed ss				*
 ***********************************************************************/
{
    int mirror = 1;
    int flip = 1;
    char *ss;
    switch(clnt)
    {
      case OK:
        XtUnmanageChild(rastop_dlg);


        XmStringGetLtoR(call->value, char_set, &ss);

        vfiledisp(ss, prec.xsize, prec.ysize, samp_factor,mirror, flip, &pmap);
        strncpy(raster_fname, ss, sizeof(raster_fname));
	XtFree(ss);
        
        break;
      case CANCEL:
        break;
    }
    XtUnmanageChild(w);
}

/*=====================================================================*/
/* ARGSUSED */
void vnodeCB ( Widget w, long clnt, XtPointer call )
/************************************************************************
 * vnodeCB								*
 *									*
 * This is the call back function for the node selection item 		*
 *									*
 * vnodeCB  ( w, clnt, call )						*
 *									*
 * Input parameters:							*
 *	w		Widget	Widget that activated the callback	*
 * 	clnt		long	Passed in number (cancel/exit)		*
 * 	call		XtPointer	Structure of info		*
 *									*
 **									*
 * Log:									*
 *	E.Wehner/EAI	6/96		Created			 	*
 ***********************************************************************/
{
    Arg al[10];
    Cardinal ac;
    Boolean set;

    ac  = 0;
 
    XtSetArg(al[ac], XmNvalue, &set); ac++;
    
    XtGetValues(w, al, ac);

    active_node = (int)clnt;

}

/*=====================================================================*/
/* ARGSUSED */
void vsizeCB ( Widget w, long clnt, XtPointer call )
/************************************************************************
 * vsizeCB                                                              *
 *                                                                      *
 * This is the call back function for the paper size selection item	*
 *                                                                      *
 * vsizeCB  ( w, clnt, call )                               		*
 *                                                                      *
 * Input parameters:                                                    *
 *      w       	Widget	Widget that activated the callback      *
 *      clnt		long	Passed in number (cancel/exit)          *
 *      call		XtPointer	Structure of info       	*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *      E.Wehner/EAI    6/96            Created                         *
 ***********************************************************************/
{
    Arg al[10];
    Cardinal ac;
    Boolean set;

    ac  = 0;

    XtSetArg(al[ac], XmNvalue, &set); ac++;

    XtGetValues(w, al, ac);

    prec.pagesz  = (int)clnt;
}
