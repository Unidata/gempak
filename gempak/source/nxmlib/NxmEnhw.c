#include "geminc.h"
#include "gemprm.h"
#include "Nxm.h"

/*
 * maximum number of enhancement tables
 */
#define ENHANCE_NAME	    40
#define FNAME_LEN	    256
#define VISIBLE_ITEM	    15	

#define ENHANCE_TBL	    "enhance.tbl"

struct enhance_t{
	int  type; 		/* type of color bank, 1=sat, 2=rad */
	char name[ENHANCE_NAME];/* title name shown in enhance list */
};
static struct enhance_t *_enhanceTbl;


static Widget _enhanceW;	/* color enhancement popup */
static Widget _enhList;		/* color enhancement popup */
static int    _enhCount;        /* total # of records in the table */

static int   _enhType;	/* last enhancement type */

static char   _curLutSAT[ENHANCE_NAME];	/* current lutfile name */
static char   _curLutRAD[ENHANCE_NAME];	/* current lutfile name */

/*
 *  Private functions
 */
void NxmEnhw_closeCb  ( Widget, XtPointer, XtPointer );
void NxmEnhw_singleCb ( Widget, XtPointer, XmListCallbackStruct* );
void (*_subEnhFunc)(void) = NULL;	/* additional func called with 
							singleCb */
void (*_subEnhFunc2)(char *) = NULL;	/* second additional func
						called with singleCb */


/************************************************************************
 * NxmEnhw.c                                                            *
 *                                                                      *
 * This module creates the color enhancement popup and defines the      *
 * callback functions.                     				*
 *                                                                      *
 * CONTENTS:                                                            *
 *  NxmEnhw_create()  creates color enhancement popup window.       	*
 *  NxmEnhw_popup()   pops up the enhancement list.       		*
 *  NxmEnhw_update()   updates the enhancement list.       		*
 *  NxmEnhw_setLutfile()   sets the lookup table name.       		*
 *                                                                      *
 *  NxmEnhw_getLutfile() return the current lutfil name.   		*
 *                                                                      *
 *  NxmEnhw_singleCb() callback for the single click in the list.   	*
 *  NxmEnhw_closeCb() callback for the close button.          		*
 ***********************************************************************/

/*=====================================================================*/

Widget NxmEnhw_create ( Widget parent, void (*func)(void),
					void (*func2)(char *) )
/************************************************************************
 * NxmEnhw_create                                                       *
 *                                                                      *
 * This function creates a color enhancement popup window.              *
 *                                                                      *
 * Widget NxmEnhw_create(parent, func, func2)                           *
 *                                                                      *
 * Input parameters:                                                    *
 *  parent       Widget  parent widget ID                               *
 *  *func()      void    additional func called with singleCb           *
 *  *func2()     void    second additional func called with singleCb    *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *       NxmEnhw_create	Widget        Widget ID of the enhance popup    *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * Chien Lin/EAI	05/93                                       	*
 * Chien Lin/EAI	07/95	clean up in freeing xm_string        	*
 * Chien Lin/EAI	09/95	modify layout of enhance list popup  	*
 * C. Lin/EAI		12/95	clean up                                *
 * C. Lin/EAI		06/96	make it Nxm library function 		*
 * C. Lin/EAI		01/97	increase fname size(S. Chiswell/Unidata)*
 * C. Lin/EAI		04/97	initialize _curLutfile                  *
 * C. Lin/EAI		12/97	rewrite for adding radar enhance 	*
 * I. Durham/GSC	05/98	changed underscore decl. to an include	*
 * S. Jacobs/NCEP	10/99	Added second function to call		*
 * T. Piper/SAIC	07/06	Changed i to ii to aid maintenance	*
 ***********************************************************************/
{
Widget        frame, button;
char 	      buffer[256];
FILE          *fp;
int           ii, count, ier;
Cardinal      argcnt;
Arg           args[10];
XmString      xmstr2;
char	      message[] = "Enhancement table is missing.";
/*---------------------------------------------------------------------*/

        _enhanceW = XmCreateFormDialog(parent,
                        "enhance_list_toplevel", NULL, 0);

/*
 * read in the enhancement table file 
 */
        fp = cfl_tbop(ENHANCE_TBL, "luts", &ier);
        if (ier != 0) {
            NxmWarn_show(XtParent(_enhanceW), message);
        }
        else {

/*
 * figure out the total # of records
 */
            count = 0;
            while ( !feof(fp) ) {
                cfl_trln(fp, 256, buffer, &ier);
                if ( ier == 0 )  count++;
            }
        }

        _enhCount = count;

        if ( count > 0 ) {

	    _enhanceTbl = (struct enhance_t *)
			malloc((size_t)count * sizeof(struct enhance_t));

            rewind(fp);

            ii = 0;
            while ( ii < count ) {

                cfl_trln(fp, 256, buffer, &ier);

                if ( ier == 0 ) {
                    sscanf(buffer, "%d %s", &(_enhanceTbl[ii].type),
                        	_enhanceTbl[ii].name);
                    ii++;
                }
            }
        }

        if ( fp )
                fclose(fp);

        frame = XtVaCreateWidget("enhanceClose",
		xmFrameWidgetClass, _enhanceW,
                XmNtopAttachment,   XmATTACH_FORM, 
                XmNleftAttachment,  XmATTACH_FORM, 
                XmNleftOffset,      5, 
                XmNrightAttachment, XmATTACH_FORM, 
                XmNrightOffset,     5, 
                NULL);
/*
 * create a scrolled list for enhancement table
 * only a single selection is allowed
 */
        argcnt = 0;
        XtSetArg(args[argcnt], XmNselectionPolicy, XmSINGLE_SELECT); argcnt++;
        _enhList = XmCreateScrolledList(frame, "enhanceList",
                                         args,
                                         argcnt);
        XtManageChild(_enhList);
        XtAddCallback(_enhList, XmNsingleSelectionCallback,
                 	(XtCallbackProc)NxmEnhw_singleCb, NULL);

        XtManageChild(frame);

/*
 * create close button
 */
	xmstr2 = XmStringCreateLocalized("Close");
        button = XtVaCreateManagedWidget("enhanceClose",
		xmPushButtonWidgetClass, _enhanceW,
                XmNlabelString,          xmstr2, 
                XmNtopAttachment,        XmATTACH_WIDGET, 
                XmNtopWidget,            frame, 
                XmNtopOffset,            10, 
                XmNleftAttachment,       XmATTACH_FORM, 
                XmNleftOffset,           10, 
                XmNbottomAttachment,     XmATTACH_FORM, 
                XmNbottomOffset,         10, 
                NULL);
	XmStringFree(xmstr2);

	XtAddCallback(button, XmNactivateCallback, 
                       	(XtCallbackProc)NxmEnhw_closeCb, 
			(XtPointer)NULL);

	if (func)
		_subEnhFunc = func;
	if (func2)
		_subEnhFunc2 = func2;

/*
 * set the initial color lookup table name (lutfile)
 */
	strcpy(_curLutSAT, "gray");
	strcpy(_curLutRAD, "DEFAULT");

	_enhType = 0;

	return(_enhanceW);

}

/*=====================================================================*/

void NxmEnhw_popup ( int ityp )
/************************************************************************
 * NxmEnhw_popup                                                        *
 *                                                                      *
 * This function pops up the color enhancement popup window.            *
 *                                                                      *
 * void NxmEnhw_popup(ityp)                                             *
 *                                                                      *
 * Input parameters:                                                    *
 *	ityp	int	data type	1=SAT, 2=RAD			*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *   Chien Lin/EAI      05/96                                       	*
 *   Chien Lin/EAI      12/97	add ityp and modification for RADAR     *
 * T. Piper/SAIC        07/06   Changed i to ii to aid maintenance      *
 * T. Piper/SAIC        07/06   Changed n to nn to aid maintenance      *
 ***********************************************************************/
{
int           ii, nn, ivis;
XmStringTable xm_strs;
XmString      xmstr;
/*---------------------------------------------------------------------*/

	if ( _enhCount <= 0 ) return;

	if ( ityp != _enhType ) {

	    xm_strs = (XmStringTable)XtMalloc((size_t)_enhCount*sizeof(XmString));

	    nn = 0;
	    for ( ii = 0; ii < _enhCount; ii++ ) {
	        if ( ityp == _enhanceTbl[ii].type ) { 
                    xm_strs[nn] = XmStringCreateLocalized(_enhanceTbl[ii].name);
		    nn++;
	        }
	    }

	    if ( nn < VISIBLE_ITEM )
	        ivis = nn;
	    else
	        ivis = VISIBLE_ITEM;

	    XmListDeleteAllItems(_enhList);

            XtVaSetValues(_enhList, 
		XmNvisibleItemCount, ivis,
		XmNitemCount,        nn,
		XmNitems,            xm_strs,
		NULL);

	    for ( ii = 0 ; ii < nn; ii++ ) {
	        XmStringFree(xm_strs[ii]);
	    }
	    XtFree( (XtPointer)xm_strs);

	    _enhType = ityp;
	}

/*
 * redisplay the selected item 
 */
	XmListDeselectAllItems(_enhList);
	if ( ityp == 1 )
	    xmstr = XmStringCreateLocalized(_curLutSAT);
	else
	    xmstr = XmStringCreateLocalized(_curLutRAD);
	XmListSelectItem(_enhList, xmstr, FALSE);
	XmListSetBottomItem(_enhList, xmstr);
	XmStringFree(xmstr); 

        XtManageChild(_enhanceW);
}

/*=====================================================================*/

void NxmEnhw_update ( int ityp )
/************************************************************************
 * NxmEnhw_update                                                       *
 *                                                                      *
 * This function updates the enhancement popup window if it is up.      *
 *                                                                      *
 * void NxmEnhw_update(ityp)                                            *
 *                                                                      *
 * Input parameters:                                                    *
 *	ityp	int	data type	1=SAT, 2=RAD			*
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *   Chien Lin/EAI      12/97                                       	*
 * S. Jacobs/NCEP	10/99	Added redisplay of widget for type=1	*
 ***********************************************************************/
{
XmString xmstr;
/*---------------------------------------------------------------------*/

        if ( ! XtIsManaged(_enhanceW) )
	    return;

	if ( ityp != _enhType ) {
            XtUnmanageChild(_enhanceW);
	    NxmEnhw_popup(ityp);
	}
	else if ( ityp == 1 ) {
/*
 * redisplay the selected item 
 */
	    XmListDeselectAllItems(_enhList);
	    xmstr = XmStringCreateLocalized(_curLutSAT);
	    XmListSelectItem(_enhList, xmstr, FALSE);
	    XmStringFree(xmstr); 
	}
	else if ( ityp == 2 ) {
/*
 * redisplay the selected item 
 */
	    XmListDeselectAllItems(_enhList);
	    xmstr = XmStringCreateLocalized(_curLutRAD);
	    XmListSelectItem(_enhList, xmstr, FALSE);
	    XmStringFree(xmstr); 
	}
}

/*=====================================================================*/

void NxmEnhw_setLutfile ( int ityp, char *lutfile )
/************************************************************************
 * NxmEnhw_setLutfile                                                   *
 *                                                                      *
 * This function returns the current lookup table file name.     	*
 *                                                                      *
 * void NxmEnhw_setLutfile(ityp, lutfile)                           	*
 *                                                                      *
 * Input parameters:                                                    *
 *  ityp 	int             data type, 1 = SAT, 2 = RAD          	*
 *  *lutfile	char            color lookup table file name            *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI		12/97						*
 * S. Jacobs/NCEP	11/99	Added check for DEFAULT lut		*
 * H. Zeng/EAI          10/02   Added check for DEFAULT lut for SAT     *
 * T. Lee/SAIC		06/04	Checked return code for im_qlut		*
 * T. Piper/SAIC	07/06	Added call to ST_NULL after im_qlut	*
 * T. Piper/SAIC	07/06	Added cst_uclc after second im_qlut	*
 ***********************************************************************/
{
	int	ier, lens;
	char	newlut[FNAME_LEN], *ptr;
/*---------------------------------------------------------------------*/

	if  ( ityp == 1 )  {
	    if ( strncasecmp(lutfile, "def",  3)  == 0 ) {
		im_qlut ( newlut, &ier, sizeof(newlut) );
		if ( ier == 0 ) {
		    st_null(newlut, newlut, &lens, &ier, sizeof(newlut),
							sizeof(newlut));
                    cst_uclc(newlut, newlut, &ier);  
		    ptr = strchr ( newlut, '.' );
		    if  ( ptr != NULL ) {
		        *ptr = '\0';
		    }
		    strcpy ( _curLutSAT, newlut );
		}
	    }
	    else {
		strcpy(_curLutSAT, lutfile);
	    }
	}
	else {
	    if ( strncasecmp(lutfile, "def", 3) == 0 )  {
		im_qlut ( newlut, &ier, sizeof(newlut) );
		if ( ier == 0 ) {
		    st_null(newlut, newlut, &lens, &ier, sizeof(newlut),               
							sizeof(newlut));
		    cst_uclc(newlut, newlut, &ier);
		    ptr = strchr ( newlut, '.' );
		    if  ( ptr != NULL ) {
		        *ptr = '\0';
		    }
		    strcpy ( _curLutRAD, newlut );
		}
	    }
	    else {
		strcpy(_curLutRAD, lutfile);
	    }
	}
}

/*=====================================================================*/

void NxmEnhw_getLutfile ( int ityp, char *lutfile )
/************************************************************************
 * NxmEnhw_getLutfile                                                   *
 *                                                                      *
 * This function returns the current lookup table file name.     	*
 *                                                                      *
 * void NxmEnhw_getLutfile(ityp, lutfile)                           	*
 *                                                                      *
 * Input parameters:                                                    *
 *  ityp 	int             data type, 1 = SAT, 2 = RAD          	*
 *                                                                      *
 * Output parameters:                                                   *
 *  *lutfile	char            color lookup table file name            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *   C. Lin/EAI      04/97                                              *
 *   C. Lin/EAI      12/97	add ityp for RADAR                      *
 ***********************************************************************/
{
    if ( ityp == 1 )
	strcpy(lutfile, _curLutSAT);
    else
	strcpy(lutfile, _curLutRAD);
}

/*=====================================================================*/
/* ARGSUSED */
void NxmEnhw_singleCb ( Widget wdgt, XtPointer clnt, 
				XmListCallbackStruct *list )
/************************************************************************
 * NxmEnhw_singleCb                                                     *
 *                                                                      *
 * Callback function for single clicking in color enhancement list.     *
 *                                                                      *
 * void NxmEnhw_singleCb(wdgt, clnt, list)                       	*
 *                                                                      *
 * Input parameters:                                                    *
 *	wdgt		Widget			widget ID		*
 *	clnt		XtPointer		client data, not used	*
 *  *list XmListCallbackStruct callback data struct                *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 *   C. Lin/EAI      02/93                                              *
 *   C. Lin/EAI      08/95        use im_lutf                           *
 *   C. Lin/EAI      12/95        clean up                              *
 *   C. Lin/EAI      01/97        increase lutfile array size 		*
 *   C. Lin/EAI      04/97 	  save the current lutfile name         *
 *   C. Lin/EAI      12/97 	  modified for RADAR         		*
 *   S. Jacobs/NCEP	10/99	Added call to second additional func	*
 *   E. Safford/SAIC	01/04	Moved _subEnhFunc2 to after lutfile save*
 ***********************************************************************/
{
int	iret;
char	*tmpstr, lutfile[FNAME_LEN];
/*---------------------------------------------------------------------*/

	XmStringGetLtoR(list->item, 
			XmFONTLIST_DEFAULT_TAG, &tmpstr);

	if ( strncasecmp(tmpstr, "gray", 4) == 0 ) 
	    strcpy( lutfile, "gray" );
	else
	    sprintf( lutfile, "%s.tbl", tmpstr );

	im_lutf( lutfile, &iret, strlen(lutfile) );
/*
 *  Save the lutfile
 */
	if ( _enhType == 1 )
	    strcpy( _curLutSAT, tmpstr );
	else
	    strcpy( _curLutRAD, tmpstr );
	XtFree(tmpstr);

/*
 *  Call the event funcs
 */
	if (_subEnhFunc2) {
	    _subEnhFunc2(lutfile);
   	}

	if (_subEnhFunc) {
	    _subEnhFunc();
	}
}

/*=====================================================================*/
/* ARGSUSED */
void NxmEnhw_closeCb ( Widget wdgt, XtPointer clnt, XtPointer call )
/************************************************************************
 * NxmEnhw_closeCb                                                      *
 *                                                                      *
 * Callback function for close button in color enhancement popup window.*
 *                                                                      *
 * void NxmEnhw_closeCb(wdgt, clnt, call)                               *
 *                                                                      *
 * Input parameters:                                                    *
 *	wdgt	Widget     widget ID                                    *
 *	clnt	XtPointer  not used                                     *
 *	call	XtPointer  not used                                     *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * C. Lin/EAI      02/93                                                *
 * C. Lin/EAI      12/95 	clean up                                *
 ***********************************************************************/
{
    XtUnmanageChild(_enhanceW);
}
