#include "geminc.h"
#include "gemprm.h"
#include "cpgcmn.h"


extern PrdRec prec;    /* standard product generation record */
extern int active_node;
extern int samp_factor;

extern Widget prod_descr_label;
extern Widget prod_name;
extern Widget prod_descr;
extern Widget status_box;
extern Widget wheel_dlg;

/* ARGSUSED */
void vwheelCB ( Widget w, long clnt, XtPointer call )
/************************************************************************
 * VWHEELCB								*
 *									*
 * This is the call back function for the wheel handler dialog box.	*
 *									*
 * vwheelCB( w, clnt, call )						*
 *									*
 * Input parameters:							*
 *	w		Widget		Widget activated in the callback*
 * 	clnt		long		Data sent			*
 * 	call		XtPointer	Data related to call		*
 *									*
 * Output parameters:							*
 **									*
 * Log:									*
 * E. Wehner/EAi	6/96		Created				*
 * E. Wehner/EAi	11/96	Put bounding on strcpy			*
 * E. Wehner/EAi	12/96	Remove null parameter to rlst		*
 * T. Piper/SAIC	1/02	Fixed memory leak; freed str		*
 * R. Tian/SAIC         05/02   Renamed pgcmn.h to be cpgcmn.h          *
 ***********************************************************************/
{
    Arg al[10];
    Cardinal ac;
    int istat;
    char *str;
    char p[2];
    Boolean set;

    ac  = 0;
    p[0] = '\0';
 
    XtSetArg(al[ac], XmNvalue, &set); ac++;
    
    XtGetValues(w, al, ac);

    switch (clnt)
    {
      case 1:       /* EXTRACT FAX INFORMATION FROM THE WHEEL LIST */
        XtManageChild(prod_descr_label);
	str = XmTextGetString(prod_name);
        switch (active_node)
        {
          case 1:    /* local */
           strncpy(prec.pwheel, str, MAX_NAMESZ-1);
/* first, have to get the settings from the file for this product */
            pg_rlst(str, p, &prec, &istat);
            break;
          case 2:    /* local as 6 bit */
           strncpy(prec.pwheel, str, MAX_NAMESZ-1);
/* first, have to get the settings from the file for this product */
            pg_rlst(str, p, &prec, &istat);
            break;
          case 3:    /* OSO ...tbd */
            break;
          case 4:    /* plot server 6 bit */
            pg_rindex(str, &prec, &istat);
            break;
          default:   /* ???? */
            break;
        }
	XtFree(str);
        if (istat == 0)
        {
            XtManageChild(prod_descr);
            XmTextSetString(prod_descr, prec.pdesc);
            XmTextSetString(status_box, "");
        }
        else
        {
            XmTextSetString(status_box, "Product not found");
        }
        break;
      case 2:       /* LOAD AND DISPLAY THE BITMAP FILE */
        istat = vfindload(active_node, &prec);
        if (istat != 0)
        {
            XmTextSetString(status_box, "Map invalid. Try another subset # ");

        }
        else
        {
            XtUnmanageChild(wheel_dlg);
            set_scroll_sz(prec.xsize/(Cardinal)samp_factor, 
			  prec.ysize/(Cardinal)samp_factor);
        }
        break;
      case 3:       /* QUIT>>>>> */
        XtUnmanageChild(wheel_dlg);
        break;
      default:
        break;
    }
}
