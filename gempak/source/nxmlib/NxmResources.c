#include "geminc.h"
#include "gemprm.h"

void NxmResources ( Widget widget );

void NxmResources ( Widget widget )
{
    unsigned char focus;
    Boolean enter;
    Dimension bwd, hgt, wid;
    Dimension bot, lft, rgt, top;
    unsigned char aln;
/*------------------------------------------------------------------------*/

    XtVaGetValues(widget,
                 XmNkeyboardFocusPolicy, &focus,
                 XmNhighlightOnEnter, &enter,
                 NULL);
   printf("Widget:\nfocus = %d, enter = %d\n", focus, enter);

/*
 * Obtain Core resources
 */
    XtVaGetValues(widget,
                XmNheight, &hgt,
                XmNwidth, &wid,
                XmNborderWidth, &bwd,
                NULL);
    printf("Widget:  Core resources -\nhgt = %d, wid = %d, borderWidth = %d\n", hgt, wid, bwd);

/*
 * Obtain XmLabel resources
 */
    XtVaGetValues(widget,
                XmNalignment, &aln,
                XmNmarginBottom, &bot,
                XmNmarginHeight, &hgt,
                XmNmarginLeft, &lft,
                XmNmarginRight, &rgt,
                XmNmarginTop, &top,
                XmNmarginWidth, &wid,
                NULL);
    printf("Widget:  XmLabel XmNalignment = %d\n", aln);
    printf("Widget:  XmLabel margin resources -\nhgt = %d, wid = %d, lft = %d, rgt = %d, top = %d, bot = %d\n", hgt, wid, lft, rgt, top, bot);
}
