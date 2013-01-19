#include "geminc.h"
#include "cpgcmn.h"

extern Widget draw_area;
extern Widget exit_dlg;    /* exit box dialog */
extern Widget rastop_dlg;
extern Widget wheel_dlg;
extern Widget size_box;
extern Widget size1;
extern Widget size2;
extern Widget size_prompt_dlg;
extern Widget fontsave_dlg;
extern int samp_factor;
extern int printing;       /* whether we are printing or not */
extern int raster_up;

/* ARGSUSED */
void vmenuCB ( Widget w, String clnt, XtPointer call )
/************************************************************************
 * vmenucb								*
 *									*
 * This program is the call back for the menu bar in the product review	*
 *									*
 * vmenucb  ( w, clnt, call )						*
 *									*
 * Input parameters:							*
 *  w		Widget		Widget causing the callback		*
 *  clnt	String		Passed in string related to the call	*
 *  call	XtPointer	Structure of widget information		*
 *									*
 * Output parameters:							*
 **									*
 * Log:									*
 * E.Wehner/EAi		6/96	Created					*
 ***********************************************************************/
{
/* 
 * Examine the client data.  Perform action based on the string
 * sent to this call back.
 */

     if (strcmp(clnt, "Quit" ) == 0)
    {
        XtManageChild(exit_dlg);
    }
    else
    {
        if (strcmp(clnt, "ClearScrn" ) == 0)
        {
            raster_up = 0;        /* set to don't display the raster anymore */
            XClearArea(XtDisplay(draw_area), XtWindow(draw_area), 0, 0, 0, 0, 
                             False);
        }
        else
        {
          if (strcmp(clnt, "Open" ) == 0)
          {
              XtManageChild(rastop_dlg);

          }
          else
          {
            if (strcmp(clnt, "sample1") == 0)
            {
              samp_factor = 1;
            }
            else
            {
              if (strcmp(clnt, "sample2") == 0)
              {
                samp_factor = 2;
              }
              else
              {
                if (strcmp(clnt, "sample3") == 0)
                {
                  samp_factor = 3;
                } 
                else
                {
                  if (strcmp(clnt, "sample4") == 0)
                  {
                    samp_factor = 4;
                  }
                  else
                  {
                    if (strcmp(clnt, "Fax_Open") == 0)
                    {
                      printing = FALSE;
                      XtManageChild(wheel_dlg);
                    }
		    else
                    {
                      if (strcmp(clnt, "Print") == 0)
                      {
                        printing = TRUE;
                        XtManageChild(wheel_dlg);
                        XtManageChild(size_box);
                        XtManageChild(size1);
                        XtManageChild(size2);
                      }
                      else
                      {
                          if (strcmp(clnt, "Fontsel" ) == 0)
                          {
                              XtManageChild(size_prompt_dlg);
                          }
                          else
                          {
                              if (strcmp(clnt, "Savefont" ) == 0)
                              {
                                  XtManageChild(fontsave_dlg);
                              }
                          }
                      }
                    }
                  }
                } 
              }
            }
          }
        }
    }
}
