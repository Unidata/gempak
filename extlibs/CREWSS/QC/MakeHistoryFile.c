#include "gui.h"


/************************************************************************
 * MakeHistoryFile                                                      *
 *                                                                      *
 * Retrieve all obs in the obs database for the specified callsign.     *
 * sort and store in /tmp/history_<callsign>                            *
 *                                                                      *
 *                                                                      *
 ***********************************************************************/

void MakeHistoryFile( Widget button, char *callsign )
{
   char command[100];
/*----------------------------------------------------------------------*/

/*
 * Turn function button red while building history file.
 */

   XtVaSetValues( button, XmNforeground, Red.pixel, NULL);
   XmUpdateDisplay( button );

 /* concatonate the db files for this callsign.  use grep so it attaches the file name,
    which includes the date.  Sort and save in a file. */

   sprintf(command, "cat $QCFILES/*_[0-1][0-8]/%s | sort > /tmp/history_%s", callsign, callsign);
   system(command);
   strcpy(History_Filename, "/tmp/history_");
   strcat(History_Filename, callsign);

/*
 * Turn function button black again.
 */

   XtVaSetValues(button, XmNforeground, Black.pixel, NULL);
   XmUpdateDisplay( button );
}   

/*============================================================================*/

float f_abs(float f)
{
float new_f;
/*----------------------------------------------------------------------------*/
   new_f = f;
   if (f < 0) new_f = -f;
   return new_f;
}
