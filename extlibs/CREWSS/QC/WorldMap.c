#include "gui.h"

/* ARGSUSED */
void WorldMapCb(Widget wid, XtPointer client, XtPointer call)
/************************************************************************
 * WorldMapCb                                                           *
 *                                                                      *
 * This routine is callback for the WorldMap button.  It redraws the    *
 * full world map and replots all obs.                                  *
 *                                                                      *
 *                                                                      *
 ***********************************************************************/
{

   Turn_Off_Select_Group(); /* If Select Group mode is on, turn it off */


/*
 *  Turn on New map flag, and turn off zoomed and valid bookmark flags
 */

    NewMap = True;       
    Zoomed = False;
    AutoZoomed = False;
    if (Valid_Bookmark) {
       Valid_Bookmark = False;
       XtVaSetValues( BookmarkWidget, XmNlabelString, XmStringCreateLocalized("No Bookmark Set"));
    }


/* turn off contours and vectors */

   if (Contour_SLP_On) {
      NhlRemoveOverlay(Mapid,Contour_SLP_id,-1);
      NhlDestroy(Contour_SLP_id);
      Contour_SLP_On = False;
   }
   if (Contour_SST_On) {
      NhlRemoveOverlay(Mapid,Contour_SST_id,-1);
      NhlDestroy(Contour_SST_id);
      Contour_SST_On = False;
   }
   if (Contour_Temp_On) {
      NhlRemoveOverlay(Mapid,Contour_Temp_id,-1);
      NhlDestroy(Contour_Temp_id);
      Contour_Temp_On = False;
   }
   if (Wind_Vectors_On) {
      NhlRemoveOverlay(Mapid,Wind_id,-1);
      Wind_Vectors_On = False;
      NhlDestroy(Wind_id);
   }

    NhlFrame(Wkid);
    NhlRLClear(setrlist);
    NhlRLSetFloat(setrlist,NhlNmpCenterLatF, 0.);
    NhlRLSetFloat(setrlist,NhlNmpCenterLonF, 0.);
    NhlRLSetString(setrlist,NhlNmpProjection, "CylindricalEquidistant"); 
    NhlRLSetString(setrlist,NhlNmpLimitMode,"MaximalArea");
    NhlRLSetFloat(setrlist,NhlNvpXF,0.0);
    NhlRLSetFloat(setrlist,NhlNvpYF,1.0);
    NhlRLSetFloat(setrlist,NhlNvpWidthF,1.0);
    NhlRLSetFloat(setrlist,NhlNvpHeightF,1.0);
    NhlSetValues(Mapid, setrlist);

    MapBnd.x[0] = 0.; MapBnd.y[0] = 1.;
    MapBnd.x[1] = 1.; MapBnd.y[1] = 0.;
    MapBnd.lat[0] = 90.; MapBnd.lon[0] = -180.;
    MapBnd.lat[1] = -90.; MapBnd.lon[1] = 180.; 

    Replot(1);
}
