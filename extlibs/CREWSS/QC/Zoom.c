#include "gui.h"


/*ARGSUSED*/
void ZoomCb( Widget w, XtPointer data, XtPointer cb_data)
/************************************************************************
 * ZoomCb                                                               *
 *                                                                      *
 * This routine is callback for the Zoom button.  It allows the user to *
 * select a box, and then  zooms in on the selected portion of the map. *
 *                                                                      *
 ***********************************************************************/
{
float x1_ndc, x2_ndc, y1_ndc, y2_ndc;
int   ret, i, badbox;

/*---------------------------------------------------------------------*/

   Turn_Off_Select_Group(); /* If Select Group mode is on, turn it off */


/*
 *  Invalidate any saved bookmark, since we are creating a new map.
 */

    if (Valid_Bookmark) {
       Valid_Bookmark = False;
       XtVaSetValues( BookmarkWidget, XmNlabelString, XmStringCreateLocalized("No Bookmark Set"));
    }

    XtVaSetValues( w, XmNforeground, Red.pixel, NULL); /* change button color to red */
    XmUpdateDisplay( w );


/*
 * Retrieve the user-selected boundry box 
 */

    badbox = Get_Box( 0, &x1_ndc, &x2_ndc, &y1_ndc, &y2_ndc);
    if (!badbox) {

/*
 *  Turn on New map and Zoomed flags
 */

       NewMap = True;       
       Zoomed = True;


      /* printf("New box normalized to window: (%f,%f) (%f,%f)\n",x1_ndc,y1_ndc,x2_ndc,y2_ndc); */

/*
 * convert the NDC coordinates of the chosen box to lat/lon
 * points and store in MapBnd structure.
 */


      i=1;
      NhlNDCToData( Mapid, &x1_ndc, &y1_ndc, i, &MapBnd.lon[0], &MapBnd.lat[0],  NULL, NULL, &ret, &missing);
      NhlNDCToData( Mapid, &x2_ndc, &y2_ndc, i, &MapBnd.lon[1], &MapBnd.lat[1],  NULL, NULL, &ret, &missing);

      /* printf("Zoom: MapBnd values: lat range: %f->%f lon range: %f->%f\n", &MapBnd.lat[0], &MapBnd.lat[1], &MapBnd.lon[0], &MapBnd.lon[1]); */


/* redraw the map */

      NhlRLClear(setrlist);
      NhlFrame(Wkid);
      NhlRLSetString(setrlist,NhlNmpProjection, "CylindricalEquidistant");
      NhlRLSetString(setrlist,NhlNmpLimitMode,"NDC");
      NhlRLSetFloat(setrlist,NhlNmpLeftNDCF,x1_ndc);
      NhlRLSetFloat(setrlist,NhlNmpRightNDCF,x2_ndc);
      NhlRLSetFloat(setrlist,NhlNmpTopNDCF,y2_ndc);
      NhlRLSetFloat(setrlist,NhlNmpBottomNDCF,y1_ndc);

      NhlSetValues(Mapid, setrlist);

/*
 * and any contours currently displayed
 */

       Replot(1);
   }

   XtVaSetValues( w, XmNforeground, Black.pixel, NULL); /* change button color to black */
   XmUpdateDisplay( w );

}
