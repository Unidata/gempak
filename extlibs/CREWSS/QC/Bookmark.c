#include "gui.h"

/* ARGSUSED */
void BookmarkCb(Widget wid, XtPointer client, XtPointer call)
/**********************************************************
 * Save the current map state:                            *
 *   The current obs (View and Priority)                  *
 *   Viewed/Unviewed state of obs on the map              *
 * Valid_Bookmark is set to false when a new map is       *
 * created (Zoom, WorldMap, and Autozoom)                 *
 *********************************************************/
{
   int i;

   Valid_Bookmark = True;
   Bookmark_Current_View_Obs = Current_View_Obs;
   Bookmark_Current_Pri_Obs = Current_Pri_Obs;
   printf("Bookmark current state--Number of obs on map = %d\n", NumOnMap);
   for (i=0; i<NumOnMap; i++) {
      /* printf("   %d  %d\n", *(Viewed + i*2), *(Viewed + i*2 + 1)); */
      *(Bookmark_Viewed + i*2) = *(Viewed + i*2);
      *(Bookmark_Viewed + i*2 + 1) = *(Viewed + i*2 + 1);
   }
   XtVaSetValues( BookmarkWidget, XmNlabelString, XmStringCreateLocalized("Bookmark Set"));
}

/*===============================================================================*/
/* ARGSUSED */
void ReturnToBookmarkCb(Widget wid, XtPointer client, XtPointer call)
/**********************************************************
 * Return to the bookmarked map state                     *
 *   The current obs (View and Priority)                  *
 *   Viewed/Unviewed state of obs on the map              *
 *********************************************************/
{
   int i;

   if (Valid_Bookmark) {
      printf("Valid bookmark--Number of obs on map = %d\n", NumOnMap);
      if ((Current_View_Obs < (int)NumObs) && (Current_View_Obs >= 0)) Previous_View_Obs = Current_View_Obs;
      if ((Current_Pri_Obs < (int)NumObs) && (Current_Pri_Obs >= 0)) Previous_Pri_Obs = Current_Pri_Obs;
      Current_View_Obs = Bookmark_Current_View_Obs;
      Current_Pri_Obs = Bookmark_Current_Pri_Obs;
      for (i=0; i<NumOnMap; i++){
         /*printf("   %d  %d\n", *(Bookmark_Viewed + i*2), *(Bookmark_Viewed + i*2 + 1)); */
         *(Viewed + i*2) = *(Bookmark_Viewed + i*2);
         *(Viewed + i*2 + 1) = *(Bookmark_Viewed + i*2 + 1);
      }
      if (Current_View_Obs != -1)
         DisplayObs(Current_View_Obs);
      else
         DisplayObs(Current_Pri_Obs);
   } else
       printf("Invalid bookmark!\n");
   XtVaSetValues( BookmarkWidget, XmNlabelString, XmStringCreateLocalized("No Bookmark Set"));

}
