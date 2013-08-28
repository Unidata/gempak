/*  panel.h  */

typedef struct  {
        int     columns;  /* number of columns in the panel */
        int     rows;     /* number of rows in the panel */
        int     selectCol;  /* selected column in the panel */
        int     selectRow;     /* selected row in the panel */
        }       Panel_str;

#ifdef PANEL

Panel_str Mpanel;
Boolean	  MpanelMode = 0;

Boolean       ValidTimeSet;
char          ValidTimeString[500][15];

Widget rowText, colText, MpanelBb, MpanelSelFm, MultipanelFrame, MpanelRc;

#else

extern Panel_str Mpanel;
extern Boolean MpanelMode;

extern Boolean ValidTimeSet;
extern char    ValidTimeString[][15];

extern Widget rowText, colText, MpanelBb, MpanelSelFm, MultipanelFrame, MpanelRc;

#endif
