#ifndef GUIGBL_H
#define GUIGBL_H
/************************************************************************
 * guigbl.h								*
 *									*
 ***********************************************************************/
#ifdef CREATE

/*
 *  Define NCAR stuff
 */

    int	appid;			/* id for application */
    int	Mapid;			/* id for map window */
    int	Wkid;			/* id for map Workstation */
    int	GKSWkid;		/* d for map (low level NCAR) */

    int	ObsWkid;		/* id for Obs Plot Workstation  */
    int	ObsPlotid;		/* id for Obs Plot Window */
    int	GKSObsPlotWkid;		/* id for Obs Plot (low level NCAR) */

    int	CruiseWkid;		/* id for Cruise Plot Workstation  */
    int	Cruiseid;		/* id for Criose Plot Window */
    int	GKSCruiseWkid;		/* id for Cruise Plot (low level NCAR) */

    int	getrlist, setrlist;	/* for setting and retrieving resource values */

/*
 *  Define  motif stuff: widgets, etc.
 */

    Widget  toplevel;		/* top level widget */
    XtAppContext   app;		/* Application context */
    Display *MapDisplay;		/* Display for map window */
    XEvent   Current_Event;		/* the current event */
    Colormap DefColorMap;		/* default color map */
    int	ScreenHeight;		/* max height of screen */
    int	WindowWidth;		/* width of application window */
    int	MenubarHeight;
    int	ObsWindowHeight;
    int	HistoryHeight;
    int	HistoryWidth;
    Widget	MenuButtons[14];	/* buttons on menubar */
    Widget	Map;			/* the map window */
    Widget	ObsWindow;		/* window for observation display & editing */
    Widget	ObsPlotWindow;		/* window for observation plot */
    Widget	LegendWindow;		/* window for legend */
    Widget	ParamLabelWidgets[16];	/* label widgets for parameter names */
    Widget	ObsValWidgets[15];	/* obs values */
    Widget	ObsEditValWidgets[8];	/* text widgets for editing obs vals */
    Widget	ObsDiffWidgets[5];	/* label widgets for diff b/t obs & model */
    Widget	ModelValWidgets[5];	/* label widgets for model values */
    Widget   ShipNameWidget;            /* label widget for ship name */
    Widget   ModelValueLabel;           /* label for Model Value button to display */
    Widget   FlagButtonWidgets[5][2];   /* Buttons for the flags */
    Widget   FlagLabelWidgets[5];       /* Buttons for flag labels */
    Widget   LegendLabelWidgets[7];     /* Legend label widgets. */
    Widget   AcceptAllWidget;           /* Button to set accept flags for all params  */
    Widget   RejectAllWidget;           /* Button to set reject flags for all params  */
    Widget   ClearAllWidget;            /* Button to clar accept and reject flags for all params  */
    Widget   DupWidget;                 /* Button to set duplicate flag */
    Widget   NextObsWidget;             /* Button on flag window for ProcessGroup */
    Widget   BookmarkWidget;            /* Button for Bookmark routine */
    Widget   ReturnWidget;              /* Button for ReturnToBookmark routine */
    Widget   CheckCallsignShell;        /* Dialog Shell for  Check Callsign button. */
    Widget   CheckCallsign;             /* Bulletin board for   "      "       "      */
    Widget   CheckCallsignLabel;        /* Label on             "      "       "      */
    Widget   CheckCallsignText;         /* Text field on        "      "       "      */
    Widget   CheckCallsignGo;           /* Go button on         "      "       "      */
    Widget   CheckCallsignDismiss;      /* Dismiss button on    "      "       "      */
    Widget   ErrorMsgShell;             /* Dialog Shell for  Error Msg dialog.        */
    Widget   ErrorMsg;                  /* Bulletin board for   "      "              */
    Widget   ErrorMsgLabel;             /* Error msg text on    "      "              */
    Widget   ErrorMsgDismiss;           /* Dismiss button on    "      "              */
    Widget   DateCycleShell;            /* Dialog Shell for Get Date Cycle button.    */
    Widget   DateCycle;                 /* Bulletin board for   "      "       "      */
    Widget   DateLabel;                 /* Date label on        "      "       "      */
    Widget   DateText;                  /* Date text field on   "      "       "      */
    Widget   CycleLabel;                /* Cycle label on       "      "       "      */
    Widget   CycleText;                 /* Cycle text field on  "      "       "      */
    Widget   DateCycleGo;               /* Go button on         "      "       "      */
    Widget   DateCycleDismiss;          /* Dismiss button on    "      "       "      */
    Widget   HistoryText=NULL;          /* Text on History window */
    Widget   HistoryLabel;              /* Label on History window for callsign */
    Widget   HistoryShell;              /* Shell around History/Cruise Map window */
    Widget   History;                   /* History Window */
    Widget   HistoryExit;               /* Exit button on History window */
    Widget   DateWidget;                /* Date of obs */
    Widget   TimeWidget;                /* Time of obs */
    int      Read, Write;               /* read/write flags for ReadWriteObs func */
    int      ZoomIn, NoZoom;            /* for station plot routines. */
    int      first_expose=1;              /* to avoid redrawing map at first expose */

/*
 * Define other variables shared by callbacks
 */

    int      Zoomed;                    /* 1 (true); if the map is currently Zoomed */
    int      AutoZoomed;                /* 1 (true); if zoomed via AutoZoom */
    int      NumPri;                    /* Number of priority levels chosen sofar */
    int      Current_Pri;               /* Current priority level. */
    int      Valid_Bookmark;            /* True if an obs has been bookmarked on map  */
    int      Bookmark_Current_View_Obs; /* Bookmarked value of Current_View_Obs */
    int      Bookmark_Current_Pri_Obs;  /* Bookmarked value of Current_Pri_Obs */
    int      FlagButtonValues[5][2];    /* Buttons for the flags */
    int      Current_View_Obs;          /* current display for ViewObs */
    int      Current_Pri_Obs;           /* current obs to qc in priority order */
    int      Previous_View_Obs;
    int      Previous_Pri_Obs;
    int      Select_Group_Screen;       /* =1 if select group screen up, else 0. */
    int      NumOnMap;                  /* Number of obs displayed on current map */
    int      NumSuspect, NumNonsuspect, /* Number of obs on map in each category */
         NumQCed;
    int      NewMap;                    /* =1 if new map being plotted, so 0 out Displayed array */
    int      one;
    float    missing=-99.;              /* used to convert lat/lon to NDC values.    */

/* currently displayed observation values */

    char     ObsValStrs[14][15];        /* String versions of obs values for display  */

#else

/*
 *  Define NCAR stuff
 */

    extern int   appid;               /* id for application */
    extern int   Mapid;               /* id for map window */
    extern int   Wkid;                /* id for map Workstation */
    extern int   GKSWkid;             /* d for map (low level NCAR) */

    extern int   ObsWkid;             /* id for Obs Plot Workstation  */
    extern int   ObsPlotid;           /* id for Obs Plot Window */
    extern int   GKSObsPlotWkid;      /* id for Obs Plot (low level NCAR) */

    extern int   CruiseWkid;          /* id for Cruise Plot Workstation  */
    extern int   Cruiseid;            /* id for Criose Plot Window */
    extern int   GKSCruiseWkid;       /* id for Cruise Plot (low level NCAR) */

    extern int   getrlist, setrlist;  /* for setting and retrieving resource values */

/*
 *  Define  motif stuff: widgets, etc.
 */

    extern Widget  toplevel;                   /* top level widget */
    extern XtAppContext   app;                 /* Application context */
    extern Display *MapDisplay;                /* Display for map window */
    extern XEvent   Current_Event;             /* the current event */
    extern Colormap DefColorMap;               /* default color map */
    extern int      ScreenHeight;              /* max height of screen */
    extern int      WindowWidth;               /* width of application window */
    extern int      MenubarHeight;            
    extern int      ObsWindowHeight;   
    extern int      HistoryHeight;             
    extern int      HistoryWidth;              
    extern Widget   MenuButtons[14];           /* buttons on menubar */
    extern Widget   Map;                       /* the map window */
    extern Widget   ObsWindow;                 /* window for observation display & editing   */
    extern Widget   ObsPlotWindow;             /* window for observation plot */
    extern Widget   LegendWindow;              /* window for legend */
    extern Widget   ParamLabelWidgets[16];     /* label widgets for parameter names */
    extern Widget   ObsValWidgets[15];         /* obs values */
    extern Widget   ObsEditValWidgets[8];      /* text widgets for editing obs vals */
    extern Widget   ObsDiffWidgets[5];         /* label widgets for diff b/t obs & model     */
    extern Widget   ModelValWidgets[5];        /* label widgets for model values */
    extern Widget   ShipNameWidget;            /* label widget for ship name */
    extern Widget   ModelValueLabel;           /* label for Model Value button to display */
    extern Widget   FlagButtonWidgets[5][2];   /* Buttons for the flags */
    extern Widget   FlagLabelWidgets[5];       /* Buttons for flag labels */
    extern Widget   LegendLabelWidgets[7];     /* Legend label widgets. */
    extern Widget   AcceptAllWidget;           /* Button to set accept flags for all params */
    extern Widget   RejectAllWidget;           /* Button to set reject flags for all params */
    extern Widget   ClearAllWidget;            /* Button to clar accept and reject flags for all params */
    extern Widget   DupWidget;                 /* Button to set duplicate flag */
    extern Widget   NextObsWidget;             /* Button on flag window for ProcessGroup */
    extern Widget   BookmarkWidget;            /* Button for Bookmark routine */
    extern Widget   ReturnWidget;              /* Button for ReturnToBookmark routine*/
    extern Widget   CheckCallsignShell;        /* Dialog Shell for  Check Callsign button. */
    extern Widget   CheckCallsign;             /* Bulletin board for   "      "       " */
    extern Widget   CheckCallsignLabel;        /* Label on             "      "       " */
    extern Widget   CheckCallsignText;         /* Text field on        "      "       " */
    extern Widget   CheckCallsignGo;           /* Go button on         "      "       " */
    extern Widget   CheckCallsignDismiss;      /* Dismiss button on    "      "       " */
    extern Widget   ErrorMsgShell;             /* Dialog Shell for  Error Msg dialog.   */
    extern Widget   ErrorMsg;                  /* Bulletin board for   "      " */
    extern Widget   ErrorMsgLabel;             /* Error msg text on    "      " */
    extern Widget   ErrorMsgDismiss;           /* Dismiss button on    "      " */
    extern Widget   DateCycleShell;            /* Dialog Shell for Get Date Cycle button.    */
    extern Widget   DateCycle;                 /* Bulletin board for   "      "       " */
    extern Widget   DateLabel;                 /* Date label on        "      "       " */
    extern Widget   DateText;                  /* Date text field on   "      "       " */
    extern Widget   CycleLabel;                /* Cycle label on       "      "       " */
    extern Widget   CycleText;                 /* Cycle text field on  "      "       " */
    extern Widget   DateCycleGo;               /* Go button on         "      "       " */
    extern Widget   DateCycleDismiss;          /* Dismiss button on    "      "       " */
    extern Widget   HistoryText;               /* Text on History window */
    extern Widget   HistoryLabel;              /* Label on History window for callsign  */
    extern Widget   HistoryShell;              /* Shell around History/Cruise Map window */
    extern Widget   History;                   /* History Window */
    extern Widget   HistoryExit;               /* Exit button on History window */
    extern Widget   DateWidget;                /* Date of obs */
    extern Widget   TimeWidget;                /* Time of obs */
    extern int      Read, Write;               /* read/write flags for ReadWriteObs func */
    extern int      ZoomIn, NoZoom;            /* for station plot routines. */
    extern int      first_expose;              /* to avoid redrawing map at first expose */
    extern char     *Date, *Cycle;

/*
 * Define other variables shared by callbacks
 */

    extern int	Zoomed;                    /* 1 (true); if the map is currently Zoomed */
    extern int	AutoZoomed;                /* 1 (true); if zoomed via AutoZoom */
    extern int	NumPri;                    /* Number of priority levels chosen sofar     */
    extern int	Current_Pri;               /* Current priority level. */
    extern int	Valid_Bookmark;            /* True if an obs has been bookmarked on map */
    extern int	Bookmark_Current_View_Obs; /* Bookmarked value of Current_View_Obs*/
    extern int	Bookmark_Current_Pri_Obs;  /* Bookmarked value of Current_Pri_Obs */
    extern int	FlagButtonValues[5][2];    /* Buttons for the flags */
    extern int	Current_View_Obs;          /* current display for ViewObs */
    extern int	Current_Pri_Obs;           /* current obs to qc in priority order */
    extern int	Previous_View_Obs;            
    extern int	Previous_Pri_Obs;   
    extern int	Select_Group_Screen;       /* =1 if select group screen up, else 0.      */
    extern int	NumOnMap;                  /* Number of obs displayed on current map     */
    extern int	NumSuspect, NumNonsuspect, /* Number of obs on map in each category */
         NumQCed;
    extern int   NewMap;                    /* =1 if new map being plotted, so 0 out Displayed array */
    extern int   one;   
    extern float missing;              /* used to convert lat/lon to NDC values. */

/* currently displayed observation values */

    extern char  ObsValStrs[14][15];        /* String versions of obs values for display  */

#endif  /* CREATE */
#endif  /* GUIGBL_H */
