/*
 *  This include file contains the function definitions.
 */

void	Alloc_Colors(Display *AppDisplay);
void	AutoZoom(float, float);
void	BookmarkCb(Widget, XtPointer, XtPointer);
void	CheckCallsignExitCb(Widget, XtPointer, XtPointer);
void	CheckObs(float, float, float, int *);
void	ClearFields(void);
void	CloseHistoryCb(Widget, XtPointer, XtPointer);
void	ContourSLPCb(Widget, XtPointer, XtPointer);
void	ContourSSTCb(Widget, XtPointer, XtPointer);
void	ContourTempCb(Widget, XtPointer, XtPointer);
Widget	Create_Help(Widget);
Widget	Create_Map(Widget);
Widget	Create_Legend(Widget);
Widget	Create_Menubar(Widget);
Widget	Create_Obs_Plot_Window(Widget);
void	Create_Obs_Windows(Widget);
void	Create_Popup_CheckCallsign(void);
void	Create_Popup_DateCycle(void);
void	Create_Popup_ErrorMsg(void);
void	CruisePlot(void);
void	DateCycleExitCb(Widget, XtPointer, XtPointer);
void	deactivate_low_level(int*);
void	Draw_Contour( float *, int *, int *, int, float);
void	Draw_Vectors(float *, float *, int *);
void	DisplaySLPValCb(Widget, int, XtPointer);
void	DisplaySSTValCb(Widget, int, XtPointer);
void	DisplayWindSpdValCb(Widget, int, XtPointer);
void	DisplayTempValCb(Widget, int, XtPointer);
void	DisplayModelVal(int, char *, float *, float *, int *);
void	DisplayObs(int);
void	DisplayObsValues(int Obs_To_Display);
int	Edited_Loc(float *, float *, float, float, float, float);
int	Edited_Obs(float *, float *, float *, float *, float *, float *, float *);
void	ErrorMsgExitCb(Widget, XtPointer, XtPointer);
void	ExitCb(Widget, int, XtPointer);
void	ExposeMap(Widget, XtPointer, XEvent *);
int	Get_Box(int, float *, float *, float *, float *);
void	GetCheckCallsignCb(Widget, XtPointer, XtPointer);
void	GetDateCycle(void);
int	Get_Grids(char *, char *, char *, char *, char *);
int	Get_Latest(char *);
void	GetModelValue(float, float, float *, int *, int, float *);
int	Get_Next_Pri_Obs(int *);
void	Get_Ship_Name(FILE *, int, char *);
void	Get_Ship_Rec(FILE *, int, char *, char *);
void	Help(Widget);              
void	PopupHistoryCb(Widget, XtPointer, XtPointer);
void	HistoryCruise(char *);
void	Initialize(void);
int	IsDir(const char *);
int	strindex(char s[], char t[]);
void	Make_Cruise_Plot(void);
void	FinishPrevObs(void);
void	MakeHistoryFile(Widget, char *);
float	f_abs(float);
void	Highlight_Labels(int);
void	MakeHistoryFieldWind(float, float, float, float, int, float *, float *, 
			   int *, int *, float *, float *, int *,  int *, char *);
void	MakeHistoryField(float, float, float, int, float *, int *, float *, int *, char *);
void	MakeObsArrays(size_t);
void	MakeFilenameStrings(char *);
int	Open_Grid_File (FILE **, char *, int *);
void	Open_ShipNames_File(FILE **, int *);
int	null_data(float);
void	Plot_Array_Wx_Obs(int, int, int *, int);
void	Plot_Obs(void);
void	PlotObsWindowCb(Widget, XtPointer, XtPointer);
void	plot_one_station(int*, float*, float*, int*, float*, float*, int*, 
				int*, float*, float*, float*, float*, float*);
void	Plot_One_Wx_Obs( int, int, int, int);
void	Plot_Points(int, int, float *, float *, int);
void	Plot_Wx_Obs(int, int);
void	PopupCheckCallsignCb(Widget, int, XmPushButtonCallbackStruct *);
void	PopupGetDateCycleCb(Widget, int, XmPushButtonCallbackStruct *);
void	PopupErrorMsg(char *);
void	PopupForecastCb(Widget, int, XmPushButtonCallbackStruct *);
void	PopupModelValCb(Widget, int, XmPushButtonCallbackStruct *);
int	PrintHistory(char *);
void	ProcessGroupCb(Widget, XtPointer, XtPointer);
int	Read_Db_Files(void);
int	Read_Grid_File (FILE **, float **, int *);
void	ReadObs(int);
void	Read_Obs_File(void);
int	ReadRec(char *, char *, char *, char *,
		float *, float *, float *,  float *, float *,
		float *, float *, float *, float *,
		int *, int *,  int *, int *, int *, int *,
		float *, float *, float *, float *, float *, char *,
		float *, int *, int *, int *,  int *, int *,
		int *, int *, int *, int *, int *);
int	ReadWriteObs( FILE **, char *, int, int,
		char *, char *, char *, float *, float *,
		float *, float *, float *, float *, float *,
		float *, float *, int *, int *,  int *,
		int *, int *, int *, float *, float *,
		float *, float *, float *,
		char *, float *, int *, int *, int *,  int *,
		int *, int *, int *, int *, int *, int *);
void	ReFocusCb(Widget, XtPointer, XtPointer);
void	Reload (char *, char *);
void	Replot(int SizeChange);
void	Replot_Curr_Wx_Obs( int, int);
void	ReturnToBookmarkCb(Widget, XtPointer, XtPointer);
int	SaveObsChanges(void);
int	Search_For_Ship_Name(FILE *, char *, int, int);
void	SelectGroupCb(Widget, XtPointer, XtPointer);
void	Select_Next_Group(void);
void	SetAllFlagsCb(Widget, int, XtPointer);
void	SetDupFlagCb(Widget, int, XtPointer);
void	setup_station_plot(int*, int*, float*, float*);
float	Speed(float, float, float, float, char *, char *, char *, char *);
float	speedf(float *, float *, float *, float *, int  *, int  *, int  *, int *);
void	SubmitQCFlagsCb(Widget, int, XtPointer);
void	ToggleFlagCb(Widget, int, XtPointer);
void	Turn_Off_Select_Group(void);
void	UnViewGroupCb(Widget, XtPointer, XtPointer);
void	Update_Legend(void);
int	Update_Params(int, int, int, char *);
void	Vector2SD(float, float, float *, float *);
void	ViewObs(Widget, XtPointer, XEvent*);
void	WindVectorsCb(Widget, XtPointer, XtPointer);
void	WorldMapCb(Widget, XtPointer, XtPointer);
void	WriteQCFlags(void);
void	ZoomCb(Widget, XtPointer, XtPointer);
