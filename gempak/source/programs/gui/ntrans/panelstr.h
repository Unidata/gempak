typedef struct {
        int     valid_tm_set;       /* Valid Time Set Using this Panel */
        int	valid_tm_frames[500];
        } ValidTimeStr;

typedef struct {
        int     panelNo;        	/* Number of panels */
        int     flag[36];       	/* 0 - no data, 1 = data */
        int     column[36];     	/* column */
        int     row[36];        	/* row */
        int     model_no[36];   	/* Model source number */
        int     file_no[36]; 	   	/* Model File number */
        char	group_name[36][150];	/* Group name */
	ValidTimeStr	valid_time[36];	/* Valid Time Set Using this Panel */
        }       Panel_srcStr;

#ifdef PANELSTR

Panel_srcStr panelSrc;
int CurrentPanel;

#else 

extern Panel_srcStr panelSrc;
extern int CurrentPanel;
#endif
