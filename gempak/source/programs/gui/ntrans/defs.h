#include "xwprm.h"

#define	LABELSTR	80	
#define MAXFRAMES	500	/*  Maximum number of frames in a file */
#define MAXPANEL        10	/*  Maximum number of rows and columns in a panel  */
#define MAX_FRAME_GROUP MAX_PIXMAP      /* Maximum number of frames in a group */
				/*  MAX_FRAME_GROUP cannot exceed MAX_PIXMAP  */
				
typedef	struct	{		
	int	off_byte;	/* offset of bytes in the meta_file	*/
	int	end_byte;	/* offset of end bytes in the meta_file	*/
	char	label[LABELSTR];  /* offset of bytes in the meta_file	*/
	} Frame_str;

typedef	struct	{
	int	num_frames;	/* number of frames in the meta_file	*/
	Frame_str frame[MAXFRAMES];/* pointer to frames of the meta_file*/
	int	frame_size;	/* amount of memory allocated to frame	*/
	}	Meta_str;

typedef struct  {
	char	groupname[150];	    /* title of the group */
        int     frames[MAX_FRAME_GROUP];            /* frames    */
        int     frame_num;     /* total number of frames in frame */
        }       Group_str;

typedef struct  {
	int	line_colr;
	int	fill_colr;
	int	fill_patn;
	int	textfont;
	float	textsize;
	int	textalgn;
	int	oldtextfont;
	float	oldtextsize;
	int	oldtextalgn;
        }       Graphics_str;

typedef struct {
	int	 flag;		/* 0 no data, 1 frames	   */
        int	 group_no; 	/* file name at each panel */
	int	 frame_num;	/* total number of valid frame */
	int  	 frame[MAX_FRAME_GROUP]; 	/* valid frame id in group */
	int	 meta_id;
	Meta_str meta_st;
        }       prt_multiPanel_t;


/*
 * DEFINE SECTION
 */
#define APP_NAME "metafile_translator"
#define APP_CLASS "MetafileTranslator"

#define DEF_WIDTH          800  /* Default width for scrolled drawing window  */
#define DEF_HEIGHT         800  /* Default height for scrolled drawing window */

#define MAX_NO_GROUPS  100

#define COLOR_BAR_WIDTH     20  /* Width of color bar in pixels               */
#define COLOR_BAR_HEIGHT (DEF_HEIGHT+5)         /* Ht of colorbar in pixels   */

#ifdef max
#undef max
#endif
#ifdef min
#undef min
#endif

#define min(a,b) ((int)(a)<(int)(b)?(int)(a):(int)(b))
#define max(a,b) ((int)(a)>(int)(b)?(int)(a):(int)(b))

#define FRAME_ALLOC     250     
