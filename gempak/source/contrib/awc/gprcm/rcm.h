#include <time.h>

#define IDLEN   	8

#define  MDNA   	-2
#define  MDOM   	-1
#define  MDNE   	0
#define  MDCLAR 	1
#define  MDPCPN 	2

#define  MAXSTNS	200

#define	MAXBUL	50000

#define MAXSZ	4906
#define MAXTMPL	MAXPATHLEN

static char nextbl[]="nexrad.tbl";

typedef struct cntr_struct {
char ggg[4];
float spd,dir;
} cntr_struct;

typedef struct meso_struct {
char ggg[4];
float nx,ny;
} meso_struct;

typedef struct tvs_struct {
char ggg[4];
float nx,ny;
} tvs_struct;

typedef struct rad_struct {
char idlst[IDLEN];
float stnlat,stnlon;
int mode;
time_t obs_time;
int maxtop;
char maxtop_ggg[4];
int ncntr;
cntr_struct cntr[100];
int nmeso;
meso_struct meso[100];
int ntvs;
tvs_struct tvs[100];
} rad_struct;

#ifdef UNDERSCORE
#define rcm     rcm_
#define text_output text_output_
#define	gqsysz	gqsysz_
#define in_filt	in_filt_
#define in_mark in_mark_
#define fl_tmpl	fl_tmpl_
#define st_rlst st_rlst_
#define tb_rstn tb_rstn_
#endif

/* prototypes */
void 	rcm (char *fname, char *wws, char *wwatt, int *idither,
          char *meso, char *tvs, char *cntr, char *maxtop,
          int *iradinfo, int *imdr, char *map, char *clrbar, 
          int *nlun, int *luns, int *ier);
void    n_filter(int npts, float *nx, float *ny, float rh, float rw, float ffactor, void *ds);
void	filter_init();
int	filter_retrieve(void **ds, float *x, float *y);
int	filter_retrieve(void **ds, float *x, float *y);
void 	get_xy(float clat, float clon, float *x, float *y);
void 	get_xy4(float clat, float clon, char *gbox, float *x, float *y);
void	read_nexcc(char *bultin, int lenbul, rad_struct RADARS[], int NEXINDX, time_t start_time, time_t end_time);
void 	read_nexaa(char *bultin, int lenbul, rad_struct RADARS[], int NEXINDX, 
                time_t start_time, time_t end_time, int *mode, int *valid);

void	set_mdrplot(int *i);
void	set_radinfo(int *i);
void	set_maxtop(char *maxtop);
void	set_cntr(char *cntr);
void	set_tvs(char *tvs);
void	set_meso(char *meso);

void	radar_info(rad_struct RADARS[], int NEXSTNS);

void	radar_stats(rad_struct RADARS[], int NEXSTNS, int *nlun, int *luns);

void	box_fill(int color, int np, float x[], float y[]);

char 	*next_group(char *bultin, int len);
int	getstr(char *buf, char *outstr);
int 	findchr(char *buf);
int	isrcm(char *bultin, int lenbul);
int 	get_nextbull(int fp, char *bultin,int *lenbul);
int	get_nextggg(char *cpos, char *buf, int maxint, int *nextoff, int *iret);
void	grid_ggg(char *buf, float i, float j, int mode);
void	plot_inten(char *gbox, int color, float i, float j);
void 	plot_repeat(int color, float i, float j);
void	mdr_cbar(char *clrbar);
