#define NUMTYP  12

#define TORNADO 0
#define SEVERE  1
#define WINTER  2
#define SPECIAL 3
#define NONPRCP 4
#define FWATCH  5
#define FWARN   6
#define FSTATE  7
#define HURRS   8
#define	SLSTSTM	9
#define	SLSTORN	10
#define SLSFFW	11


typedef struct
{
  int current_fill_color;
  int current_line_color;
  int current_line_type;
  int current_line_width;
  int current_fill_type;
  int expired_fill_color;
  int expired_line_color;
  int expired_line_type;
  int expired_line_width;
  int expired_fill_type;
} gpwarn_config_type;

/*
 * Structure for compiling list of county/zones to fill
 */
typedef struct zonelist {
   char *zone;
   struct zonelist *next;
   } zonelist;


/* function prototypes */
void gpwarn_read_config(gpwarn_config_type *config);
int is_valid(char *expires, int *isval);
void get_map(char *zone, char *expires);
void do_all(char *zone, int *nzones,zonelist **head);
void do_range(char *zline, char *lzone, int *cpos, int *nzones, zonelist **head);
void get_zones(char *line);
void get_valid(char *wws);
int check_bullet(char *line, int state);
void templregex (char *instr, char *outstr);
void warn_search();
void gpwarn_color_init();
void read_bull(char *wwtmpl, char *wws, char *wwatt, int *nlun, int *luns, int *ier);

void clo_off(char *srchstr, int bindex, int col1, int col2, int ltype, int lwidth);
void bounds_fill(float *X, float *Y, int nsegs, int color);
void bounds_line(float *X, float *Y, int nsegs, int color, int ltype, int lwidth);
void boundry_segs(long int offset, int npts, char *bndtbl, int col1, int col2,
                        int ltype, int lwidth);
void plot_ugc(int type, char *zone, int col1, int col2, int ltype,
                int lwidth, int ftype );
void plot_slsc ( int *cval, int *nfips);
