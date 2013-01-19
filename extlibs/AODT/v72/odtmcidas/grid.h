#define UPSIZE 75000
#define DCSIZE 4000
#ifdef MAIN_R
long jbuff [UPSIZE];
long ibuff [UPSIZE];
float fbuff[UPSIZE];
long docsec[DCSIZE];
#else
extern long jbuff [UPSIZE];
extern long ibuff [UPSIZE];
extern float fbuff[UPSIZE];
extern long docsec[DCSIZE];
#endif
/*  GRID file structure (0-based)
  0 - 7, gridfile title
  10 maximum number of grids, a negative number for current version
  11 - (max grids+10), 0-based word pointers to locations of start of grids
	in file
  max grids + 11,  location of next free word for new grid
*/

#define GDIR 16		/* should be greater than 13 */
struct GRIDCOM {
 int fd;	/* file number from open call */
 char *gridfile_name;	/* file name with optional path */
 int maxgrd;		/* the maximum number of grids for this file */
			/* a -1 means this file is new and has not seen igmakx */
 long dir[GDIR];		/* the directory */
 int dir_loc;		/* the index of the current contents of dir */
 int flip;	/* set to 0 for no flip, 1 for flip, -1 for invalid file */
};

#define N_GRIDS 1

struct GRIDCOM gridcom[N_GRIDS];
/**** Gridhdr.h */

/** defines the data structure to hold grid data read from a GRIB file */

#define NLEVELS 13

typedef struct Guessgrid {
	int size;
	int nrows;
	int ncols;
 	int day;
	int time;
	int fcst;
	int latn;	/* lat and lon bounds are multiplied by 10000 */
	int lonw;
	int lats;
	int lone;
	int incr; 	/* increments are multiplied by 10000 */
	int incn;
 	int i_scan;	/* 0 indicates +i direction along lat 
			   ie. West to East. 1 is opposite direction */
	int j_scan;	/* 0 indicates +j direction, ie. South to North
			   1 is North to South */
	int Tscale;	/* temperature, usually x 100 (scale 2) */
	int Dscale;	/* dew point temperature, usually x 100 (scale 2) */
	int Zscale;	/* height, usually x 10 (scale 1) */
	int Pscale;	/* pressure, usually / 10 (scale -1) */
	int UVscale; 	/* wind components, usually x 10 (scale 1) */
	int ***Tdata;   /* Tdata[180][360][NLEVELS] */
	int ***Ddata;   /* Ddata[180][360][NLEVELS] */
	int **Z1000;    /* Z1000[180][360] */
	int **MSLP;     /* MSLP[180][360] */
	int ***Udata;   /* Udata[180][360][NLEVELS]	 U and V are X 10 */
	int ***Vdata;   /* Vdata[180][360][NLEVELS] */

	} GUESS;

int igopen (char *gridfile, char red_wrt, int *icreat);
int igmake(int fd, char *ident, long maxgrd);
int igput(int fd, int gno, int *grid, int nr, int nc, int *header, int *agno);
int lwrd(int fd, int loc, int len, long *buf);
int lwwr(int fd, int loc, int len, long *buf);
int igclos(int fd);
int gridtype (int grid_id);

