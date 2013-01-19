/************************************************************************
 * GBCMN								*
 *									*
 * This header file contains general include files, defined parameters,	*
 * the structures of data read from a GRIB file, and the global		*
 * variables.								*
 *									*
 **									*
 * Log:									*
 * J. Chou/EAI		01/93						*
 * S. Jacobs/EAI	 1/94	Clean up; Rename variables		*
 * S. Jacobs/NMC	 8/94	Moved general macros to gemprm.h	*
 * D.W.Plummer/NCEP	 3/96	Added GBDIAGs				*
 * S. Jacobs/NCEP	 4/99	Added include for undscr.h		*
 * S. Jacobs/NCEP	 4/99	Changed the length of end_string: 4->5	*
 * S. Jacobs/NCEP	12/00	Added private prototypes		*
 * M. Li/SAIC		05/07	Added gb_ecmwfens and gb_ecmwfcpc	*
 * M. Li/SAIC		06/07	Added gb_ecmwfclu			*
 ***********************************************************************/

#include "geminc.h"
#include "gemprm.h"

/*---------------------------------------------------------------------*/


#define TRUE		1
#define FALSE		0
				/* Set values for TRUE and FALSE */

#define BUFFSIZE	8192
				/* The size of the buffer */
#define DEG_TO_RAD	( PI / 180.0 )
#define RAD_TO_DEG	( 180.0 / PI )
				/* Degrees <--> Radians */

#define ISLENGTH	8
#define ESLENGTH	4 

/*---------------------------------------------------------------------*/

extern  int     GBDIAG_IDS;
extern  int     GBDIAG_PDS;
extern  int     GBDIAG_GDS;
extern  int     GBDIAG_BMS;
extern  int     GBDIAG_BDS;
extern  int     GBDIAG_END;
                                                     /* Define GBDIAGs */

/*---------------------------------------------------------------------*/

struct gribfile {
	char	name[256];
	FILE	*fptr;
};
		/* Define GRIB file and GRIB index file info structure */

/*---------------------------------------------------------------------*/

struct indexmsg {
	int		position;
	int		pdsrpos;
	int		gdsrpos;
	int		bmsrpos;
	int		bdsrpos;
	int		length;
	int		version;
	unsigned char	pds[28];
	unsigned char	gds[42];
	unsigned char	bms[6];
	unsigned char	bds[11];
	unsigned char	pdsext[40];
};
		 /* Define GRIB index file message contents structure */

struct indexfile {
	char		name[256];
	FILE		*fptr;
	char		basename[40];
	unsigned char	hdr1[82];
	unsigned char	hdr2[82];
	int		nmsgs;
	struct indexmsg	msg[MMHDRS];
	int		curmsg;
};
			  /* Define GRIB index file contents structure */

/*---------------------------------------------------------------------*/

struct sec0 {
	int	msg_length;
	int	edition;
};


struct sec1 {
	int	length;
	int	version; 
	int	center;
	int	process;
	int	grid_id;
	int	flag;
	int	parameter;
	int	vcoord;
	int	level;
	int	level_1;
	int	level_2;
	int	year;
	int	month;
	int	day;
	int	hour;
	int	minute;
	int	time_unit;
	int	time_p1;
	int	time_p2;
	int	time_range;
	int	avg_num;
	int	avg_miss;
	int	century;
	int	izero;
	int	dec_scale;
	int	isgds;
	int	isbms;
	int	pdse;
	char	extension[32];
};

	
struct sec2 {
        int	length;
        int	NV;
        int	PV;
        int	grid_proj;
	int	kx;
	int	ky;
	double	latll;
	double	latur;
	double	lonll;
	double	lonur;
	float	angle1;
	float	angle2;
	float	angle3;
	int	scan_mode;
	int	flag1;
	int	flag2; 
};


struct sec3 {
	int	length;
	int	unused_bits;
	int	table;
};


struct sec4 {
	int	length;
	int	flag;
	int	binary_scale;
	float	ref_value;
	int	num_bits;
};


struct sec5 {
	char	end_string[5];
};

/*---------------------------------------------------------------------*/

#ifdef GB_DEF 

  int		pdslength, gdslength, bdslength, bmslength;
			/* Length of the different sections */
  int		cursor, cursor1;
			/* Index to the buffer */
  struct gribfile	gbfile;
			/* Structure for GRIB file information */
  struct indexfile	infile;
			/* Structure for GRIB index file information */
  struct sec0	ids;
			/* Structure to hold section 0 */
  struct sec1	pds;
			/* Structure to hold section 1 */
  struct sec2	gds;
			/* Structure to hold section 2 */
  struct sec3	bms;
			/* Structure to hold section 3 */
  struct sec4	bds;
			/* Structure to hold section 4 */
  struct sec5	es; 
			/* Structure to hold section 5 */

#else

  extern int		pdslength, gdslength, bdslength, bmslength;
  extern int		cursor, cursor1;
  extern struct gribfile	gbfile; 
  extern struct indexfile	infile; 
  extern struct sec0	ids; 
  extern struct sec1	pds;
  extern struct sec2	gds;
  extern struct sec3	bms;
  extern struct sec4	bds;
  extern struct sec5	es;

#endif 

/* Prototypes for GB library private functions */

void gb_bds  (	unsigned char	*ptarray,
		int		kxky,
		int		*xgrid );

void gb_bms  (	unsigned char	*ptarray );

int  gb_btoi (	unsigned char	*ptarray,
		int		indx,
		int		no_bytes,
		int		neg );

void gb_ecmwfcpc ( unsigned char *buff );

void gb_ecmwfens ( unsigned char *ptarry );

void gb_ecmwfclu ( unsigned char *buff );

void gb_ends (	unsigned char	*ptarry );

void gb_ensemble ( unsigned char *buff );

void gb_ftim (	int		*itime,
		int		*iaccm,
		int		*iret );

void gb_gaus (	unsigned char	*ptarry );

void gb_gds  (	unsigned char	*ptarry );

void gb_ges  (	int		*iret );

void gb_gids (	FILE		*ifdes,
		int		*iret );

void gb_gmtx (	unsigned char	*ptarray,
		int		indx,
		int		length,
		int		*xgrid );

void gb_gsec (	unsigned char	*ptarray,
		int		indx,
		int		length,
		int		*xgrid );
		
void gb_gspk (	unsigned char	*ptarray,
		int		indx,
		int		length,
		int		*xgrid );

void gb_ids  (	unsigned char	*ptarry );

void gb_lamb (	unsigned char	*ptarry );

void gb_ltln (	unsigned char	*ptarry );

void gb_merc (	unsigned char	*ptarry );

void gb_pds  (	unsigned char	*ptarry );

void gb_pdsext ( unsigned char	*ptarry );

void gb_polr (	unsigned char	*ptarry );

void gb_rindx (	FILE		*fp,
		int		*iret );

void gb_scan (	FILE		*fnfptr,
		FILE		*infptr,
		int		*ivers,
		int		*iret );

void gb_scpk (	unsigned char	*ptarray,
		int		indx,
		int		length,
		int		*xgrid );

void gb_sphr (	unsigned char	*ptarry );

void gb_sspk (	unsigned char	*ptarray,
		int		indx,
		int		length,
		int		*xgrid );

void gb_tdlens ( unsigned char	*buff );

void gb_unpk (	int		*igrid,
		int		npts,
		int		nbits,
		float		*fgrid,
		int		*iret );

void gb_vlev (	int		*ilevel,
		int		*iret );

/* End Prototypes */
