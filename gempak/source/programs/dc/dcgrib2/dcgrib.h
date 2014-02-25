/************************************************************************
 * dcgrib.h                                                             *
 *                                                                      *
 * This file contains function prototypes and global variables for use	*
 * in dcgrib2.								*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * S. Chiswell/Unidata	12/02	Created					*
 ***********************************************************************/
#include "gb2def.h"

#ifndef PROTO_DCGRIB
#define PROTO_DCGRIB

#ifdef	_FGRID_
	float *FGRID=NULL, *TMPGRD2=NULL;
	int   *XGRID=NULL;  
	int   FGSIZ=0, TGSIZ=0, XGSIZ=0;
#else
	extern	float *FGRID, *TMPGRD2;
	extern	int *XGRID;
	extern	int FGSIZ, TGSIZ, XGSIZ;
#endif

#ifdef UNDERSCORE
#define dcgfint	dcgfint_
#define dcgfcls dcgfcls_
#define dcgfcyl dcgfcyl_
#define dcgfiles dcgfiles_
#define dcitoc  dcitoc_

#define tbsgrid	tbsgrid_
#define tbsubc  tbsubc_
#define tbsublist       tbsublist_

/*
 * GEMLIB routines called
 */
#define gd_init	gd_init_
#define gd_opnr gd_opnr_

#endif

/*
 *  dcgrib prototypes
 */

void    tbglist ( );
char	*tbsubcenter (int *center, int *subcenter);
void	tbtmpl ( int center, int subcenter, int model, int grid, char *filnam,
                int *maxgrids, int *iret);

void	dcflnam ( char *gemfil, char *filnam, int *maxgrids, int *ier);
void	dcflnam2 ( char *gemfil, char *filnam, int *maxgrids,
                gribfield *gfld, char *gdattm1, char *gdattm2, int *ier);
void	dcogfil ( char *gemfil, int *iflno, int maxgrid, int specgrid);
void	dcogfil2 ( char *gemfil, int *iflno, int maxgrid, Geminfo ginfo);
unsigned char *dc_ggrib ( size_t *griblen, int *version, int *iret);
int	dcgnav ( );
void	dcfillgrid (int iflno, int *xgrid, int lengrd, float rmsval,
		int iuscal, int ighdr[], char gdattm[][DTTMSZ], int level[2], 
		int ivcord, char *parm, int *iret);
void	dcmisjpg ( Gribmsg g2, float *fgrid);
void	dcsubgrid ( int iflno, int *xgrid, int lengrd, float rmsval,
		int iuscal, int ighdr[], char gdattm[][DTTMSZ], int level[2],
		int ivcord, char *parm, int subgrid);

void dcchecksub ( int iflno, int center, int processid,
                int level[], int ivcord, char gdattim[][DTTMSZ],
                char *parm, int nx,
                int ny, int *iret);

void	dcquasi ( int nrow, unsigned char *buf, int *isum);
int	dcsgrid ( );
void	dcwppg ( int iflno, int *xgrid, int subgrid);
void	decode_grib ( char *gemfil, int maxgrd, int *iret);

void	decode_grib1 ( unsigned char *cgrib, int len, char *gemfil, char *filnam,
		int maxgrd, int *iret);
void	decode_grib2 ( unsigned char *cgrib, int len, char *gemfil, char *filnam,
		int maxgrd);
/*int	decode_g2gds(int tnum, int tmplen, int igdtmpl[]);
void	decode_g2ltln ( int tnum, int *gdstmpl );
void	decode_g2lamb ( int tnum, int *gdstmpl );
void	decode_g2pdt ( char *g2name, int namlen, int templnum, int templ[], 
		int level[], int *ivcord, int idsectlen, int idsect[], 
		int gbftim[][3], int *ier );
int	decode_g2gds(int tnum, int tmplen, int igdtmpl[]);*/
int     decode_g2gnum ( gribfield *gfld);


void	qlin(int nrows, int ix[], float *idat, int ni, int nj, float *odat);

void staggerv(float *dp, float *fg, int gx, int gy);
void staggerh(float *dp, float *fg, int gx, int gy);
void stagger ( float *ingrid, float *fgrid, int npts, int kx, int ky,
                 char *parm, int *iret );
void stagger_e ( float *ingrid, float *fgrid, int npts, int kx, int ky,
                 char *parm, int *iret );

void dcsubsav ( float *tmpgrd, int nx, int ny);
void dclastsub ( float *tmpgrd, int *nx, int *ny, int *iret);

/*
 * Fortran routines called from C routines
 */

void	dcgfint ( int *numfil );
void	dcgfcls ( int *iret );
void	dcgfcyl ( char *filnam, int *iflno, int *iret, size_t);
void	dcgfiles ( char *filnam,int *iflno, int *ier, size_t);

void 	tbsubc ( int *center, char *filnam, int *subcenter, size_t);
void	tbsgrid ( int *lun, int *cntr, int *gid, int *cid, char *proj,
		  float *ang, float *gar, int *nx, int *ny, int *scol, int *srow,
		  int *iret, size_t);

/*
 * GEMPAK library routine prototypes
 */

void	fl_clos	( int *lun, int *iret);
void	fl_tbop	( char *table, char *type, int *lun, int *iret, size_t, size_t );

void	gd_opnr ( char *filnam, int *igdfln, int *navsz, float *rnvblk,
		  int *ianlsz, float *anlblk, int *ihdrsz, int *maxgrd, 
		  int *iret, size_t);

void	tb_grnv ( int *lun, char *namgd, int *numgd, char *prjgd,
		  float *anggd, float *gargd, int *nxgd, int *nygd,
		  float *deln, float *extnd, int *iret, size_t, size_t );


#endif
