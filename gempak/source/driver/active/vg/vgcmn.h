/************************************************************************
 * VGCMN.H								*
 *									*
 * This header file declares the variables used in the Vector Graphics	*
 * File device driver (VG).						*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 3/97						*
 * D. Keiser/GSC	 4/97	Added special line			*
 * E. Safford/GSC	 6/97	Added special text			*
 * E. Wehner/EAi	 6/97	Added group type			*
 * S. Jacobs/NCEP	 7/97	Added max group number			*
 * S. Jacobs/NCEP	 9/97	Removed kstlc, kstfc, kstxc		*
 * S. Jacobs/NCEP	 9/97	Added kbrdr, krrotn, kjust		*
 * S. Jacobs/NCEP	 2/98	Added ksmtyp, rdens, ketype, rtensn	*
 * S. Jacobs/NCEP	 3/98	Added rszdsh				*
 * S. Jacobs/NCEP	 3/98	Added NPATFL, kfillt, rszfil		*
 * I. Durham/GSC	 4/98	Added khwid, klwidh, and rhshsz		*
 * A. Hardy/GSC	 	10/98	Added kcsywd, rcsysz                    *
 * M. Li/GSC		07/00	Added kcolr2				*
 * J. Wu/GSC		12/00	Included 'drwids.h'			*
 * J. Wu/GSC		02/01	Added flun/opnfil pointed to output file*
 *				& prototypes for vopen, vclosp, vendd	*
 * D. Kidwell/NCEP	 6/02	Added kgrpns & prototype for vsetgrps,  *
 *                              deleted mxgpnm                          *
 * D. Kidwell/NCEP	 6/02	Added kgindx, changed vsetgrps prototype*
 * m.gamazaychikov/SAIC 01/03   Removed NPATFL                          *
 * T. Piper/SAIC	03/04	Added numerous prototypes		*
 * E. Safford/SAIC	01/08	include proto_xw.h			*
 ***********************************************************************/

#include "geminc.h"
#include "gemprm.h"
#include "vgstruct.h"
#include "drwids.h"
#include "pgprm.h"
#include "proto_xw.h"


#define MXFLEN	133

/*---------------------------------------------------------------------*/

#ifdef GLOBAL

	int	kctype;
				/* Color scheme type
				   0 = Monochrome
				   1 = Grayscale
				   2 = Color */

	int	kunit;
				/* Type of output (Used for XW)
				   1 = GEMPAK
				   2 = MOTIF */

	char	curfil[MXFLEN];
				/* Current filename */

	FILE	*flun;
				/* Output file identifier */
	int	opnfil;
				/* Open file flag */

	int	kcolr;
	int	kcolr2;
				/* Color setting */
	int	kfcod, kpipsz, kpipst, kpipdr;
				/* Front attributes */
	float	rwtsiz;
	int	kwtwid;
				/* Weather symbol attributes */
	float	rctsiz;
	int	kctwid;
				/* Cloud type symbol attributes */
	float	ricsiz;
	int	kicwid;
				/* Icing symbol attributes */
	float	rptsiz;
	int	kptwid;
				/* Pressure tendency symbol attributes */
	float	rpwsiz;
	int	kpwwid;
				/* Past weather symbol attributes */
	float	rsksiz;
	int	kskwid, ksktyp;
				/* Sky cover symbol attributes */
	float	rspsiz;
	int	kspwid;
				/* Special symbol attributes */
	float	rtbsiz;
	int	ktbwid;
				/* Turbulence symbol attributes */
	int	kmark, kmkwid, kmktyp;
	float	rmksiz;
				/* Marker attributes */
	float	rbrsiz;
	int	kbrwid, kbrtyp;
				/* Wind barb attributes */
	float	rarsiz, rarhsz;
	int	karwid, kartyp;
				/* Wind arrow attributes */
	int	kltyp, klthw, klwid, klwhw;
				/* Line attributes */
	float	rslsiz;
	int	ksltyp, kslstr, ksldir, kslwid;
				/* Special line attributes */
	float	rtxsz;
	int	ktxfn, ktxhw, ktxwid, kbrdr, krrotn, kjust;
				/* Text attributes */
	char	kgtyp;
 	int	kgnum;
	int	kgrpns[MAX_GROUP_TYPE];
	int	kgindx[MAX_GROUP_TYPE];
				/* Group type and group number */
	int	ksmtyp, ketype;
	float	rdens, rtensn;
				/* Line smoothing attributes */
	float	rszdsh;
				/* Line dashing scale */
	int	kfillt;
	float	rszfil;
				/* Fill type attributes */
	int	khwid;
	int	klwidh;
	float	rhshsz;
				/* Hash mark attributes */
	float	rcsysz;
	int	kcsywd;
				/* Combination symbol attributes */
#else

	extern int	kctype;
	extern int	kunit;

	extern char	curfil[MXFLEN];
	extern FILE	*flun;
	extern int	opnfil;

	extern int	kcolr;
	extern int	kcolr2;
	extern int	kfcod, kpipsz, kpipst, kpipdr;
	extern float	rwtsiz;
	extern int	kwtwid;
	extern float	rctsiz;
	extern int	kctwid;
	extern float	ricsiz;
	extern int	kicwid;
	extern float	rptsiz;
	extern int	kptwid;
	extern float	rpwsiz;
	extern int	kpwwid;
	extern float	rsksiz;
	extern int	kskwid, ksktyp;
	extern float	rspsiz;
	extern int	kspwid;
	extern float	rtbsiz;
	extern int	ktbwid;
	extern int	kmark, kmkwid, kmktyp;
	extern float	rmksiz;
	extern float	rbrsiz;
	extern int	kbrwid, kbrtyp;
	extern float	rarsiz, rarhsz;
	extern int	karwid, kartyp;
	extern int	kltyp, klthw, klwid, klwhw;
	extern float	rslsiz;
	extern int	ksltyp, kslstr, ksldir, kslwid;
	extern float	rtxsz;
	extern int	ktxfn, ktxhw, ktxwid, kbrdr, krrotn, kjust;
	extern char	kgtyp;
	extern int	kgnum;
	extern int	kgrpns[MAX_GROUP_TYPE];
	extern int	kgindx[MAX_GROUP_TYPE];
	extern int	ksmtyp, ketype;
	extern float	rdens, rtensn;
	extern float	rszdsh;
	extern int	kfillt;
	extern float	rszfil;
	extern int	khwid, klwidh;
	extern float	rhshsz;
	extern float	rcsysz;
	extern int	kcsywd;

#endif

/* Prototype for private functions used for VG */
void vcirc ( float *xcent, float *ycent, float *xcumpt,
                                float *ycumpt, int *np, int *iret );
void vegrp ( int *iret );
void vfill ( int *np, float x[], float y[], int *iret );
void vfrnt ( int *np, float x[], float y[], int *iret );
void vhash ( int *np, float x[], float y[], float dir[], int *iret );
void vinita ( int *iunit, char *filnam, int *lenf, int *itype, int *iret );
void vline ( int *np, float x[], float y[], int *iret );
void vmark ( int *np, float x[], float y[], int *iret );
void vopen ( int *iret );
void vsclr2 ( int *icolr, int *iclr2, int *iret );
void vscolr ( int *icolr, int *iret );
void vsdash ( float *szdsh, int *iret );
void vsdatt ( int *iunit, char *fname, int *itype, int *iret );
void vclosp ( int *iret );
void vendd ( int *ieop, int *iret );
void vsetgrps (	int *iret );
void vsfill ( float *szfil, int *iftyp, int *iret );
void vsfrnt ( int *mfcod, float *pipsz, int *mpipst, int *mpipdr, int *iret );
void vsgrp ( int *igroup, int *iret );
void vsgtgn ( int *igtyp, int *ignum, int *iret );
void vshash ( float *size, int *ihwid, int *ilwid, int *iret );
void vsline ( int *mltyp, int *mlthw, int *mlwid, int *mlwhw, int *iret );
void vsmark ( int *imark, int *imkhw, float *size, int *iwidth, int *iret );
void vspln ( int *np, float x[], float y[], int *iret );
void vssmth ( int *ismtyp, float *dens, int *ietype, float *tensn, int *iret );
void vsspln ( int *msltyp, int *mslstr, int *msldir, float *tslsiz,
                                                int *mslwid, int *iret );
void vssymb ( int *isym, int *itype, float *size, int *iwidth, int *iret );
void vstext ( int *mtxfn, int *mtxhw, float *ttxsz, int *mtxwid,
                        int *mbrdr, int *mrrotn, int *mjust, int *iret );
void vswind ( int *iwnd, float *size, int *iwidth, int *itype,
                                                float *hdsiz, int *iret );
void vsymb ( int *isym, int *np, float code[], float x[], float y[],
                                int ixoff[], int iyoff[], int *iret );
void vtext ( float *x, float *y, char *text, int *lens, int *ixoff,
                                int *iyoff, float *rotat, int *iret );
void vtxsy ( int *itype, int *isym, int *ijust, int *ixoff, int *iyoff,
                float *rotn, float *x, float *y, char *text, int *lens,
                int *iret );
void vwind ( int *iwnd, int *np, float x[], float y[], float spd[],
                                                float dir[], int *iret );

	
