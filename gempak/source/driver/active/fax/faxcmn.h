/************************************************************************
 * FAXCMN.H								*
 *									*
 * This header file declares the variables used in the FAX device driver*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 6/96   Created					*
 * S. Jacobs/NCEP	 1/97	Removed fontus				*
 * E. Safford/GSC	03/97	Removed include scnfll.h		*
 * E. Wehner/Eai	03/97	Fix for rotation			*
 * S. Jacobs/NCEP	 7/97	Reorganized and cleaned up		*
 * S. Jacobs/NCEP	 7/97	Added sixbit array and new parameters	*
 * S. Jacobs/NCEP	 7/97	Added kind for the indent table value	*
 * S. Jacobs/NCEP	 8/97	Added krsv for the reserved table value	*
 * S. Jacobs/NCEP	 9/97	Added tsfill and kfillt for fill type	*
 * S. Jacobs/NCEP	 5/98	Changed to allow for multiple subsets	*
 * A. Hardy/GSC		 1/00   Added the fax subroutine prototypes	*
 * T. Piper/SAIC	02/04	Calling seq. change for rinita/rsdatt	*
 * T. Piper/SAIC	12/07	Removed txsizr; not used!		*
 ***********************************************************************/

#include "geminc.h"
#include "gemprm.h"

#define MAXSIZ		1000000

#define MAXSUB		5

#define NHEAD		48

#define ZEROS_MODE	0
#define ONES_MODE	1
#define NO_MODE		2

#ifdef PGLOBAL

	int		faxflg;
				/* Flag to convert raster to 6-bit fax */

	unsigned char	rasimg[MAXSIZ];
				/* Raster graphics image */

	unsigned char	sixbit[MAXSIZ];
				/* 6-bit run length encoded format */

	char		wheel[5];
				/* Fax product wheel ID */

	char		subset[MAXSUB][5];
				/* Fax product subset ID */

	char		descr[MAXSUB][41];
				/* Fax product description */

	int		kbit[MAXSUB], klin[MAXSUB], krot[MAXSUB],
			kind[MAXSUB], krsv[MAXSUB];
				/* Bits/Line, Lines, Rotation,
				   Number of bits to indent,
				   Reserved value */

	int		nsub;
				/* Number of subsets */

	int		msize;
				/* Number of bytes in the raster image */

	int		nncolr;
				/* Number of colors allowed */

	int		opnfil;
				/* Open file flag */

	int		isfont, irfont;
				/* Text font set and requested */

	float		txszx, txszy;
				/* Text font size in the
				   X and Y directions */

	int		kfntsz;
				/* Saved font size */

   	int		ispanx, ispany;
				/* Direction of increasing x and y */

	char		filnam[80];
				/* Current output file name */

	int		pixval;
				/* Pixel value -- 0 for off, 1 for on */

	char		fontmap[94][170];
				/* loaded font map. max 34 pt. fnt */

	unsigned char	mskon[8], mskoff[8];
	unsigned char	mskonr[8], mskoffr[8];
				/* On and off bit masks */

	float		tsfill;
	int		kfillt;
				/* Fill pattern size and type */

#else
	
	extern int		faxflg;
	extern unsigned char	rasimg[MAXSIZ];
	extern unsigned char	sixbit[MAXSIZ];
	extern char		wheel[5];
	extern char		subset[MAXSUB][5];
	extern char		descr[MAXSUB][41];
	extern int		kbit[MAXSUB], klin[MAXSUB], krot[MAXSUB],
				kind[MAXSUB], krsv[MAXSUB];
	extern int		nsub;
	extern int		msize;
	extern int		nncolr;
	extern int		opnfil;
	extern int		isfont, irfont;
	extern float		txszx, txszy;
	extern int		kfntsz;
	extern int 		ispanx;
	extern int 		ispany;
	extern char		filnam[80];
	extern int		pixval;
	extern char		fontmap[94][170];
	extern unsigned char	mskon[8], mskoff[8];
	extern unsigned char	mskonr[8], mskoffr[8];
	extern float		tsfill;
	extern int		kfillt;

#endif

/*
 *  FAX prototypes
 */

void 	ras26bit ( 	int	*iret );

void 	rbrescirc ( 	int	icentx,
			int	icenty,
			int	irad,
			int	*iret );

void 	rcircpts ( 	int	ix,
			int	iy,
			int	icentx,
			int	icenty,
			int	*iret );

void 	rclear ( 	int	*iret );

void 	rclosp ( 	int	*iret );

int 	rcvt2cdc ( 	char	chasc );

void 	rcvt6bt ( 	int	*nbyte,
			int	*iret );

void 	rdots ( 	int	*ix,
			int	*iy,
			int	*ilwid,
			int	*iret );

void 	rendd ( 	int	*ieop,
			int	*iret );

void 	rfill ( 	int	*np,
			int	ix[],
			int	iy[],
			int	*iret );

void 	rinita ( 	char	*pname,
			float	*xsz,
			float	*ysz,
			int	*ileft,
			int	*ibot,
			int	*iright,
			int	*itop,
			int	*numclr,
			int	*iret );

void 	risched ( 	int	*ioff,
			int	*iret );

void 	rline ( 	int	*np,
			int	ix[],
			int	iy[],
			int	*iret );

void 	rmapid ( 	int	*iret );

void 	ropen ( 	int	*iret );

void 	rpadrec ( 	int	*istart,
			int	irecsz,
			int	*iret );

void 	rrunle ( 	int	*ipos,
			int	lenrun,
			int	nvalue,
			int	*iret );

void 	rscnfll ( 	int	*np,
			int	ix[],
			int	iy[],
			int	*iret );

void 	rscolr ( 	int	*icolr,
			int	*iret );

void 	rsdatt ( 	char	*pname,
			float	*xsz,
			float	*ysz,
			int	*ileft,
			int	*ibot,
			int	*iright,
			int	*itop,
			int	*numclr,
			int	*iret );

void 	rsdump (	int	*iret );

void 	rsfill (	float	*szfil,
			int	*iftyp,
			int	*iret );

void 	rsndsix (	int		jpos,
			unsigned  char	ch,
			int		*iret );

void 	rspan ( 	int	*inspanx,
			int	*inspany,
			int	*iret );

void 	rwrpxl ( 	int	bnum,
			int	lnum,
			int	*iret );
