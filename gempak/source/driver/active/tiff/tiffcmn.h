/************************************************************************
 * TIFFCMN.H								*
 *									*
 * This header file declares the variables used in the TIFF device	*
 * driver.								*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	12/98						*
 * S. Jacobs/NCEP	 9/00	Increased MAXSIZ; Added union Tstrip;	*
 *				Added ktype				*
 * S. Jacobs/NCEP	 5/02	Incresed MAXLIN: 3000 -> 4000		*
 * T. Piper/SAIC	03/04	Added tinita and tline			*
 * E. Safford/SAIC	01/08	include proto_xw.h			*
 ***********************************************************************/

#include "geminc.h"
#include "gemprm.h"
#include "proto_xw.h"


#define MAXSIZ		7000000
#define MAXLIN		4000

#define MXSHRT		0xffff
#define MXINT		0xffffffff

#define MMTAGS		17

#define TFSHFT		( 256*256 )

typedef union {
    struct {
	unsigned short	ident;
	unsigned short	ivers;
	unsigned int	ifdloc;
    } thdr;
    unsigned char	bytes[8];
} Tfilhdr;

typedef union {
    unsigned short	num;
    unsigned char	bytes[2];
} Tnumtags;

typedef union {
    struct {
	unsigned short	icode;
	unsigned short	itype;
	unsigned int	nitem;
	unsigned int	ivalue;
    } tinfo;
    unsigned char	bytes[12];
} Ttag;

typedef union {
    struct {
	unsigned int	ival1;
    	unsigned int	ival2;
    } ratio;
    unsigned char	bytes[8];
} Tdata;

typedef union {
    unsigned int	ivalue;
    unsigned char	bytes[4];
} Tstrip;

#ifdef TGLOBAL

   	int		ispanx, ispany;
				/* Direction of increasing x and y */

	unsigned char	rasimg[MAXSIZ];
				/* Raster graphics image */

	unsigned char	group4[MAXSIZ];
				/* Group4 Fax run length encoded
				   format */

	int		kbit, klin, krot;
				/* Bits/Line, Lines, Rotation */

	int		msize;
				/* Number of bytes in the raster image */

	int		nncolr;
				/* Number of colors allowed */

	int		opnfil;
				/* Open file flag */

	char		filnam[133];
				/* Current output file name */

	int		pixval;
				/* Pixel value -- 0 for off, 1 for on */

	unsigned char	mskon[8], mskoff[8];
				/* On and off bit masks */

	float		tszfil;
	int		kfillt;
				/* Fill pattern size and type */

	int		ktype;
				/* Pixmap type
				   B/W Compressed
				   Gray Uncompressed */

#else
	
	extern int 		ispanx, ispany;
	extern unsigned char	rasimg[MAXSIZ];
	extern unsigned char	group4[MAXSIZ];
	extern int		kbit, klin, krot;
	extern int		msize;
	extern int		nncolr;
	extern int		opnfil;
	extern char		filnam[133];
	extern int		pixval;
	extern unsigned char	mskon[8], mskoff[8];
	extern float		tszfil;
	extern int		kfillt;
	extern int		ktype;

#endif

/*
 *  TIFF prototypes
 */

void tbrescirc	( int	icentx,
		  int	icenty,
		  int	irad,
		  int	*iret );
void tcircpts	( int	ix,
		  int	iy,
		  int	icentx,
		  int	icenty,
		  int	*iret );
void tclear	( int	*iret );
void tclosp	( int	*iret );
void tdots 	( int	*ix,
		  int	*iy,
		  int	*ilwid,
		  int	*iret );
void tendd	( int	*ieop,
		  int	*iret );
void tfill	( int	*np,
		  int	ix[],
		  int	iy[],
		  int	*iret );
void tg4comp	( int	*kpos,
		  int	*iret );
void tinita	( char	*pname,
		  int	*itype,
		  float	*xsz,
		  float	*ysz,
		  int	*ileft,
		  int	*ibot,
		  int	*iright,
		  int	*itop,
		  int	*numclr,
		  int	*iret );
void tline	( int	*np,
		  int	ix[],
		  int	iy[],
		  int	*iret );
void tsatim	( char	filnam [],
		  int	*xispace0,
		  int	*yispace0,
		  int	*xispace1,
		  int	*yispace1,
		  int	*iret );
void tscnfll	( int	*np,
		  int	ix[],
		  int	iy[],
		  int	*iret );
void tscolr	( int	*icolr,
		  int	*iret );
void tsdatt	( char	*pname,
		  float	*xsz,
		  float	*ysz,
		  int	*ileft,
		  int	*ibot,
		  int	*iright,
		  int	*itop,
		  int	*numclr,
		  int	*iret );
void tsfill	( float	*szfil,
		  int	*iftyp,
		  int	*iret );
void tsicmn	( int	*hv,
		  int	*iret );
void tsopen	( int	*iret );
void tspan	( int	*inspanx,
		  int	*inspany,
		  int	*iret );
void ttiff	( int	*nbyte,
		  int	*iret );
void tuncomp	( int	*kpos,
		  int	*iret );
void twrpxl	( int	bnum,
		  int	lnum,
		  int	*iret );

