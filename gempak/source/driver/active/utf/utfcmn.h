/************************************************************************
 * UTFCMN.H								*
 *									*
 * This header file declares the global variables used in the UTF	*
 * device driver.							*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NCEP	 7/97	Added asize				*
 * S. Jacobs/NCEP	 8/97	Reorganized and renamed variables	*
 * S. Jacobs/NCEP	 9/97	Removed minutes				*
 * S. Jacobs/NCEP	10/97	Replaced minutes			*
 * S. Jacobs/NCEP	10/97	Added kjust				*
 * S. Jacobs/NCEP	 3/99	Added info for medium range products	*
 ***********************************************************************/

#include "geminc.h"
#include "gemprm.h"

#define BMASK		0xff
				/* Mask for getting a single byte */

#define MXAFOS		16108
				/* Max number of bytes in an AFOS file */

#define MXPTS		5000
				/* Max number of vertices on a line */

/*---------------------------------------------------------------------*/

#ifdef UGLOBAL

	char		filnam[80];
					/* Output file name */

	FILE		*flun;
					/* Output file identifier */

	int		opnfil;
					/* Open file flag */

  	float		txszx, txszy;
					/* Text font pixel size in the
					   X and Y directions */

	int		nfntsz;
					/* AFOS text size */

	float		asize;
					/* Fractional text size */

	int		kctype;
					/* Output file type
					     2 = NAFOS
					   <>2 = AFOS */

	int		kunit;
					/* Type of output (Used for XW)
					   1 = GEMPAK
					   2 = MOTIF */

        int		kxsize;
					/* X size in pixels */

        int		kysize;
					/* Y size in pixels */

        int		kmap, ktime, kgscl;
					/* AFOS map id, Valid time
					   for the product,
					   Geographic scale factor */

        char            pil[4];
					/* PIL id */

        char		descr[33];
					/* Product description */
        
	unsigned char	outbuf[MXAFOS];
					/* Output buffer */

	int		numout;
					/* Number of bytes in outbuf */

	int		kyy, kmm, kdd, khh, knn, kss, kjd;
	int		minutes;
					/* Date and time information */

	int		kcolr;
					/* Current color */

	int		kjust;
					/* Text justification */

	int		blank;
					/* Blank line flag (move to) */

	int		kgtyp, kgnum;
					/* Group type and group number */

	int		mxx, myy;
					/* Stored X, Y for med range */

	char		mrchr[4][13];
	int		mlen[4];
	int		nchr;
					/* String array for med range,
					   String lengths,
					   Number of strings in group */

#else

	extern char		filnam[80];
	extern FILE		*flun;
	extern int		opnfil;
	extern float		txszx, txszy;
	extern int		nfntsz;
	extern float		asize;
	extern int		kctype;
	extern int		kunit;
	extern int		kxsize;
	extern int		kysize;
        extern int		kmap, ktime, kgscl;
        extern char             pil[4];
        extern char		descr[33];
	extern unsigned char	outbuf[MXAFOS];  
	extern int		numout;
	extern int		kyy, kmm, kdd, khh, knn, kss, kjd;
	extern int		minutes;
	extern int		kcolr;
	extern int		kjust;
	extern int		blank;
	extern int		kgtyp, kgnum;
	extern int		mxx, myy;
	extern char		mrchr[4][13];
	extern int		mlen[4];
	extern int		nchr;

#endif

/*
 *  UTF prototypes
 */

void uclear (	int	*iret );
void uclosp (	int 	*iret );
void uegrp  (	int	*iret );
void uendd  (	int	*ieop,
		int	*iret );
void ufill  (	int     *np,
	        int      ix[],
        	int      iy[],
        	int     *iret );
void uinita (	int     *iunit,
		char    *fname,
	 	int     *lenf,
		int     *itype,
	 	int     *ibase,
		float   *xsz,
		float   *ysz,
		int     *ileft,
		int     *ibot,
		int     *iright,
		int     *itop,
		int     *numclr,
		int     *iret );
void uline (	int	*np,
		int	 ix[],
		int	 iy[],
		int	*iret );
void uopen (	int	*iret );
void uqdatt (	int	*iunit,
		char    *fname,
       		int     *lenf,
        	int     *itype,
        	float   *xsz,
        	float   *ysz,
        	int     *ncurwn,
        	int     *iret );
void uscolr (	int	*icolr,
		int	*iret );
void usdatt (	int     *iunit,
        	char    *fnamer,
        	int     *lenfr,
        	int     *itype,
        	int     *ibase,
        	float   *xsz,
        	float   *ysz,
        	int     *ileft,
        	int     *ibot,
        	int     *iright,
        	int     *itop,
        	int     *numclr,
        	int     *iret );
void usgrp (	int     *igroup,
		int     *iret );
void ustext (	int     *itxfn,
        	float   *sztext,
        	int     *ijust,
        	float   *txsize,
        	int     *iret );
void uswap (	unsigned char   va[],
        	int             ilen,
        	unsigned char   newva[],
        	int             *jlen,
        	int             *iret );
void usymb (	int     *isym,
        	int     *np,
        	float   code[],
        	float   x[],
        	float   y[],
        	int     ixoff[],
        	int     iyoff[],
        	int     *ispanx,
        	int     *ispany,
        	int     *icleft,
        	int     *icrght,
        	int     *icbot,
        	int     *ictop,
        	int     *iret );
void utext (	float   *xr,
        	float   *yr,
        	char    *cchar,
        	int     *lens,
        	int     *ixoff,
        	int     *iyoff,
        	float   *rotat,
        	int     *ispanx,
        	int     *ispany,
        	int     *icleft,
        	int     *icrght,
        	int     *icbot,
        	int     *ictop,
        	int     *iret );
void uwrbuf (	unsigned char   in[],
        	int             num,
        	int             *iret );










