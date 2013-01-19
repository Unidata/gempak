/************************************************************************
 * ARBCMN.H								*
 *									*
 * This header file declares the global variables used in the RBK 	*
 * device driver.							*
 *									*
 **									*
 * Log:									*
 * A. Hardy/GSC		 8/98						*
 * A. Hardy/GSC		 9/98  	Added newname; change array size for	*
 *				flcls, bckgnd and basdat		*
 * S. Jacobs/NCEP	 8/99	Added info for medium range products	*
 ***********************************************************************/

#include "geminc.h"
#include "gemprm.h"

#define BMASK		0xff
				/* Mask for getting a single byte */

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
					     2 = AWIPS DATA BASE
					   <>2 = OSO TRANSMISSION */

	int		kunit;
					/* Type of output (Used for XW)
					   1 = GEMPAK
					   2 = MOTIF */

        int		kxsize;
					/* X size in pixels */

        int		kysize;
					/* Y size in pixels */

        int		kmap, ktime;
					/* AFOS map id, Valid time
					   for the product */

        char            pil[4];
					/* PIL id */

        char		descr[33];
					/* Product description */
        
	int		numout;
					/* Number of bytes in output file */

	int		kyy, kmm, kdd, khh, knn, kss, kjd;
	int		minutes;
					/* Date and time information 
                                           for valid time */

	int		jyy, jmm, jdd, jhh, jnn, jss, jjd;
					/* Date and time information
                                           for creation time */

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

        char            awpid[11];
                                        /* AFOS ID number */
					
        char            extnd[7];
                                        /* Product ID extension */

        char            wmo[7];           
                                        /* WMO header */

        char            prodid[11];
					/* Product identification */

        char            basdat[9];
					/* Base date and time */
	
        char            bckgnd[7];
					/* Background name */

        char            flcls[2];
					/* Field classification */

        char            newname[10];
					/* Full PIL name */

        int             nrefpt;
					/* Number of reference points */
        int             cordfg;
	                                /* Background corrdinate flag */


        int             sclint, sclfac, rettim, crdflg, areacd, label; 
					/*  Codes and corner points */

	int             ilat[4], ilon[4], iangles[3];
					/*  Corner and central lats./lons. */

        int             txtchr;

        char            kloc[5];
					/*  KWBC header */
        int             pick;           
					/*  Map boundaries flag */
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
        extern int		kmap, ktime;
        extern char             pil[4];
        extern char		descr[33];
	extern int		numout;
	extern int		kyy, kmm, kdd, khh, knn, kss, kjd; 
	extern int		jyy, jmm, jdd, jhh, jnn, jss, jjd;
	extern int		minutes;
	extern int		kcolr;
	extern int		kjust;
	extern int		blank;
	extern int		kgtyp, kgnum;
	extern int		mxx, myy;
	extern char		mrchr[4][13];
	extern int		mlen[4];
	extern int		nchr;

        extern char             awpid[11];
        extern char             extnd[7];
        extern char             kloc[5];
        extern char             wmo[7];
        extern char             prodid[11];
        extern char             basdat[9];
        extern char             bckgnd[7];
        extern char             flcls[2];
        extern char             newname[10];
	extern int              pimap;
        extern int              rettim;
        extern int              crdflg;
        extern int              areacd;
        extern int              label; 
	extern int              cordfg;
        extern int              nrefpt;
        extern int              sclint, sclfac; 
	extern int              ilat[4];
        extern int              ilon[4];
	extern int              iangles[3];
        extern int              txtchr;
        extern int              pick;

#endif

/*
 *  RBK prototypes
 */

void aclear ( int *iret );
void aclosp ( int *iret );
void aegrp ( int *iret );
void aendd ( int *ieop, int *iret );
void afill ( int *np, int ix[], int iy[], int *iret );
void ainita ( int *iunit, char *fname, int *lenf, int *itype,
		int ibase[5], float *xsz, float *ysz, int *ileft,
		int *ibot, int *iright, int *itop, int *numclr, int *iret );
void aline  ( int *np, int ix[], int iy[], int *iret );
void aopen  ( int *iret );
void aqdatt ( int *iunit, char *fname, int *lenf, int *itype,
		float *xsz, float *ysz, int *ncurwn, int *iret );
void ascolr ( int *icolr, int *iret ); 
void asdatt ( int *iunit, char *fname, int *lenf, int *itype,
		int ibase[5], float *xsz, float *ysz, int *ileft,
		int *ibot, int *iright, int *itop, int *numclr, int *iret );
void asgrp ( int *igroup, int *iret ); 
void astext ( int *itxfn, float *sztext, int *ijust, float *txsize, int *iret );
void asymb ( int *isym, int *np, float code[], float x[], float y[],
		int ixoff[], int iyoff[], int *ispanx, int *ispany,
		int *icleft, int *icrght, int *icbot, int *ictop, int *iret );
void atext ( float *xr, float *yr, char *cchar, int *lens, int *ixoff,
		int *iyoff, float *rotat, int *ispanx, int *ispany, 
		int *icleft, int *icrght, int *icbot, int *ictop, int *iret );
void awrbuf ( unsigned char in[], int num, int *iret );

