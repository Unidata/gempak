/************************************************************************
 * NCCMN.H								*
 *									*
 * This header file declares the variables used in the Computer		*
 * Graphics Metafile device driver (NC).				*
 *									*
 **									*
 * Log:									*
 * S. Jacobs/NMC	 5/94						*
 * S. Jacobs/NMC	 6/94	General clean up			*
 * C. Lin/NMC	        10/94	Add text alignment			*
 * S. Jacobs/NCEP	 3/98	Added NPATFL				*
 * S. Jacobs/NCEP	 7/98	Added tsize, txszx and txszy		*
 * R. Tian/SAIC		 4/02	Added fxsize and fysize			*
 * m.gamazaychikov/SAIC 01/03   Removed NPATFL                          *
 * T. Piper/SAIC	03/04	Added numerous prototypes		*
 * E. Safford/SAIC	01/08	include proto_xw.h			*
 ***********************************************************************/

#include "geminc.h"
#include "gemprm.h"
#include "cgmcmn.h"
#include "proto_xw.h"

/*---------------------------------------------------------------------*/

#ifdef GLOBAL			/* declare variables */

	FILE		*meta_fp;
				/* File pointer */
	int		meta_id, byte_count, frame_count;
				/* File id number,
				   Byte count in file,
				   Frame count for file */

	nc_file_header	meta_head;
				/* File header structure */
	nc_frame_header meta_frame[MAX_NFRM];
				/* Array of frame header
				   structures */

	char		ncbuffer[MAX_BUFSZ];
				/* Output buffer */

	char		curfil[MAX_FNLEN];
				/* Current filename */
	int		opnfil, exist;
				/* File open flag, 
				   File exist flag */
	int		maxfrm, versn, machtp;
				/* For the current file:
				   Maximum number of frames,
				   File version number,
				   Machine type */

	int		lcolor_req, lcolor_set;
				/* Line color requested,
				   Line color set */
	int		lwidth_req, lwidth_set;
				/* Line width requested,
				   Line width set */
	int		fcolor_req, fcolor_set;
				/* Fill color requested,
				   Fill color set */
	int		fstyle_req, fstyle_set;
				/* Fill style requested,
				   Fill style set */
	int		txfont_req, txfont_set;
				/* Text font requested,
				   Text font set */
	int		txsize_req, txsize_set;
				/* Text size requested,
				   Text size set */
	int		txalgn_req, txalgn_set;
				/* Text alignment requested,
				 * 0 -- normal *
				 * 1 -- left   *
				 * 2 -- center *
				 * 3 -- right  *
				 * Text alignment set */

	int		kctype;
				/* Color scheme type
				   0 = Monochrome
				   1 = Grayscale
				   2 = Color */

	int		kunit;
				/* Type of output (Used for XW)
				   1 = GEMPAK
				   2 = MOTIF */

	float		tsize, txszx, txszy;
				/* Text font size in the
				   X and Y directions */

	unsigned short		fxsize, fysize;
				/* Current CGM frame size in the
				   X and Y directions */

#else				/* declare variables as external */

	extern FILE		*meta_fp;
	extern int		meta_id, byte_count, frame_count;

	extern nc_file_header	meta_head;
	extern nc_frame_header 	meta_frame[MAX_NFRM];

	extern char		ncbuffer[MAX_BUFSZ];

	extern char		curfil[MAX_FNLEN];
	extern int		opnfil, exist;
	extern int		maxfrm, versn, machtp;

	extern int		lcolor_req, lcolor_set;
	extern int		lwidth_req, lwidth_set;
	extern int		fcolor_req, fcolor_set;
	extern int		fstyle_req, fstyle_set;
	extern int		txfont_req, txfont_set;
	extern int		txsize_req, txsize_set;
	extern int		txalgn_req, txalgn_set;

	extern int		kctype;
	extern int		kunit;

	extern float		tsize, txszx, txszy;
	extern unsigned short	fxsize, fysize;

#endif					/* GLOBAL */

/*
 *  NC prototypes
 */

void mclear ( int *iret );
void mclose ( int *iret );
void mdots ( int *ix, int *iy, int *irad, int *iret );
void mfill ( int *np, int ix[], int iy[], int *iret );
void minita ( int *iunit, char *filnam, int *lenf, int *itype,
              float *xsize, float *ysize, int *ileft, int *ibot,
              int *iright, int *itop, int *iret );
void mline ( int *np, int ix[], int iy[], int *iret );
void mmesg ( char *messag, int *ilen, int *iret );
void mopen ( int *iret );
void mqcomp ( int *index, char *clrnam, int *red, int *green,
                int *blue, char *xname, int *clen, int *xlen, int *iret );
void mqdatt ( int *iunit, char *filnam, int *lenf, int *itype,
                float *xsize, float *ysize, int *ncurwn, int *iret );
void mrflhd ( int *iret );
void mrfrhd ( int *iret );
void mscolr ( int *icolr, int *iret );
void msdatt ( int *iunit, char *filnam, int *lenf, int *itype,
              float *xsize, float *ysize, int *ileft, int *ibot,
              int *iright, int *itop, int *iret );
void msfill ( float *szfil, int *iftyp, int *iret );
void mslwid ( int *ilwid, int *iret );
void mstext ( int *font, float *size, int *ijust, float *txsize, int *iret );
void mtext ( float *xr, float *yr, char textstr[], int *ilen,
                int *ixoff, int *iyoff, float *rotat, int *ispanx,
                int *ispany, int *icleft, int *icrght, int *icbot,
                int *ictop, int *iret );
void mtextc ( float *xr, float *yr, char textstr[], int *ilen,
                int *ixoff, int *iyoff, float *rotat, int *ispanx, 
                int *ispany, int *icleft, int *icrght, int *icbot, 
                int *ictop, int *iret );
void mwflhd ( int *iret );
void mwfrhd ( int *iret );

