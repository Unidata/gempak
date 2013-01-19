/************************************************************************
 * nagrib.h                                                             *
 *                                                                      *
 * This header file is used in the NAGRIB program.			*
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         07/06   Created                                 *
 ************************************************************************/

#ifndef NAGRIB_H_
#define NAGRIB_H_

#include	"geminc.h"
#include        "gemprm.h"

/*
 * Structure for user inputs.
 */
typedef struct {
    char gbfile[LLMXLN+1];	/* GRIB data file name */
    char indxfl[LLMXLN+1];	/* GRIB index file name */
    char gdoutf[LLMXLN+1];	/* Output grid file */
    char proj[LLMXLN+1];	/* Map projection/angles/margins|drop flag */
    char grdarea[LLMXLN+1];	/* Area covered by grid */
    char kxky[LLMXLN+1];	/* Number of grid points in x;y */
    char maxgrd[LLMXLN+1];	/* Maximum number of grids */
    char cpyfil[LLMXLN+1];	/* Grid file whose navigation is to be used
                                 * in new grid file | subare */
    char garea[LLMXLN+1];	/* Graphics area */
    char output[LLMXLN+1];	/* Output device/filename */
    char gbtbls[LLMXLN+1];	/* Input GRIB decoding tables */
    char gbdiag[LLMXLN+1];	/* GRIB diagnostic elements */
    int  pdsext;		/* Y or N, add PDS extension if found */
    int  overwr;		/* Overwr flag */
} NAGRIB_input;

/*
 * APIs.
 */
void naginp ( NAGRIB_input *ui, int *iret );
void nagcut ( const float *sg, const int *ixf, const int *iyf,
              const char *parm, float *sgo, int *iret );
void nagfil ( const float *sg, const int *ixf, const int *iyf,
              const char *parm, float *fg, float *fhv, int *iret );
void nagflh ( const int *intflg, const float *v, const int *nxvh,
              const int *nyvh, float *vh, int *iret );
void nagflv ( const int *intflg, const float *h, const int *nxhv,
              const int *nyhv, float *hv, int *iret );

#endif
