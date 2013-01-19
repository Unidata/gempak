/************************************************************************
 * gddiag.h                                                             *
 *                                                                      *
 * This header file is used in the GDLIST program.                      *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC         09/06   Created                                 *
 ************************************************************************/

#ifndef GDDIAG_H_
#define GDDIAG_H_

#include "geminc.h"
#include "gemprm.h"

/*
 * Structure for user inputs.
 */
typedef struct {
    char gdfile[LLMXLN+1];      /* Grid file */
    char gdoutf[LLMXLN+1];	/* Output grid file */
    char gfunc [LLMXLN+1];	/* Scalar grid */
    char gdatim[LLMXLN+1];      /* Grid date/time */
    char glevel[LLMXLN+1];      /* Grid level */
    char gvcord[LLMXLN+1];      /* Grid vertical coordinate */
    char grdnam[LLMXLN+1];	/* Grid parameter name */
    char grdtyp[LLMXLN+1];	/* Diagnostic grid type (Scalar/Vector) */
    char gpack [LLMXLN+1];	/* Packing type/number of bits */
    char grdhdr[LLMXLN+1];	/* Grid Header Flags */
    char proj  [LLMXLN+1];      /* Map projection/angles/margins|drop flag */
    char gdarea[LLMXLN+1];      /* Area covered by grid */
    char kxky  [LLMXLN+1];	/* Number of grid points in x;y */
    char maxgrd[LLMXLN+1];	/* Maximum number of grids */
    char cpyfil[LLMXLN+1];	/* rid file whose navigation is to be used
    				 * in new grid file | subare */
    char anlyss[LLMXLN+1];      /* Grid analysis block */
} GDDIAG_input;

/*
 * Could be removed after GDCFIL is translated.
 */
#ifdef UNDERSCORE
#define gdgcfl	gdgcfl_
#endif

/*
 * APIs.
 */
void gdginp ( GDDIAG_input *ui, int *iret );
void gdgwrt ( const float *grid, const char *time1, const char *time2,
              const int *level1, const int *level2, const int *ivcord,
              const char *parm, const char *grdnam, const char *gpack,
              const int *ihzrmp, const int *idrct, int *iret );

/*
 * Could be removed after GDCFIL is translated.
 */
void gdgcfl ( const char *gdfile, const char *proj, const char *gdarea,
              const char *kxky, const char *maxgrd, const char *cpyfil,
              const char *anlyss, int *iret, size_t, size_t, size_t,
	      size_t, size_t, size_t, size_t );

#endif
