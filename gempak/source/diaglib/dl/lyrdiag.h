/************************************************************************
 * lyrdiag.h                                                            *
 *                                                                      *
 * This header file is used in the layer grid diagnostics package.   	*
 **                                                                     *
 * Log:                                                                 *
 * S. Gilbert/NCEP	12/05	Created					*
 * R. Tian/SAIC		 9/06	Removed grc.h, fortran_wrappers.h	*
 ************************************************************************/

#ifndef LYRDIAG_H_
#define LYRDIAG_H_

#include "geminc.h"
#include "gemprm.h"
#include "dl.h"

/*
 * Maximum number of layer function arguments.
 */
#define MAXARG	16

struct lyrdiag {
    /*
     * List of levels/layers.
     */
    int lyrlvs1[LLMXLV];
    int lyrlvs2[LLMXLV];

    /*
     * ID # to name LYR_ output.
     */
    int lyrnid;
};

/*
 * DL globals.
 */
#ifdef DE_GLOBAL
    struct lyrdiag _lyrdiag;
#else
    extern struct lyrdiag _lyrdiag;
#endif

#endif
