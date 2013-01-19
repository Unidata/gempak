
/************************************************************************
 * crgcmn.h								*
 * This header file contains the structure for the display range of	*
 * each element in the vg file.						*
 *									*
 **									*
 * E. Wehner/EAi	 8/97	Created					*
 * F.J.Yen/NCEP		 1/98	Cleaned up.				*
 * E. Safford/GSC	03/98	add selected field to rangeinfo		*
 * C. Lin/EAI		04/98	add grptyp, and grpnum fields		*
 * D.W.Plummer/NCEP	 4/98	Added vg_type				*
 * S. Law/GSC		05/98	Added vg_class				*
 * J. Wu/GSC		03/01	Added definitions for EXTRA & EXTRA_SM  *
 * J. Wu/SAIC		12/01	add "layer" to RangeInfo		*
 * E. Safford/SAIC	01/02	reduce EXTRA_SM from 100 to 30		*
 * J. Wu/SAIC		07/04	add "dsplyFilter" to RangeInfo		*
 * J. Wu/SAIC		07/07	add "assocRec" and "auxRec" to RangeInfo*
 ***********************************************************************/

#ifndef CRGCMN_H
#define CRGCMN_H

#include "geminc.h"
#include "gemprm.h"
#include "pgprm.h"

/*
 * RangeInfo - Ranging information record.
 */

typedef struct rangeinfo
{
    char        vg_class;
    char        vg_type;
    char        grptyp;
    int         grpnum;
    float       rleft;
    float       rright;
    float       rtop;
    float       rbottom;
    int         ioffset;         /* file offset into VGF file of the elem */
    char	selected;
    int         layer;		 /* layer of the elem. in PGEN */
    filter_t    dsplyFilter;	 /* display filter of the elem.*/
    int         assocRec;	/* index value for second range record */
    int         auxRec;		/* indicate if this is primary/second record */
} RangeInfo;


#ifdef CRG_GLOBAL
	RangeInfo   range[MAX_EDITABLE_ELEMS]; /* ranging information */
#else
	extern RangeInfo	range[];
#endif

#define EXTRA_SM  30   /* extra space allowed for line, front, circle */
#define EXTRA     10   /* extra space allowed for all others          */

#define NO_ASSOC_REC	(-1)   /* indicate second range record 	*/
#define PRIMARY_REC	(0)    /* indicate the primary range record */
#define SECONDARY_REC	(1)    /* indicate the second range record */

#endif
