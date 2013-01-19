/************************************************************************
 * sfvcmn								*
 *									*
 * This is the header file for the code used to access the VG file in	*
 * the program SFVGSF.							*
 *									*
 **									*
 * S. Jacobs/NCEP	 3/99						*
 * S. Jacobs/NCEP	 3/99	Fixed the size of the array declared	*
 ***********************************************************************/

#include "geminc.h"
#include "gemprm.h"

#include "vgstruct.h"

/*
 * This parameter defines the group type number for station models.
 */
#define STNMDL	10

#ifdef UNDERSCORE
#define sfvvgf	sfvvgf_
#define sfvget	sfvget_
#endif

/*
 * Global data structure for reading a VG file and saving the data.
 */
typedef struct {
    char	stid[9];
    float	rdata[MMPARM];
} Gem_data_t;

#ifdef SFVINIT

	Gem_data_t	stns[LLSTFL+1];
				/* Structures for station data */

#else

	extern Gem_data_t	stns[LLSTFL+1];

#endif

/*
 * prototypes
 */

void sfvget ( int *ngrp, int *nparm, char stid[], int *nstr,
				float rdata[], int *iret );

void sfvvgf ( char vgfile[], int *lenv, int *nparm, int iploc[],
				int icolr[], int *ngrps, int *iret );


