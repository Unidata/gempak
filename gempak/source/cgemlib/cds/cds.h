/************************************************************************
 * cds.h								*
 * Contains any structures or global variables for the cds library.	*
 *									*
 **									*
 * D.W.Plummer/NCEP	07/97	Created					*
 * D.W.Plummer/NCEP	10/97	Added cdsFill				*
 * C. Lin/EAI		03/98	Added scaling factors			*
 * S. Law/GSC		04/98	Added density factors			*
 * F. J. Yen/NCEP	05/98	Updated type for cdsColor and cdsFill;	*
 *				removed cdsPipStroke and cdsSplStroke	*
 * D.W.Plummer/NCEP	 6/98	change cdsColor and cdsFill back to int	*
 * F. J. Yen/NCEP	10/99	Added cdsUset and included cescmn.h	*
 * F. J. Yen/NCEP	10/99	Removed definition of UATTRIBD_TBL	*
 * T. Piper/SAIC	12/05	Made cdsUattr a pointer			*
 ***********************************************************************/
#include "cescmn.h"

/* 
 * 	Color for override
 */

extern int     cdsColor;

/* 
 * 	Flag for fill
 */

extern int     cdsFill;

/* 
 * 	Attribute scaling factors	
 */

extern float   cdsPipSiz;
extern float   cdsFrntStrngth;

extern float   cdsSymWdth;
extern float   cdsSymSiz;

extern float   cdsWindWdth;
extern float   cdsWindSiz;
extern float   cdsWindHdsiz;

extern float   cdsLineWdth;

extern float   cdsSplWdth;
extern float   cdsSplSiz;

extern float   cdsTxtSiz;
extern float   cdsTxtWdth;

extern float   cdsSptSiz;
extern float   cdsSptWdth;

/* 
 * 	Smoothing density factors	
 */

extern float	cdsSmthDens[3];

/*
 *	User attribute structure
 */

#ifdef CDS_GLOBAL
	Setting_t		*cdsUattr;
	int			numUset;
#else
	extern Setting_t	*cdsUattr;
	extern int		numUset;
#endif
