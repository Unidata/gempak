#include "geminc.h"
#include "gemprm.h"

void gdxgtsf ( char *time, int *ivcord, float *rgx, float *rgy, int *nhxs, 
               float *grid, float *vclsfc, int *havsfc, char *parm, int *iret,
               size_t timesize, size_t parmsize );

void gdxgts  ( char *time, int *ivcord, float *rgx, float *rgy, int *nhxs, 
               float *vclsfc, int *havsfc, char *parm, int *iret,
               size_t timesize, size_t parmsize );

void gdxgts  ( char *time, int *ivcord, float *rgx, float *rgy, int *nhxs, 
               float *vclsfc, int *havsfc, char *parm, int *iret,
               size_t timesize, size_t parmsize )
/************************************************************************
 * gdxgts								*
 *									*
 * This subroutine gets the surface data for a cross section.		*
 * This function is a wrapper for subroutine GDXGTSF.  It's sole        *
 * purpose is to dynamically allocate a work array, and then pass       *
 * it and all of this function's arguments on to GDXGTSF.               *
 * The work array is freed from memory as soon as GDXGTSF returns.      *
 *									*
 * gdxgts  ( time, ivcord, rgx, rgy, nhxs, vclsfc, havsfc, parm, iret ) *
 *									*
 * Input parameters:							*
 *	TIME  (2)	  CHAR*		Time to search for levels	*
 *	IVCORD		  INTEGER	Vertical coordinate for search	*
 *	RGX  (NHXS)	  REAL		X grid coordinates		*
 *	RGY  (NHXS)	  REAL		Y grid coordinates		*
 *	NHXS		  INTEGER	Number of xsect pts in horiz.	*
 *									*
 * Output parameters:							*
 *      VCLSFC (NHXS)     REAL 		Vert coord location of sfc	*
 *      HAVSFC            LOGICAL       Flag for existence of sfc	*
 *	PARM		  CHAR*		Parameter name			*
 *	IRET		  INTEGER	Return code			*
 *					  0 = normal return		*
 *					 -6 = GVCORD is invalid		*
 *					 +2 = no sfc value found	*
 **									*
 * Log:									*
 * S. Gilbert/NCEP       8/07   					*
 * S. Jacobs/NCEP	02/08	Set grid size as LLMXGD			*
 * T. Piper/SAIC	03/08	Replace cmm functions with Macros	*
 * B. Hebbard/NCEP	08/21	Changed strlen in protos int->size_t    *
 ***********************************************************************/
{
    int   kx, ky, kxy, ier;
    float *grid;
/*---------------------------------------------------------------------*/
    *iret = 0;

/*
 *  Allocate work array
 */
    kxy = LLMXGD;
    G_MALLOC ( grid, float, kxy, "gdxgts - grid");

    gdxgtsf ( time, ivcord, rgx, rgy, nhxs, 
              grid, vclsfc, havsfc, parm, iret,
              timesize, parmsize );

/*
 *  Free work array
 */
    G_FREE ( grid, float );

}
