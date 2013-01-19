#include "crgcmn.h"


void crg_clear( int elnum, int *iret )
/************************************************************************
 * crg_clear                                                            *
 *                                                                      *
 * This function clears the range record for an element.		*
 *                                                                      *
 * void crg_clear ( elnum, iret ) 					*
 *                                                                      *
 * Input parameters:                                                    *
 *	elnum		int		Element number to clear		*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					-2 = Element is out of bounds	*
 *									*
 * Log:                                                                 *
 * E. Wehner/EAi         8/97	Created					*
 * F.J.Yen/NCEP		 1/98	Cleaned up.				*
 * E. Safford/GSC	03/98	Added selected initialization		*
 * C. Lin/EAI		04/98	Added grptyp, grpnum initialization	*
 * F.J.Yen/NCEP		04/98	Added vg_type. Removed extra ioffset	*
 * J. Wu/SAIC		12/01	initialize layers			*
 * E. Safford/SAIC	04/02	return error code if elnum bad		*
 * J. Wu/SAIC		07/04	initialize display filter		*
 * J. Wu/SAIC		07/07	clear second range record		*
 ***********************************************************************/
{
    *iret = 0;

    if ( elnum < 0 || elnum >= MAX_EDITABLE_ELEMS) {
	*iret = -2;
    }
    else {
        range[elnum].vg_class =  0;
        range[elnum].vg_type  =  0;
        range[elnum].grptyp   =  0;
        range[elnum].grpnum   =  0;
        range[elnum].ioffset  = -1;
        range[elnum].selected =  0;
        range[elnum].layer    =  0;
        strcpy ( range[elnum].dsplyFilter, "ALL" );
        
	if ( range[elnum].assocRec != NO_ASSOC_REC ) {
	    crg_clear ( range[elnum].assocRec, iret );
	}   
	
	range[elnum].assocRec = NO_ASSOC_REC;
	range[elnum].auxRec = PRIMARY_REC;
	
    }

}
