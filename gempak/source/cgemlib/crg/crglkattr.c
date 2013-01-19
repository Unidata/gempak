#include "crgcmn.h"


void crg_lkattr ( int elnum, int *iret )
/************************************************************************
 * crg_lklink                                                      	*
 *                                                                      *
 * This function links an "assocRec"'s fields the same as the pritmary	*
 * range record's.							*
 *                                                                      *
 * void crg_lkattr ( elnum, iret )					*
 *                                                                      *
 * Input parameters:                                                    *
 *	elnum		int		Element number 			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					-2 = elnum is out of bound	*
 *									*
 * Log:                                                                 *
 * J. Wu/SAIC		07/07	initial coding				*
 ***********************************************************************/
{
    int		assocrec;   
/*---------------------------------------------------------------------*/    
    
    *iret = 0;
    assocrec = range[ elnum ].assocRec;
    
    if ( elnum < 0 || elnum >= MAX_EDITABLE_ELEMS ||
         assocrec == NO_ASSOC_REC ) {
	
	*iret = -2;
    }
    else {    
        range[ assocrec ].vg_class =  range[ elnum ].vg_class;
        range[ assocrec ].vg_type  =  range[ elnum ].vg_type;
        range[ assocrec ].grptyp   =  range[ elnum ].grptyp;
        range[ assocrec ].grpnum   =  range[ elnum ].grpnum;
        range[ assocrec ].selected =  range[ elnum ].selected;
        range[ assocrec ].layer    =  range[ elnum ].layer;
        
	strcpy ( range[ assocrec ].dsplyFilter, range[ elnum ].dsplyFilter );
    }

}
