#include "xwcmn.h"

void xpgsetlayer ( int layer )
/************************************************************************
 * xpgsetlayer                                        			*
 *                                                                      *
 * This function sets the current PGEN layer in XW. 			*
 *                                                                      *
 * void xpgsetlayer ( layer )  						*
 *                                                                      *
 * Input parameters:                                                    *
 *      layer           int             Current PGEN layer              *
 *                                                                      *
 * Output parameters:                                                   *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * J. Wu/SAIC		12/01	initial coding				*
 ***********************************************************************/
{
    if ( layer >= 0 ) {
        _pgLayer = layer;
    }
}

