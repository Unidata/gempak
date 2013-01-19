#include "cvgcmn.h"
#include "gemprm.h"
#include "vgstruct.h"

void cvg_freeBkpts ( VG_DBStruct * el )
/************************************************************************
 * cvg_freeBkpts                                                        *
 *                                                                      *
 * This function frees the allocated break point array of a TCA element.*
 *                                                                      *
 * void cvg_freeBkpts ( el )   		                                *
 *                                                                      *
 * Input parameters:                                                    *
 *      el              VG_DBStruct *   pointer to a TCA element        *
 *                                                                      *
 * Output parameters:                                                   *
 *                      NONE                                            *
 * Return parameters:                                                   *
 *                      NONE                                            *
 *                                                                      *
 **                                                                     *
 * Log:                                                                 *
 * B. Yin/SAIC          08/04   Created                                 *
 * E. Safford/SAIC	05/05	add sanity check on vg_type		*
 ***********************************************************************/
{
   int ii;
/*---------------------------------------------------------------------*/


   if( el->hdr.vg_type != TCA_ELM ) {
      return;
   }

   for ( ii = 0; ii < el->elem.tca.info.wwNum; ii++ ) {
       
       if ( el->elem.tca.info.tcaww[ ii ].breakPnt ) {
          free ( el->elem.tca.info.tcaww[ ii ].breakPnt );
       }
   }

}
