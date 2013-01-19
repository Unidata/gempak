#include "cvgcmn.h"
#include "gemprm.h"
#include "vgstruct.h"

void cvg_freeTct ( VG_DBStruct * el )
/************************************************************************
 * cvg_freeTct                                                          *
 *                                                                      *
 * This function frees the allocated break point array of a TCA element.*
 *                                                                      *
 * void cvg_freeTct ( el )   		                                *
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
 * m.gamazaychikov/SAIC          06/07   Created                        *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/

   if( el->hdr.vg_type != TCTRK_ELM ) {
      return;
   }
       
   if ( el->elem.tct.trackPnt ) free ( el->elem.tct.trackPnt );

}

/*=======================================================================*/

void cvg_freeTcb ( VG_DBStruct * el )
/************************************************************************
 * cvg_freeTcb                                                          *
 *                                                                      *
 * This function frees the allocated break point array of a TCA element.*
 *                                                                      *
 * void cvg_freeTcb ( el )                                              *
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
 * m.gamazaychikov/SAIC          06/07   Created                        *
 ***********************************************************************/
{
/*---------------------------------------------------------------------*/
       
   if( el->hdr.vg_type != TCBKL_ELM ) {
      return;
   }
                                                                                    
   if ( el->elem.tcb.bkPntLn ) free ( el->elem.tcb.bkPntLn );

}

