#include <mel_bufr.h>
#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"
#include "sigbcmn.h"

void sigbjets ( FILE *ifpout, char *chlvl, int *iret )
/************************************************************************
 * sigbjets                                                             *
 *                                                                      *
 * This function loops through the jets in a sigwx bufr file and writes *
 * them to an ASCII file.                                               *
 *                                                                      *
 * sigbjets ( ifpout, chlvl, iret )                                     *
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout         FILE            Output file name pointer        *
 *      *chlvl          char            Chart level                     *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * D. Kidwell/NCEP       1/02                                           *
 * M. Li/SAIC		04/04	Set flight level & deltas values	*
 * M. Li/SAIC		09/04	Added chlvl				*
 * S. Jacobs/NCEP	10/04	Updated for new version of MELBUFR	*
 * M. Li/SAIC           12/04   Added input parameter chlvl     	*
 * K. Tyle/UAlbany      11/10   Increased dimeension of errgrp		*
 ***********************************************************************/
{
    int    	n, descrip;
    int  	numjets, mode, lvlflg, ier;
    int		leverr, numerr;
    char	errgrp[8], cc[2];
    Boolean     memalc;

    BUFR_Val_t  bv;
    jets_t      *jet, *head, *new, *ptr, *ptr2;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    numjets = 0;
    mode    = 1;
    memalc  = FALSE;
    jet     = NULL;
    head    = NULL;
    leverr  = 0;
    strcpy ( errgrp, "SIGBUFR" );
    strcpy ( cc, " " );

   /* 
    * Get each value (and any associated fields) from the BUFR message.
    */ 

    while ( (n=(int)BUFR_Get_Value( &bv, 1 )) != BUFR_EOM
             && n != BUFR_EOF ) {
        if ( n == BUFR_ERROR ) {
            sigberror ( "sigbjets", &ier );
            *iret = BUFR_EOF;
            return;
        }

       /*
        * Test for IGNORE_FXY, used to indicate that information that
        * was returned should be ignored.  This happens when a local
        * descriptor that is not defined in Table B is the last
        * descriptor in the data set.
        */

        if ( bv.FXY_Val != (FXY_t)IGNORE_FXY ) {
            descrip = FXY_Unpack_Dec (bv.FXY_Val);
           /*
            * This is a JETS message.
            */             
            if ( descrip == D_MET_FEATURE ) {
                if ( mode == 0 ) {
                    mode = 1;
                }
                else if ( mode == 1 ) {
                    mode = 0;
                }
            }
            else if ( descrip == D_DIMEN_ATTRIB ) {
		lvlflg = 0;
                if ( mode == 1 ) {
                    mode   = 2;
                    new    = ( jets_t *) malloc (sizeof ( jets_t ) );
                    memalc = TRUE;
                    new -> next = NULL;
                    if ( numjets != 0 ) {
                        jet -> next = new;
                        jet         = new;
                    }
                    else {
                        jet  = new;
                        head = jet;
                    }
                    numjets++;
                    jet -> npt  = 0;
                }
                else if ( mode == 2 ) {
                    mode = 1;
                }
            }
            else if ( mode == 2 ) {

               /*
                * Store jet information into current linked list.
                */

                if ( descrip == D_LAT ) {
                    jet -> lat [jet -> npt] = (float)bv.Val.number;
                }
                else if ( descrip == D_LON ) {
                    jet -> lon [jet -> npt] = (float)bv.Val.number;
                }
                else if ( descrip == D_HEIGHT ) {
                    if ( bv.missing_flag != 0 ) {
                        jet -> level [jet -> npt] = (float)bv.Val.int_number;
                    }
                    else {
                        jet -> level [jet -> npt] = SIGRLMS;
                    }
		    jet -> levabv [jet->npt] = SIGRLMS;
		    jet -> levblw [jet->npt] = SIGRLMS;
                }
		else if ( descrip == D_FLVL_SIGNIF ) {
		    if ( bv.missing_flag != 0 ) {
			if ( bv.Val.int_number == 60 ) {
			    lvlflg = 1;
			}
			else if ( bv.Val.int_number == 61 ) {
                            lvlflg = -1;
                        }
		    }
		    else {
			lvlflg = 0;
		    }
		}
		else if ( descrip == D_FLIGHT_LVL ) {
		    if ( bv.missing_flag != 0 ) {
			if ( lvlflg == 0 ) {
			    jet -> level [jet -> npt] = (float)bv.Val.int_number;
			}
			else if ( lvlflg == 1 ) {
			    jet -> levabv [(jet->npt) - 1 ] = (float)bv.Val.int_number;
			}
			else if ( lvlflg == -1 ) {
                            jet -> levblw [(jet->npt) - 1] = (float)bv.Val.int_number;
                        }
		    }
		    else {
			if ( lvlflg == 0 ) {
                            jet -> level [jet -> npt] = SIGRLMS; 
                        }
                        else if ( lvlflg == 1 ) {
                            jet -> levabv [(jet->npt) - 1] = SIGRLMS; 
                        }
                        else if ( lvlflg == -1 ) {
                            jet -> levblw [(jet->npt) - 1] = SIGRLMS; 
                        }
                    }
                }
                else if ( descrip == D_WSPD ) {
                    if ( bv.missing_flag != 0 ) {
                        jet -> speed [jet -> npt] = (float)bv.Val.number;
                    }
                    else {
                        jet -> speed [jet -> npt] = SIGRLMS;
                    }
                    jet -> npt++;
                }
            }
         /* BUFR_Val_Print ( bv, 1, 0, stdout ); */
        }
        if ( n == BUFR_EOD ) {
           /*
            * This decoded value is the last one in the dataset.
            */
            numerr = -10;
            er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                      strlen(errgrp), strlen(cc) );
        }
    }

    if ( n == BUFR_EOM ) {
        sigbeom ( &n, &ier );
    }

   /*
    * Write out JETS information in BUFR ASCII format.
    */

    if ( numjets > 0 ) {
        ptr = head;
        cas_wrjets ( ifpout, ptr, numjets, chlvl, &ier );
    }

    sigbeof ( n, &ier );
    *iret = BUFR_EOF;

   /*
    * Free memory from the linked list.
    */

    if ( memalc ) {
        ptr  = head;
        ptr2 = ptr -> next;
        if ( numjets > 1 ) {
            while ( ptr2 -> next != NULL ) {
                ptr  = ptr2;
                ptr2 = ptr2 -> next;
                free (ptr);
            }
        }
        free ( ptr2 );
    }

    BUFR_Destroy(1);

}
