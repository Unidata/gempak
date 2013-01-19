#include <mel_bufr.h>
#include "geminc.h"
#include "gemprm.h"
#include "cascmn.h"
#include "sigbcmn.h"

void sigbmcld ( FILE *ifpoutm, int *iret )
/************************************************************************
 * sigbmcld                                                             *
 *                                                                      *
 * This function loops through the mid level cloud areas in a sigwx 	*
 * bufr file and writes them to an ASCII file.                         	*
 *                                                                      *
 * sigbmcld ( ifpoutm, iret )                                           *
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpoutm         FILE           Output file name pointer       	*
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 **                                                                     *
 * Log:                                                                 *
 * M. Li/SAIC		12/04						*
 * K. Tyle/UAlbany      11/10   Increased dimeension of errgrp		*
 ***********************************************************************/
{
    int         n, descrip;
    int         numclds, mode, ier;
    int		leverr, numerr;
    float	tmplvl1, tmplvl2;
    char	errgrp[8], cc[2];
    Boolean     memalc;

    BUFR_Val_t  bv;
    mcloud_t    *cld, *head, *new, *ptr, *ptr2;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    numclds = 0;
    mode    = 1;
    memalc  = FALSE;
    cld     = NULL;
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
            sigberror ( "sigmbcld", &ier );
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
            if ( descrip == D_DIMEN_ATTRIB ) {
		mode = 0;
            	new    = ( mcloud_t *) malloc (sizeof ( mcloud_t ) );
                memalc = TRUE;
                new -> next = NULL;
                if ( numclds != 0 ) {
	            cld -> next = new;
                    cld         = new;
                }
                else {
                    cld  = new;
                    head = cld;
                }
                numclds++;
                cld -> npt   = 0;
		cld -> ncld  = 0;
		cld -> ntyp  = 0;
		cld -> turb  = 0;
		cld -> icing = 0;
		cld -> fcb   = 0;
            }

    	   /*
            * Store mid level cloud information into current linked list.
            */

            else if ( mode == 0 ) {
		if ( descrip == D_LAT ) {
                     cld -> lat [cld -> npt] = (float)bv.Val.number;
                }
                else if ( descrip == D_LON ) {
                    cld -> lon [cld -> npt] = (float)bv.Val.number;
                    cld -> npt++;
                }
		else if ( descrip == D_CLOUD_AMT ) {
		    if ( bv.missing_flag != 0 ) {
		    	cld -> ncdis [cld -> ncld] = bv.Val.int_number;
		    }
		    else {
			cld -> ncdis [cld -> ncld] = SIGIMSS;
		    } 
		    cld -> ncld++;
		}
		else if ( descrip == D_CLOUD_TYPE ) {
		    if ( bv.missing_flag != 0 ) {
                        cld -> nctyp [cld -> ntyp] = bv.Val.int_number;
                    }
                    else {
                        cld -> nctyp [cld -> ntyp] = SIGIMSS;
                    }
		    cld -> ntyp++;
		}
		else if ( descrip == D_FLIGHT_LVL ) {
		    if ( bv.missing_flag != 0 ) {
			tmplvl1 = (float)bv.Val.int_number;
		    }
		    else {
			tmplvl1 = SIGRLMS;
                    }
		    mode = 1;
		}
	    }
	    else if ( mode == 1 ) {	
		if ( descrip == D_FLIGHT_LVL ) {
		    if ( bv.missing_flag != 0 ) {
                        tmplvl2 = (float)bv.Val.int_number;
                    }
                    else {
                        tmplvl2 = SIGRLMS;
                    }
                    mode = 2;
                }
	    }
	    else if ( mode == 2 ) {
		if ( descrip == D_EXT_DEG_TURB || descrip == D_DEG_TURB ) {
		    if ( bv.missing_flag != 0 ) {
		    	cld -> tdeg = bv.Val.int_number;		
		    }
		    else {
			cld -> tdeg = SIGIMSS;
		    }
		    cld -> ttop  = tmplvl2;
		    cld -> tbase = tmplvl1;
		    cld -> turb  = 1;
		    mode = 0;
		}
		else if ( descrip == D_AF_ICING ) {
		    if ( bv.missing_flag != 0 ) {
                        cld -> dic = bv.Val.int_number;
                    }
                    else {
                        cld -> dic = SIGIMSS;
                    }
                    cld -> ictop  = tmplvl2;
                    cld -> icbase = tmplvl1;
		    cld -> icing  = 1;
                    mode = 0;
                }
                else if ( descrip == D_CLOUD_AMT ) { 
                    if ( bv.missing_flag != 0 ) {
                        cld -> cbdis = bv.Val.int_number;
                    }
                    else {
                        cld -> cbdis = SIGIMSS;
                    }
                    cld -> cbtop  = tmplvl2;
                    cld -> cbbase = tmplvl1;
		    cld -> fcb    = 1;
                }
		else if ( descrip == D_CLOUD_TYPE ) {
		    if ( bv.missing_flag != 0 ) {
                        cld -> cbtyp = bv.Val.int_number;
                    }
                    else {
                        cld -> cbtyp = SIGIMSS;
                    }
		    mode = 0;
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
    * Write out Mid level CLOUD information in BUFR ASCII format.
    */

    if ( numclds > 0 ) { 
        ptr = head;
        cas_wrmcld ( ifpoutm, ptr, numclds, &ier );
    }

    sigbeof ( n, &ier );
    *iret = BUFR_EOF;

   /*
    * Free memory from the linked list.
    */

    if ( memalc ) {
        ptr  = head;
        ptr2 = ptr -> next;
        if ( numclds > 1 ) {
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
