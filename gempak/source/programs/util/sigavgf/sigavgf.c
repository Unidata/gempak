#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"

#include "cascmn.h"
#include "proto_sigavgf.h"

int main ( int argc, char *argv[] )
/************************************************************************
 * sigavgf                                                              *
 *                                                                      *
 * This program reads Significant Weather ASCII files and encodes the   *
 * information into a VG file format.   	                        *
 *                                                                      *
 * command line:                                                        *
 *      sigavgf dattim hh                                               *
 *              dattim      GEMPAK date/time string                     *
 *              hh          Valid time (hours) - must be 18 or 24       *
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC     3/02      Created					* 
 * A. Hardy/SAIC     5/02      Added optional input file name; changed  *
 * 			       default input file name			*
 * M. Li/SAIC	     7/04      Removed grpch from sigajet		*
 * M. Li/SAIC	     9/04      Add idcent, chbase and chtop to cas_rdhdr*
 * M. Li/SAIC	     1/05      Process SWM 				*
 * M. Li/SAIC	    10/05      Checked for new format of jet info	*
 ***********************************************************************/
{
    int 	ier, numerr, leverr, pagflg, grpid, iret;
    int         itime[5], jtime[5], idcent;
    float	chbase, chtop;
    int         numtrp, rnum, lnum, hnum, membx, memhi, memlo;
    int         memcld, memmcld, memfrt, memjet, memstm, memtur, memvlr;
    int         numcld, nummcld, numfrt, numjet, numstm, numtur, numvlr;

    char 	fhour [3], fname[256], gtstr[12], grpch, chlvl[10];
    char	errgrp[8], cc[50], casgrp[4];
    char	path[256];
    char	newflvl[10];
    long	size;


    cloud_t     *ptrc, *head,  *ptr2;
    mcloud_t    *ptrm, *headm, *ptr2m;
    jets_t      *ptrj, *headj, *ptr2j;
    front_t     *ptrf, *headf, *ptr2f;
    turb_t      *ptrb, *headb, *ptr2b;
    storm_t     *ptrs, *heads, *ptr2s;
    volrad_t    *ptrv, *headv, *ptr2v;
    trop_t      *ptrr, *headr, *ptr2r;
    trophi_t    *ptrh, *headh, *ptr2h;
    troplo_t    *ptrl, *headl, *ptr2l;

    Boolean     readflg;

    FILE *ifpout;
    /*---------------------------------------------------------------------*/
 
    iret    = 0;
    leverr  = 0;
    readflg = True;
    ifpout  = NULL;

    strcpy ( errgrp, "SIGAVGF" );
    strcpy ( casgrp, "CAS");
    strcpy ( cc, " " );

    rnum   = lnum   = hnum   = 0;
    membx  = memhi  = memlo  = 0;
    memcld = memmcld = memfrt = memjet = memstm = memtur = memvlr = 0;
    numcld = nummcld = numfrt = numjet = numstm = numtur = numvlr = 0;

    ptrc   =  NULL;
    ptrm   =  NULL;
    ptrj   =  NULL;
    ptrf   =  NULL;
    ptrb   =  NULL;
    ptrs   =  NULL;
    ptrv   =  NULL;
    ptrr   =  NULL;
    ptrh   =  NULL;
    ptrl   =  NULL;

    in_bdta ( &ier );

   /*
    * If the forecast hour is not on the command line, print help 
    * and exit.
    */

    if ( argc < 3 ) {
        pagflg = G_FALSE;
        ip_help ( errgrp, &pagflg, &ier, 
                  strlen(errgrp) );
        numerr = -1;
        er_lmsg ( &leverr, errgrp, &numerr, cc, &ier,
                  strlen(errgrp), strlen(cc) );
        exit (1);
    }

    strcpy ( fhour, argv[2] );
    if  ( strcmp ( fhour, "24") != 0 ) { 
        if ( strcmp ( fhour, "18") != 0 ) {
            numerr = -2;
            er_lmsg ( &leverr, errgrp, &numerr, fhour, &ier,
                      strlen(errgrp), strlen(fhour) );
            exit (1);
	}
    }

   /*
    *  Check for input file.
    */

    if ( argv[3] != NULL ) {
        strcpy ( fname, argv[3] );
    }
    else {
       
       /*
	* If an input file is not specified, check for the existence of 
	* either SIGWXHI.txt or SIGWXMID.txt.
	*/

        sprintf ( fname, "SIGWXHI.txt" );
	cfl_inqr ( fname, NULL, &size, path, &ier );
	if ( ier != 0 ) {
	    sprintf ( fname, "SIGWXMID.txt" );
            cfl_inqr ( fname, NULL, &size, path, &ier );
	    if ( ier != 0 ) {
		numerr = -3;
                er_lmsg ( &leverr, errgrp, &numerr, fname, &ier,
                          strlen(errgrp), strlen(fname) );
                exit (1);
            }
	}

    }
    ifpout = cas_open ( fname, readflg, &ier );

   /*
    * If input ASCII file failed to open, exit program.
    */

    if ( ier != 0 ) {
         numerr = -3;
         er_lmsg ( &leverr, errgrp, &numerr, fname, &ier,
                      strlen(errgrp), strlen(fname) );
         exit (1);
    }

   /*
    * Read in the input ASCII file into the CAS structures.
    */

    cas_rdhdr ( ifpout, itime, jtime, &idcent, &chbase, &chtop, &ier );

    rewind (ifpout);
    if ( G_ABS ( HI_BASE - chbase ) < 0.5F &&
         G_ABS ( HI_TOP  - chtop  ) < 0.5F ) {
	strcpy ( chlvl, "SWH" );
        cas_rdcld ( ifpout, &numcld, &ptrc, &memcld, &ier );
    } else if ( G_ABS ( MID_BASE - chbase ) < 0.5F &&
                G_ABS ( MID_TOP  - chtop  ) < 0.5F ) {
        strcpy ( chlvl, "SWM" );
   	cas_rdmcld ( ifpout, &nummcld, &ptrm, &memmcld, &ier );
    }

    rewind (ifpout);
    cas_rdjets ( ifpout, &numjet, &ptrj, &memjet, &ier );

    rewind (ifpout);
    cas_rdturb ( ifpout, &numtur, &ptrb, &memtur, &ier );

    rewind (ifpout);
    cas_rdfrt ( ifpout, &numfrt, &ptrf, &memfrt, &ier);

    rewind (ifpout);
    cas_rdtrop ( ifpout, &numtrp, &ptrr, &membx, &rnum,
                 &ptrl, &memlo, &lnum, &ptrh, &memhi,
		 &hnum, &ier );

    rewind (ifpout);
    cas_rdstm ( ifpout, &numstm, &ptrs, &memstm, &ier );

    rewind (ifpout);
    cas_rdvlrd ( ifpout, &numvlr, &ptrv, &memvlr, &ier );

   /*
    * Close input file.
    */

    cas_clos ( ifpout, &ier );
    
   /*
    * If ASCII file failed to close, write error message.
    */

    if ( ier != 0 ) {
         numerr = -4;
         er_lmsg ( &leverr, casgrp, &numerr, fname, &ier,
                      strlen(casgrp), strlen(fname) );
    }

   /*
    * Initialize the group type table.
    */

    ces_gtrtbl ( &iret );

   /*
    * Call appropriate VG file encoding subroutines.
    *
    * Create cloud VG file.
    */

    strcpy ( gtstr, "CLOUD");
    ces_gtgid (gtstr, &grpid, &ier);
    grpch = (char) grpid;
    if ( strcmp ( chlvl, "SWH" ) == 0 ) {
        sigacld ( fhour, numcld, ptrc, itime, grpch, &ier );
    }
    else {
	sigamcld ( fhour, nummcld, ptrm, itime, grpch, &ier );
    }

   /*
    * Create jets VG file.
    */

    ctb_rdprf ( "prefs.tbl", "config", "SIGWX_FLIGHT_LEVELS", newflvl, &ier );
    sigajet ( fhour, numjet, ptrj, itime, chlvl, newflvl, &ier );

   /*
    * Create turbulence VG file.
    */

    strcpy ( gtstr, "TURB");
    ces_gtgid (gtstr, &grpid, &ier);
    grpch = (char) grpid;
    sigatur ( fhour, numtur, ptrb, itime, grpch, chlvl, &ier );

   /*
    * Create front VG file.
    */

    strcpy ( gtstr, "FRONT");
    ces_gtgid (gtstr, &grpid, &ier);
    grpch = (char) grpid;
    sigafrt ( fhour, numfrt, ptrf, itime, grpch, chlvl, &ier );

   /*
    * Create tropopause VG file.
    */

    sigatrp ( fhour, rnum, ptrr, hnum, ptrh, lnum, ptrl, 
	      itime, chlvl, &ier );

   /*
    * Create symbol VG file.
    */

    strcpy ( gtstr, "LABEL");
    ces_gtgid (gtstr, &grpid, &ier);
    grpch = (char) grpid;
    sigavts ( fhour, numstm, ptrs, numvlr, ptrv, 
                       itime, grpch, chlvl, &ier );

   /*
    *  Free all created linked lists.
    */

    /* Free cloud */
    if ( memcld ) {
        head = ptrc;
        ptr2 = head -> next;
        while ( ptr2 != NULL ) {
            free (head);
            head = ptr2;
            ptr2 = ptr2 -> next;
        }
        free ( head );
    }

    /* Free mcloud */
    if ( memmcld ) {
        headm = ptrm;
        ptr2m = headm -> next;
        while ( ptr2m != NULL ) {
            free (headm);
            headm = ptr2m;
            ptr2m = ptr2m -> next;
        }
        free ( headm );
    }


    /* Free jet */
    if ( memjet ) {
        headj = ptrj;
        ptr2j = headj -> next;
        while ( ptr2j != NULL ) {
            free (headj);
            headj = ptr2j;
            ptr2j = ptr2j -> next;
        }
        free ( headj );
    }

    /* Free turb */
    if ( memtur ) {
        headb = ptrb;
        ptr2b = headb -> next;
        while ( ptr2b != NULL ) {
            free (headb);
            headb = ptr2b;
            ptr2b = ptr2b -> next;
        }
        free ( headb );
    }

    /* Free front*/
    if ( memfrt ) {
        headf = ptrf;
        ptr2f = headf -> next;
        while ( ptr2f != NULL ) {
            free (headf);
            headf = ptr2f;
            ptr2f = ptr2f -> next;
        }
        free ( headf );
    }
    /* Free trop*/
    if ( membx ) {
        headr = ptrr;
        ptr2r = headr -> next;
        while ( ptr2r != NULL ) {
            free ( headr );
            headr = ptr2r;
            ptr2r = ptr2r -> next;
        }
        free ( headr );
    }
    if ( memhi ) {
        headh = ptrh;
        ptr2h = headh -> next;
        while ( ptr2h != NULL ) {
            free ( headh );
            headh = ptr2h;
            ptr2h = ptr2h -> next;
        }
        free ( headh );
    }
    if ( memlo ) {
        headl = ptrl;
        ptr2l = headl -> next;
        while ( ptr2l != NULL ) {
            free ( headl );
            headl = ptr2l;
            ptr2l = ptr2l -> next;
        }
        free ( headl );
    }

    if ( memstm ) {
        heads = ptrs;
        ptr2s = heads -> next;
        while ( ptr2s != NULL ) {
            free ( heads );
            heads = ptr2s;
            ptr2s = ptr2s -> next;
        }
        free ( heads );
    }
    if ( memvlr ) {
        headv = ptrv;
        ptr2v = headv -> next;
        while ( ptr2v != NULL ) {
            free ( headv );
            headv = ptr2v;
            ptr2v = ptr2v -> next;
        }
        free ( headv );
    }

    return 0;
}
