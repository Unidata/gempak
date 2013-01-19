#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

#include "proto_uka.h"
#include "cascmn.h"

void uka_ptstm ( FILE *ifpout, char *fname, FILE *fptr,	long size,
                 char gptyp, char *gtstr, int maxgnm, int *iret )
/************************************************************************
 * uka_ptstm 								*
 *                                                                      *
 * This function scans a VG file for elements of group type "LABEL",    *
 * look for tropical storms and sandstorms, then the program will       *
 * print out all that match to an ASCII file.				*
 *                                                                      *
 * uka_ptstm ( ifpout, fname, fptr, size, gptyp, gtstr, maxgnm, iret )  *
 *                                                                      *
 * Input parameters:                                                    *
 *      *ifpout		FILE		Output file name pointer	*
 *      *fname		char		Input file name			*
 *      *fptr		FILE		Input file pointer		*
 *      size		long		Size of input file in bytes	*
 *      gptyp		char		"LABEL" group type		*
 *      *gtstr		char		"LABEL" group type name		*
 *	maxgnm		int		Maximum number of group numbers *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *					= 0  normal return		*
 *					= 16 no STORM found		*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        11/01   Created					*
 * A. Hardy/SAIC        01/02   Removed unused code & fixed freeing	*
 * A. Hardy/SAIC        03/02   Removed unused variables icnt and level *
 * A. Hardy/NCEP	06/02   Added sig_grpchk			*
 * A. Hardy/NCEP	07/02   Fixed memory leak			*
 * M. Li/SAIC		05/04	Copied from sig_ptstm			*
 * M. Li/SAIC		06/04	Added check for STORM			*
 ***********************************************************************/
{
    int 	istoff, ieloff, gpnum, flag;
    int         members[MAXOFF], pos; 
    int		ii, jj, ij, ingrp, numstm, ier, ier1;
    size_t	im;
    int	 	tmptyp, *tmpgrp, level;
    float       sbtyp[] = { 25.0F, 26.0F, 27.0F, 28.0F };
    float	tmplat, tmplon;
    char	vgclss, grp[4], strm[8], strln[256];
    char        vgnum[] = {SPSYM_ELM, SPSYM_ELM, SPSYM_ELM, SPSYM_ELM };
    char        outstr[40], symtyp[6], sggrp[12];
    Boolean     done, memstm, found, process;

    storm_t	*stm=NULL, *head, *new, *ptr, *ptr2;
    VG_DBStruct     el;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    istoff  = 0;
    numstm  = 0;
    level   = 0;
    done    = False;
    memstm  = False;
    found   = False;
    process = False;
    strcpy(grp, "CVG");
    strcpy(sggrp, "UKA");

   /*
    * Set VG Group Class.
    */

    vgclss = CLASS_SYMBOLS;

    tmpgrp = (int *) malloc(sizeof(int *) * (size_t)maxgnm);
    for ( ij = 0; ij < maxgnm; ij++){
	tmpgrp[ij] = 0;
    }


   /*
    * Read an element header to find the group type number.
    */

    for ( im = 0; im < sizeof(sbtyp)/sizeof(float); im++ ) {
       istoff = 0;
       rewind(fptr);
       done = False;
       while ( !done ) {

            cvg_rdgtn ( fname, fptr, &size, istoff, gptyp, vgclss, 
                        vgnum[im], (int)sbtyp[im], &ieloff, &gpnum, &ier );
 
            istoff = ieloff;
            if ( gpnum > 0 ) {
	        if ( tmpgrp[gpnum-1] == 0 ) {
		    found = False;
		}
	    }
	    else {
		found = False;
	    }

           /* 
            * If the element offset exists, the group number has been 
            * found, the return value is equal to 0, continue.
            */

            if ( ( ieloff != IMISSD ) && ( gpnum > 0 ) && ( ier == 0 ) &&
	         ( !found ) ) {

                cvg_srchgrp ( fname, fptr, &size, gptyp, gpnum, MAXOFF, 
	                      members, &ingrp, &ier);
                uka_grpchk ( fname, fptr, size, gtstr, members, ingrp, 
	                 &process, &ier);

                if ( ier != 0 ) {
		    sprintf (strln,"TROPICAL STORMS, group number %d", gpnum);
                    er_lmsg ( &level, sggrp, &ier, strln, &ier1,
	                      strlen(sggrp), strlen(strln) );
	        }

	       /*
	        * Begin looping through the individual elements with the 
	        * same group type and group number.
	        */

		if ( process ) {
                    for ( ii = 0; ii < ingrp; ii++ ) {

	                cvg_rdhdr ( fname, fptr, members[ii], (int)size, &el, 
		                    &flag, &ier );
                        cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);

	                if ( el.hdr.vg_class == CLASS_SYMBOLS ) {

                            for ( jj = 0; jj < el.elem.sym.info.numsym; jj++) {

		               /*
			        * Check for tropical storm symbols.
			        */ 

                                if ( G_DIFF(el.elem.sym.data.code[jj], sbtyp[im]) ) {
	                            numstm++;
			            tmptyp = 2;
                                    tmplat = el.elem.sym.data.latlon[jj];
                                    tmplon = el.elem.sym.data.latlon[jj + 
			                       el.elem.sym.info.numsym];
		                }
	                    } 

		        }     
	                else if ( el.hdr.vg_class == CLASS_TEXT ) {

		           /* 
		            * Save storm name.
		            */

		            strcpy (strm,  "\""); 
		            cst_rmst ( el.elem.spt.text, strm, &pos, outstr,
			               &ier);
                            if ( ier != -1 ) {
		                cst_rmst ( outstr, strm, &pos, outstr, &ier);
		            }
	                } 

                        if ( !found ) {
	                    found = True;
		            tmpgrp[gpnum-1] = 1;
	                }
                    }  
		}
               /*
	        * Set up linked list.
	        */

		if ( process ) {
	        if ( ingrp > 0 ) {
	            memstm = True;
                    new = (storm_t *) malloc(sizeof(storm_t));
                    new -> next = NULL;
                    if ( numstm > 1 ) { 
	                stm -> next = new;
                        stm = new;
                    }
                    else {
                        stm = new;
                        head = stm;
		    }
                }
	        strcpy ( stm -> name, outstr);
                stm -> lat = tmplat;
                stm -> lon = tmplon;
	        stm -> stmtyp = tmptyp;
		}
            } 
            else {
                done = True; 
	        ier = 0;
            }
        }
    }

   /* 
    * Write out LABEL information in BUFR ASCII format.
    */

    strcpy ( symtyp, "STORM" );
    if ( ( ier == 0 ) && ( numstm > 0 ) ) {
        ptr = head;

	cas_wrstm ( ifpout, ptr, numstm, symtyp, &ier); 

       /*
        * Free linked list memory for tropical storms, if any.
        */

	if ( memstm ) {
            ptr = head;
            ptr2 = ptr -> next;
            while (ptr2 != NULL ) { 
	        free ( ptr );
                ptr = ptr2;
	        ptr2 = ptr2 -> next;
            }
	    free ( ptr );
	}
    }
    else {
   	*iret = 16;
	er_lmsg ( &level, sggrp, iret, symtyp, &ier1,
                              strlen(sggrp), strlen(symtyp) );
    }
    free ( tmpgrp );
}
