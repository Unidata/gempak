#include "geminc.h"
#include "gemprm.h"
#include "drwids.h"
#include "vgstruct.h"

#include "proto_uka.h"
#include "cascmn.h"

#define  LABEL_VOLC 	201.0F

void uka_ptvol ( FILE *ifpout, char *fname, FILE *fptr, long size,
                   char gptyp, char *gtstr, int	maxgnm, int *iret )
/************************************************************************
 * uka_ptvol 								*
 *                                                                      *
 * This function scans a VG file for elements of group type "LABEL",    *
 * look for volcanos and print out all that match to an ASCII file.	*
 *                                                                      *
 * uka_ptvol( ifpout, fname, fptr, size, gptyp, gtstr, maxgnm, iret )	*
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
 *					=  0 normal return		*
 *					= 16 no VOLCANO found		*
 **                                                                     *
 * Log:                                                                 *
 * A. Hardy/SAIC        11/01   Created					*
 * A. Hardy/SAIC        01/02   Added fixed error checking & freeing    *
 * A. Hardy/SAIC        03/02   Removed unused variable level		*
 * A. Hardy/NCEP         6/02   Added sig_grpchk			*
 * A. Hardy/NCEP         7/02   Fixed memory leak			*
 * M. Li/SAIC		 5/04	Copied from sig_volcano			*
 * M. Li/SAIC		 6/04	Added check for VOLCANO			*
 ***********************************************************************/
{
    int 	istoff, sbtyp, ieloff, gpnum, flag;
    int         members[MAXOFF], *tmpgrp, level; 
    int		ii, ingrp, numvol, numstr, ier, ier1;
    float       tmplat, tmplon;
    char	vgclss, vgnum, grp[4], tmpnam[40]; 
    char        **aryptr, cget, symtyp[8], strln[256], sggrp[12];
    Boolean     done, memvol, found, process;

    volrad_t	*vol=NULL, *head, *new, *ptr, *ptr2;
    VG_DBStruct     el;
/*---------------------------------------------------------------------*/
    *iret   = 0;
    ier     = 0;
    istoff  = 0;
    numvol  = 0;
    level   = 0;
    done    = False;
    memvol  = False;
    process = False;
    strcpy(grp, "CVG");
    strcpy(sggrp, "UKA");

   /*
    * Set VG Group Class, VG Group Type number and VG Subtype number.
    */

    vgclss = CLASS_SYMBOLS;
    vgnum  = WXSYM_ELM;
    sbtyp  = (int)LABEL_VOLC;

   /*
    * Declaring the memory size for parsing the text string.
    */

    aryptr = (char **) malloc(sizeof(char *) * 3);
    for ( ii = 0; ii < 3; ii++ ) {
        aryptr[ii] = (char *) malloc(40);
    }
    tmpgrp = (int *) malloc(sizeof(int*) * (size_t)maxgnm);
    for ( ii = 0; ii < maxgnm; ii++ ) {
        tmpgrp[ii] = 0;
    }

   /*
    * Read an element header to find the group type number.
    */

    while ( !done ) {

        cvg_rdgtn ( fname, fptr, &size, istoff, gptyp, vgclss, vgnum, 
                    sbtyp, &ieloff, &gpnum, &ier );

        istoff = ieloff;
	if ( ( gpnum > 0 ) && ( ier == 0 ) && (ieloff != IMISSD ) ) {
	    if ( tmpgrp[gpnum-1] == 0 ) {
		found = False;
	    }
	}

       /* 
        * If the element offset exists, the group number has been found, 
        * the return value is equal to 0, continue.
        */

        if ( ( ieloff != IMISSD ) && ( gpnum > 0 ) && ( ier == 0 ) &&
	     ( !found ) ) {

            cvg_srchgrp ( fname, fptr, &size, gptyp, gpnum, MAXOFF, 
	                  members, &ingrp, &ier);
            uka_grpchk ( fname, fptr, size, gtstr, members, ingrp, 
	                 &process, &ier);
            if ( ier != 0 ) {
		sprintf (strln,"VOLCANOS, group number %d", gpnum);
                er_lmsg ( &level, sggrp, &ier, strln, &ier1,
	                  strlen(sggrp), strlen(strln) );
	    }

           if ( process ) {
	       /*
	        * Begin looping through the individual elements with the 
	        * same group type and group number.
	        */

                for ( ii = 0; ii < ingrp; ii++ ) {

	            cvg_rdhdr ( fname, fptr, members[ii], (int)size, &el, 
		                &flag, &ier );
                    cvg_rdele(&el, members[ii], el.hdr.recsz, fptr, &ier);

                   /*
		    * Check for volcano symbol.
		    */ 

	            if ( el.hdr.vg_class == CLASS_SYMBOLS &&
	                 el.hdr.vg_type == WXSYM_ELM      &&
                         G_DIFF(el.elem.sym.data.code[0], LABEL_VOLC) ) {

		        numvol++;
                        tmplat = el.elem.sym.data.latlon[0];
                        tmplon = el.elem.sym.data.latlon[0 + 
			                       el.elem.sym.info.numsym];
		    } 
	            else if ( el.hdr.vg_class == CLASS_TEXT ) {

		       /* 
		        * Save volcano name.
		        */

		        cget = CHLF;
		        cst_clst ( el.elem.spt.text, cget, " ", 3, 40,
			            aryptr, &numstr, &ier );
		        strcpy (tmpnam,  aryptr[0]);
	            }  
                } 
	    } 

           /*
            * Fill out the volcano structure, setting date/time
	    * information to missing.
            */

            if ( ( ingrp > 0 ) && ( process ) ) {
		memvol = True;
                new = (volrad_t *) malloc(sizeof(volrad_t));
                new -> next = NULL;
                if ( numvol > 1 ) { 
	            vol -> next = new;
                    vol = new;
                }
                else {
                    vol = new;
                    head = vol;
	        }

                vol -> lat = tmplat;
                vol -> lon = tmplon;
		strcpy (vol -> name, tmpnam); 
                vol -> year   = SIGIMSS;
                vol -> month  = SIGIMSS;
                vol -> day    = SIGIMSS;
                vol -> hour   = SIGIMSS;
                vol -> minute = SIGIMSS;
		if ( !found ) {
		    found = True;
		    tmpgrp[gpnum-1] = 1;
		}
            }
        }  
        else {
            ier  = 0;
            done = True; 
	    if ( numvol == 0 ) {
		ier = -1;
	    }
        }
    } 

   /* 
    * Write out LABEL information in BUFR ASCII format.
    */

    strcpy ( symtyp, "VOLCANO" );
    if ( ier == 0 ) {
        ptr = head;
	cas_wrvlrd ( ifpout, ptr, numvol, symtyp, &ier); 
    }
    else {
	*iret = 16;
        er_lmsg ( &level, sggrp, iret, symtyp, &ier1,
                              strlen(sggrp), strlen(symtyp) );
    }

   /*
    * Free linked list memory for volcanos.
    */

    if ( memvol ) {
        ptr = head;
        ptr2 = ptr -> next;
        while ( ptr2 != NULL ) {
	    free ( ptr );
            ptr = ptr2;
	    ptr2 = ptr2 -> next;
	}
	free ( ptr );
    }

   /*
    * Free memory from parsing text string and tmp. group number.
    */

    for ( ii = 0; ii < 3; ii++ ) {
        free ( aryptr[ii] );
    }

    free ( aryptr );
    free ( tmpgrp );
}
