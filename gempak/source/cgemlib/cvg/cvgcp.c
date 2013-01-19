#include "cvgcmn.h"
#include "pgprm.h"
#include "drwids.h"

#define CP_APP 0
#define CP_NEW 1


int cvg_cp ( char *ifname, char *ofname, int initflag, int *iret )
/************************************************************************
 * cvg_cp								*
 *									*
 * This function performs a copy of data from a VGF file into a passed	*
 * in VGF file.  							*
 *									*
 * If initflag is 1 then the ofname file is created or reinitialized.   *
 * if initflag is 0 then the ifname contents are appended to the ofname *
 * file, and the contents of ofname, if any, are retained.		*
 *									*
 * Deleted elements in ifname are not copied.  Also extra space in the  *
 * vg elements in ifname is stripped before being written to ofname.    * 
 *									* 
 *									*
 * int cvg_cp ( ifname, ofname,  initflag,  iret )			*
 *									*
 * Input parameters:							*
 *	*ifname		char		Name of file to import		*
 *	*ofname		char		Name of file to store as	*
 * 	initflag	int		Flag to create file for output	*
 *					  0  = Use existing file	*
 *					  1  = Create new file		*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal			*
 *					 -1 = error opening VG file	*
 *					 -2 = error closing VG file	*
 *					 -5 = VG ifname file is empty	*
 *					-13 = error reading ifname file *
 *					-15 = error creating ofname     *
 *					-17 = error writing to ofname   *
 *					-47 = no ifname specified       *
 *					-48 = no ofname specified       *
 *	cvg_cp		int		Number of copied vg elems	*
 *									*
 **									*
 * Log:									*
 * E. Wehner/EAi	 6/97	Created					*
 * E. Wehner/EAi	 6/97	Add check to not copy header		*
 * E. Wehner/EAi	 8/97	Remove unused includes			*
 * D.W.Plummer/NCEP	 9/97	Bug fix when copying wnd or sym		*
 * E. Wehner/Eai	 9/97	Set error and return on null fname	*
 * G. Krueger/EAI	 4/98	Truncate too long elements to MAXPTS-1.	*
 * S. Jacobs/NCEP	 4/98	Compute record size before delete check	*
 * I. Durham/GSC	 5/98	Changed underscore decl. to an include	*
 * C. Lin/EAI	         6/98	open input file with G_FALSE flag	*
 * E. Safford/GSC	10/98	clean up, redesign errors, add return   *
 * S. Law/GSC		02/99	added group renumbering			*
 * E. Safford/GSC	02/99	added check for deleted el on cvg_set   *
 * E. Safford/GSC	02/99	fixed group incr bug on CP_NEW ops      *
 * E. Safford/GSC	02/99	change check on maxbytes to <= 1 for AIX*
 * E. Safford/GSC	03/99	remove misplaced cast on grpnum         *
 * E. Safford/GSC	04/99	no return value compiler warnings	*
 * D.W.Plummer/NCEP     12/99   call cfl_writ for WBOX_ELM processing   *
 * M. Li/GSC		12/99	Added cvg_swap to swap byte order(LINUX)*
 * E. Safford/GSC	04/00	add swap on watches			*
 * E. Safford/GSC	04/00	rmv special watch handling		*
 * F. J. Yen/NCEP	08/00	Added special watch and sigmet handling.*
 *				Corrected record size.			*
 * R. Curtis/EAI	10/00	Added CCF check in if statement for     *
 *                              Sigmet type				*
 * E. Safford/GSC	11/00	added cpd_elms incr to watch handling	*
 * J. Wu/GSC		11/00	Uncoupled the function from crg library	*
 *                              and removed calls to er_lmsg            *
 * E. Safford/GSC	12/00	remove duplicate reads, simplify write 	*
 * A. Hardy/GSC          1/01   changed ifptr and ofptr from int to FILE*
 * J. Wu/GSC		02/01	Cleanup & used cvg_write() to avoid     *
 * 		                overhead file accessing for writing   	*
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * B. Yin/SAIC          08/04   Added code to free TCA memory           *
 * B. Yin/SAIC          08/04   changed pgtca_freeBkpts to cvg_freeBkpts*
 * J. Wu/SAIC		10/04	free GFA block pointers			*
 * S. Danz/AWC		07/06	Switch to new cvg_writeD() function     *
 ***********************************************************************/
{
    int 	curpos, more, ier, flag, orecsz, nrecsz, cpd_elms;
    int		ii, curr_grp, grpnums[MAX_GROUP_TYPE];
    char 	newfil[256], onfname[256], append;
    long	maxbytes, cursiz;
    VG_DBStruct	el;
    FILE	*ifptr, *ofptr;
/*---------------------------------------------------------------------*/

    *iret    = G_NORMAL;
    cpd_elms = 0;
    curpos   = 0;
    maxbytes = 0;

    if ( ifname == NULL ) {
        *iret = -47;
	return (0);
    }

    if ( ofname == NULL ) {
        *iret = -48;
	return (0);
    }

    /*  
     *  If the specified input file doesn't exist or it is empty,
     *  return & leave the output file untouched. Otherwise, open it.
     */    

    cfl_inqr(ifname, NULL, &maxbytes, newfil, &ier);
    if ( ier < 0 ) {
        *iret = -1;
        return (0);
    }

    if ( maxbytes <= 1 ) {	/* AIX doesn't have 0 byte files */
	*iret = -5;
	return (0);
    }
    else {
        cvg_open(newfil, G_FALSE, &ifptr, &ier);

	if ( ( ier < 0 ) || ( ifptr == NULL ) ) {
	    *iret = -1;
	    return (0);
	}
    }


   /*
    *  Check the integrity of the specified output file.  If it doesn't
    *  exist, create it if authorized. Otherwise, open it for overwriting 
    *  (initflag == CP_NEW) or appending to (initflag == CP_APP). 
    */

    cfl_inqr(ofname, NULL, &cursiz, onfname, &ier);
    if ( initflag == CP_APP && ( ier != 0 ) ) {
	*iret = -1;
        return (0);
    }

    if ( initflag == CP_NEW ) {
	cvg_crvgf(onfname, &ier);
	if ( ier < 0 ) {
	    *iret = -15;
	    return (0);
	}
	append = G_FALSE;
    }
    else {
	append = G_TRUE;
    }

    cvg_open(onfname, G_TRUE, &ofptr, &ier);

    if ( ( ier < 0 ) || ( ofptr == NULL ) ) {
        *iret = -1;
         return (0);
    }
    
    /*
     *  If appending to an existing file, initialize the grpnums array &
     *  store the largest group number in the existing file for
     *  each group type.
     */
    if ( append ) {

        for (ii = 0; ii < MAX_GROUP_TYPE; ii++) {
	    grpnums[ii] = 0;
        }

        curpos = sizeof(el.hdr) + sizeof(el.elem.fhed);
        more = G_TRUE;

        while ( ( more ) && ( (long)curpos < cursiz ) ) {

 	    cvg_rdhdr(onfname, ofptr, curpos, (int)cursiz, &el, &flag, &ier);

	    if (ier < 0) { 
	        *iret = -13;
	        more = G_FALSE;
	    }		
	    else if ( ( el.hdr.recsz > 0 ) ) {
	        orecsz = el.hdr.recsz;
                
	        if ( (int) el.hdr.grptyp != 0 )	{	     
		    curr_grp = el.hdr.grptyp;
		    
		    if ( grpnums[curr_grp] < el.hdr.grpnum ) {
                         grpnums[curr_grp] = el.hdr.grpnum;
	            }
		}
	        
		curpos += orecsz;
            }
	    
	    else {
	        more = G_FALSE;
            }
	}
    }


    /* 
     *  Read all elements in input file and write/append to outfile. 
     *  Reassign group numbers if necessary.
     */
    curpos = sizeof(el.hdr) + sizeof(el.elem.fhed);
    more = G_TRUE;

    cfl_inqr(onfname, NULL, &cursiz, onfname, &ier);

    while ( ( more ) && ( (long)curpos < maxbytes ) ) {

	cvg_rdhdr(newfil, ifptr, curpos, (int)maxbytes, &el, &flag, &ier);

	if (ier < 0) { 
	    *iret = -13;
	    more = G_FALSE;
	}		
        else if ( ( el.hdr.recsz > 0 ) ) {

	    /*
	     *  orecsz is the size of the element as recorded in the
	     *  vg file.  This _could_ be different than the size returned
	     *  from the subsequent call to cvg_rdele, if the element contains
	     *  more lat/lon points than MAXPTS.  In that case cvg_rdele
	     *  will adjust the el.hdr.recsz to reflect the truncation.
	     */ 
	    orecsz = el.hdr.recsz;

	    cvg_rdele(&el, curpos, el.hdr.recsz, ifptr, &ier);

	    if (ier != 0) {
	        *iret = -13;
	        more = G_FALSE;
	    }

	    /*
	     * Continue if the element is valid
 	     */
	    if ( ( more ) && (el.hdr.delete == 0) && 
	    	 (el.hdr.vg_type != FILEHEAD_ELM) )  {

	        nrecsz = el.hdr.recsz;

	        /*
	         * Renumber the groups as needed
	         */
	        if ( append && (int)el.hdr.grptyp != 0 ) {
		    curr_grp = el.hdr.grptyp;   
                    el.hdr.grpnum = grpnums[curr_grp] + el.hdr.grpnum;
                }

		cvg_writeD(&el, (int)cursiz, nrecsz, ofptr, &ier);		
		cursiz = cursiz + (long)nrecsz;

		if ( ier == 0 ) {
		    cpd_elms++;
		}
		else {
	            *iret = -17;
		    more = G_FALSE;
		}

	    }
	    curpos += orecsz;

	    /*
              * Free TCA/GFA memory
              */
            if ( el.hdr.vg_type == TCA_ELM ) {
                cvg_freeBkpts ( &el );
            }
            else if ( el.hdr.vg_type == GFA_ELM ) {
                cvg_freeElPtr ( &el );
	    }

	}
	else {
	    more = G_FALSE;
	}
    }

    cfl_clos(ifptr, &ier);
    if ( ier != 0 )
	*iret = -2;

    cfl_clos( ofptr, &ier);
    if ( ier != 0 )
	*iret = -2;
  

    return (cpd_elms);
}
