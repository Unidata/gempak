#include "cvgcmn.h"
#include "pgprm.h"


void cvg_rndef ( void )
/************************************************************************
 * cvg_rndef								*
 *									*
 * This function renames default WORK_FILE to WORK_FILE.save when it is *
 * not empty.  Otherwise it just removes the WORK_FILE.			*
 *									*
 * cvg_rndef ( )							*
 *									*
 * Input parameters:							*
 * Output parameters:							*
 *					 				*
 **									*
 * Log:									*
 * C. Lin/EAI	 	12/97	Created					*
 * T. Piper/GSC		10/98	Prolog update				*
 * E. Safford/GSC	04/99	use cvg_cp instead of rename		*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 * T. Lee/SAIC		12/03	enabled environ. variable in prefs.tbl	*
 ***********************************************************************/
{
int    ier;
long   iflen;
char   newfile[128],savfile[128];
struct stat buf;
/*---------------------------------------------------------------------*/

    cfl_inqr(work_file, NULL, &iflen, newfile, &ier);

    if ( stat(newfile, &buf) == 0 ) {

	if ( (size_t)iflen > (sizeof(VG_HdrStruct)+sizeof(FileHeadType)) ) { 

            /*
             * non-empty WORK_FILE, rename it to WORK_FILE.save
             */
            sprintf(savfile, "%s.save", newfile);
	    cvg_cp (newfile, savfile, 1, &ier);
	    remove (newfile);

	}
	else { 

	    /* 
	     * empty WORK_FILE, remove it
	     */
	    remove(newfile);
	}
    }

}
