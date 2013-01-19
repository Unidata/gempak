#include "cvgcmn.h"
#include "pgprm.h"

void cvg_getfname ( char *fname, int *iret )
/************************************************************************
 * cvg_getfname                                                     	*
 *                                                                      *
 * Build product filename from VG Filename using the first three	*
 * leters of the the VG file name with a dat extension.			*
 *									*
 * cvg_getfname ( fname, iret )						*
 *									*
 * Input parameters:                                                    *
 *									*
 * Output parameters:                                                   *
 *  *fname	char	Filename					*
 *  *iret	int	Return Code					*
 *									*
 **									*
 * Log:									*
 * F . J. Yen/NCEP	 3/99	Renamed from pgqpf_getfname.		*
 * S. Law/GSC		03/00	stopped  "NoVGFileName" to prefix3 copy	*
 * S. Law/GSC		03/00	pgfilw_qFilename -> pgfilw_getFileName	*
 * T. Lee/SAIC          11/03   added user directory to work_file       *
 * T. Lee/SAIC		11/03	used cvgcmn.h				*
 ***********************************************************************/
{
    char	fnm[256], prefix3[4];
/*---------------------------------------------------------------------*/

    *iret = 0;

    pgfilw_getFileName (0, fnm);

    if (strlen (fnm) == (size_t)0 ||
	strncmp (fnm, work_file, strlen (work_file)) == 0) {
	strcpy (fname, "NoVGFileName.dat");
    }
    else {
	cst_ncpy (prefix3, fnm, 3, iret);
	sprintf (fname, "%s.dat", prefix3);
    }
}

/*=====================================================================*/
