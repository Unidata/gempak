#include "geminc.h"
#include "gemprm.h"

void ctb_wrcpf ( char *cpf_name, int np, float *cplat, float *cplon, 
		 int *iret )
/************************************************************************
 * ctb_wrcpf                                                            *
 *                                                                      *
 * This subroutine writes the Cursor Point lat/lon to a file (CPF).	*
 *                                                                      *
 * ctb_wrcpf ( cpf_name, np, cplat, cplon, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      *cpf_name	char		CPF filename			*
 *      np              int             Number of Cursor Points		*
 *      *cplat          float           Cursor Points latitude          *
 *      *cplon          float           Cursor Points longitude         *
 *                                                                      *
 * Output parameters:                                                   *
 *      *iret           int             Return code                     *
 *                                        0  = Normal			*
 *                                        -1 = Error		        *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		 2/03	Coding					* 
 * D.W.Plummer/NCEP	 3/03	Add cpf_name to calling sequence	*
 ***********************************************************************/
{
        static char *header = "!nmap2.cpf\n!\n!Cursor Point File (CPF)\t"
                               __DATE__
                               "\n!\n!This file is created by NMAP2, "
                               "DO NOT EDIT\n!\n";

        FILE    *cpfp;
	char	buffer[18];
        int     i, ier;

/*---------------------------------------------------------------------*/
	*iret = 0;

	cpfp = cfl_wopn (cpf_name, &ier );
	cfl_writ ( cpfp, (int)strlen(header), (unsigned char *)header, &ier );

	for ( i = 0; i < np; i++ ) {
	    sprintf ( buffer, "%7.2f; %7.2f\n", cplat[i], cplon[i] );
	    cfl_writ ( cpfp, sizeof(buffer)-1, (unsigned char *)buffer, &ier );
	}

	if ( ier != 0 ) {
	    *iret = -1;
	}

	cfl_clos ( cpfp, &ier );
}
