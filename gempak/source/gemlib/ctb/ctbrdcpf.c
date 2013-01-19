#include "geminc.h"
#include "gemprm.h"

void ctb_rdcpf ( char *cpf_name, int *np, float *cplat, float *cplon, 
                 int *iret )
/************************************************************************
 * ctb_rdcpf                                                            *
 *                                                                      *
 * This subroutine reads the Cursor Point lat/lon from a file (CPF).	*
 *                                                                      *
 * ctb_rdcpf ( cpf_name, np, cplat, cplon, iret )			*
 *                                                                      *
 * Input parameters:                                                    *
 *      *cpf_name       int             CPF filename			*
 *                                                                      *
 * Output parameters:                                                   *
 *      *np             int             Number of Cursor Points		*
 *      *cplat          float           Cursor Points latitude          *
 *      *cplon          float           Cursor Points longitude         *
 *      *iret           int             Return code                     *
 *                                        0  = Normal			*
 *                                        -1 = Error		        *
 **                                                                     *
 * Log:                                                                 *
 * R. Tian/SAIC		 2/03	Coding					* 
 * D.W.Plummer/NCEP	 3/03	Add cpf filename to calling sequence	*
 ***********************************************************************/
{
        FILE    *cpfp;
	char	buffer[18];
	float	latlon[2];
        int     numval, ier;

/*---------------------------------------------------------------------*/
	*iret = 0;
	ier = 0;
	*np = 0;

	cpfp = cfl_ropn (cpf_name, NULL, &ier );
	while ( ier == 0 ) {
	    cfl_trln ( cpfp, sizeof(buffer), buffer, &ier );
	    if ( ier != 0 ) 
		break;
	    cst_rlst ( buffer, ';', RMISSD, 2, latlon, &numval, &ier );
	    cplat[*np] = latlon[0];
	    cplon[*np] = latlon[1];
	    (*np)++;
	}

/*
 *	Return code 4 is EOF reached.
 */
	if ( ier != 4 ) {
	    *iret = -1;
	}

	cfl_clos ( cpfp, &ier );
}
