#include "lyrdiag.h"

void dl_driv ( char *lfunc, int *iret )
/************************************************************************
 * dl_driv								*
 *									*
 * This subroutine scans the GFUNC/GDPFUN user input for LYR_ functions	*
 * and calls the appropriate layer function subroutine for each one	*
 * found.								*
 *									*
 * dl_driv  ( lfunc, iret )						*
 *									*
 * Input and Output parameters:						*
 *	*lfunc		char		LYR_ function			*
 *									*
 * Output parameters:							*
 *	*iret		int		Return code			*
 *					  0 = normal return		*
 *					 -1 = operator not recognized	*
 *					 -6 = ensemble in layer diag.	*
 **									*
 * Log:									*
 * T. Lee/SAIC		03/05						*
 * S. Gilbert/NCEP	12/05	Translation from Fortran		*
 ************************************************************************/
{
	char		llfunc[LLMXLN], errst[61], inglev[LLMXLN];
	char		uarg[LLMXLN], stprm[LLMXLN];
	char		*cpos, *tpos, f[17];
	int		ier, nval, ldlevl1, ldlevl2;
	int		done, ipos, jpos, kpos, npos, lenc, lens;
/*----------------------------------------------------------------------*/
	*iret = 0;

        /*
         *	Check if LFUNC is layer function.  If not, returns.
         */
	cst_lcuc ( lfunc, lfunc, &ier );
	cpos = strstr ( lfunc, "LYR_" );
        ipos = (int) (cpos - lfunc);
	if ( ( cpos == 0 ) || ( strlen(lfunc) == 0 ) )
	    return;
	else {
            cst_lstr ( lfunc, &lenc, &ier);
            memset ( llfunc, 0, LLMXLN );
	    cst_ncpy (llfunc, lfunc, lenc, &ier);
        }

        /*
         *	Save DGCMN block variables, ldlevl (2) to local variables.
         */
        nval = 1;
        dg_iget ( "LDLEVL1", &nval, &ldlevl1, iret );
        dg_iget ( "LDLEVL2", &nval, &ldlevl2, iret );
	dg_cget ( "INGLEV", inglev, iret );

        /*
         *	Parse the layer function and call the appropriate subroutine.
         */
	done = 0;
	while ( done == 0 ) {
            cst_nocc ( llfunc+ipos, '(', 1, 1, &jpos, &ier);
	    npos = ipos + jpos + 1;
	    st_opcl ( llfunc, &npos, llfunc, &kpos, &ier,LLMXLN,LLMXLN );
            cst_ncpy ( f, llfunc+(ipos+4), npos-ipos-5, &ier );
            cst_ncpy ( uarg, llfunc+npos, kpos-npos-1, &ier );

            /*	    
             *	    If a layer function is embedded in layer diagnostics,
             *	    return with an error.
             */
	    tpos = strstr ( uarg, "LYR_" );
	    if  ( tpos != 0 )  {
	 	*iret = -6;
		er_wmsg ( "DL", iret, " ", &ier, 2, 1 );
		return;
	    }

	    strcpy (errst, "" );
	    if  ( strcmp( f, "SWTM" ) == 0 ) {
		dl_swtm ( uarg, inglev, stprm, iret );
	    } else if ( strcmp ( f, "MXMN" ) == 0 ) {
		dl_mxmn ( uarg, inglev, stprm, iret );
	    } else if ( strcmp ( f, "FVONISFC" ) == 0 ) {
		dl_fvonisfc ( uarg, inglev, stprm, iret );
	    } else {
		*iret  = -1;
		strcpy ( errst, f );
		done  = 1;
		er_wmsg ( "DL", iret, errst, &ier,2,strlen(errst) );
	    }

	    if  ( *iret == 0 )  {
                cst_rmbl ( stprm, stprm, &lens, &ier );
                strcat ( stprm, llfunc+kpos );
                cst_lstr ( stprm, &lens, &ier);
		if  ( ipos > 0 )  {
		    cst_ncpy( llfunc, llfunc, ipos, &ier );
		    strcat ( llfunc, stprm );
		}
		else {
		    cst_ncpy ( llfunc, stprm, lens, &ier );
		}
            } else if ( strcmp(errst, f ) != 0 )  {
                strcat ( errst, " in " );
                strcat ( errst, f );
                dg_cset ( "ERRST", errst, &ier );
		done = 1;
		er_wmsg ( "DL", iret, errst, &ier, 2,strlen(errst) );
	    }

	    cpos = strstr ( llfunc, "LYR_" );
            ipos = (int) (cpos - llfunc);
	    if  ( cpos == 0 )  done = 1;
	}
		
        memset ( lfunc, 0, lenc );
	strcpy (lfunc, llfunc );

        /*
         *	Restore ldlevl (2) values.
         */
        nval = 1;
        dg_iset ( "LDLEVL1", &nval, &ldlevl1, &ier );
        dg_iset ( "LDLEVL2", &nval, &ldlevl2, &ier );

	return;
}
