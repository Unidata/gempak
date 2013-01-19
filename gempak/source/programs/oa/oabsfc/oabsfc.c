#include "geminc.h"
#include "gemprm.h"

int main ( void )
/************************************************************************
 * OABSFC								*
 *									*
 * This program performs a Barnes analysis for surface data.		*
 *									*
 **									*
 * Log:									*
 * R. Tian/SAIC		 4/05	Modified from grphgd.c			*
 * M. Li/SAIC		05/05   Set device				*
 * D.W.Plummer/NCEP	05/05	Move DG lib initialization to OABSDR	*
 * T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
 ***********************************************************************/
{
int	respnd, iperr, done, one=1, ier, iret;
float	*gelat, *gelon, *coslat, *buffer;
int	infoflg=G_TRUE;
int     iunit, itype;
char	device[13], dfilnam[128];
float	xsize, ysize;
size_t	nbytes=LLMXTG;
/*---------------------------------------------------------------------*/

    /*
     * Initialize TAE.
     */
    ip_init ( &respnd, &iperr );
    gd_init ( &ier );

    if ( iperr == G_NORMAL )  {

	ip_idnt ( "OABSFC", &ier, strlen("OABSFC") );
	done = G_FALSE;

    }
    else  {

	iperr = -1;
	done = G_TRUE;

    }

    /*
     * Initialize GEMPLT in order to set grid navigation later.
     */
    if ( done == G_FALSE )  {

	gg_init ( &one, &iret );

	if ( iret != G_NORMAL )  {
	    iperr = -3;
	    done = G_TRUE;
	}
	else  {
	    strcpy ( device, "GN" );
	    iunit = 1;
	    strcpy ( dfilnam, "OABSFC" );
	    itype = 1;
	    xsize = 500.0F;
	    ysize = 500.0F;
	    gsdeva ( device, &iunit, dfilnam, &itype, &xsize, &ysize, &ier,
		     strlen(device), strlen(dfilnam) );
	}
    }

    /*
     * Main loop.
     */
    while ( done == G_FALSE ) {

	G_MALLOC ( gelat,  float, nbytes, "Error allocating gelat" );
	G_MALLOC ( gelon,  float, nbytes, "Error allocating gelon" );
	G_MALLOC ( coslat, float, nbytes, "Error allocating coslat" );
	G_MALLOC ( buffer, float, nbytes, "Error allocating buffer" );

	oabsdr ( gelat, gelon, coslat, buffer, &infoflg, &ier );

	G_FREE ( gelat, float );
	G_FREE ( gelon, float );
	G_FREE ( coslat, float );
	G_FREE ( buffer, float );

	/*
	 * Call dynamic tutor.
	 */
	ip_dynm ( &done, &ier );

    }

    /*
     * Final error messages.
     */
    if ( iperr != G_NORMAL )  {
	er_wmsg ( "OABSFC", &iperr, " ", &ier, strlen("OABSFC"), strlen(" ") );
    }

    ip_exit ( &iret );

    return 0;

}
